
rm(list = ls())

### Required packages

library(longformer) # https://github.com/babeheim/longformer
library(testthat)
library(dplyr)
library(purrr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

source("support_functions.r")

#################
# load database #
#################

db <- load_database("thlhp_db/data")
# commit 7385cb2 from dec 2020

print("database loaded")

t <- read.csv("./input/tsimane_pids_in_afib_sample.csv", stringsAsFactor = FALSE)

##########################
# define starting sample #
##########################

# one row per person
# drawing on info from their first ECG visit, ONLY

cens <- db$visits
cdm <- db$id_changes
reg <- db$people
med <- db$medical_cheqseg

ecg <- db$ecg_inventory

ecg$age_ecg <- round(ecg$age,2)
ecg$age_ecg[ecg$age_ecg == 0] <- NA

ecg$date_of_birth <- reg$date_of_birth[match(ecg$pid, reg$pid)] 
ecg$age <- round(as.numeric(as.Date(ecg$scan_date) - as.Date(ecg$date_of_birth))/365,2)

ecg$age[is.na(ecg$age)] <- ecg$age_ecg[is.na(ecg$age)]
ecg <- ecg[which(!is.na(ecg$pid)),]

## Also extract the unseen sample, who fit our criteria but we never saw for whatever reason
cens$reg_dob <- reg$date_of_birth[match(cens$pid, reg$pid)]
cens$age <- as.numeric(as.Date(cens$date)-as.Date(cens$reg_dob))/365
cens$date_of_death[cens$date_of_death==""] <- NA
reg$date_of_death[reg$date_of_death==""] <- NA

included_tsi <- which(
  cens$population=="Tsimane" & 
  cens$community %in% cens$community[which(cens$vid %in% ecg$vid)] &
	cens$age >= 40 &
	(as.Date(cens$date_of_death)> as.Date(cens$date) | is.na(cens$date_of_death)) &
	cens$has_data &
	cens$date > "2005-10-01" &
	cens$date < "2019-09-13" &
	!cens$pid %in% ecg$pid
)

included_mos <- which(
  reg$population=="Moseten" & 
  as.numeric(as.Date("2018-01-01")-as.Date(reg$date_of_birth))/365 >= 40 & 
	is.na(reg$date_of_death) &
	!reg$pid %in% ecg$pid &
	reg$pid %in% med$pid
)

d <- reg[reg$pid %in% c(ecg$pid, unique(cens$pid[included_tsi]),reg$pid[included_mos]),]

d <- select(d,
  pid,
  midpid_16,
  first_name,
  last_name_1,
  last_name_2,
  male,
  yrborn_source,
  date_of_birth,
  date_of_death,
  father_pid,
  father_name,
  population
)

d$has_ecg <- as.numeric(d$pid %in% ecg$pid)
expect_true(nrow(d) == 2934)

# patients who have been checked by a cardiologist:
# are either on the t$pid list OR a are Moseten with an ECG
tsimane_checked <- t$pid
d$checked <- as.numeric(d$pid %in% tsimane_checked |
  d$pid %in% reg$pid[reg$population == "Moseten"] & d$pid %in% ecg$pid)

expect_true(sum(d$checked) == 1867)

d$first_ecg_vid <- NA
d$last_ecg_vid <- NA
d$first_over40_ecg_vid <- NA
d$n_ecg_visits <- 0
d$ecg <- 0

for (i in 1:nrow(d)) {
  my_rows <- which(ecg$pid == d$pid[i])
  
  if (length(my_rows) > 0) {
    d$ecg[i] <- length(my_rows)
    d$n_ecg_visits[i] <- length(unique(ecg$vid[my_rows]))
    first_row <- my_rows[which.min(as.Date(ecg$scan_date[my_rows]))]
    d$first_ecg_vid[i] <- ecg$vid[first_row]
    
    last_row <- my_rows[which.max(as.Date(ecg$scan_date[my_rows]))]
    d$last_ecg_vid[i] <- ecg$vid[last_row]

    my_over40_rows <- which(ecg$pid == d$pid[i] & ecg$age >= 40)
    if (length(my_over40_rows) > 0) {
      first_over40_row <- my_over40_rows[which.min(as.Date(ecg$scan_date[my_over40_rows]))]
      d$first_over40_ecg_vid[i] <- ecg$vid[first_over40_row]
    }
  }
}

d$first_ecg_date <- ecg$scan_date[match(d$first_ecg_vid, ecg$vid)]
d$last_ecg_date <- ecg$scan_date[match(d$last_ecg_vid, ecg$vid)]

d$obs_time <- as.numeric(as.Date(d$last_ecg_date) - as.Date(d$first_ecg_date))

d$first_ecg_age <- round(as.numeric(as.Date(d$first_ecg_date) - as.Date(d$date_of_birth))/365, 2)
d$last_ecg_age <- round(as.numeric(as.Date(d$last_ecg_date) - as.Date(d$date_of_birth))/365, 2)

# incidence subset has more than one ecg visit
d$incidence <- as.numeric(d$n_ecg_visits > 1)

expect_true(!any(d$obs_time < 0,na.rm=T))
expect_true(!any(d$obs_time_over40 < 0, na.rm = TRUE))

expect_true(mean(is.na(d$first_ecg_age)) < 0.11)
expect_true(abs(mean(d$first_ecg_age, na.rm = TRUE) - 47.3) < 0.1) 

expect_true(all(d$obs_time[d$n_ecg_visits > 1] > 45))
expect_true(all(d$obs_time[d$n_ecg_visits == 1] == 0))

# now calculate just the time spent over 40

d$first_over40_ecg_date <- NA
tar <- which(!is.na(d$first_over40_ecg_vid))
d$first_over40_ecg_date[tar] <- ecg$scan_date[match(d$first_over40_ecg_vid[tar], ecg$vid)]
d$first_over40_ecg_age <- round(as.numeric(as.Date(d$first_over40_ecg_date) - as.Date(d$date_of_birth))/365, 2)
d$obs_time_over40 <- as.numeric(as.Date(d$last_ecg_date) - as.Date(d$first_over40_ecg_date))

expect_true(all(d$first_ecg_date <= d$first_over40_ecg_date, na.rm = TRUE))
expect_true(all(d$first_over40_ecg_date <= d$last_ecg_date, na.rm = TRUE))
expect_true(all(d$first_ecg_age <= d$first_over40_ecg_age, na.rm = TRUE))
expect_true(all(d$first_over40_ecg_age <= d$last_ecg_age, na.rm = TRUE))

print("ecg sample defined")

####################
# extract ecg data #
####################

ecg <- ecg[which(!duplicated(ecg$obs_id)),]

d$pr <- NA
d$qrs <- NA
d$hr <- NA
d$afib_first_visit <- NA
d$afib_any_visit <- NA
d$afib_last_visit <- NA

for(i in 1:nrow(d)){


  first_ecg_visit <- which(ecg$vid==d$first_ecg_vid[i])
  first_ecg_visit <- first_ecg_visit[which.max(as.Date(strptime(ecg$scan_time[first_ecg_visit],"%H:%M:%S")))]
  
  last_ecg_visit <- which(ecg$vid==d$last_ecg_vid[i])
  last_ecg_visit <- last_ecg_visit[which.max(as.Date(strptime(ecg$scan_time[last_ecg_visit],"%H:%M:%S")))]
  
  if (length(first_ecg_visit) > 0) {
    d$pr[i] <- ecg$pr[first_ecg_visit]
    d$qrs[i] <- ecg$qrs[first_ecg_visit]
    d$hr[i] <- ecg$heart_rate[first_ecg_visit]
    d$afib_first_visit[i] <- as.numeric(grepl("AF",ecg$rhythm[first_ecg_visit]))
    d$afib_last_visit[i] <- as.numeric(grepl("AF",ecg$rhythm[last_ecg_visit]))
    d$afib_any_visit[i] <- as.numeric(any(grepl("AF",ecg$rhythm[which(ecg$pid == d$pid[i])])))
  }
}
d$incident_afib <- as.numeric(d$afib_first_visit==0 & d$afib_any_visit[i]==1)

expect_true(abs(mean(d$pr, na.rm = TRUE) - 157.1) < 0.1)
expect_true(mean(is.na(d$pr)) < 0.118)

expect_true(abs(mean(d$qrs, na.rm = TRUE) - 102.4) < 0.1)
expect_true(mean(is.na(d$qrs)) < 0.107)

print("ecg data added")

##############################
# extract anthropometry data #
##############################

a <- db$anthropometry

d$waist <- NA
exclusion.score <- 0

for(i in 1:nrow(d)){
  my.obs <- a$Cinturna[which(a$pid==d$pid[i])]
  drop <- which(is.na(my.obs) | abs(my.obs - mean(my.obs, na.rm=TRUE)) > 20)
  if(length(drop)>0) my.obs <- my.obs[-drop]
  exclusion.score <- exclusion.score + length(drop)
  d$waist[i] <- mean(my.obs, na.rm=TRUE)
}

expect_true(exclusion.score == 5599)
expect_true(abs(mean(d$waist, na.rm = TRUE) - 86.1) < 0.1)
expect_true(mean(is.na(d$waist)) < 0.446)

print("anthropometry data added")

########################
# extract medical data #
########################

m <- db$medical_cheqseg

# clean systolic blood pressure
m$PAenHGSys <- gsub(",", ".", m$PAenHGSys)
m$PAenHGSys <- gsub("'", "", m$PAenHGSys)
m$PAenHGSys <- gsub("\\}", "", m$PAenHGSys)
m$PAenHGSys[m$PAenHGSys == "0"] <- NA
m$PAenHGSys[m$PAenHGSys == "10"] <- NA
m$PAenHGSys[m$PAenHGSys == "000"] <- NA
m$PAenHGSys[m$PAenHGSys == ""] <- NA
m$PAenHGSys <- as.numeric(m$PAenHGSys)
m$PAenHGSys[which(m$PAenHGSys > 300 | m$PAenHGSys < 50)] <- NA

# clean diastolic blood pressure
m$PAenHGDias[m$PAenHGDias == "60."] <- "60.0"
m$PAenHGDias <- gsub(",", ".", m$PAenHGDias)
m$PAenHGDias <- gsub("'", "", m$PAenHGDias)
m$PAenHGDias <- gsub("\\}", "", m$PAenHGDias)
m$PAenHGDias[m$PAenHGDias == "0"] <- NA
m$PAenHGDias[m$PAenHGDias == "10"] <- NA
m$PAenHGDias[m$PAenHGDias == "000"] <- NA
m$PAenHGDias[m$PAenHGDias == ""] <- NA
m$PAenHGDias <- as.numeric(m$PAenHGDias)
m$PAenHGDias[which(m$PAenHGDias<30 | m$PAenHGDias>120)] <- NA

# clean height
m$TallaDePie[m$TallaDePie == "b"] <- ""
m$TallaDePie[m$TallaDePie == "f"] <- ""
m$TallaDePie <- gsub(",", ".", m$TallaDePie)
m$TallaDePie <- gsub("\\.\\.", ".", m$TallaDePie)
m$TallaDePie <- gsub("\\+", "", m$TallaDePie)
m$TallaDePie <- gsub("\\|", "", m$TallaDePie)
m$TallaDePie <- gsub("G", "", m$TallaDePie)
m$TallaDePie <- gsub("14.9.4                                  b", "", m$TallaDePie)
m$TallaDePie <- gsub("158.4.119", "158.4", m$TallaDePie)
m$TallaDePie <- gsub("161.437.4", "161.4", m$TallaDePie)
m$TallaDePie[m$TallaDePie == ""] <- NA
m$TallaDePie <- as.numeric(m$TallaDePie)
m$TallaDePie[which(m$TallaDePie > 250 | m$TallaDePie < 110)] <- NA

m$TallaDePie[which(m$TallaDePie < 130 & m$age_reg > 18)] <- NA
m$TallaDePie[which(m$TallaDePie > 180 & m$age_reg < 18)] <- NA

tar <- which(m$TallaDePie > 110 + 5 * m$age_reg)
m$TallaDePie[tar] <- NA

drop <- which(m$vid %in% c("7GMW7R", "TLYP3M", "BTJPCA"))
m$TallaDePie[drop] <- NA

# clean weight
m$Peso <- gsub(",", ".", m$Peso)
m$Peso <- gsub("\\.\\.", ".", m$Peso)
m$Peso <- gsub("\\|", "", m$Peso)
m$Peso <- gsub("\\{", "", m$Peso)
m$Peso <- gsub("^\\.", "", m$Peso)
m$Peso <- gsub("^\\/\\/", "", m$Peso)
m$Peso <- as.numeric(m$Peso)
m$Peso[which(m$Peso > 130 | m$Peso < 30)] <- NA

exclusion.score <- 0

for(i in 1:nrow(d)){
  my.obs <- m$PAenHGSys[which(m$MidPID==d$pid[i])]
  drop <- which(is.na(my.obs) | abs(my.obs - mean(my.obs, na.rm=TRUE)) > 50)
  if(length(drop)>0) my.obs <- my.obs[-drop]
  exclusion.score <- exclusion.score + length(drop)
  d$systolic_bp[i] <- mean(my.obs, na.rm=TRUE)
}

expect_true(exclusion.score == 1414)
expect_true(abs(mean(d$systolic_bp, na.rm = TRUE) - 113.6) < 0.1)
expect_true(mean(is.na(d$systolic_bp)) < 0.04)

# Diastolic BP: exclue obs above 120 or less than 30, exclude obs if more than 50 away from person"s mean

exclusion.score <- 0

for(i in 1:nrow(d)){
  my.obs <- m$PAenHGDias[which(m$MidPID==d$pid[i])]
  drop <- which(is.na(my.obs) | abs(my.obs - mean(my.obs, na.rm=TRUE)) > 50)
  if(length(drop)>0) my.obs <- my.obs[-drop]
  exclusion.score <- exclusion.score + length(drop)
  d$diastolic_bp[i] <- mean(my.obs, na.rm=TRUE)
}

expect_true(exclusion.score == 1410)
expect_true(abs(mean(d$diastolic_bp, na.rm = TRUE) - 71.1) < 0.1)
expect_true(mean(is.na(d$diastolic_bp)) < 0.04)


# Weight - exclude obs > 200 kg or < 30 kg, obs greater than 30kg from person's mean

exclusion.score <- 0

for(i in 1:nrow(d)){
  my.obs <- m$Peso[which(m$MidPID==d$pid[i])]
  drop <- which(is.na(my.obs) | abs(my.obs - mean(my.obs, na.rm=TRUE)) > 30)
  if(length(drop)>0) my.obs <- my.obs[-drop]
  exclusion.score <- exclusion.score + length(drop)
  d$weight[i] <- mean(my.obs, na.rm=TRUE)
}

expect_true(exclusion.score == 1176)
expect_true(abs(mean(d$weight, na.rm = TRUE) - 58.6) < 0.1)
expect_true(mean(is.na(d$weight)) < 0.05)

# Height - exclude obs > 250 cm or < 110 cm, obs greater than 20 cm from person's mean

exclusion.score <- 0

for(i in 1:nrow(d)){
  my.obs <- m$TallaDePie[which(m$MidPID==d$pid[i])]
  drop <- which(is.na(my.obs) | abs(my.obs - mean(my.obs, na.rm=TRUE)) > 20)
  if(length(drop)>0) my.obs <- my.obs[-drop]
  exclusion.score <- exclusion.score + length(drop)
  d$height[i] <- mean(my.obs, na.rm=TRUE)
}

expect_true(exclusion.score == 943)
expect_true(abs(mean(d$height, na.rm = TRUE) - 154.1) < 0.1)
expect_true(mean(is.na(d$height)) < 0.35)

d$bmi <- as.numeric(d$weight) / (as.numeric(d$height)/100)^2

expect_true(abs(mean(d$bmi, na.rm = TRUE) - 24.5) < 0.1)
expect_true(mean(is.na(d$bmi)) < 0.051)

print("medical data added")


#####################################
# extract biochem  and quimica data #
#####################################
q <- db$quimica
q$ldl_combined <- as.numeric(q$ldl)
q$ldl_combined[is.na(q$ldl_combined)] <- as.numeric(q$ldl2[is.na(q$ldl_combined)])

q$chol_combined <- as.numeric(q$chemcolesterol)
q$chol_combined[is.na(q$chol_combined)] <- as.numeric(q$chemcolesterol2[is.na(q$chol_combined)])

q$hdl_combined <- as.numeric(q$chemhdl)
q$hdl_combined[is.na(q$hdl_combined)] <- as.numeric(q$chemhdl2[is.na(q$hdl_combined)])

q$trig_combined <- as.numeric(q$chemtrigliceridos)
q$trig_combined[is.na(q$trig_combined)] <- as.numeric(q$chemtrigliceridos2[is.na(q$trig_combined)])


b <- db$biochem

b$ldl_combined <- as.numeric(b$c_calc_ldl)
b$ldl_combined[is.na(b$ldl_combined)] <- q$ldl_combined[match(b$vid[is.na(b$ldl_combined)],q$vid)]
b$ldl_combined[which(b$ldl_combined < 20 | b$ldl_combined > 250)] <- NA


b$hdl_combined <- as.numeric(b$c_hdl)
b$hdl_combined[is.na(b$hdl_combined)] <- q$hdl_combined[match(b$vid[is.na(b$hdl_combined)],q$vid)]
b$hdl_combined[which(b$hdl_combined < 20 | b$hdl_combined > 250)] <- NA

b$chol_combined <- as.numeric(b$c_colesterol)
b$chol_combined[is.na(b$chol_combined)] <- q$chol_combined[match(b$vid[is.na(b$chol_combined)],q$vid)]
b$chol_combined[which(b$chol_combined < 80 | b$chol_combined > 500)] <- NA

b$trig_combined <- as.numeric(b$c_trigliceridos)
b$trig_combined[is.na(b$trig_combined)] <- q$trig_combined[match(b$vid[is.na(b$trig_combined)],q$vid)]
b$trig_combined[which(b$trig_combined<20 | b$trig_combined>500)] <- NA

b$vsg_combined <- as.numeric(b$c_vsg)
b$vsg_combined[is.na(b$vsg_combined)] <- b$vsg[is.na(b$vsg_combined)]


b$c_eosinophils <- as.numeric(b$c_eosinophils)
b$wbc <- as.numeric(b$wbc)
b$wbc[which(b$wbc>100)] <- NA
# wbc: exclude any over 100

d$ldl <- NA
d$esr <- NA
d$eosinophils <- NA
d$cholesterol <- NA
d$wbc <- NA
d$hdl <- NA
d$triglycerides <- NA

for(i in 1:nrow(d)){
  my_obs <- which(b$pid==d$pid[i])
  d$ldl[i] <- mean(b$ldl_combined[my_obs], na.rm=TRUE)
  d$hdl[i] <- mean(b$hdl_combined[my_obs], na.rm=TRUE)
  d$cholesterol[i] <- mean(b$chol_combined[my_obs], na.rm=TRUE)
  d$esr[i] <- mean(b$vsg_combined[my_obs], na.rm=TRUE)
  d$wbc[i] <- mean(b$wbc[my_obs], na.rm=TRUE)
  d$triglycerides[i] <- mean(b$trig_combined[my_obs], na.rm=TRUE)
}

expect_true(mean(is.na(d$ldl)) < 0.418)
expect_true(abs(mean(d$ldl, na.rm = TRUE) - 90.2) < 0.1) 

expect_true(mean(is.na(d$hdl)) < 0.416)
expect_true(abs(mean(d$hdl, na.rm = TRUE) - 37.3) < 0.1) 

expect_true(mean(is.na(d$cholesterol)) < 0.361) 
expect_true(abs(mean(d$cholesterol, na.rm = TRUE) - 145.5) < 0.1)

expect_true(mean(is.na(d$esr)) < 0.11)
expect_true(abs(mean(d$esr, na.rm = TRUE) - 31.3) < 0.1)

expect_true(mean(is.na(d$wbc)) < 0.33)
expect_true(abs(mean(d$wbc, na.rm = TRUE) - 10.4) < 0.1)

expect_true(mean(is.na(d$triglycerides)) < 0.37)
expect_true(abs(mean(d$triglycerides, na.rm = TRUE) - 112.7) < 0.1)

print("biochem data added")

####################
# extract crp data #
####################

biov15 <- db$biochem_v15CS_ucsb

biov15$crp <- as.numeric(biov15$crp_mgdl)
d$crp <- NA
for(i in 1:nrow(d)){
  my.obs <- biov15$crp[which(biov15$pid == d$pid[i])]
  d$crp[i] <- gmean(my.obs, na.rm=TRUE)
}

expect_true(abs(mean(d$crp, na.rm = TRUE) - 3.31) < 0.1)
expect_true(mean(is.na(d$crp)) < 0.671)

print("crp data added")


#####################
# extract echo data #
#####################

echo <- read.csv("./input/Echos_All_Compacted_8Sep2014.csv", stringsAsFactors=FALSE)

echo$EF <- as.numeric(echo$EF.mod.bp)
echo$EF[which(is.na(echo$EF))] <- as.numeric(echo$EF.mod.sp4[which(is.na(echo$EF))])
echo$EF[which(is.na(echo$EF))] <- as.numeric(echo$EF.mod.sp2[which(is.na(echo$EF))])
echo$EF[which(is.na(echo$EF))] <- as.numeric(echo$EF.sp4.el[which(is.na(echo$EF))])
echo$EF[which(is.na(echo$EF))] <- as.numeric(echo$EF.teich[which(is.na(echo$EF))])

link <- match(d$midpid_16, echo$pid)
d$LA.vol <- as.numeric(echo$LA.vol[link])
d$LA.vol.index <- as.numeric(echo$LA.vol.index[link])
d$EF <- as.numeric(echo$EF[link])

expect_true(mean(is.na(d$EF)) < 0.69) 
expect_true(abs(mean(d$EF, na.rm = TRUE) - 65.9) < 0.1)

expect_true(mean(is.na(d$LA.vol)) < 0.71)
expect_true(abs(mean(d$LA.vol, na.rm = TRUE) - 66.0) < 0.1)

expect_true(mean(is.na(d$LA.vol.index)) < 0.71)
expect_true(abs(mean(d$LA.vol.index, na.rm = TRUE) - 41.3) < 0.1)

print("echo data added")


#####################
# extract cyto data #
#####################

biov15$il6[which(biov15$il6=="< 3.2")] <- 1.6
biov15$il6[which(biov15$il6=="< 3.2")] <- 1.6
biov15$il6 <- as.numeric(biov15$il6)

d$il6 <- NA
for(i in 1:nrow(d)){
  my.obs <- biov15$il6[which(biov15$pid==d$pid[i])]
  d$il6[i] <- gmean(my.obs, na.rm=TRUE)
}

expect_true(mean(is.na(d$il6)) < 0.675)
expect_true(abs(mean(d$il6, na.rm = TRUE) - 3.99) < 0.1)

print("cytokine data added")


####################
# extract CAC data #
####################

ct <- db$ct_chest

d$cac <- NA
for(i in 1:nrow(d)){
  if (d$pid[i] %in% ct$pid) d$cac[i] <- mean(ct$cac[which(ct$pid == d$pid[i])], na.rm=TRUE)
}

expect_true(mean(is.na(d$cac)) < 0.55) # ???
expect_true(abs(mean(d$cac, na.rm = TRUE) - 15.8) < 0.1) # up from 10

print("ct CAC data added")


################################
# Calculate derived variables  #
################################

# construct an age at first visit using date of birth and the first visit
d$age <- d$first_over40_ecg_age

# if a Moseten was never seen for ecg, use their age on Jan. 1, 2018
tar <- which(d$has_ecg==0 & d$population=="Moseten")
d$age[tar] <- as.numeric(as.Date("2018-01-01")-as.Date(d$date_of_birth[tar]))/365

# if a Tsimane was never seen for ecg, use their average age from the census records
censages <- aggregate(cens$age[included_tsi], by=list(cens$pid[included_tsi]), mean)
tar <- which(d$has_ecg==0 & d$population=="Tsimane")
d$age[tar] <- censages$x[match(d$pid[tar], censages$Group.1)]

drop <- which(is.na(d$age))
expect_true(length(drop) == 505)
d <- d[-drop,] # 505 patients dropped for lack of age at start of test

# bin ages into 10-year categories
age_cats <- c(40, 50, 60, 70, 80, 120)
cat_labels <- c("40-49", "50-59", "60-69", "70-79", "80+")
d$agecats <- cut(d$age, breaks = age_cats, right = FALSE, labels = cat_labels)

# re-anonymize pids
pids <- sort(unique(d$pid))
d$pid <- match(d$pid, pids)

d <- select(d,
  -midpid_16, -first_name, -last_name_1, -last_name_2, -yrborn_source,
  -date_of_birth, -father_pid, -father_name, -first_ecg_vid, -last_ecg_vid,
  -first_over40_ecg_vid, -first_ecg_date, -last_ecg_date,
  -first_ecg_age, -last_ecg_age, -first_over40_ecg_date, -first_over40_ecg_age,
  -date_of_death)

# convert waist circumference to inches
d$waist <- d$waist / 2.54

# add risk categories

d$waist_risk <- NA
d$waist_risk[which(d$male == 1)] <- as.numeric(d$waist[which(d$male == 1)] > 35)
d$waist_risk[which(d$male == 0)] <- as.numeric(d$waist[which(d$male == 0)] > 40)

d$bmi_risk <- as.numeric(d$bmi > 30)

d$cholesterol_risk <- as.numeric(d$cholesterol > 240)
d$ldl_risk <- as.numeric(d$ldl > 130)
d$hdl_risk <- as.numeric(d$hdl < 40)
d$triglycerides_risk <- as.numeric(d$triglycerides > 200)

d$crp_risk <- as.numeric(d$crp > 3)
d$esr_risk <- NA
d$esr_risk[which(d$male == 1)] <- as.numeric(d$esr[which(d$male == 1)] > 22)
d$esr_risk[which(d$male == 0)] <- as.numeric(d$esr[which(d$male == 0)] > 29)

d$systolic_bp_risk <- as.numeric(d$systolic_bp > 120)
d$diastolic_bp_risk <- as.numeric(d$diastolic_bp > 80)
d$hypertension <- as.numeric(d$systolic_bp > 130 | d$diastolic_bp > 80) 

d$pr_risk <- as.numeric(d$pr > 200)
d$qrs_risk <- as.numeric(d$qrs > 120)

d$cac_zero <- as.numeric(d$cac == 0)
d$cac_risk <- as.numeric(d$cac > 100)

d$log_il6 <- log(d$il6)


###################################################
# data disguise step: randomly permute each data column
# this preserves key summary statistics, but not all;

key <- paste(d$population, d$checked, d$incidence, d$agecat)
scramble_cols <- setdiff(colnames(d), c("age", "agecat", "checked", "incidence", "population"))

for (i in seq_along(scramble_cols)) {
  d[[scramble_cols[i]]] <- sample_cats(d[[scramble_cols[i]]], key)
}

# one more reshuffle
d <- d[sample(1:nrow(d)),]

# note that publication results in Gurven, et al. (2021)
# did not use this step!
# un-scrambled data is available upon request
###################################################

# save to file

write.csv(d, "./analysis_data.csv", row.names=FALSE)

print("analysis table created")
