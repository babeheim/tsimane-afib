
# sample() when only one item in the bag
sample_safe <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

sample_cats <- function(x, cats) {
  cats <- as.character(cats)
  cat_list <- sort(unique(cats))
  out <- x
  for (i in seq_along(cat_list)) {
    tar <- which(cats == cat_list[i])
    out[tar] <- sample_safe(x[tar])
  }
  return(out)
}

### Functions for means/medians/SEs/SDs etc ###

calc_comparisons <- function(variable, label, data) {

  check_formula <- as.formula(paste0(variable, "~checked"))
  t_check <- t.test(check_formula,data=data)

  data_checked <- data[data$checked == 1,]
  incidence_formula <- as.formula(paste0(variable, "~incidence"))
  t_incidence <- t.test(incidence_formula,data=data_checked)

  list(
    `Variable` = label,
    `N not sampled` = sum(data$checked == 0 & !is.na(data[[variable]])),
    `Not Sampled Mean` = sprintf("%.1f", t_check$estimate[["mean in group 0"]]),
    `N sampled` = sum(data$checked == 1 & !is.na(data[[variable]])),
    `Sampled Mean` = sprintf("%.1f", t_check$estimate[["mean in group 1"]]),
    `Significance` = sprintf("%.3f", t_check$p.value),
    `N not resampled` = sum(data_checked$incidence == 0 & !is.na(data_checked[[variable]])),
    `Not Resampled Mean` = sprintf("%.1f", t_incidence$estimate[["mean in group 0"]]),
    `N resampled` = sum(data_checked$incidence == 1 & !is.na(data_checked[[variable]])),
    `Resampled Mean` = sprintf("%.1f", t_incidence$estimate[["mean in group 1"]]),
    `Significance2` = sprintf("%.3f", t_incidence$p.value)
  ) %>% flatten() -> out
  out$Significance2[out$Significance2 == "0.000"] <- "<0.001"
  out$Significance[out$Significance == "0.000"] <- "<0.001"
  return(out)
}

calc_age_pvalue <- function(variable, data) {
  model <- as.formula(paste(variable, "age", sep = "~"))
  sprintf("%.3f", anova(lm(model, data = data))[["Pr(>F)"]][1])
}

gmean <- function(x, na.rm = TRUE){
  exp(sum(log(x), na.rm = na.rm)/sum(!is.na(x)))
}

mean_se <- function(x, prop=FALSE, digs="%.1f") {
  paste0(
    sprintf(digs, mean(x, na.rm = TRUE)*100^as.numeric(prop)),
    " (",
    sprintf(digs, 100^as.numeric(prop)*sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))),
    ")"
  )
}

mean_sd <- function(x, prop=FALSE, digs="%.1f") {
  paste0(
    sprintf(digs, mean(x, na.rm = TRUE)*100^as.numeric(prop)),
    " (",
    sprintf(digs, sd(x, na.rm = TRUE)*100^as.numeric(prop)),
    ")"
  )
}

median_mad <- function(x, prop=FALSE, digs="%.1f") {
  paste0(
    sprintf(digs, median(x, na.rm = TRUE)*100^as.numeric(prop)),
    " (",
    sprintf(digs, mad(x, na.rm = TRUE)*100^as.numeric(prop)),
    ")"
  )
}

adj_mean_sd <- function(x, by, weights, digs="%.1f"){

  mn <- aggregate(x, by=list(by), function(.) mean(.,na.rm=T))$x
  vari <- aggregate(x, by=list(by), function(.) var(.,na.rm=T))$x
  paste0(
    sprintf(digs, sum(mn * weights)),
    " (",
    sprintf(digs, sqrt(sum(vari * weights))),
    ")"
  )
}

adj_mean_se <- function(x, by, weights, digs="%.1f"){

  mn <- aggregate(x,by=list(by),function(.)mean(.,na.rm=T))$x
  vari <- aggregate(x,by=list(by),function(.)var(.,na.rm=T)/sum(!is.na(.)))$x
  paste0(
    sprintf(digs, sum(mn * weights)),
    " (",
    sprintf(digs, sqrt(sum(vari * weights^2))),
    ")"
  )
}
