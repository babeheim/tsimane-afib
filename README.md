tsimane-afib
============

Analysis materials for Rowan, C.J., Eskander, M.A., Seabright, E., Rodriguez, D.E., Linares, E.C., Gutierrez, R.Q., Adrian, J.C., Cummings, D., Beheim, B., Tolstrup, K., Achrekar, A., Kraft, T., Michalik, D.E., Miyamoto, M.I., Allam, A.H., Wann, L.S., Narula, J., Trumble, B.C., Stieglitz, J., Thompson, R.C., Thomas, G.S., Kaplan, H.S. and Gurven, M.D., 2021. Very Low Prevalence and Incidence of Atrial Fibrillation among Bolivian Forager-Farmers. *Annals of Global Health*, 87(1), p.18. DOI: http://doi.org/10.5334/aogh.3252

This repository contains two analysis R scripts:

- `1_prep_data.R` - query/cleaning script to create de-identified `analysis_data.csv`
- `2_create_summary_tables.R` - prepares all manuscript figures and tables, starting from `analysis_data.csv`

There is an additional script, `support_functions.R`, which contains bespoke functions.

To run `2_create_summary_tables.R`, use R v4.0.1 or greater and install the following packages:

```r
library(testthat) # 3.0.1 or greater
library(dplyr) # 1.0.2 or greater
library(purrr) # 0.3.4 or greater
library(DiagrammeR) # 1.0.6.1 or greater
library(DiagrammeRsvg) # 0.1 or greater
library(rsvg) # 2.1 or greater
```

Before public upload, we also randomly re-ordered each column of data in such a way to preserve univariate summary statistics, but protect the privacy of participants - the original data used to produce the exact publication output is available upon request.

This code was written by [Bret Beheim](https://github.com/babeheim) and [Ed Seabright](https://github.com/edseab), and is available under Creative Commons [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). See LICENSE.md for details.

[![License: CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)
