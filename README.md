# Analysis of COVID-19 case numbers: adjustment for diagnostic misclassification on the example of German case reporting data


This repository contains code and data needed in order to reproduce results presented in "Analysis of COVID-19 case numbers: adjustment for diagnostic misclassification on the example of German case reporting data". The person-specific data from Bavaria is not the original data set due to data privacy restriction, but a synthetic dataset with a similar structure. All results based on the person-specific Bavarian data (e.g., the results of the misclassification-adjusted case numbers and the estimated epidemic curves from nowcasting as well as the underlying estimated statistical models) do differ slightly from the reported results in the manuscript.

The repository contains three folders including code, data and (intermediate) results:

1. **`data`**:
  - `2020_09_21_bav_synth.csv` : synthetic person-specific case reporting data from Bavaria
  - `200921_test_bav.RData`: daily reported number of reported performed and positive PCR tests by Bavarian laboratories 
  - `201001_cases_test_germany.RData`:  Weekly number of reported SARS-CoV-2 case counts in Germany as well as the weekly number of reported PCR tests and positive reported PCR tests by Germany laboratories
  - **`nowcast_current`**: Results of (misclassification-adjusted) nowcasting in Bavaria based on data from January 21st, 2021 (Suppl. Figure 2)

2. **`code`**: 
  - `1_analysis_adj_case_counts.R`: Basic analyses and misclassification adjustments of case counts. Creates Figures 1-4 + Suppl Figure 1 of the manuscript and misclassification-adjusted person-specific Bavarian case reporting data for misclassification adjusted estimation of the epidemic curve
  - `2_nowcast_*.R`: Misclassification adjusted and unadjusted estimation of the epidemic curve based on bayesian hierarchical nowcasting model (for different assumptions regarding the specificity, unadjusted (spec=1), and spec=0.999, spec=0.997, spec=0.995). Additionally, the false-positive adjusted estimation of the time-varying effective reproduction number R(t) 
  - `3_summarise_nowcast_results.R`: Summarise the results of the misclassification-adjusted nowcast for different assumptions on sensitivity and specificity. Figures are created based on the nowcasting results on the synthetic data as well as the saved intermediate results of the original data.
  - `4_supp_fig_2.R`: Summary of the misclassification-adjusted nowcast based on Bavarian data from January, 21st, 2021. Creates Suppl. Figure 2
  - `analysis_fun_stanmodel.R`, `functions.R`, and the folder `general` contain helper functions for the misclassification adjustment and estimation of the Bayesian hierarchical nowcast via rstan as well as estimation of the time-varying R(t).
  
3. **`results`**:
  - **`figures`**: Includes the original figures of the publication as well as the figures created from appying the codes based on the original data from Germany and the synthetic data from Bavaria
 - **`nowcast_bav`** Intermediate and final results for the estimation of the nowcast: imputed person-specific synthetic data (unadjusted and false-positive adjusted), results of the nowcast (based on original data and synthetic data as created from running the code files), results of the estimation of the time-varying effective reproduction number R(t)
  


The whole analysis can be reproduced in one go by running

```r
source("run-analyses.R")
```

Note that the estimation of the Bayesian hierarchical model takes a considerable amount of time, and can be skipped. Figures can be created based on saved intermediate results.