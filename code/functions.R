# Estimate test model
est_pos_test_model = function(data, max_lag = 7, date_start = ymd("2020-05-01")) {
  mod_data = data
  for(i in 1:max_lag) {
    mod_data = mutate(mod_data, "n_pcr_pos_lag_{i}" := lag(x = n_pcr_pos, i),
                      "n_pcr_lag_{i}" := lag(x=n_pcr, i))
  }
  # Model formulas
  lag_comb = lapply(1:max_lag, function(x) {
    combn(c("n_pcr_pos", paste0("n_pcr_pos_lag_", 1:max_lag)), x)
  })
  formula_lin = unlist(lapply(lag_comb, function(x) apply(x, MARGIN = 2, function(y) paste0("n_cases ~ -1+",
                                                                                            paste0(y, collapse = "+")))))
  formula_time_vary = unlist(lapply(lag_comb, function(x) apply(x, MARGIN = 2, function(y) paste0("n_cases ~ -1+",
                                                                                                  paste0(
                                                                                                    paste0("s(t, by=", y,")"),
                                                                                                    collapse = "+")))))
  formulas = c(formula_lin, formula_time_vary)
  
  mod_data = mod_data %>% filter(date>= date_start) %>%
    mutate(t=1:n())
  models = lapply(formulas, function(x) {
    tryCatch(mgcv::gam(formula = as.formula(x), data = mod_data), error = function(e) e)
  })
  model_min_BIC = models[[which.min(unlist(lapply(models, function(x) tryCatch(BIC(x), error = function(e) NA))))]]
  # Predict number of examined persons based on number of pcr tests
  pred_data = mod_data
  pred_data$n_pcr_pos = pred_data$n_pcr
  for(i in 1:max_lag) {
    pred_data[,paste0("n_pcr_pos_lag_", i)] = pred_data[,paste0("n_pcr_lag_", i)]
  }
  
  res_data = mod_data %>% select(date, t, n_pcr_pos, n_pcr, n_cases) %>%
    mutate(n_cases_pred = predict(model_min_BIC), 
           n_exam_pers=predict(model_min_BIC, newdata = pred_data))
  list(model_min_BIC, res_data)
}

# Adjust case numbers based on assumed sensitivity, specificity, observed number of cases and 
# number of examined persons
adjust_n_case = function(n_cases, n_exam_pers, sens, spec) {
  pmax(((n_cases - n_exam_pers*(1 - spec)) /
          (sens + spec - 1)) , 0)
}

# Adjust case-specific imputed data for false-positive cases
adjust_case_data = function(imputed_data, tib_n_cases_adj, seed = 123) {
  dates = unlist(tib_n_cases_adj$date)
  imputed_dt = data.table(imputed_data %>% dplyr::filter(rep_date_local %in% dates))
  
  sampleGroup<-function(df,group, size) {
    df[sample(nrow(df),size=size[group]),]
  }
  size = round(unlist(tib_n_cases_adj$n_cases_adj))
  names(size) = unlist(tib_n_cases_adj$date)
  imputed_dt[,rep_date_local := as.character(rep_date_local)]
  set.seed(seed)
  dat_adj = imputed_dt[, sampleGroup(.SD, group = .BY[[1]], size = size), by=rep_date_local]
  dat_adj[,rep_date_local := ymd(rep_date_local)]
  dat_adj
}

