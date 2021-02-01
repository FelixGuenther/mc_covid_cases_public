case_test_dat_ger = read_csv("../data/original_ger/test_data_01102020.csv")

case_test_dat_ger = case_test_dat_ger %>% 
  mutate(t=1:n()) %>%
  select(t, 
         KW = KW,
         n_pcr = AnzahlTestungen, 
         n_pcr_pos = Positivgetestet, 
         n_cases = Cases)
save(case_test_dat_ger, file = "../data/201001_cases_tests_germany.RData")
