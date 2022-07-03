security_config <- readxl::read_xlsx("../config/securities_summary.xlsx")

rates_summary <- security_config %>% 
  dplyr::filter(Module == "Rates") %>% 
  dplyr::mutate(Value = Value/1e9)


econ_summary <- security_config %>% 
  dplyr::filter(Module == "Economics") 
