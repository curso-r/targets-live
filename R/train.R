
telco_url <- function() {
  "https://raw.githubusercontent.com/curso-r/intro-ml-mestre/master/dados/telco.csv"
}


split_data <- function(data, seed = 32) {
  set.seed(seed)
  telco_initial_split <- data %>% 
    select(-customerID) %>%
    initial_split(prop = 0.8, strata = "Churn")
  
  telco_initial_split
}


data_recipe <- function(train_data) {
  recipe(Churn ~ ., train_data) %>% 
    step_corr(all_numeric_predictors()) %>%
    step_impute_mean(TotalCharges) %>% 
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
}


run_tune_grid <- function(wf, resamples, grid) {
  wf %>% 
    tune_grid(
      resamples = resamples,
      grid = grid,
      control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
    )
}