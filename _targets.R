# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble", "tidymodels", "patchwork"),
  format = "rds"
)

tar_source()

########### The Targets List ############
list(
  tar_target(telco_data, readr::read_csv(telco_url())),
  tar_target(telco_initial_split, split_data(telco_data)),
  tar_quarto(eda_report, path = "eda_report.qmd"),

  tar_target(train, rsample::training(telco_initial_split)),
  tar_target(test, rsample::testing(telco_initial_split)),

  tar_target(boxplots_churn_train, boxplots_churn(train)),
  tar_target(boxplots_churn_test, boxplots_churn(test)),
  tar_target(boxplots_churn_test_png, boxplots_churn(test, save = TRUE), format = "file"),
  tar_target(boxplots_churn_test_train, wrap_plots(boxplots_churn_test, boxplots_churn_train)),

  tar_target(telco_recipe, data_recipe(train)),
  tar_target(telco_resamples, vfold_cv(train, v = 5)),
  tar_target(telco_wf, {
    workflow() %>% add_recipe(telco_recipe)
  }),

  # step1
  tar_target(tune_step1_wf, {
    set.seed(1)
    telco_model <- boost_tree(
      mtry = 0.8,
      trees = tune(),
      min_n = 5,
      tree_depth = 4,
      loss_reduction = 0,
      learn_rate = tune(),
      sample_size = 0.8
    ) %>%
      set_mode("classification") %>%
      set_engine("xgboost", nthread = 4, counts = FALSE)

    telco_wf %>%
      add_model(telco_model)
  }),
  tar_target(tune_step1_grid, {
    expand.grid(
      learn_rate = c(0.05, 0.3),
      trees = c(250, 500, 1000)
    )
  }),
  tar_target(tune_step1_results, run_tune_grid(tune_step1_wf, telco_resamples, tune_step1_grid))

  # step2

)



