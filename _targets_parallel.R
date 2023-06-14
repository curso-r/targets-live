# Load packages required to define the pipeline:
library(targets)
library(crew)

tar_option_set(
  controller = crew_controller_local(workers = 3),
  packages = c("parsnip", "titanic", "dplyr", "recipes")
)
# tar_source()

formula <- Survived ~ Pclass + SibSp + Fare + Sex + Pclass + Age

get_train_data <- function() {
  titanic_train %>%
    mutate(Survived = as.factor(Survived))
}

get_test_data <- function() {
  titanic_test
}

fit_dataprep <- function(data) {
  recipe(data, formula) %>%
    step_impute_bag(Age, Fare) %>%
    prep(training = data)
}

model_xgb_def <- function() {
  boost_tree(mode = "classification", engine = "xgboost", trees = 100)
}

model_rf_def <- function() {
  rand_forest(mode = "classification", engine = "randomForest", trees = 100)
}

model_lr_def <- function() {
  logistic_reg(mode = "classification", engine = "glmnet", penalty = 0.001)
}

fit_model <- function(model, data) {
  Sys.sleep(3)
  fit(model, formula, data = data)
}

pred <- function(fitted_model, data) {
  parsnip::predict.model_fit(fitted_model, new_data = data, type = "prob")
}

roc_plot_fun <- function(...) {
  preds <- list(...)
  browser()
}

list(
  tar_target(name = train, command = get_train_data()),
  tar_target(name = test, command = get_test_data()),

  tar_target(name = dataprep_recipe, command = fit_dataprep(train)),

  tar_target(name = model_xgb, command = model_xgb_def()),
  tar_target(name = model_rf, command = model_rf_def()),
  tar_target(name = model_lr, command = model_lr_def()),

  tar_target(name = train_prepped, command = bake(dataprep_recipe, train)),

  tar_target(name = fit_xgb, command = fit_model(model_xgb, train_prepped)),
  tar_target(name = fit_rf, command = fit_model(model_rf, train_prepped)),
  tar_target(name = fit_lr, command = fit_model(model_lr, train_prepped)),

  tar_target(name = test_prepped, command = bake(dataprep_recipe, test)),

  tar_target(name = pred_xgb, command = pred(fit_xgb, test_prepped)),
  tar_target(name = pred_rf, command = pred(fit_rf, test_prepped)),
  tar_target(name = pred_lr, command = pred(fit_lr, test_prepped)),

  tar_target(name = roc_plot, command = roc_plot_fun(pred_xgb, pred_rf, pred_lr))
)
