eval_model <- function(mod, df_train, df_test){
  df_train <- df_train |> drop_na()
  df_test <- df_test |> drop_na()
  
  df_train$fitted <- predict(mod, newdata = df_train)
  df_test$fitted  <- predict(mod, newdata = df_test)

  metrics_train <- df_train |> yardstick::metrics(GPP_NT_VUT_REF, fitted)
  metrics_test  <- df_test  |> yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  rmse_train <- metrics_train |> filter(.metric == "rmse") |> pull(.estimate)
  rsq_train  <- metrics_train |> filter(.metric == "rsq")  |> pull(.estimate)
  
  rmse_test <- metrics_test |> filter(.metric == "rmse") |> pull(.estimate)
  rsq_test  <- metrics_test |> filter(.metric == "rsq")  |> pull(.estimate)

  plot_1 <- ggplot(df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(title = "Training set",
         subtitle = bquote(italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                             RMSE == .(format(rmse_train, digits = 3)))) +
    theme_classic()
  
  plot_2 <- ggplot(df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(title = "Test set",
         subtitle = bquote(italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                             RMSE == .(format(rmse_test, digits = 3)))) +
    theme_classic()

  cowplot::plot_grid(plot_1, plot_2)
}
