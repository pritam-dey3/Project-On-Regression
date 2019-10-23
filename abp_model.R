#final code writing by me
abp_models <- function (X, resp_var, trafos = NULL) {
  base_model <- lm(resp_var ~ X)
  plot(cooks.distance(base_model))
  outliers <- as.integer(strsplit(readline("Enter outliers: "), " ")[[1]])  
  
  #detecting outliers
  while(length(outliers) != 0) {
    resp_var <- resp_var[-outliers]
    X <- X[-outliers, ]
    base_model <- lm(resp_var ~ X)
    plot(cooks.distance(base_model))
    outliers <- as.integer(strsplit(readline("Enter outliers: "), " ")[[1]])  
  }

  results <- matrix(ncol = 6, nrow = 0)
  
  for (tr in trafos) {
    temp_model <- trafo_lm(base_model, trafo = tr)$trafo_mod
    criterias <- abp_summary(temp_model)
    results <- rbind(results, as.character(c(transf = tr, criterias)))
  }
  colnames(results) <- c("transf", "rss", "adjusted_r", "aic", "kappa", "cp")
  write.csv(results, "table3.csv")
}