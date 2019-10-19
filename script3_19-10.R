#function that takes some model and calculates rss, mallows cp, aic, adjusted r2
abp_summary <- function(model, full.model = model1) {
  s <- summary(model)
  rss <- s$sigma
  adjusted_r <- s$adj.r.squared
  aic <- ols_aic(model)
  cp <- ols_mallows_cp(model, full.model)
  plot(model)
  criterias <- c(rss, adjusted_r, aic, cp)
  names(criterias) <- c("rss", "adjusted_r", "aic", "cp")
  criterias
}

#testing function
model3 <- trafo_lm(object = model2, trafo = "boxcox") 
abp_summary(model3$trafo_mod)
