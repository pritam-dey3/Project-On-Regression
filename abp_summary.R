#function that takes some model and calculates rss, mallows cp, aic, adjusted r2
abp_summary <- function(model, full.model = M_full, kap = TRUE) {
  s <- summary(model)
  rss <- s$sigma^2 * s$df[2]
  adjusted_r <- s$adj.r.squared
  aic <- ols_aic(model)
  x = model$model[, -1]
  y = model$model[, 1]
  if(kap)
    kap <- omcdiag(x[, -1], y)$odiags[6][1]
  else
    kap <- 0
  #plot(model)
  cp <- ols_mallows_cp(model, M_full)
  criterias <- c(rss, adjusted_r, aic, kap, cp)
  names(criterias) <- c("rss", "adjusted_r", "aic", "kappa", "cp")
  criterias
}

abp_kappa <- function(model) {
  X <- as.matrix(model$model[, -1])
  eigen_values <- eigen(t(X) %*% X)$values
  sqrt(max(eigen_values) / min(eigen_values))
}

