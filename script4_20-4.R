#seeing plots
avPlots(model_u$trafo_mod)
crPlots(model_u$trafo_mod)
summary(model_u$trafo_mod)

#ridge regression
X <- model.matrix(~ 1 + explanatory_variables)  
ridge_model <- cv.glmnet(X, response_variable)
opt_lambda <- ridge_model$lambda.min
ridge_fit <- ridge_model$glmnet.fit
summary(ridge_fit)
y_ridge <- predict(ridge_fit, s = opt_lambda, newx = X)
plot(y_ridge, response_variable)
abline(0, 1)

sst <- sum((response_variable - mean(response_variable))^2)
sse <- sum((y_ridge - response_variable)^2)

# R squared
rsq <- 1 - sse / sst
rsq

print_glmnet_coefs <- function(cvfit, s="lambda.min") {
  ind <- which(coef(cvfit, s=s) != 0)
  df <- data.frame(
    feature=rownames(coef(cvfit, s=s))[ind],
    coeficient=coef(cvfit, s=s)[ind]
  )
  kable(df)
}

print_glmnet_coefs(ridge_fit, opt_lambda)
