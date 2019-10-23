#ridge model fitting
model1 <- model2 <- lm(volact ~ 1 + race + fire + age + income, data = project_data[c(-7),])
plot(model1)
y_tr <- sqrtshift(model1)$zt

X <- model.matrix(~ 1 + race + fire + age + theft + income, data = project_data)  
ridge_model <- cv.glmnet(X, y_tr)
opt_lambda <- ridge_model$lambda.min
ridge_fit <- ridge_model$glmnet.fit
y_ridge <- predict(ridge_fit, s = opt_lambda, newx = X)
plot(y_ridge, y_tr)
abline(0, 1)

sst <- sum((y_tr - mean(y_tr))^2)
sse <- sum((y_ridge - y_tr)^2)

# R squared
rsq <- 1 - sse / sst
rsq

print_glmnet_coefs(ridge_fit, opt_lambda)
