#new package: mctest
#firstly fit the whole model and see what happens

#model including all the variables
X <- model.matrix(~ 1 + race + age + theft + fire + involact + income, data = project_data)
y <- project_data[["volact"]]

M_full <- lm(y ~ X)
plot(M)
#two outliers can be seen
#remove outliers
X <- model.matrix(~ 0 + race + age + theft + fire + involact + income, data = project_data[-c(24, 7), ])
y <- project_data[["volact"]][-c(24, 7)]
M <- lm(y ~ X)
abp_summary(M, kap = TRUE)
summary(M)
#kappa value is high
#lots of multi-collinearity can be seen from multiplot
#firstly scale down the income column to have better estimates
data <- cbind(project_data[,6], project_data[, 2:5], project_data[,7], project_data[,8] / 1000)
names(data)[[1]] <- "volact"
names(data)[[6]] <- "involact"
names(data)[[7]] <- "income"
#we will reduce variables one by one till we have a satisfactory kappa value
#otherwise the estimates will not make any sense
X <- model.matrix(~ 1 + race + age + fire + income + involact, data = data[, ])
y <- project_data[["volact"]][]
M <- lm(y ~ X)
plot(M)
#remove outlier
X <- model.matrix(~ 1 + race + age + fire + income + involact, data = data[-7, ])
y <- project_data[["volact"]][-7]
M <- lm(y ~ X)
abp_summary(M, kap = TRUE)
#rss, and aic does not increase significantly, but mallows cp increases
#next we will remove involact, because it could also be explained by other variables
X <- model.matrix(~ 0 + race + age + fire + income, data = data[-7, ])
y <- project_data[["volact"]][-7] #we do not remove 7 because it has an outlier in income column
M <- lm(y ~ X)
plot(M) #no outliers
abp_summary(M, kap = TRUE)
#rss, and aic increases by very small amount
#next we note that race and fire are highly related with each other
#and their relation with volact is also similar
#so we are going to remove race first, to check how fire, income, age affects volact
X <- model.matrix(~ 0 + age + fire + income, data = data[-c(7, 23), ])
y <- project_data[["volact"]][-c(7, 23)] #we do not remove 7 because it has an outlier in income column
M <- lm(y ~ X)
plot(M) #23 is an influential point
abp_summary(M, kap = TRUE)
#aic increases negligibly, cp increases significantly
#now we will run through various transformations of y to see the best fit
trans <- c("boxcox", "bickeldoksum", "logshiftopt", "yeojohnson", "sqrtshift", "manly", "modulus", "dual", "gpower", "log", "glog", "neglog", "reciprocal")
abp_models(X, y, trafos = trans)

#lots of good models... now we include race instead of fire and would compare
#different models
X <- model.matrix(~ 0 + race + age + income, data = data[-c(7), ])
y <- data[["volact"]][-c(7)] #we do not remove 7 because it has an outlier in income column
M <- lm(y ~ X)
plot(M) #no outliers
abp_summary(M, kap = TRUE)
#race makes a better model than fire

cor(data[["race"]][-c(7, 23)], data[["fire"]][-c(7, 23)])
#highly nonlinear relationship
summary(M)
abp_models(X, y, trans)
plot(M$fitted.values, y)
abline(0, 1)

#lets include involact now
#we want to check if including involact increases efficiency of the model
X <- model.matrix(~ 0 + race + age + income + involact, data = data[-c(7), ])
y <- data[["volact"]][-c(7)] #we do not remove 7 because it has an outlier in income column
M <- lm(y ~ X)
plot(M) #no outliers
abp_summary(M, kap = TRUE)
abp_models(X, y, trans)
#including involact decreases rss little bit but increases collinearity, 
#that would create trouble in estimating process
#so we proceed with race, age, income

assumptions(M)
#normality tests and homoscadasticity tests satisfy

#we are done finding good model!
y <- logshiftopt(M)$zt
X <- model.matrix(~ 0 + race + age + income, data = data[-c(7), ])
splom(cbind(y, X))
#explains relation with race, age and income
#also explains why we should remove involact

M <- lm(y ~ X)
summary(M)
plot(M)

-------
X <- model.matrix(~ 1 + race + age + theft + fire + income, data = project_data[-c(24, 7), ])
y <- project_data[["volact"]][-c(24, 7)]
M <- lm(y ~ X)
abp_summary(M, kap = TRUE)

#notice the correlation between income and volact
cor(project_data[["volact"]][-7], project_data[["income"]][-7])
#minimum target for adjusted r2 should be 0.88

X <- model.matrix(~ 1 + income, data = project_data[])
y <- project_data[["volact"]][]
M <- lm(y ~ X)
plot(M)
#remove outliers
#also scale down the income column to have better estimate of coefficient
#of income
X <- model.matrix(~ 0 + income, data = data[-7,])
y <- data[["volact"]][-7]
M <- lm(y ~ X)
plot(M)
abp_summary(M, kap = FALSE)
summary(M)

trans <- c("boxcox", "bickeldoksum", "logshiftopt", "yeojohnson", "sqrtshift", "manly", "modulus", "dual", "gpower", "log", "glog", "neglog", "reciprocal")
