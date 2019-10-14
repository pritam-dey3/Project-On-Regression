#importing the dataset
library(readxl)
project_data <- read_excel("E:/study/project/project_data.xlsx")
View(project_data)

#initial model
model1 <- lm(project_data$volact ~ 1 + project_data$race + project_data$fire + project_data$theft +
             project_data$age + project_data$involact + project_data$income, data = project_data)

summary(model1)
plot(model1) #explanation of the plots needed..!!

#remove two outliers blindly
project_data_WO <- project_data[c(-24, -7),] #data without outliers

model2 = lm(project_data_WO$volact ~ 1 + project_data_WO$race + project_data_WO$fire +
              project_data_WO$theft + project_data_WO$age + project_data_WO$involact + 
              project_data_WO$income, data = project_data_WO)
plot(model2)

#residuals vs y_hat graph showed some sort of pattern
b = (model1$residuals^2) / (1 - hatvalues(model1)) 
plot(model1$fitted.values, b, xlab = "Fitted Values") #this gives a better picture for non-constant variance
#this plot matches seberly pg:283 fig (a)


#10/10/19
plot(model2$residuals)

e_star <- model2$residuals + model2$coefficients[2] * project_data_WO[,2]
added_variable <- data.frame(project_data_WO[,2], e_star)
plot(added_variable)

par(mfrow = c(2,3))
for (i in 2:8) {
  if (i == 6)
    next
  e_star <- model2$residuals + model2$coefficients[i] * project_data_WO[,i]
  added_variable <- data.frame(project_data_WO[,i], e_star)
  plot(added_variable)
}
# observed vs fitted plot<- our model is good
plot(model1$fitted.values, project_data$volact, ylab= "fitted", xlab = "observed")
abline(a = 0, b = 1, col = "purple")
plot(project_data$volact)
lines(model1$fitted.values, col = "green")


#acf and pacf plot<- there is no indiaction of dependence among the residuals
acf(model1$residuals, lag.max = NULL, type = "correlation", plot = TRUE) 
pacf(model1$residuals, lag.max = NULL, plot = TRUE)

#test for non-constant variance
ols_test_breusch_pagan(model2)

#finding a smoother
plot(model2$residuals)
lw <- loess(model2$residuals^2 ~ model2$fitted.values)
plot(model2$residuals^2, )

#14/10/19
explanatory_variables <- model.matrix(~ race + fire + theft + age + involact + income - 1, data = project_data_WO)
response_variable <- project_data_WO$volact
model3 <- lmvar(y = response_variable, X_mu = explanatory_variables,
                X_sigma = explanatory_variables, check_hessian = TRUE)
summary(model3) #ridiculously small estimates... each getting rejected!!
y_hat3 <- cbind(vec1, explanatory_variables) %*% model3$coefficients_mu

plot(y_hat3, response_variable)
abline(a = 0, b = 1)
cor(y_hat3, response_variable) #0.903
qqPlot(response_variable - y_hat3) #requires car package ##what kind of qq plot is this!!!

#trying with log transformation
log_response <- log(project_data_WO$volact)
model4 <- lm(log_response ~ 1 + explanatory_variables)
summary(model4)
kappa(model4)
vif(model2)
#except for theft and age everything seems to be correlated
#particularly race, involact and income
#we need to check more models through this