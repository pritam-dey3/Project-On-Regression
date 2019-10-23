##required packages lmvar, car, olsrr, trafo, mass
model1 <- lm(volact ~ 1 + race + fire + theft + age + income, data = project_data)
kappa(model1)
vif(model1)
#income is creatinng problems
cor(explanatory_variables1)
#remove income
model2 <- lm(volact ~ 1 + race + fire + age + income, data = project_data_WO)
kappa(model2)
vif(model2)
summary(model2)
plot(model2$fitted.values, model2$residuals)

#now bring in trafo
assumptions(model2)
#wow!!! look at modulus transformation
model3 <- trafo_lm(model2, trafo = "modulus")
summary(model3$trafo_mod)
plot(model3)
plot(model3$fitted.values, modulus_y)

model3 <- trafo_lm(object = model2, trafo = "sqrtshift") 
summary(model3)
plot(model3)
vif(model3)

model4 <- lm(involact ~ 1 + race + fire + theft + age + income, data = project_data_WO ) 
summary(model4)
plot(model4)
plot(model4$fitted.values, project_data_WO$involact)

model4 <- trafo_lm(object = model2, trafo = "log") 
summary(model4)
plot(model4)
vif(model4)

fivenum(project_data$theft)
fivenum(project_data$theft[c(-24, -6, -7)])
fivenum(project_data$race)
fivenum(project_data$fire)
        
model5 <- lm(fire ~ race, data = project_data[-24,])
assumptions(model5)
model5 <- trafo_lm(model5, trafo = "log")
summary(model5)
plot(model5)

#including ridge regression
model6 <- cv.glmnet()
summary(model6)

#mallow's cp calculation
ols_mallows_cp(model4$trafo_mod, model2)
ols_mallows_cp(model3$trafo_mod, model2)
#aic calculation
ols_aic(model2)
ols_aic(model4$trafo_mod)
ols_aic(model3$trafo_mod)
#RSS

#multiplot
library(lattice)
splom(project_data[,-1])
