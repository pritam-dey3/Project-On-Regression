#selecting models
model4 <- lm(involact ~ 1 + fire + age + income, data = project_data)
assumptions(model4)

model3_1 <- trafo_lm(model3, trafo = "dual")
abp_summary(model3_1$trafo_mod)
# rss    adjusted_r           aic            cp         kappa 
# 4.995220e-01  8.709183e-01  7.193491e+01 -3.119678e+01  1.645639e+05
model3_2 <- trafo_lm(model3, trafo = "manly")
abp_summary(model3_2$trafo_mod)

model2_3 <- trafo_lm(model3, trafo = "sqrtshift")
abp_summary(model2_3$trafo_mod)

#considering involact as response
o <- which(project_data$involact != 0)#remove rows with zero involacts
model5 <- lm(involact ~ 1 + race + fire + age + income, data = project_data[o,])
assumptions(model4)

fivenum(project_data$involact)
hist(project_data$involact[o])

model5_1 <- trafo_lm(model4, trafo = "boxcox")
abp_summary(model5)
#rss    adjusted_r           aic            cp         kappa 
#0.2454727     0.3012300     6.6466171   -23.3570953 78892.7385336

model_ultimate <- lm(volact ~ 1 + race + fire + age + income + involact, data = project_data[c(-24, -7, -6), ])
assumptions(model_ultimate)
model_u <- trafo_lm(model_ultimate, trafo = "sqrtshift")
abp_summary(model_u$trafo_mod)
