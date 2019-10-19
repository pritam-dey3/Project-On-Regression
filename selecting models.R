#selecting models
model2 <- lm(volact ~ 1 + race + fire + theft + age + income, data = project_data[c(-7, -24),])
assumptions(model2)

model2_1 <- trafo_lm(model2, trafo = "dual")
abp_summary(model2_1$trafo_mod)
# rss    adjusted_r           aic            cp         kappa 
# 4.995220e-01  8.709183e-01  7.193491e+01 -3.119678e+01  1.645639e+05
model2_2 <- trafo_lm(model2, trafo = "manly")
abp_summary(model2_2$trafo_mod)

model2_3 <- trafo_lm(model2, trafo = "sqrtshift")
abp_summary(model2_3$trafo_mod)
