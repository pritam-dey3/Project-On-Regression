library(readxl)
project_data <- read_excel("E:/study/project/project_data.xlsx")
View(project_data)
model1 <- lm(project_data$volact ~ 1 + project_data$race + project_data$fire + project_data$theft +
             project_data$age + project_data$involact + project_data$income, data = project_data)
summary(model1)
plot(model1)
