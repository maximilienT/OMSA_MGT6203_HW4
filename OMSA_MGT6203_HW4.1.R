install.packages("AER")
library(AER)

ed.data <- read.csv("Education_data.csv")
str(ed.data)


hist(ed.data$wage, breaks =50)
hist(log(ed.data$wage), breaks =50)

# 1. run simple model
ols.res <- lm(log(wage)~educ+exper+I(exper^2), data = ed.data)
summary(ols.res)

# 3. nearc4 as instrumental variable
stage1 <- lm(educ~nearc4+exper+I(exper^2), data = ed.data)
summary(stage1)

# 4. stage 2 results
stage2 <- lm(log(wage)~fitted(stage1)+exper+I(exper^2), data = ed.data)
summary(stage2)

# 5. package results match manual results
TSLS.res <- ivreg(log(wage)~educ+exper+I(exper^2)|nearc4+exper+I(exper^2), data = ed.data)
summary(TSLS.res)

# 6. compare simple model with IV model
cbind(coef(ols.res),coef(TSLS.res))
