# Assignment 4: Logistic Regression - Maxwell Martin - QMST 3334 - Spring 2020


# 1
disease <- read.table(file = "disease.txt", 
                      col.names = c("case", "age", "socioeconomic_status", 
                                    "sector", "disease_status", "savings"),
                      colClasses = c("factor", "numeric", "factor", "factor", "factor", "factor"))
head(disease)


# 2
m1 <- glm(disease_status ~ age + socioeconomic_status + sector + savings, 
          family = binomial("logit"), data = disease)
summary(m1)


# 2(b)
exp(1.52320) # The estimated change in odds of having contracted the disease when from sector 2.


# 2(d)
1-(m1$deviance/m1$null.deviance)


# 3
m2 <- glm(disease_status ~ sector + age, family = binomial("logit"), data = disease)
summary(m2)


# 3(b)
1-(m2$deviance/m2$null.deviance)


# 4(a)
newdata <- data.frame(sector=c(1), age=c(25))
newdata$sector <- as.factor(newdata$sector)
loddsratio <- predict(m2, newdata=newdata, type = "link")
loddsratio
prob_sector1 <- 1 / (1 + exp(-loddsratio))
prob_sector1


# 4(b)
newdata <- data.frame(sector=c(2), age=c(25))
newdata$sector <- as.factor(newdata$sector)
loddsratio <- predict(m2, newdata=newdata, type = "link")
loddsratio
prob_sector2 <- 1 / (1 + exp(-loddsratio))
prob_sector2