# Assignment 3: Model Selection - Maxwell Martin - QMST 3334 - Spring 2020


# 1
roofing <- read.table("RoofingShingles.DAT", 
                     col.names = c("Sales", "PromoExp", "ActiveAcc", "NumCompBrands", 
                                   "DistPotential"))
head(roofing)


# 2
library(car)
scatterplotMatrix(~Sales + PromoExp + ActiveAcc + NumCompBrands + DistPotential, 
                  smooth=FALSE, reg.line=TRUE, data = roofing)

# 3
m0 <- lm(Sales ~ PromoExp + ActiveAcc + NumCompBrands + DistPotential, data = roofing)
summary(m0)


# 4
cor(roofing)
m1 <- lm(Sales ~ NumCompBrands, data = roofing)
m2 <- lm(Sales ~ NumCompBrands + ActiveAcc, data = roofing)
m3 <- lm(Sales ~ NumCompBrands + ActiveAcc + DistPotential, data = roofing)
m4 <- lm(Sales ~ NumCompBrands + ActiveAcc + DistPotential + PromoExp, data = roofing)
anova(m1, m2, m3, m4)


# 5
summary(m2) # My chosen model
summary(m4) # The complete model