# Assignment 2: Linear Regression Matrix Algebra
# Maxwell Martin - QMST 3334.251 - Spring 2020


# Question 1.
  magazines <- read.table("magazines.csv", header = TRUE, sep = ",")
  
  
# Question 2.
  Y <- magazines[,1]
  head(Y)


# Question 3.
  X <- magazines[,-1]
  X <- cbind(Intercept = 1, X)
  class(X)
  X <- as.matrix(X)
  class(X)
  head(X)
  
  
# Question 4.
  XTX <- t(X) %*% X
  head(XTX)  


# Question 5.
  solve(XTX)
  round(solve(XTX) %*% XTX, 4) # The identity matrix, so inverse was found.
  
  
# Question 6.
  solve(XTX) %*% t(X) %*% Y
    

# Question 7.
  lm(AdRevenue ~ AdPages + SubRevenue + NewsRevenue, data = magazines)
    