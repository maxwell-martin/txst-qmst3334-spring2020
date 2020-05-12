# Assignment 1 - Maxwell Martin - QMST 3334.251 - Spring 2020

invoices <- read.table(file = "invoices.txt", header = TRUE)
head(invoices)
tail(invoices)

# Exercise 1
  library(ggplot2)
  ggplot(data = invoices, aes(x = Invoices, y = Time)) + geom_point()

# Exercise 2
  m1 <- lm(Time ~ Invoices, data = invoices)
  summary(m1)

# Exercise 3

  # a)
    confint(m1)
  
  # b)
    # Using an offset to test for Ho: B1 = 0.01
    m2 <- lm(Time ~ Invoices, data = invoices, offset = (0.01 * Invoices))
    summary(m2) # p-value = 0.126
    
    # Calculating t-value and using pt() to get p-value.
    b1 <- m1$coefficients[2]
    b1
    se.b1 <- summary(m1)$coefficients[4]
    se.b1
    t.value <- (b1 - 0.01)/se.b1
    t.value
    p.value = 2*pt(-abs(t.value), df=dim(invoices)[1]-2)
    p.value # p-value = 0.1257402
    
  # c)
    data_star <- data.frame(Invoices=120)
    predict(object = m1, newdata = data_star, interval = "prediction", level = 0.99)