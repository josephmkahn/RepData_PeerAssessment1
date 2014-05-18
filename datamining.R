library(e1071)  #  <-- Do once to get into the work area.

# Simulation parameters.
Niter <- 1000
Nx.per.y <- 10
mux0 <- rep(0, Nx.per.y)
mux1 <- c(rep(1, Nx.per.y / 2), rep(0, Nx.per.y / 2))
Ny0 <- 50
Ny1 <- 50
Ny <- Ny0 + Ny1

error = rep(0, Niter)
for (i in 1 : Niter) { #i =1
  
  # Generate training data set with Ny0 observations from one class and Ny1
  # observations from the other.
  x0 <-
    matrix(rnorm(Ny0 * Nx.per.y, mean = mux0), ncol = Nx.per.y, byrow = TRUE)
  
  y0 <- c(rep(0, Ny0))
  x1 <-
    matrix(rnorm(Ny1 * Nx.per.y, mean = mux1), ncol = Nx.per.y, byrow = TRUE)
  
  y1 <- c(rep(1, Ny1))
  xdat1 <- rbind(x0, x1)
  ydat1 <- c(y0, y1)
  traindat <- data.frame(x = xdat1, y = as.factor(ydat1))
  
  # Fit default support vector classifier.
  svmfit <- svm(y ~ ., data = traindat)
  
  # Generate test data set.
  x0test <-
    matrix(rnorm(Ny0 * Nx.per.y, mean = mux0), ncol = Nx.per.y, byrow = TRUE)
  
  y0test <- c(rep(0, Ny0))
  x1test <-
    matrix(rnorm(Ny1 * Nx.per.y, mean = mux1), ncol = Nx.per.y, byrow = TRUE)
  
  y1test <- c(rep(1, Ny1))
  xdat1test <- rbind(x0test, x1test)
  ydat1test <- c(y0, y1)
  testdat <- data.frame(x = xdat1test, y = as.factor(ydat1test))
  
  # Predict the class labels of these test observations using default svmfit.
  # and calculate the error
  ypred <- predict(svmfit, testdat)
  error[i] <- sum(as.character(ypred) != as.character(testdat$y)) / Ny
} # next i
hist(error)
mean(error)
sd(error)
