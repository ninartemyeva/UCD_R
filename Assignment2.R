## SETTING UP WORKING DIRECTORY

setwd("/Users/nina/study/R")

######           Task 1: statistical modelling

admission <- read.csv("admission.csv")

# my_subset returns a data frame for training the model (removes one part of the data and returns the remaining data)
my_subset <- function(data, n, num, i, k) {
  if (i == 1){
    return (data[(num + 1):n, ]) 
  }
  if (i == k){
    return (data[1:(n-num), ])
  }
  return(data[c(1:(num*(i-1)),(i*num+1):n),])
}
### k-fold  cross  validation

kFoldValid <- function(x, y, k){ # the function takes explanatory vector, response vector and k
  data <- data.frame(explan = x, response = y)
  n <- nrow(data)
  num <- n/k # the number of observations in each piece of data
  for (i in 1:k){ 
    data_training <- my_subset(data,n,num,i,k) 
    mod <- glm(response ~ explan, data = data_training, family = binomial) # building a linear model
    test <- data[(num*(i-1)+1):(i*num),,drop = F] # test contains the removed data for the prediction
    # saving our prediction in new column 
    data$prediction[(num*(i-1)+1):(i*num)] <- round(predict(mod, newdata = test , type = "response"), 0)
  }
  return(mean(abs(data$prediction-data$response)))

}

# Test the kFoldValid function on differernt explanatory variables and k

kFoldValid(admission$gre,admission$admit,10)
kFoldValid(admission$gre,admission$admit,2)

kFoldValid(admission$gpa,admission$admit,10)
kFoldValid(admission$gpa,admission$admit,2)

kFoldValid(admission$rank,admission$admit,10)
kFoldValid(admission$rank,admission$admit,2)


######           Task 2: determinat of a matrix

# Matrices for the test

M <- matrix(rnorm(2^2), 2, 2)
M2 <- matrix(rnorm(3^2), 3, 3)
M3 <- matrix(rnorm(5^2), 5, 5)

# colNumber returns a vector of the column indices for finding a minor
colNumber <- function(i, N) { 
  if (i == 1){
    return (c((i + 1):N))
  }
  if (i == N){
    return (c(1:(N - 1)))
  }
  return(c(1:(i - 1), (i + 1):N))
}

# determin() computes determinant of the matrix
determin <- function(matr) {
  n <- dim(matr)
  N <- n[1] # finding determinant using the rows of the matrix
  if (N == 1) {
    return(matr[1,1])
  }
  det <- 0
  
  # the loop takes each element of the first row of the matrix
  # and multiplies it by the determinant of the minor and coefficient
  for (i in 1:N){ 
    a <- matr[1,i] 
    det <- det + a*((-1)^(i+1))*determin(matr[2:N, colNumber(i, N), drop = F])
  }
  return(det)
}

# The difference between the result of detrermin() and det() is 0
determin(M) - det(M)
determin(M2) - det(M2)
determin(M3) - det(M3)

######           Task 3: writing S3 methods

hardness <- read.csv("Hardness.csv", header = F, col.names = c("Sample", "Hardness"))

# hard() returns the object (list) of class shewhart containing
# Sample ID, Sample mu, and two vectors of 0's and 1's which indicate if the sample mean
# falls outside the warning limits or action limits
hard <- function(data, s, mu, sigma) {
  n <- nrow(data)/s
  l.out <- list()
  warn.limits <- c(mu - ((2*sigma)/sqrt(n)) , mu + ((2*sigma)/sqrt(n)))
  act.limits <- c(mu - ((3*sigma)/sqrt(n)) , mu + ((3*sigma)/sqrt(n)))                 
  out <- aggregate(data$Hardness, list(data$Sample), mean) # object out contains sample ID amd sample mean
  colnames(out) <- c("Sample_ID", "Sample_mu")
  # the information about whether each sample mean is outside the
  # warning and/or action limits.
  out$Warning <- ifelse((out$Sample_mu < warn.limits[1]) | (out$Sample_mu > warn.limits[2]), 1, 0)
  out$Action <- ifelse((out$Sample_mu < act.limits[1]) | (out$Sample_mu > act.limits[2]), 1, 0)
  l.out <- as.list(out)
  class(l.out) <- 'shewhart'
  return(l.out)
}
mout <- hard(hardness, 25, 127, 3.4)

# warningline() builds the warning limits on the plot
warningline <- function(mu, sigma, n) {
  abline(h = mu - ((2*sigma)/sqrt(n)), col = "pink", lty = "dotted" )
  abline(h = mu + ((2*sigma)/sqrt(n)), col = "pink", lty = "dotted" )
}

# actionline() builds the action limits on the plot
actionline <- function(mu, sigma, n){
  abline(h = mu - ((3*sigma)/sqrt(n)), col = "red", lty = "dotted" )
  abline(h = mu + ((3*sigma)/sqrt(n)), col = "red", lty = "dotted" )
}

# creating function plot() for the objects of class "shewhart"

plot.shewhart <- function(obj, mu, sigma, n, remove = "") {
  par(las = 1)
  #plotting means of samples and a line indicating mu
  plot(obj$Sample_ID, obj$Sample_mu, xlab = "Sample ID", ylab = "Sample mean", main = "Shewhart chart", xaxt = "n",  ylim = c((mu - 3*sigma),(mu + 3*sigma)))
  axis(1, at = pretty(obj$Sample_ID, n = 25))
  abline(h = mu, col = "green")

  # check the value of the argument 'remove' and drow the warning and the action limits according to it
  if (remove == "action_limits") {
    warningline(mu, sigma, n)
  }
  else if (remove == "warning_limits") {
    actionline(mu, sigma, n)
  }
  else {
    warningline(mu, sigma, n)
    actionline(mu, sigma, n)
  }
 # Using colors to identify the noteworthy point
  xw <- obj$Sample_ID[obj$Warning == 1] 
  yw <- obj$Sample_mu[obj$Warning == 1]
  
  points(xw, yw, col = "pink", pch = 20)
  
  xa <- obj$Sample_ID[obj$Action == 1] 
  ya <- obj$Sample_mu[obj$Action == 1]
  
  points(xa, ya, col = "red", pch = 20)
}
# Testing the function
plot(mout, 127, 3.4, 4)
plot(mout, 127, 3.4, 4, remove = "action_limits")
plot(mout, 127, 3.4, 4, remove = "warning_limits")

