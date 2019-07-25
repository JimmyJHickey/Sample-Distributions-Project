###
# Meredith Saunders 
# 2019-07-25
# Limiting Distribution of L Standardization
###

set.seed(1978)

# Problem 4

# Simulate 50 samples of size 10, 50, 100, 250 from N(mu, sigma) distribution

N = 50
n <-c(10, 50, 100, 250)
b = 5

mu <- (-2)*b^2
sigma <- sqrt(20*b^4)

# Create a list to save data values in 
data <- list()
for (i in 1:length(n))
{
  data[[i]] <- matrix(0, nrow = N, ncol = n[i])
}

# Create the data by looping over sample sizes
for (j in 1: length(n))
{
  for (i in 1: N )
  {
    data[[j]][i,] <- rnorm(n=n[j], mean = mu, sd=sigma)
  }
}

# Calculate the z statistic for each set of samples 
means10<-apply(X=data[[1]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[1]))},MARGIN=1)
means50 <- apply(X = data[[2]], FUN = function(data) {(mean(data)-mu)/(sigma/sqrt(n[2]))}, MARGIN = 1)
means100 <- apply(X = data[[3]], FUN = function(data) {(mean(data)-mu)/(sigma/sqrt(n[3]))}, MARGIN = 1)
means250 <- apply(X = data[[4]], FUN = function(data) {(mean(data)-mu)/(sigma/sqrt(n[4]))}, MARGIN = 1)

# Generate a histogram to inspect the shape of the sampling distribution of the standardized L
hist(means10, main = paste("Histogram of z's with n = ", n[1], " From N(", mu, "," , sigma^2, ")", sep = ""), prob = T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))

hist(means50, main = paste("Histogram of z's with n = ", n[2], " From N(", mu, "," , sigma^2, ")", sep = ""), prob = T)
lines(seq(from = -10, to =10, by = 0.01), dnorm(seq(from = -10, to = 10, by = 0.01)))
     
hist(means100, main = paste("Histogram of z's with n = ", n[3], " From N(", mu, "," , sigma^2, ")", sep = ""), prob = T)
lines(seq(from = -10, to =10, by = 0.01), dnorm(seq(from = -10, to = 10, by = 0.01)))
          
hist(means250, main = paste("Histogram of z's with n = ", n[4], " From N(", mu, "," , sigma^2, ")", sep = ""), prob = T)
lines(seq(from = -10, to =10, by = 0.01), dnorm(seq(from = -10, to = 10, by = 0.01)))

