###
# Jimmy Hickey
# 2019-07-18
# Sampling Distribution of the Laplace Distribution.
###

library(scales)


set.seed(1978)

# Problem 1.

# d.

# We’ll generate N = 50 values of L and K for every value of n from 1 to 250.
# That is, consider n = 1. We want to generate N = 50 datasets with n = 1.
# For each data set we want to find L and K. Now, for n = 2 we want to
# generate 50 datasets, find L and K for each one. All the way until we get to
# n = 250.

max_sample_size = 250 
n = seq(1:max_sample_size)
N = 50

K_Matrix = matrix(data = NA, nrow = N, ncol = max_sample_size)
L_Matrix = matrix(data = NA, nrow = N, ncol = max_sample_size)

mu = 0
b = 5

# F inverse
# Plug in values from a Uniform(0, 1) into the y parameter
# to get a realization of the Laplace CDF
F_inv = function(mu, b, y){
  return(mu - b * sign(y - 0.5) * log(1 - 2 * abs(y - 0.5)))
}

# L
# Sum the squares of an array of inputs and divide by the length
L_calc = function(ys){
  n = length(ys)
  return(1/n * sum(ys^2))
}


# K
# K = sqrt(L)
K_calc = function(ys){
  return(sqrt(L_calc(ys)))
} 


# Loop over sample sizes
for (j in 1:max_sample_size){
  
  # Loop through data sets
  for (i in 1:N){
    random = runif(n[j])
    K_Matrix[i, j] = K_calc(F_inv(mu, b, random))
    L_Matrix[i, j] = L_calc(F_inv(mu, b, random))
  }
}

# Plotting K and L

plotting_function = function(
  sample_sizes,
  data_matrix,
  expected_mean,
  epsilon,
  title,
  y_lab,
  ylim){
  
  # Create plot
  plot(sample_sizes, data_matrix[1,], main=title, 
       xlab="Sample Size, n", ylab=y_lab, 
       add = TRUE, col = alpha("black", 0.2), pch = 16, ylim=ylim)
  
  # Add additional data points
  for (i in 2:nrow(K_Matrix)){
    points(n, data_matrix[i,], col = alpha("black", 0.2), pch = 16)
  }  
  
  # Add line at expected mean
  abline(h = expected_mean, col="blue", lwd = 4)
  
  # Add epsilon lines
  abline(h = expected_mean + epsilon, col = "red", lwd = 5, lty = 2)
  abline(h = expected_mean - epsilon, col = "red", lwd = 5, lty = 2)
  
  # Add legend
  # Add legend to top right, outside plot region
  legend("topright", legend=c(sprintf("Theoretical Convergence to %.4f", expected_mean), 
                              sprintf("Epsilon Bounds with ε = %d", epsilon)),
         col=c("blue", "red"), lty=1:2, cex=0.8)
}

plotting_function(sample_sizes = n,
                  data_matrix = K_Matrix,
                  expected_mean = sqrt(50),
                  epsilon = 3,
                  title = "Estimates of K as n increases",
                  y_lab = "Estimates of K",
                  c(0,20))

plotting_function(sample_sizes = n,
                  data_matrix = L_Matrix,
                  expected_mean = 50,
                  epsilon = 20,
                  title = "Estimates of L as n increases",
                  y_lab = "Estimates of L",
                  c(0,150))

# These graphs both demonstrate that our RVs L and K are converging in probability
# At each sample size n, the same number of samples (50) were taken. 
# Notice the trend as the number of observations (n) in each sample increases.
# It is clear that there is far less spread.
# As n increases, the RVs are converging to the blue line.
# The observed values of L are geting closer and closer to 50.
# And the observed values of K are approaching sqrt(50). 
# As n increases we could continue to shrink our epsilon bubble around the expected value
# and we will continue to see this convergence.

###
# Shaleni Kovach
# 2019-07-22
# Distribution of L as n increases. 
###

set.seed(1978)

# Problem 2.

#(a) Take the N values of L you have for each of n = 10,50,100,250 and create a histogram to inspect the shape of the sampling distribution of L for those values of n.
#(b) Make sure the graph(s) have appropriate titles and labels.
#(c) In a comment, does it appear as though L or K is converging to a normal distribution?


max_sample_size = 250 
n = seq(1:max_sample_size)
N = 50

L_Matrix = matrix(data = NA, nrow = N, ncol = max_sample_size)
K_Matrix = matrix(data = NA, nrow = N, ncol = max_sample_size)

mu = 0
b = 5

# F inverse
# Plug in values from a Uniform(0, 1) into the y parameter
# to get a realization of the Laplace CDF
F_inv = function(mu, b, y){
  return(mu - b * sign(y - 0.5) * log(1 - 2 * abs(y - 0.5)))
}

# L
# Sum the squares of an array of inputs and divide by the length
L_calc = function(ys){
  n = length(ys)
  return(1/n * sum(ys^2))
}


# K
# K = sqrt(L)
K_calc = function(ys){
  return(sqrt(L_calc(ys)))
} 


# Loop over sample sizes
for (j in 1:max_sample_size){
  
  # Loop through data sets
  for (i in 1:N){
    random = runif(n[j])
    K_Matrix[i, j] = K_calc(F_inv(mu, b, random))
    L_Matrix[i, j] = L_calc(F_inv(mu, b, random))
  }
}

#Get just the values of L we want (all i, j where j = 10,50,100,250)
n.hist <- c(10,50,100,250)
par(mfrow=c(2,2))
for (n in n.hist){
  hist(L_Matrix[,n], main=paste("n=", n),
       xlab="Estimates of L", xlim=range(c(0,100)))
}

for (n in n.hist){
  hist(K_Matrix[,n], main=paste("n=", n),
       xlab="Estimates of K", xlim=range(c(0,15)))
}


#We can see from the plots that as n increases, the estimates of L do appear
#to converge to a normal distribution. Similarly, as the n increases, the estimates
#of K also appear to converge to a normal distribution, just with a smaller variance. 

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

###
# Stephanie Stewart
# 2019-07-24
###

set.seed(1978)


# Problem 5.
N=50
#N=10000

#Simulate 50/10000 samples of size 1000,10000 from N(mu,sigma) distribution
n<-c(1000,10000)

#b as in problem 1.
b <- 5
#using expected value and variance from problem 3.
mu<-2*b^2
sigma<-sqrt(20*b^4)

#list to save data values in
data<-list()
for(i in 1:length(n)){data[[i]]<-matrix(0,nrow=N,ncol=n[i])}

#Create the data#loop over sample sizes
for (j in 1:length(n)){
  #loop over data sets
  for (i in 1:N){data[[j]][i,]<-rnorm(n=n[j],mean=mu,sd=sigma)}
}

#calculate the z statistic for each sample
means1000<-apply(X=data[[1]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[1]))},MARGIN=1)
means10000<-apply(X=data[[2]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[2]))},MARGIN=1)

# Problem 6.
hist(means1000,main=paste("Histogram of z's with ",N," samples, n=1,000 from N(",mu,",",sigma^2,")",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means10000,main=paste("Histogram of z's with ",N," samples, n=10,000 from N(",mu,",",sigma^2,")",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))

#Using N=50, while the samples seem to be converging to normal, they still do not appear quite normal even for sample size n=10,000
#When we change to N=10,000 both n=1,000 and n=10,000 appear normal
#This shows it is not enough for n>30 the number of sample repetitions also plays a role in CLT.