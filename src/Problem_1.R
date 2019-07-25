###
# Jimmy Hickey
# 2019-07-18
# Sampling Distribution of the Laplace Distribution.
###

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