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

