library(matrixcalc)
samplesize=1e+6
s<-sample(c('H', 'T'), samplesize, replace=T)
print(s)

ind<-which(s=='H')
ind
l<-length(ind)
l

l/samplesize

samplesize=1e+6
s<-sample(c('H', 'T'), samplesize, replace=T)
(-1)*(length(which(s=='H'))/samplesize)+1*(length(which(s=='T'))/samplesize)


#Problem 1
#Simulate flipping three fair coins and counting the numberXof heads
#1. Use your simulation to estimateP(X= 1)andEX. 
#Compare the estimates with the true values, derivedfrom theoretical computations.

s1<-sample(c('H', 'T'),samplesize,replace=T)
s2<-sample(c('H', 'T'),samplesize,replace=T)
s3<-sample(c('H', 'T'),samplesize,replace=T)
count = 0
for (i in 1:length(s1)) {
  prob_str = paste(s1[i],s2[i],s3[i], sep='')

  if (prob_str == 'HTT' || prob_str == 'THT' ||prob_str == 'TTH') {
    count = count + 1
  }
}
print(count)
print(length(s1))
count/length(s1)


#Modify the above to allow for a biased coin whereP(heads)=3/4.
?sample
s1<-sample(c('H', 'T'),samplesize,replace=T, prob=c(0.75, 0.25))
s2<-sample(c('H', 'T'),samplesize,replace=T)
s3<-sample(c('H', 'T'),samplesize,replace=T)
count = 0
for (i in 1:length(s1)) {
  prob_str = paste(s1[i],s2[i],s3[i], sep='')
  
  if (prob_str == 'HTT' || prob_str == 'THT' ||prob_str == 'TTH') {
    count = count + 1
  }
}
print(count)
print(length(s1))
count/length(s1)

0.1875 + 0.0625 + 0.0625

# Problem 2
P = matrix(c(0.1, 0.3, 0.6, 0, 0.4, 0.6, 0.3, 0.2, 0.5), ncol=3, byrow = T)
alpha = matrix(c(0.2, 0.3, 0.5), ncol=3,byrow=T)


alpha %*% P %*% P %*% P %*% P %*% P %*% P

?runif
A<-runif(5,0,1)
A

?cut
cut(A, c(0.5, 1))

start = runif(1, 0, 1)
start
cutted = cut(start, c(0, 0.5, 1), labels = c(1, 2))
cutted[1]

count = 1

mat2 = matrix(c(0.5,0.5,0,1), ncol = 2,byrow=T);
mat2


# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters = 50, firstState) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)
  
  # initialize variable for first state 
  states[1]    <- firstState
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}
count = 0
stat = 0
while (count < 50000) {
  start = runif(1, 0, 1)
  cutted = cut(start, c(0, 0.5, 1), labels = c(1, 2))
  temp = run.mc.sim(mat2, 5, cutted[1])
  if (temp[4] == 1) {
    stat = stat + 1
  }
  if (temp[2] == 1) {
    stat = stat + 1
  }
  count = count + 1
}
stat


mat2
matrix(c(0.5, 0.5), ncol = 2, byrow = T) %*% mat2%*% mat2%*% mat2
#0.125 * 1 + 0.875 * 2
#0.0625 = 0.046
#0.25 + 0.0625
matrix.power(mat2, 2)








