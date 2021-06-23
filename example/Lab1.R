?factorial
1:10

-3:5
5:1
seq(1, 10)
seq(10,1,-2)
c(2,3,1,4)
firstfiveprimes<-c(2,3,5,7,11)

firstfiveprimes
which(firstfiveprimes < 6)
print(firstfiveprimes[1])
a<-seq(0,30,4)
a

# Randomness
sample(1:10, 1)
sample(1:10, 5)
sample(c(-1, 0, 1), 6, replace=T)

?sample
s<-sample(c('A', 'B', 'C', 'D'), 30, replace=T,
          prob=c(0.1,0.2,0.2,0.5))
s
index <- which(s=='D')
index

runif(6,0,1)
pexp(3,1)
dbinom(10,20,0.5)
?dbinom



# Matrix
matrix(1:9, nrow=3)
matrix(1:9, nrow=3,byrow=T)

A<-matrix(c(1,3,0,-4),ncol = 2,byrow = T)
B<-matrix(1:4,ncol = 2, byrow = T)
A
B
A*B
A%*%B

