dpois(x=0,lambda=1)


lambda<-1
k<-0
(exp(-lambda)*((lambda)^k))/factorial(k)


ppois(2, lambda=1)


lambda=1
k<-0
s0<-(exp(-lambda)*(lambda)^k)/factorial(k)
k<-1
s1<-(exp(-lambda)*(lambda)^k)/factorial(k)
k<-2
s2<-(exp(-lambda)*(lambda)^k)/factorial(k)
print(s0+s1+s2)

rpois(n=100, lambda=7)

mean(rpois(n=100, lambda=7))
var(rpois(n=100, lambda=7))


length(which(rpois(n=1e6, lambda=7)==6))/1e6
dpois(6,lambda=7)



# Problem 2
# 1.
mean(rexp(n=100000, rate=10))
var(rexp(n=100000, rate=10))
# 2.










