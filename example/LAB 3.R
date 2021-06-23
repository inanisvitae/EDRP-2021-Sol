library(ggplot2)

#Problem 1

#tools
#dpois(x, lambda, log = FALSE)
#ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
#qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
#rpois(n, lambda)

#p(x=3) with lambda = 1
print(dpois(x=3,lambda=1))

mean(rpois(1000000, lambda = 6))

lambda<-1
k<-3
print((exp(-lambda)*(lambda)**k)/factorial(k))

#P(X<=2) with lambda = 1
print(ppois(2, lambda=1))

lambda=1
k<-0
s0<-(exp(-lambda)*(lambda)**k)/factorial(k)
k<-1
s1<-(exp(-lambda)*(lambda)**k)/factorial(k)
k<-2
s2<-(exp(-lambda)*(lambda)**k)/factorial(k)
print(s0+s1+s2)

#random number according to a poisson distribution
rpois(n=100, lambda=7)
mean(rpois(n=100, lambda=7)) #which verify that EX = lambda

length(which(rpois(n=1e6, lambda=7)==6))/1e6 #proportion
dpois(6,lambda=7)

first<-rpois(n=1e6, lambda=3)
second<-rpois(n=1e6, lambda=2)
result<-first+second
print(result)
for (i in 0:10) {
  var1<-length(which(result==i))/1e6
  var2<-dpois(i,lambda=5)
  cat("proportion =", var1, ", P(X =", i, ") =", var2, "\n")
}

#Problem 2
rexp(n=1000, 3)
mean(rexp(n=1000, 3))
print(1/3)

f<-0
s<-0
t<-0
for (i in 1:1000) {
  first<-rexp(n=1, 3)
  second<-rexp(n=1, 2)
  third<-rexp(n=1, 1)
  M<-min(first,second,third)
  if (M==first) {
    f<-f+1
  }
  if (M==second) {
    s<-s+1
  }
  if (M==third) {
    t<-t+1
  }
}

cat("P(M=first) =", f/1000, ", theorem :", 0.5, "\n")
cat("P(M=second) =", s/1000, ", theorem :", 1/3, "\n")
cat("P(M=third) =", t/1000, ", theorem :", 1/6, "\n")

#Problem3
rho<-rexp(n=1, 0.5)
to<-rho
tau<-c(0,to)
for (i in 2:20) {
  t<-rexp(n=1, 0.5)
  rho<-c(rho,t)
  to<-to+t
  tau<-c(tau, to)
}
Nt<-0:20
plot(tau,Nt,type = 's')
#ggplot(data.frame(t = tau, count = seq_along(tau)), aes(x = t, y = Nt)) + geom_step()

Nten<-c()
for (i in 1:10000) {
  rho<-c()
  to<-0
  tau<-c(0)
  rep<-TRUE
  for (i in 0:20) {
    t<-rexp(n=1, 0.5)
    rho<-c(rho,t)
    to<-to+t
    tau<-c(tau, to)
    while (to>10 & rep==TRUE) {
      rep<-FALSE
      Nten<-c(Nten,i)
    }
  }
}
expect<-0
for (i in 0:9) {
  var1<-length(which(Nten==i))/10000
  lambda<-0.5*10
  var2<-(exp(-lambda)*(lambda)**i)/factorial(i)
  cat("P(N10 =",i ,") =", var1, ", theorie", var2, "\n")
  expect<-expect+i*var1
}
print(expect)

#Problem4
N<-rpois(n=1, lambda=0.5*40)
print(N)
arrtimep<-runif(N, min = 0, max = 40) 
arrtime<-c(0,sort(arrtimep))
Nt<-0:N
ggplot(data.frame(t = arrtime, count = seq_along(arrtime)), aes(x = t, y = Nt)) + geom_step()

Nten<-c()
for (i in 1:10000) {
  N<-rpois(n=1, lambda=0.5*40)
  arrtimep<-runif(N, min = 0, max = 40) 
  arrtime<-c(0,sort(arrtimep))
  rep<-TRUE
  for (i in 1:10000) {
    while (arrtime[i]>10 & rep==TRUE) {
      rep<-FALSE
      Nten<-c(Nten, arrtime[i])
    }
  }
}
expect<-0
for (i in 0:9) {
  var1<-length(which(Nten==i))/10000
  lambda<-0.5*10
  var2<-(exp(-lambda)*(lambda)**i)/factorial(i)
  cat("P(N10 =",i ,") =", var1, ", theorie", var2, "\n")
  expect<-expect+i*var1
}
print(expect)

#Problem5
rho<-rexp(n=1, 1)
to<-rho
tau<-c(0,to)
for (i in 2:50) {
  t<-rexp(n=1, 1)
  rho<-c(rho,t)
  to<-to+t
  tau<-c(tau, to)
}
dist<-c(0.5,0.2,0.3)
print(tau)
samp<-c(0)
for (i in 1:50) {
  samp<-c(samp,sample(c(1,2,3),1,replace=F,prob=(dist)))
}
df<-data.frame(A=tau,B=samp)
df

poi1<-c(0)
poi2<-c(0)
poi3<-c(0)
for (i in 1:50) {
  if(df$B[i] == 1) {
    poi1<-c(poi1,df$A[i])
  }
  if(df$B[i] == 2) {
    poi2<-c(poi2,df$A[i])
  }
  if(df$B[i] == 3) {
    poi3<-c(poi3,df$A[i])
  }
}
print(poi1)
print(poi2)
print(poi3)

Nthree1<-c()
Nthree2<-c()
Nthree3<-c()
for (k in 1:10000){
  rho<-c()
  to<-0
  tau<-c(0)
  for (i in 1:50) {
    t<-rexp(n=1, 1)
    rho<-c(rho,t)
    to<-to+t
    tau<-c(tau, to)
  }
  dist<-c(0.5,0.2,0.3)
  samp<-c(0)
  for (i in 1:50) {
    samp<-c(samp,sample(c(1,2,3),1,replace=F,prob=(dist)))
  }
  df<-data.frame(A=tau,B=samp)
  poi1<-c(0)
  poi2<-c(0)
  poi3<-c(0)
  for (i in 1:50) {
    if(df$B[i] == 1) {
      poi1<-c(poi1,df$A[i])
    }
    if(df$B[i] == 2) {
      poi2<-c(poi2,df$A[i])
    }
    if(df$B[i] == 3) {
      poi3<-c(poi3,df$A[i])
    }
  }
  rep1<-TRUE
  for (i in 1:length(poi1)) {
    while (poi1[i]>3 & rep1==TRUE) {
      rep1<-FALSE
      Nthree1<-c(Nthree1,i)
    }
  }
  rep2<-TRUE
  for (i in 1:length(poi2)) {
    while (poi2[i]>3 & rep2==TRUE) {
      rep2<-FALSE
      Nthree2<-c(Nthree2,i)
    }
  }
  rep3<-TRUE
  for (i in 1:length(poi3)) {
    while (poi3[i]>3 & rep3==TRUE) {
      rep3<-FALSE
      Nthree3<-c(Nthree3,i)
    }
  }
}
for (i in 0:5) {
  var11<-length(which(Nthree1==i+2))/10000
  lambda1<-1*3*0.5
  var21<-(exp(-lambda1)*(lambda1)**i)/factorial(i)
  cat("P(N3(1) =",i ,") =", var11, ", theorie", var21, "\n")
}
for (i in 0:5) {
  var12<-length(which(Nthree2==i+2))/10000
  lambda2<-1*3*0.2
  var22<-(exp(-lambda2)*(lambda2)**i)/factorial(i)
  cat("P(N3(2) =",i ,") =", var12, ", theorie", var22, "\n")
}
for (i in 0:5) {
  var13<-length(which(Nthree3==i+2))/10000
  lambda3<-1*3*0.5
  var23<-(exp(-lambda3)*(lambda3)**i)/factorial(i)
  cat("P(N3(3) =",i ,") =", var13, ", theorie", var23, "\n")
}