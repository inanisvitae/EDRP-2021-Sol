#average time of extinction of the branching process
#of the branching process

# a = (4/5, 1/10, 1/10)


# estimated mean N3 of (Nt)t - a posson process with 
#lambda = 2

branchingprocesses<-function(n,a) {
  offpossible<-c(seq(0,length(a)-1))
  z<-c(1)
  tmp<-1
  for (i in 1:n) {
    Zi<-0
    for (j in 1:tmp) {
      Xi<-sample(c(offpossible),1,replace=F,prob=(a))
      Zi<-Zi+Xi
    }
    tmp<-Zi
    z<-c(z,Zi)
    if (tmp==0){
      return(z)
    }
  }
  return(z)
}
# Q1
nsim<-10000
a<-c(4/5,1/10,1/10)
#simlist<-replicate(nsim, branchingprocesses(10,a))
branchingprocesses(20,a)
end<-0
for (i in 1:10000){
  z<-c(branchingprocesses(20,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:10){
    if (z[j] == 0 & rep == TRUE){
      end<-end+1
      rep<-FALSE
    }
  }
}
print(end/10000)


#q2
# 3*2 = 6

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
    t<-rexp(n=1, 2)
    rho<-c(rho,t)
    to<-to+t
    tau<-c(tau, to)
    while (to>3 & rep==TRUE) {
      rep<-FALSE
      Nten<-c(Nten,i)
    }
  }
}

#Nten<-c()
expect<-0
for (i in 0:9) {
  var1<-length(which(Nten==i))/10000
  lambda<-2*3
  var2<-(exp(-lambda)*(lambda)**i)/factorial(i)
  cat("P(N3 =",i ,") =", var1, ", theorie", var2, "\n")
  expect<-expect+i*var1
}
print(expect)












