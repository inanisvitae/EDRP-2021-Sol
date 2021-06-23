#Problem 1
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

print("subcritical")
print(branchingprocesses(100,c(2/3,0,1/3)))
print("critical")
print(branchingprocesses(100,c(1/3,1/3,1/3)))
print("supercritical")
print(branchingprocesses(10,c(1/3,0,2/3)))

#Problem2
nsim<-10000
a<-c(0.6,0.2,0.2)
simlist<-replicate(nsim, branchingprocesses(10,a))
simlist


z1<-0
z5<-0
z10<-0
for (i in 1:10000){
  end<-1
  z<-c(branchingprocesses(10,a),0,0,0,0,0,0,0,0,0,0,0)
  print(z)
  rep<-TRUE
  for (j in 2:10){
    if (z[j] == 0 & rep == TRUE){
      end<-j-1
      rep<-FALSE
    }
  }
  if(1<end){
    z1<-z1+z[2]
  }
  if(5<end){
    z5<-z5+z[6]
  }
  if(10<end){
    z10<-z10+z[11]
  }
}
u1<-z1/10000
u5<-z5/10000
u10<-z10/10000
print(u1)
print(u5)
print(u10)


a<-c(0.6,0.2,0.2)
tmp<-c()
for (i in 0:length(a)-1){
  tmp<-c(tmp,i*a[i+1])
}
mu1<-sum(tmp)
print(mu1)
print(mu1**5)
print(mu1**10)


nsim<-10000
a<-c(0.2,0.6,0.2)
#simlist<-replicate(nsim, branchingprocesses(10,a))
z1<-0
z5<-0
z10<-0
for (i in 1:10000){
  end<-1
  z<-c(branchingprocesses(10,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:10){
    if (z[j] == 0 & rep == TRUE){
      end<-j-1
      rep<-FALSE
    }
  }
  print(z)
  print(z[11])
  if(1<end){
    z1<-z1+z[2]
  }
  if(5<end){
    z5<-z5+z[6]
  }
  if(10<end){
    z10<-z10+z[11]
  }
}
u1<-z1/10000
u5<-z5/10000
u10<-z10/10000
print(u1)
print(u5)
print(u10)


a<-c(0.2,0.6,0.2)
tmp<-c()
for (i in 0:length(a)-1){
  tmp<-c(tmp,i*a[i+1])
}
mu1<-sum(tmp)
print(mu1)
print(mu1**5)
print(mu1**10)


nsim<-10000
a<-c(0.2,0.2,0.6)
#simlist<-replicate(nsim, branchingprocesses(10,a))
z1<-0
z5<-0
z10<-0
for (i in 1:10000){
  end<-1
  z<-c(branchingprocesses(10,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:10){
    if (z[j] == 0 & rep == TRUE){
      end<-j-1
      rep<-FALSE
    }
  }
  if(1<end){
    z1<-z1+z[2]
  }
  if(5<end){
    z5<-z5+z[6]
  }
  if(10<end){
    z10<-z10+z[11]
  }
}
u1<-z1/10000
u5<-z5/10000
u10<-z10/10000
print(u1)
print(u5)
print(u10)


a<-c(0.2,0.2,0.6)
tmp<-c()
for (i in 0:length(a)-1){
  tmp<-c(tmp,i*a[i+1])
}
mu1<-sum(tmp)
print(mu1)
print(mu1**5)
print(mu1**10)


#Problem3
nsim<-100
a<-c(0.25,0.25,0.5)
#simlist<-replicate(nsim, branchingprocesses(10,a))
branchingprocesses(20,a)
end<-0
for (i in 1:100){
  z<-c(branchingprocesses(20,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:10){
    if (z[j] == 0 & rep == TRUE){
      end<-end+1
      rep<-FALSE
    }
  }
}
print(end/100)
print("theorically, it's 0.5")

#Problem4
a<-c(0.25,0.25,0.5)
Gfunction<-function(s) {
  return(a[1]+a[2]*s+a[3]*s**2)
}
s<-runif(1)
for (i in 1:80){
  s<-Gfunction(s)
  print(s)
}
print(s)

#Problem5
nsim<-100
a<-c(0.8,0,0,0,0.1,0,0,0,0,0.1)
#simlist<-replicate(nsim, branchingprocesses(10,a))
end<-0
for (i in 1:100){
  z<-c(branchingprocesses(20,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:20){
    if (z[j] == 0 & rep == TRUE){
      end<-end+1
      rep<-FALSE
    }
  }
}
print(end/100)

a<-c(0.8,0,0,0,0.1,0,0,0,0,0.1)
Gfunction<-function(s) {
  return(a[1]+a[2]*s+a[3]*s**2)
}
s<-runif(1)
for (i in 1:80){
  s<-Gfunction(s)
}
print(s)


nsim<-100
a<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
#simlist<-replicate(nsim, branchingprocesses(10,a))
end<-0
for (i in 1:100){
  z<-c(branchingprocesses(20,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:20){
    if (z[j] == 0 & rep == TRUE){
      end<-end+1
      rep<-FALSE
    }
  }
}
print(end/100)

a<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
Gfunction<-function(s) {
  return(a[1]+a[2]*s+a[3]*s**2)
}
s<-runif(1)
for (i in 1:80){
  s<-Gfunction(s)
}
print(s)


nsim<-100
a<-c(0.6,0,0,0.2,0,0,0.1,0,0,0,0,0,0.1)
#simlist<-replicate(nsim, branchingprocesses(10,a))
end<-0
for (i in 1:100){
  z<-c(branchingprocesses(20,a),0,0,0,0,0,0,0,0,0,0,0)
  rep<-TRUE
  for (j in 2:20){
    if (z[j] == 0 & rep == TRUE){
      end<-end+1
      rep<-FALSE
    }
  }
}
print(end/100)

a<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
Gfunction<-function(s) {
  return(a[1]+a[2]*s+a[3]*s**2)
}
s<-runif(1)
for (i in 1:80){
  s<-Gfunction(s)
}
print(s)