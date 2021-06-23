#Problem1
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

branchingprocessesBis<-function(n,a) {
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
      l<-n-length(z)
      for (k in 1:l) {
        z<-c(z,0)
      }
      return(z)
    }
  }
  return(z)
}

a<-c(1/4,3/4)
case1<-0
case2<-0
count<-0
for (i in 1:10000) {
  z<-branchingprocessesBis(20,a)
  if (z[3]==0 & z[4]==0){
    case1<-case1+1
    count<-count+1
  }
  if (z[3]!=0 & z[4]==0){
    case2<-case2+1
    count<-count+1
  }
}
print(case1/10000)
print(case2/10000)
print(count/10000)

tot<-0
for (i in 1:10000) {
  z<-branchingprocesses(20,a)
  tot<-tot+length(z)-1
}
print(tot/10000)

a<-c(3/4,1/4)
case1<-0
case2<-0
count<-0
for (i in 1:10000) {
  z<-branchingprocessesBis(20,a)
  if (z[3]==0 & z[4]==0){
    case1<-case1+1
    count<-count+1
  }
  if (z[3]!=0 & z[4]==0){
    case2<-case2+1
    count<-count+1
  }
}
print(case1/10000)
print(case2/10000)
print(count/10000)
# 3. mean time of extinction
tot<-0
for (i in 1:10000) {
  z<-branchingprocesses(20,a)
  tot<-tot+length(z)-1
}
print(tot/10000)


#Problem 2
# dpois for bp
a<-c()
for (i in 0:150) {
  a<-c(a,dpois(i,0.6))
}

tot<-c()
for (i in 1:10000) {
  z<-branchingprocesses(50,a)
  tot<-c(tot,sum(z))
}

print(mean(tot))
print(var(tot))

distrib<-c()
for (i in 1:50){
  distrib<-c(distrib, length(which(tot==i))/10000)
}
print(distrib)

#Problem 3
branchingprocessesImm<-function(n,a,b) {
  offpossible<-c(seq(0,length(a)-1))
  immipossible<-c(seq(0,length(b)-1))
  z<-c(1)
  tmp<-1
  for (i in 1:n) {
    Zi<-0
    for (j in 1:tmp) {
      Xi<-sample(c(offpossible),1,replace=F,prob=(a))
      Wi<-sample(c(immipossible),1,replace=F,prob=(b))
      Zi<-Zi+Xi+Wi
    }
    tmp<-Zi
    z<-c(z,Zi)
    if (tmp==0){
      return(z)
    }
  }
  return(z)
}

a<-c(1/4,3/4)
b<-c()
for (i in 0:150) {
  b<-c(b,dpois(i,1.2))
}
print(branchingprocessesImm(100,a,b)[100])

#Problem4
a<-c(0.5)
for (i in 1:100) {
  a<-c(a,0.5*((5/6)**(i-1))*(1/6))
}

end<-0
case<-0
z3<-c()
for (i in 1:1000) {
  z<-branchingprocessesBis(6,a)
  zb<-branchingprocesses(6,a)
  z3<-c(z3,z[4])
  if (z[3]!=0 & z[4]==0){
    case<-case+1
  }
  if (length(which(zb==0))==1){
    end<-end+1
  }
}
print(mean(z3))
print(case/1000)
print(end/1000)

