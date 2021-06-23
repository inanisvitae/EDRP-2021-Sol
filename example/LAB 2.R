#Problem 1
matrixpower<-function(mat,k) {
  tmp<-mat
  if (k>1) {
    for (i in 1:(k-1)) {
      mat<-mat%*%tmp
    }
  }
  return(mat)
}

print("Test")
A<-matrix(c(0.5,0.5,0,1),ncol = 2, byrow=T)
print(A)
print(matrixpower(A,2))

#Problem 2
P<-matrix(c(0.2,0.6,0.2,0.1,0.8,0.1,0.1,0.6,0.3),ncol = 3, byrow=T)
Pt<-t(P)
print("Eigenvalues :")
eigen(Pt)$values
print("Eigenvectors :")
eigen(Pt)$vectors

print("Eigenvector of lambda = 1 normalized :")
print(eigen(Pt)$vectors[ ,1]/sum(eigen(Pt)$vectors[1,1],eigen(Pt)$vectors[2,1],eigen(Pt)$vectors[3,1]))

#Problem 3
P<-matrix(c(0,0.5,0.5,1,0,0,0.5,0.5,0),ncol = 3, byrow=T)
Pt<-t(P)
print(P)
print(matrixpower(P,2))
print(matrixpower(P,3))
print(matrixpower(P,4))
print("The smallest positive power of P is 4")

print("The limiting distribution is :")
print(matrixpower(P,50)[1, ])

eigen(Pt)$values
eigen(Pt)$vectors
print("The stationary distribution is :")
print(eigen(Pt)$vectors[ ,1]/sum(eigen(Pt)$vectors[1,1],eigen(Pt)$vectors[2,1],eigen(Pt)$vectors[3,1]))

#Problem 4
P<-matrix(c(0.1,0.2,0.4,0.3,0.4,0,0.4,0.2,0.3,0.3,0,0.4,0.2,0.1,0.4,0.3),ncol = 4, byrow=T)
print(matrixpower(P,10))

alpha<-c(0.25,0.25,0.25,0.25)
nb<-10
X0<-sample(c("1","2","3","4"),1,replace=F,prob=alpha)
for (i in 1:nb) {
  Xi<-X0[i]
  if (Xi=="1"){
    Xn<-sample(c("1","2","3","4"),1,replace=F,prob=(P[1, ]))
  }
  if (Xi=="2"){
    Xn<-sample(c("1","2","3","4"),1,replace=F,prob=(P[2, ]))
  }
  if (Xi=="3"){
    Xn<-sample(c("1","2","3","4"),1,replace=F,prob=(P[3, ]))
  }
  if (Xi=="4"){
    Xn<-sample(c("1","2","3","4"),1,replace=F,prob=(P[4, ]))
  }
  X0<-c(X0,Xn)
}

rep<-table(X0)
print(rep)

#Problem 5 return time
P<-matrix(c(0,1,0,0.5,0,0.5,1/3,1/3,1/3),ncol = 3, byrow=T)

returntime<-0
rt<-c()
nb<-25
X0<-sample(c("1","2","3"),1,replace=F,prob=c(1/3, 1/3, 1/3))
print(X0)
for (i in 1:nb) {
  Xi<-X0[i]
  if (Xi=="1"){
    Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[1, ]))
    rt<-c(rt,returntime)
    print(returntime)
    returntime<-0
    returntime<-returntime+1
  }
  if (Xi=="2"){
    Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[2, ]))
    returntime<-returntime+1
  }
  
  if (Xi=="3"){
    Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[3, ]))
    returntime<-returntime+1
  }
#  if (length(rt)==2) {
#    rt<-c(rt,25)
#  }
  X0<-c(X0,Xn)
}
print(X0)
print(rt[3])
print(rt)

stepMC1<-function(P,alpha,nb) {
  returntime<-0
  rt<-c(0)
  X0<-sample(c("1","2","3"),1,replace=F,prob=alpha)
  for (i in 1:nb) {
    Xi<-X0[i]
    if (Xi=="1"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[1, ]))
      rt<-c(rt,returntime)
      returntime<-0
      returntime<-returntime+1
    }
    if (Xi=="2"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[2, ]))
      returntime<-returntime+1
    }
    
    if (Xi=="3"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[3, ]))
      returntime<-returntime+1
    }
    X0<-c(X0,Xn)
  }
  if (length(rt)==2) {
    rt<-c(rt,25)
  }
  return(rt[3])
}

alpha<-c(1,0,0)
rtlist<-stepMC1(P,alpha,nb)
for (i in 2:10000) {
  rtlist<-c(rtlist,stepMC1(P,alpha,nb))
}
print(rtlist)
mu1<-mean(rtlist)
print(mu1)

stepMC2<-function(P,alpha,nb) {
  returntime<-0
  rt<-c(0)
  X0<-sample(c("1","2","3"),1,replace=F,prob=alpha)
  for (i in 1:nb) {
    Xi<-X0[i]
    if (Xi=="1"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[1, ]))
      returntime<-returntime+1
    }
    if (Xi=="2"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[2, ]))
      rt<-c(rt,returntime)
      returntime<-0
      returntime<-returntime+1
    }
    
    if (Xi=="3"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[3, ]))
      returntime<-returntime+1
    }
    X0<-c(X0,Xn)
  }
  if (length(rt)==2) {
    rt<-c(rt,25)
  }
  return(rt[3])
}

alpha<-c(0,1,0)
rtlist<-stepMC2(P,alpha,nb)
for (i in 2:10000) {
  rtlist<-c(rtlist,stepMC2(P,alpha,nb))
}
mu2<-mean(rtlist)
print(mu2)

stepMC3<-function(P,alpha,nb) {
  returntime<-0
  rt<-c(0)
  X0<-sample(c("1","2","3"),1,replace=F,prob=alpha)
  for (i in 1:nb) {
    Xi<-X0[i]
    if (Xi=="1"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[1, ]))
      returntime<-returntime+1
    }
    if (Xi=="2"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[2, ]))
      returntime<-returntime+1
    }
    
    if (Xi=="3"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[3, ]))
      rt<-c(rt,returntime)
      returntime<-0
      returntime<-returntime+1
    }
    X0<-c(X0,Xn)
  }
  if (length(rt)==2) {
    rt<-c(rt,25)
  }
  return(rt[3])
}

alpha<-c(0,0,1)
rtlist<-stepMC3(P,alpha,nb)
print(rtlist)
for (i in 2:10000) {
  rtlist<-c(rtlist,stepMC3(P,alpha,nb))
}
mu3<-mean(rtlist)
print(mu3)

Pt<-t(P)
eigen(Pt)$values
eigen(Pt)$vectors
print("The stationary distribution is :")
print(eigen(Pt)$vectors[ ,1]/sum(eigen(Pt)$vectors[1,1],eigen(Pt)$vectors[2,1],eigen(Pt)$vectors[3,1]))
mu<-c(1/mu1,1/mu2,1/mu3)
print(mu)

#Problem 6
P<-matrix(c(0,1,0,0.5,0,0.5,1/3,1/3,1/3),ncol = 3, byrow=T)

returntime<-0
rt<-c(0)
nb<-50
alpha<-c(1,0,0)
X0<-sample(c("1","2","3"),1,replace=F,prob=alpha)
for (i in 1:nb) {
  Xi<-X0[i]
  if (Xi=="1"){
    Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[1, ]))
    returntime<-returntime+1
  }
  if (Xi=="2"){
    Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[2, ]))
    rt<-c(rt,returntime)
    returntime<-0
    returntime<-returntime+1
  }
  if (Xi=="3"){
    Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[3, ]))
    returntime<-returntime+1
  }
  X0<-c(X0,Xn)
}
print(X0)
print(rt[4])
print(rt)

stepMC2bis<-function(P,alpha,nb) {
  returntime<-0
  rt<-c(0)
  X0<-sample(c("1","2","3"),1,replace=F,prob=alpha)
  for (i in 1:nb) {
    Xi<-X0[i]
    if (Xi=="1"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[1, ]))
      returntime<-returntime+1
    }
    if (Xi=="2"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[2, ]))
      rt<-c(rt,returntime)
      returntime<-0
      returntime<-returntime+1
    }
    
    if (Xi=="3"){
      Xn<-sample(c("1","2","3"),1,replace=F,prob=(P[3, ]))
      returntime<-returntime+1
    }
    X0<-c(X0,Xn)
  }
  if (length(rt)==2) {
    rt<-c(rt,25)
  }
  return(rt[4])
}

alpha<-c(0,1,0)
rtlist<-stepMC2bis(P,alpha,nb)
for (i in 2:10000) {
  rtlist<-c(rtlist,stepMC2bis(P,alpha,nb))
}
mu2bis<-mean(rtlist)
print(mu2)
print(mu2bis)