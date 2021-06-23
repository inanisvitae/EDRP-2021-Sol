#introduction and tips
samplesize = 100
s<-sample(c("H","T"),samplesize,replace=T)
print(s)

ind<-which(s=="H")
ind
l<-length(ind)
l
l/samplesize

samplesize=1e+6
s<-sample(c("H","T"),samplesize,replace=T)
length(which(s=="H"))/samplesize
(-1)*(length(which(s=="H"))/samplesize)+1*(length(which(s=="T"))/samplesize)

#problem 1
samplesize = 1e+5
coin=3
nb_X0 = 0
nb_X1 = 0
nb_X2 = 0
nb_X3 = 0
#s<-sample(c("HHH","HHT","HTH","THH","HTT","THT","TTH","TTT"),samplesize,replace=T)
#ind0<-which(s="TTT")
#ind1<-which(s="HTT"|s="THT"|s="TTH)
#...
for (i in 0:samplesize) {
  s<-sample(c("H","T"),coin,replace=T,prob=c(0.75,0.25))
  if (length(which(s=="H"))==0){
    nb_X0 = nb_X0 + 1
  }
  if (length(which(s=="H"))==1){
    nb_X1 = nb_X1 + 1
  }
  if (length(which(s=="H"))==2){
    nb_X2 = nb_X2 + 1
  }
  if (length(which(s=="H"))==3){
    nb_X3 = nb_X3 + 1
  }
}
A1<-nb_X1/samplesize
A1
A2<-0*nb_X0/samplesize+1*nb_X1/samplesize+2*nb_X2/samplesize+3*nb_X3/samplesize
A2

#Problem 2
nb<-5
alpha<-c(0.5,0.5)
P<-matrix(c(0.5,0.5,0,1),nrow=2,byrow=T)
P[1,2]
samplesize=100

markovChain<-function(nb,alpha,P) {
  X0<-sample(c("1","2"),1,replace=F,prob=alpha)
  for (i in 1:nb) {
    Xi<-X0[i]
    if (Xi=="1"){
      Xn<-sample(c("1","2"),1,replace=F,prob=(P[1, ]))
      }
    if (Xi=="2"){
      Xn<-sample(c("1","2"),1,replace=F,prob=(P[2, ]))
      }
    X0<-c(X0,Xn)
    }
  return(X0)
}

S0<-markovChain(nb,alpha,P)
for (i in 2:samplesize){
  S0<-c(S0,markovChain(nb,alpha,P))
}

df <- data.frame(matrix(unlist(S0), ncol=nb+1, byrow=T))
names(df)<-c("X0","X1","X2","X3","X4","X5")
df

print("Part 1")
print("Question 1")
rep1<-((sum(df$X0 == 1 & df$X1 == 1)/samplesize)/(sum(df$X0 == 1)/samplesize))
rep1
print("Question 2")
rep2<-((sum(df$X0 == 1 & df$X2 == 1)/samplesize)/(sum(df$X0 == 1)/samplesize))
rep2
print("Question 3")
rep3_1<-(sum(df$X0 == 1 & df$X2 == 1 & df$X5 == 2)/samplesize)/(sum(df$X0 == 1 & df$X2 == 1)/samplesize)
rep3_1
rep3_2<-(sum(df$X2 == 1 & df$X5 == 2)/samplesize)/(sum(df$X2 == 1)/samplesize)
rep3_2
print("Question 4")
rep4<-1*sum(df$X2 == 1)/samplesize+2*sum(df$X2 == 2)/samplesize
rep4
print("Question 5")
rep5<-sum(df$X1 == 1 & df$X3 == 1)/samplesize
rep5

alpha<-c(1,0)
S0<-markovChain(nb,alpha,P)
for (i in 2:samplesize){
  S0<-c(S0,markovChain(nb,alpha,P))
}

df <- data.frame(matrix(unlist(S0), ncol=nb+1, byrow=T))
names(df)<-c("X0","X1","X2","X3","X4","X5")
df

print("Part 2")
print("Question 1")
rep1<-((sum(df$X0 == 1 & df$X1 == 1)/samplesize)/(sum(df$X0 == 1)/samplesize))
rep1
print("Question 2")
rep2<-((sum(df$X0 == 1 & df$X2 == 1)/samplesize)/(sum(df$X0 == 1)/samplesize))
rep2
print("Question 3")
rep3_1<-(sum(df$X0 == 1 & df$X2 == 1 & df$X5 == 2)/samplesize)/(sum(df$X0 == 1 & df$X2 == 1)/samplesize)
rep3_1
rep3_2<-(sum(df$X2 == 1 & df$X5 == 2)/samplesize)/(sum(df$X2 == 1)/samplesize)
rep3_2
print("Question 4")
rep4<-1*sum(df$X2 == 1)/samplesize+2*sum(df$X2 == 2)/samplesize
rep4
print("Question 5")
rep5<-sum(df$X1 == 1 & df$X3 == 1)/samplesize
rep5

alpha<-c(0,1)
S0<-markovChain(nb,alpha,P)
for (i in 2:samplesize){
  S0<-c(S0,markovChain(nb,alpha,P))
}

df <- data.frame(matrix(unlist(S0), ncol=nb+1, byrow=T))
names(df)<-c("X0","X1","X2","X3","X4","X5")
df

print("Part 3")
print("Question 1")
rep1<-((sum(df$X0 == 1 & df$X1 == 1)/samplesize)/(sum(df$X0 == 1)/samplesize))
rep1
print("Question 2")
rep2<-((sum(df$X0 == 1 & df$X2 == 1)/samplesize)/(sum(df$X0 == 1)/samplesize))
rep2
print("Question 3")
rep3_1<-(sum(df$X0 == 1 & df$X2 == 1 & df$X5 == 2)/samplesize)/(sum(df$X0 == 1 & df$X2 == 1)/samplesize)
rep3_1
rep3_2<-(sum(df$X2 == 1 & df$X5 == 2)/samplesize)/(sum(df$X2 == 1)/samplesize)
rep3_2
print("Question 4")
rep4<-1*sum(df$X2 == 1)/samplesize+2*sum(df$X2 == 2)/samplesize
rep4
print("Question 5")
rep5<-sum(df$X1 == 1 & df$X3 == 1)/samplesize
rep5