set.seed(1)

n <- 100000
p <- 0.5
s<-sample(c(-1, 1), n, replace=TRUE, prob=c(1-p, p))
s
rw <- cumsum(s)
plot(rw,type="l")
