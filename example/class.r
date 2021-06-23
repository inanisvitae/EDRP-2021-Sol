n<-1e3
ext_time <- numeric(n)

f1<-function()
{
  z<-1
  e_t<-0
  while (z != 0)
  {
    e_t <- e_t + 1
    z <- sum(rgeom(z, 0.5))
  }
  e_t +1
}

ext_time <- replicate(n, f1())
mean(ext_time)
var(ext_time)
table(ext_time)

x <- rcauchy(1e5)
mean(abs(x))
var(abs(x))

x<-rnorm(1e5, mean=0, sd=1)
mean(x)
var(x)




