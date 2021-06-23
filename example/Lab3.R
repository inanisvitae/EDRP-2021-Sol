library(matrixcalc)

# Problem 1 - Matrix powers in R
matrixpower <- function(mat, k) {
  count = 0
  resultMat = mat
  while (count <= k) {
    resultMat = resultMat %*% mat
    count = count + 1
  }
  return(resultMat)
}
mat2 <- matrix(c(0.5,0.5,0,1), ncol = 2,byrow=T)
matrixpower(mat2, 4)
matrix.power(mat2, 5)

#Problem2
p <- matrix(c(0.2,0.6,0.2,0.1,0.8,0.1,0.1,0.6,0.3), ncol = 3, byrow = T)
pt_eigen <- eigen(t(p))$vectors



# Problem 3
matP <- matrix(c(0, 0.5, 0.5, 1, 0,0 ,0.5,0.5,0), ncol= 3, byrow = T)
matrixpower(matP, 30)
matP
eigen(matP)$vectors

t(matP)




