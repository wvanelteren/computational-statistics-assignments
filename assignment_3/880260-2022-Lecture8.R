setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

#Toy example to explain 'best model' selection using loocv
#Linear regression model with 2 predictors
#All possible non-null models considered (X1,X2,X1+X2)

#loocv function: input is matrix of predictors X and outcome vector y
LOOCV <- function(X, y) {
  squared_errors <- rep(NA, length(y))
  y_pred <- rep(NA, length(y))
  beta <- matrix(NA, nrow=length(y),ncol = dim(X[drop=F])[2]+1)
  for (i in 1:length(y)) {
    y_pred[i] <- c(1, X[i, ]) %*% lm(y[-i] ~ X[-i, ])$coef 
    squared_errors[i] <- (y[i] - y_pred[i])^2
    beta[i,]<-lm(y[-i] ~ X[-i, ])$coef
  }
  outlist <- list(beta,y_pred,squared_errors,mean(squared_errors))
  #return(mean(squared_errors))
  return(outlist)
}

#generation of data
library(MASS)
set.seed(123)
X <- mvrnorm(n=5,c(0,0),Sigma = matrix(c(1,0,0,1),nrow=2))
X <- as.matrix(X)
y <- 1+0.5*X[,1]+0.0002*X[,2]+rnorm(5,sd=0.02)

#loocv error for model Y=X1+residual
error1 <- LOOCV(X[,1, drop=F],y)#drop argument so vector treated as matrix
error1
#loocv error for model Y=X2+residual
error2 <- LOOCV(X[,2, drop=F],y)#drop argument so vector treated as matrix
error2
#loocv error for model Y=X1+X2+residual
error3 <- LOOCV(X,y)
error3

write.table(cbind(y,X,error1[[1]],error1[[3]],error2[[1]],error2[[3]],error3[[1]],error3[[3]]),"output.txt", sep = "\t", eol = "\n", col.names = TRUE, dec = ",")

#select 'best' model, here with lowest CVE
#Fit to entire data set to find the final estimates of the selected model
finalmodel <- lm(y~X[,1])$coefficients
finalmodel
