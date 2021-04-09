             
library("dplyr") 

airpol = read.csv("Table1_5.csv")

airpold <- airpol %>% select(1:3,5,6)

apfa <- factanal(airpold, factors=2)
apfa

com1 <- apply(apfa$loadings^2,1,sum)
com1

Psi1 <- 1 - com1
Psi1
Lambda <- apfa$loadings
Psi <- diag(apfa$uniquenesses)
Lambda
Psi
