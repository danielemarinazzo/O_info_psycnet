library(lavaan)
library(semPlot)
#set factor loadings and covariance within triplet
l1 <- sqrt(0.99)
l2 <- sqrt(0.70)
l3 <- sqrt(0.30)
A.ecov <- -0.38
t1 <- 1 - l1^2 #variance
t2 <- 1 - l2^2
t3 <- 1 - l3^2

A.ecor <- A.ecov / (sqrt(t2) * sqrt(t3)) #correlation
A.t2s <- t2 - abs(A.ecor)*t2 #used in lavaan syntax
A.t3s <- t3 - abs(A.ecor)*t3 #used in lavaan syntax
A.l2s <- 1 * sqrt(abs(A.ecor)*t2)
A.l3s <- sign(A.ecor) * sqrt(abs(A.ecor)*t3)

pop.model <- c("
# model A
A =~ (", l1, ")*A.x + (", l2, ")*A.y + (", l3, ")*A.z
A.bf =~ (", A.l2s, ")*A.y + (", A.l3s, ")*A.z
A.x ~~ (", t1, ")*A.x
A.y ~~ (", A.t2s, ")*A.y
A.z ~~ (", A.t3s, ")*A.z

A ~~ 1*A
A.bf ~~ 1*A.bf
A ~~ 0*A.bf
")

# generate perfect data
Data <- simulateData(pop.model, sample.nobs = 200L, empirical = TRUE)
fit <- sem(pop.model, data = Data)

#visualize lavaan SEM
semPaths(fit)
