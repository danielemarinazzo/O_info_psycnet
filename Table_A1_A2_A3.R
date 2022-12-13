# Table A1, A2 and A3

library(lavaan)
library(qgraph)
#set factor loadings and covariance within triplet
l1 <- sqrt(0.99)
l2 <- sqrt(0.70)
l3 <- sqrt(0.30)
S.ecov <- -0.39 # synergy
#Z.ecov <- -0.14849 # ~zero interaction information
#R.ecov <- 0.22 # redundancy
t1 <- 1 - l1^2 #variance
t2 <- 1 - l2^2
t3 <- 1 - l3^2

S.ecor <- S.ecov / (sqrt(t2) * sqrt(t3)) #correlation
S.t2s <- t2 - abs(S.ecor)*t2 #used in lavaan syntax
S.t3s <- t3 - abs(S.ecor)*t3 #used in lavaan syntax
S.l2s <- 1 * sqrt(abs(S.ecor)*t2)
S.l3s <- sign(S.ecor) * sqrt(abs(S.ecor)*t3)

pop.model <- c("
# model S
S =~ (", l1, ")*S.x + (", l2, ")*S.y + (", l3, ")*S.z
S.bf =~ (", S.l2s, ")*S.y + (", S.l3s, ")*S.z
S.x ~~ (", t1, ")*S.x
S.y ~~ (", S.t2s, ")*S.y
S.z ~~ (", S.t3s, ")*S.z

S ~~ 1*S
S.bf ~~ 1*S.bf
S ~~ 0*S.bf
")

fit <- lavaan(pop.model)
COV <- lavInspect(fit, "implied")$cov
#COV
#pdf("app_syn.pdf")
graph_r1.g<-qgraph(COV, layout="circular", graph="glasso", 
                   sampleSize=2000, cut=0, theme="colorblind")
#dev.off()

