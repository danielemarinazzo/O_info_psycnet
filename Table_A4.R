# table A4
library(lavaan)
library(qgraph)
#set factor loadings and covariance within triplet
l1 <- sqrt(0.99)
l2 <- sqrt(0.70)
l3 <- sqrt(0.30)

R.ecov <- 0.22 #redundancy
Z.ecov <- -0.15 #zero-interaction
S.ecov <- -0.39 #synergy

t1 <- 1 - l1^2 #variance
t2 <- 1 - l2^2
t3 <- 1 - l3^2

R.ecor <- R.ecov / (sqrt(t2) * sqrt(t3)) #correlation
R.t2s <- t2 - abs(R.ecor)*t2 #used in lavaan syntax

R.t3s <- t3 - abs(R.ecor)*t3 #used in lavaan syntax
R.l2s <- 1 * sqrt(abs(R.ecor)*t2)
R.l3s <- sign(R.ecor) * sqrt(abs(R.ecor)*t3)

Z.ecor <- Z.ecov / (sqrt(t2) * sqrt(t3))
Z.l2s <- 1 * sqrt(abs(Z.ecor)*t2)
Z.l3s <- sign(Z.ecor) * sqrt(abs(Z.ecor)*t3)
Z.t2s <- t2 - abs(Z.ecor)*t2
Z.t3s <- t3 - abs(Z.ecor)*t3

S.ecor <- S.ecov / (sqrt(t2) * sqrt(t3))
S.l2s <- 1 * sqrt(abs(S.ecor)*t2)
S.l3s <- sign(S.ecor) * sqrt(abs(S.ecor)*t3)
S.t2s <- t2 - abs(S.ecor)*t2
S.t3s <- t3 - abs(S.ecor)*t3

pop.model <- c("

# model R
R =~ (", l1, ")*R.x + (", l2, ")*R.y + (", l3, ")*R.z
R.bf =~ (", R.l2s, ")*R.y + (", R.l3s, ")*R.z

R.x ~~ (", t1, ")*R.x
R.y ~~ (", R.t2s, ")*R.y
R.z ~~ (", R.t3s, ")*R.z

R ~~ 1*R
R.bf ~~ 1*R.bf
R ~~ 0*R.bf

# model Z
Z =~ (", l1, ")*Z.x + (", l2, ")*Z.y + (", l3, ")*Z.z
Z.bf =~ (", Z.l2s, ")*Z.y + (", Z.l3s, ")*Z.z
Z.x ~~ (", t1, ")*Z.x
Z.y ~~ (", Z.t2s, ")*Z.y
Z.z ~~ (", Z.t3s, ")*Z.z

Z ~~ 1*Z
Z.bf ~~ 1*Z.bf
Z.bf ~~ 0*Z

# model S
S =~ (", l1, ")*S.x + (", l2, ")*S.y + (", l3, ")*S.z
S.bf =~ (", S.l2s, ")*S.y + (", S.l3s, ")*S.z

S.x ~~ (", t1, ")*S.x
S.y ~~ (", S.t2s, ")*S.y

S.z ~~ (", S.t3s, ")*S.z

S ~~ 1*S
S.bf ~~ 1*S.bf
S.bf ~~ 0*S

# residual correlations
R.z ~~ 0.15*Z.z
Z.z ~~ 0.15*S.z
R.z ~~ 0.15*S.z
")
fit <- lavaan(pop.model)

Data1 <- simulateData(pop.model, sample.nobs = 2000)
#pdf("app_A4_spring.pdf")
graph_r1.g<-qgraph(cor(Data1), layout="spring", graph="glasso", 
                   sampleSize=2000, cut=0, theme="colorblind")
#dev.off()