
# create triplet based on two-way interaction 

# generate x1 and x2, with correlation of 0.2
# set.seed(1234) # to re-create exactly the same numbers
N <- 2000
Sigma <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
X <- MASS::mvrnorm(n = N, Sigma = Sigma, mu = rep(0, 2))
# check
cor(X)

# extract x1 and x2
x1 <- X[,1]
x2 <- X[,2]

# create triplet with NO interaction
y <- 0.2*x1 + 0.4*x2 + rnorm(N, sd = 1)
Data.no.int <- data.frame(y = y, x1 = x1, x2 = x2)

# correlation for the triplet y/x1/x2:
cor(Data.no.int)

# create two-way interaction term
x1.x2 <- x1 * x2

# create y as a function of x1, x2 AND x1.x2
y <- 0.5*x1 + 0.7*x2 + 0.4*x1.x2 # change the 0.4 to increase/decrease the
                                 # strength of the interaction


# create data.frame
Data <- data.frame(y = y, x1 = x1, x2 = x2, x1.x2 = x1.x2)

# quadruplet contains all information: y, x1, x2 AND x1.x2
cor(Data)

# triplet: remove the x1.x2 row/col
cor(Data[,c("y","x1","x2")])

# but different from cor(Data.no.int)

