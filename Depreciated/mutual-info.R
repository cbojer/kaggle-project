# Entropy-based ----

install.packages("entropy")
library("entropy")
### 1D example 
##### sample from continuous uniformdistribution
x1 = runif(10000)
hist(x1, xlim=c(0,1), freq=FALSE)
# discretize into 10 categories
y1 = discretize(x1, numBins=10, r=c(0,1))
entropy(y1)/log(10)

x2 = rbeta(10000, 750, 250)
hist(x2, xlim=c(0,1), freq=FALSE)
y2 <- discretize(x2, numBins = 10, r = c(0,1))
entropy(y2)/log(10)


x1 <- runif(1000)
x2 <- runif(1000)
y2d <- discretize2d(x1, x2, numBins1 = 10, numBins2 = 10)

h12 <- entropy(y2d)
h12/log(100)

mi.empirical(y2d)



h1 <- y2d %>% rowSums() %>% entropy
h2 <- y2d %>% colSums %>% entropy

h1; h2; h1 + h2 - h12


mi.empirical(y2d)/sqrt(h1*h2)


x1 <- runif(100)
y1 <- 3*x1 + rnorm(100, mean = 0, sd = 0.5)
y2 <- 3*x1 + rnorm(100, mean = 0, sd = 0.3)
y3 <- 3*x1 + rnorm(100, mean = 0, sd = 0.1)
y4 <- 3*x1 
plot(x1, y1)



h1 <- entropy(discretize(x1, numBins = 10))
h2 <- entropy(discretize(y1, numBins = 10))
h12 <- entropy(discretize2d(x1, y1, numBins1 = 10, numBins2 = 10))
mi.empirical(discretize2d(x1, y1, numBins1 = 10, numBins2 = 10))/sqrt(h1*h2)
mi.empirical(discretize2d(x1, y2, numBins1 = 10, numBins2 = 10))/sqrt(h1*h2)

nmi <- function(x, y, num_bins = 10) {
  h_x <- entropy(discretize(x, numBins = num_bins))
  h_y <- entropy(discretize(y, numBins = num_bins))
  mi <- mi.empirical(discretize2d(x, y, numBins1 = num_bins, numBins2 = num_bins))
  mi/sqrt(h_x*h_y)
}

