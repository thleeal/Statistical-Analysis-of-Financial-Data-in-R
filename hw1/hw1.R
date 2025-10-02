rm(list = ls())
setwd("C:/Users/USER/Downloads")

# Q1(1)
N = 1024
X = rexp(N, 0.2)

#Q1(2)
x_exp = seq(0, 35, by = 0.01)

par(mfrow = c(2,2))

bw_hist = c(10, 20, 30, 40)
for (i in 1:4){
  hist(X, breaks = bw_hist[i], freq = F, main = "density of the distribution of X and histagram of X")
}

# histogram with bin width = 20 which is the most appropriate and performs best
par(mfrow = c(1,1))
hist(X, breaks = 20, freq = F, main = "density of the distribution of X and histagram of X")
lines(x_exp, dexp(x_exp, 0.2),lwd = 2, col = "red")

#Q1(3)
par(mfrow = c(2,2))

bw_KD = c(0.4, 0.6, 0.8, 1) 
for (i in 1:4){
  KD = density(X, bw = bw_KD[i])
  plot(KD)
}

par(mfrow = c(1,1))

# KDE with bandwidth = 1 which is the most appropriate and performs best
KD = density(X, bw = 1)
plot(KD)
points(x_exp, dexp(x_exp, 0.2), type="l", col="red")

#Q1(4)
par(mfrow = c(2,1))

hist(X, breaks = 20, freq = F, main = "density of the distribution of X and histagram of X")
lines(x_exp, dexp(x_exp, 0.2),lwd = 2, col = "red")

plot(KD)
points(x_exp, dexp(x_exp, 0.2), type="l", col="red")

# Histogram has rectangular bumps while KDE is a smooth curve which is smoother. Also, the histogram depends on the end points of bins which also means that it depends on the choice of origin, however, KDE does not have this dependence.
# First, KDE does not depend on the choice of the origin because it uses a smooth kernel (the Gaussian kernel) which centres at each observation from X, however, it has to depend the choice of bandwidth. 
# Second, histogram has rectangular bumps while KDE is a smooth curve which is smoother because for histogram, we have an interval of width 2 and a density(the height) for that interval so width*height makes those retangles while for KDE, as it uses a smooth, continuous kernal(the Gaussian kernel) and thus the average of kernels centered at the observations from X is still a smooth curve
# I prefer the kernel density estimate of the distribution of X because it is more like the theoretical one and it means that the density described for each observation from X by the kernal one is closer to the theoretical one
# and thus the probability or the area under the curve of each interval calculated by KDE is closer to that calculated by the theorectical one

#Q2
#Plot 1.
#The right tail of Y is heavier than normal distribution.
#The left tail of Y is heavier than normal distribution
#Plot 2.
#The right tail of Z is similarly heavy-tailed to normal distribution
#The left tail of Z is heavier than normal distribution
#Plot 3.
#The right tail of T is heavier than the right tail of X.
#Plot 4.
#The right tail of T is heavier than the right tail of E.

#Q3(1)
n=10000
x_exp=rexp(n)
# cdf of exponential is 1-exp(-x)
cdf_exp =1-exp(-x_exp)
# The density is very much like uniform [0,1]
x_unif = runif(10000)

par(mfrow = c(3, 1))
plot(cdf_exp, main = "CDF of Exp(1)")
hist(cdf_exp, freq=F, main = "Histogram of Exp(1)")
qqplot(x_unif,cdf_exp, main = "Q-Q plot of Unif[0,1] and CDF of Exp(1)")

# thus the cdf of exp(1) really follows an uniform distribution

myrexp = function(N, LAMBDA){
  u_cdf_exp = runif(N)
  return(-log(1-u_cdf_exp)/LAMBDA)
}

class(myrexp(1024,1/1.5))
is.vector((myrexp(1024, 1/1.5)))
length(myrexp(1024, 1/1.5))

#Q3(2)
# mean of exponential distribution = 1/LAMBDA, thus LAMBDA = 1/1.5 = 2/3
my_x = myrexp(1024, 1/1.5)

x_exp2 = rexp(1024, 1/1.5)

par(mfrow = c(2, 1))

plot(my_x)
plot(x_exp2)

par(mfrow = c(1,1))

qqplot(x_exp2, my_x, main = "Q-Q plot of the two samples")
abline(0, 1, col = "red")

# yes, I am satisfied with the performance of the simulation function myrexp
# because the points linked together look like a straight line, y = x, which means that probability of q-quantile for these 2 distributions is the same for most of the quantiles, which is both q
# So they can be said to be having the same distribution which is exponential and that's why I'm satisfied with the performance

#Q4(1)
DHSI <- read.table("DHSI.csv",header = T, sep=",")

attach(DHSI)
summary(DHSI)
names(DHSI)

HSI<-(DHSI$Close)

HSI_time<- seq(from=1987,to=2023.8,length.out=length(HSI)) 

plot(HSI_time, HSI, type="l",xlab="Date",main="Daily HSI index from Jan 1987 to Aug 2023")

DHSILR <- diff(log(HSI))  # compute log returns; 

plot(HSI_time[1:length(DHSILR)], DHSILR, type="l", xlab="Date",main="Daily log return of HSI from Jan 1987 to Aug 2023")

#Q4(2)
par(mfrow=c(2,2))  # to divide the plotting area into 2 by 2  
hist(DHSILR, breaks = 20, freq = F, main="Histogram of DHSILR, #bins = 20")   
hist(DHSILR, breaks=50, freq = F,main="Histogram of DHSILR, #bins = 50") 
hist(DHSILR,breaks=500,  freq = F,main="Histogram of DHSILR, #bins = 500")   
hist(DHSILR,breaks=5000, freq = F,main="Histogram of DHSILR, #bins = 5000")

#Q4(3)
par(mfrow=c(1,1)) 
hist(DHSILR,breaks=50,  freq = F,main="Histogram of DHSILR vs Fitted Normal Density")   

mu_DHSILR <- mean(DHSILR)
sd_DHSILR <- sd(DHSILR)

x<-seq(-0.2,0.2,by=0.001)
y<-dnorm(x,mean=mu_DHSILR,sd = sd_DHSILR)
points(x,y,type="l",col="red")

#Q4(4)
par(mfrow=c(2,2))
KD1 <- density(DHSILR, kernel = "gaussian", bw = .005) 
plot(KD1, type="l",ylim=c(0,45), main="KDE of DHSILR with Gaussian Kernel")
KD2 <- density(DHSILR, kernel = "rectangular", bw = .005) 
plot(KD2, type="l",ylim=c(0,45), main="KDE of DHSILR with rectangular Kernel")
KD3 <- density(DHSILR, kernel = "triangular", bw = .005) 
plot(KD3, type="l",ylim=c(0,45), main="KDE of DHSILR with triangular Kernel")
KD4 <- density(DHSILR, kernel = "cosine", bw = .005) 
plot(KD4, type="l", ylim=c(0,45),main="KDE of DHSILR with cosine Kernel")

#Q4(5)
par(mfrow=c(2,2))
KD5 <- density(DHSILR, kernel = "gaussian", bw = .01) 
plot(KD5, type="l",ylim=c(0,45), main="KDE of DHSILR with BW=.01")
KD6 <- density(DHSILR, kernel = "gaussian", bw = .005) 
plot(KD6, type="l",ylim=c(0,45), main="KDE of DHSILR with BW=.005")
KD7 <- density(DHSILR, kernel = "gaussian", bw = .001) 
plot(KD7, type="l",ylim=c(0,45), main="KDE of DHSILR with BW=.001")

par(mfrow=c(2,1))
hist(DHSILR,breaks=100,  freq = F,main="Histogram & KDE of DHSILR, #bin = 100, bw=0.01",ylim=c(0,45))   
KD8 <- density(DHSILR, kernel = "gaussian", bw = .01) 
points(KD8,type="l",col="red")

KD9 <- density(DHSILR, kernel = "gaussian", bw = .001) 
hist(DHSILR,breaks=100,  freq = F,main="Histogram & KDE of DHSILR, #bin = 50, bw=0.001",ylim=c(0,45))   
points(KD9,type="l",col="red")

# KD9 has a better density estimation

#Q4(6)
# based on empirical distribution
q <- 0.01 
VaR_emp <- - quantile(DHSILR,q)
VaR_emp

# based on normal assumption
mu_DHSILR <- mean(DHSILR)
sd_DHSILR <- sd(DHSILR)

VaR_normal <- - qnorm(q,mu_DHSILR, sd_DHSILR)
VaR_normal

c(VaR_normal, VaR_emp)

#Q4(7)
q<-0.01
VaR_emp <- - quantile(DHSILR,q)
ES_emp <- mean(- DHSILR[- DHSILR > VaR_emp])

mu_DHSILR <- mean(DHSILR)
sd_DHSILR <- sd(DHSILR)
VaR_normal <-  - qnorm(q,mu_DHSILR, sd_DHSILR)

N<-100000
X<-rnorm(N,mu_DHSILR,sd_DHSILR)
ES_normal <- mean( - X[- X > VaR_normal])

c(ES_emp, ES_normal)





