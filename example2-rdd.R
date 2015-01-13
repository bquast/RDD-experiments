# example2-rdd.R
# Bastiaan Quast
# bquast@gmail.com

# load the package
library(rdd)

## Artificial data example from p.9 rdd manual
x<-runif(1000,-1,1)
cov<-rnorm(1000) # extra auxiliary variable
y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)

# example builds in a treatment effect of 10 points for those selected on x (x>0)
# story? students with high (or higher) ability selected for enriched instruction

# run the rdd function just using X
RDestimate(y~x)
summary(RDestimate(y~x))


plot(RDestimate(y~x))

#compare with our simple ancova approach
rubin = lm(y ~ (x>0) + x)
summary(rubin)

summary(RDestimate(y~x| cov))

rubincov = lm(y ~ (x>0) + x +cov)
summary(rubincov)
