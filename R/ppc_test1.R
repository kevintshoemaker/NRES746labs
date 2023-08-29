############
# Bayesian analysis and posterior predictive check
############


library(jagsUI)
library(modeest)

cat(
"
model{

  # likelihood
  for(i in 1:ndata){
    titer[i] ~ dgamma(shape,rate[i])
    expected[i] <- param.a[grade[i]]*days[i]*exp(-1*param.b[grade[i]]*days[i])
    rate[i] <- shape/expected[i]
  }

  # priors
  for(g in 1:ngrades){
    param.a[g] ~ dgamma(0.001,0.001)
    param.b[g] ~ dgamma(0.001,0.001)
  }
  shape ~ dgamma(0.001,0.001)

}
",
file="testjags1.txt"
)

myxdat <- emdbook::MyxoTiter_sum
myxdat$grade2 <- as.numeric(as.factor(myxdat$grade))

data.for.jags <- list(
  titer = myxdat$titer,
  days = myxdat$day,
  grade = myxdat$grade2,
  ndata = nrow(myxdat),
  ngrades = max(myxdat$grade2)
)

initfunc <- function(){
  list(
    param.a = runif(max(myxdat$grade2),1,5),
    param.b = runif(max(myxdat$grade2),0.1,0.2),
    shape = runif(1,20,150)
  )
}
initfunc()

params.to.store <- c(
  "param.a",
  "param.b",
  "shape"
)

?jags

mod <- jags(data=data.for.jags,inits=initfunc, parameters.to.save = params.to.store,
            model.file="testjags1.txt",n.chains=3,n.adapt=1000,n.iter=100000,n.burnin=25000,n.thin=5,
            parallel=T)



mcmcout <- mod$samples

jpd <- mod$sims.list


plot(mcmcout[,"param.a[1]"])
plot(mcmcout[,"param.a[2]"])

gelman.diag(mcmcout)



##############
# posterior predictive check
##############


Ricker <- function(x,a,b){
  a*x*exp(-b*x)
}

lots = 1000

nMCMC = nrow(jpd$param.a)

SSEobs <- numeric(lots)
SSEsim <- numeric(lots)

allsimdat <- matrix(0,nrow=lots,ncol=nrow(myxdat))

i=1
for(i in 1:lots){
  thisrn <- sample(1:nMCMC,1)
  thisa <- jpd$param.a[thisrn,]
  thisb <- jpd$param.b[thisrn,]
  thisshape <- jpd$shape[thisrn]

  expval <- Ricker(myxdat$day,thisa[myxdat$grade2],thisb[myxdat$grade2])

  allsimdat[i,] <- rgamma(nrow(myxdat),shape=thisshape,rate=thisshape/expval)

  SSEobs[i] <- sum((myxdat$titer-expval)^2)
  SSEsim[i] <- sum((allsimdat[i,]-expval)^2)

}

plot(SSEobs,SSEsim)
abline(0,1)

pval <- length(which(SSEsim>SSEobs))/lots

myxsub <- subset(myxdat,grade2==1)
thissub <- allsimdat[,myxdat$grade2==1]

thissub <- thissub[,!duplicated(myxsub$day)]

colnames(thissub) = myxsub$day[!duplicated(myxsub$day)]

thissub <- thissub[,order(colnames(thissub))]

boxplot(thissub,xlab="days",ylab="titer",at=as.numeric(colnames(thissub)))
points(myxsub$day,myxsub$titer,col="red",cex=3,pch=20)



