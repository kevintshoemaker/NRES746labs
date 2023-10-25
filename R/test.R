#
#
# library(emdbook)
# library(tidyverse)
#
# data("ReedfrogFuncresp")
# plot(Killed~Initial,ReedfrogFuncresp)
#
#
# params = c(a=0.5,h=1/80)
# dens=c(10,20,30)
# holl2 <- function(params,dens){
#   (params["a"]*dens)/(1+params["a"]*params["h"]*dens)
# }
# holl2(params,dens)
#
#
# curve(holl2(params,x),add=T)
#
#
# inits <- 1:110
#
# lines(inits,qbinom(0.975,inits,holl2(params,inits)/inits))
#
# lines(inits,qbinom(0.025,inits,holl2(params,inits)/inits))
#
#
# ### example 10/10/23
#
# library(emdbook)
# data("MyxoTiter_sum")
# myx <- MyxoTiter_sum %>% filter(grade==1) %>% select(!grade)
#
# myx
#
#
# plot(myx,xlim=c(0,10),ylim=c(0,10))
#
# params = c(slop1=2.5,slop2=0.5,brak=2.5,rate=10)
# brokenstick <- function(params,x){
#   ifelse(x<=params["brak"],params["slop1"]*x,params["slop1"]*params["brak"]+params["slop2"]*(x-params["brak"]))
# }
# plot(myx,xlim=c(0,10),ylim=c(0,10))
# curve(brokenstick(params,x),add=T)
#
# # add plug-in prediction interval
# days=seq(1,12,length=100)
# lines(days,qgamma(0.975,brokenstick(params,days)*params["rate"],params["rate"]),lty=2 )
# lines(days,qgamma(0.025,brokenstick(params,days)*params["rate"],params["rate"]),lty=2 )
#
# nll_myxBrok <- function(params){
#   expTiter <- brokenstick(params,myx$day)
#   -sum(dgamma(myx$titer,expTiter*params["rate"],params["rate"],log=T))
# }
#
# opt <- optim(params,nll_myxBrok,hessian = T)
#
# opt$par
#
# plot(myx,xlim=c(0,10),ylim=c(0,10))
# curve(brokenstick(opt$par,x),add=T)
#
# # add plug-in prediction interval
# days=seq(1,12,length=100)
# lines(days,qgamma(0.975,brokenstick(opt$par,days)*opt$par["rate"],opt$par["rate"]),lty=2 )
# lines(days,qgamma(0.025,brokenstick(opt$par,days)*opt$par["rate"],opt$par["rate"]),lty=2 )
#
# ses <- sqrt(diag(solve(opt$hessian)))
# ses
#
#
# params <- c()
# ricker <- function(params,x){
#
# }
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
