

data("ReedfrogFuncresp")
plot(Killed~Initial,ReedfrogFuncresp)


params = c(a=0.5,h=1/80)
dens=c(10,20,30)
holl2 <- function(params,dens){
  (params["a"]*dens)/(1+params["a"]*params["h"]*dens)
}
holl2(params,dens)


curve(holl2(params,x),add=T)


inits <- 1:110

lines(inits,qbinom(0.975,inits,holl2(params,inits)/inits))

lines(inits,qbinom(0.025,inits,holl2(params,inits)/inits))


