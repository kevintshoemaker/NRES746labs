
############################################################
####                                                    ####  
####  NRES 746, Lab 1                                   ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Computational algorithms and standard statistics  ####
############################################################


myfunc <- function(vector){     # demo function (not part of lab)
    sum(vector)
}


######################
# LAB 1 answer key (lots of other possibilities)
######################

#############
## exercise 1a

CoefVar <- function(vector){    
  cv <- sd(vector,na.rm=TRUE)/mean(vector,na.rm=TRUE)
  return(cv)     
}
#CoefVar(c(2,3,4,3,2,3,4))   # test your function


############
# Explore the "trees" dataset

?trees

summary(trees)    # learn more about the data

trees$Height    # extract the "Height" column from the trees dataset.

CoefVar(trees$Height)    # run your new function!


#############
## exercise 1b

DrawLine <- function(x,y){  
  plot(y~x)
  mod <- lm(y~x)
  coefs <- coef(mod)
  abline(mod)
  return(coefs)
}


# ?faithful
# summary(faithful)

DrawLine(faithful$waiting,faithful$eruptions)    # test your function using the old faithful eruptions data


#############
## exercise 1c

DrawLine2 <- function(x,y,smooth=TRUE,span=1){  
  plot(y~x)
  if(smooth==FALSE){
    mod <- lm(y~x)
    coefs <- coef(mod)
    abline(coefs)
  }else{
    coefs <- loess(y~x,span=span)
    scatter.smooth(y~x,span=span)
  }
  return(coefs)     
}

xvec <- c(1:10)
yvec <- rnorm(length(xvec),c(2:6,7:3),2)
DrawLine2(xvec,yvec,smooth=T,span=0.5)    # run your new function!

DrawLine2(x=trees$Height,y=trees$Volume,smooth=F,span=NA)

DrawLine2(faithful$waiting,faithful$eruptions,smooth=T,span=.5)    # test using the old faithful eruptions data

DrawLine2(faithful$waiting,faithful$eruptions,smooth=T,span=.1)


#############
## exercise 1d

####################
# CENTRAL LIMIT THEOREM function
####################

CLTdemo <- function(n.samples=1000,sample.size=10,min=10,max=20){

    infinity <- 100000       # number approximating infinity      
    data_population <- runif(infinity,min,max)      

     #######
     # Draw multiple samples from the pool (population) of possible data.  

    samplemean <- numeric(n.samples)     # set up storage vector
    for(i in 1:n.samples){      # for each replicate (independent random sample)
      sample <- sample(data_population,sample.size)   # draw an independent random sample from the population of interest
      samplemean[i] <- mean(sample)    # compute and record the sample mean
    }
    
    layout(matrix(1:2,nrow=1))
    hist(samplemean)    
    qqnorm(samplemean)
    toret <- shapiro.test(samplemean)
    return(toret)
}


answer1d <- CLTdemo(n.samples=5000,sample.size=4,min=10,max=20)    # run your new function!



##############
# Exercise 1e

reps <- 50
sizes <- seq(3,10,1)
prop <- numeric(length(sizes))
i=sizes[1]
counter=1
for(i in sizes){
  okay <- logical(reps)
  j=1
  for(j in 1:reps){
    temp <- CLTdemo(n.samples=1000,sample.size=i,min=10,max=20,plot=FALSE)    # run your new function!
    okay[j] <- temp$p.value<=0.05
  }
  num <- length(which(okay))
  prop[counter] <- num/reps   # proportion rejected
  counter=counter+1
}

par(mfrow=c(1,1))
names(prop) <- sizes
barplot(prop,xlab="sample size",ylab="Normality rejected (proportion)")

# practice space for regression analysis!


#############
# CHALLENGE 4: brute force z-tests!
#############

##### from lecture...

z.test.algorithm <- function(sample, pop.mean, pop.sd){
  
  #############
  # Compute the sample statistic
  #############
  
  observed_mean <- mean(sample)
  
  sample.size <- length(sample)   # compute sample size

  #################
  # Generate SAMPLING DISTRIBUTION
  #################
  
  reps <- 1000                 # set the number of replicate samples
  null_dist <- numeric(reps)       # initialize a storage structure for sampling distribution
  
  for(i in 1:reps){            # for each replicate... 
    nullsamp <- rnorm(sample.size,pop.mean,pop.sd)      # draw a sample assuming no treatment effect       
    null_dist[i] <- mean(nullsamp)           # compute and store the samples produced under the null hypothesis
  }
  
  more.extreme <- length(which(null_dist<=observed_mean))       # how many of these samples equal or exceed the sample statistic?
  p_value <- more.extreme/reps
  
  to_return <- list()   # initialize object to return
  
  to_return$null_dist <- null_dist
  to_return$p_value <- p_value
  to_return$observed_mean <- observed_mean
  
  return(to_return)

}

# population.mean = 4.5
# population.sd = 0.9
# my.sample = c(3.14,3.27,2.56,3.77,3.34,4.32,3.84,2.19,5.24,3.09)
# ztest <- z.test.algorithm(sample = my.sample, pop.mean=population.mean, pop.sd=population.sd )


###############
# Question 4a

z.test.q4a <- function(sample, pop.mean, pop.sd, onetail=T){
  
  #############
  # Compute the sample statistic
  #############
  
  observed_mean <- mean(sample)
  
  sample.size <- length(sample)   # compute sample size

  #################
  # Generate SAMPLING DISTRIBUTION
  #################
  
  reps <- 1000                 # set the number of replicate samples
  null_dist <- numeric(reps)       # initialize a storage structure for sampling distribution
  
  for(i in 1:reps){            # for each replicate... 
    nullsamp <- rnorm(sample.size,pop.mean,pop.sd)      # draw a sample assuming no treatment effect       
    null_dist[i] <- mean(nullsamp)           # compute and store the sample produced under the null hypothesis
  }
  
  if(onetail==T){
    more.extreme <- length(which(null_dist<=observed_mean))       # how many of these samples are more extreme than the sample statistic?
    p_value <- more.extreme/reps
  }else{
    obs_dev <- abs(observed_mean-pop.mean)
    low <- pop.mean-obs_dev       # limit to extremeness in low body mass
    high <- pop.mean+obs_dev       # limit to extremeness in high body mass
    more.extreme <- length(which((null_dist<=low)|(null_dist>=high)))
    p_value <- more.extreme/reps
  }
 
  
  to_return <- list()   # initialize object to return
  
  to_return$null_dist <- null_dist
  to_return$p_value <- p_value
  to_return$observed_mean <- observed_mean
  
  return(to_return)

}



population.mean = 4.5
population.sd = 0.9
my.sample = c(5.14,3.27,4.56,3.77,3.34,4.32,3.84,3.19,5.24,4.09)
z.test.q4a(sample = my.sample, pop.mean=population.mean, pop.sd=population.sd,onetail=F )

   # also try using the code from lecture to compare against a "real" z-test!


###############
# Question 4b

z.test.q4b <- function(sample=my.sample, null.data){
  
  #############
  # Compute the sample statistic
  #############
  
  observed_mean <- mean(sample)
  
  sample.size <- length(sample)   # compute sample size

  #################
  # Generate SAMPLING DISTRIBUTION
  #################
  
  reps <- 1000                 # set the number of replicate samples
  null_dist <- numeric(reps)       # initialize a storage structure for sampling distribution
  
  for(i in 1:reps){            # for each replicate... 
    nullsamp <- sample(null.data,sample.size,replace=T)      # draw a sample assuming no treatment effect       
    null_dist[i] <- mean(nullsamp)           # compute and store the sample produced under the null hypothesis
  }
  
  
  more.extreme <- length(which(null_dist<=observed_mean))       # how many of these samples are more extreme than the sample statistic?
  p_value <- more.extreme/reps
  
  to_return <- list()   # initialize object to return
  
  to_return$null_dist <- null_dist
  to_return$p_value <- p_value
  to_return$observed_mean <- observed_mean
  
  return(to_return)

}



null.data=c(2.2,3.86,6.39,4.6,3.43,5.16,4.36,4.22,6.31,4.61,5.13,4.12,4.64,4.03,5.01,7.33,5.35,4.7,2.82,4.87,3.87,5.95,5.28,4.02,3.58,4.03,5.38,5.5,3.07,3.29,3.45,5.25,5.7,1.26,5.28,4.19,4.76,4.2,4.81,2.5)
my.sample = c(3.14,3.27,2.56,3.77,3.34,4.32,3.84,2.19,5.24,3.09)
z.test.q4b(sample = my.sample, null.data )


#####################
# CHALLENGE 5: Bootstrapping regression coefficients!
#####################

# first, grab the code from the lecture (bootstrapping R-squared)

#########
# Function for returning a vector of R-squared statistics from models regressing a response variable on multiple possible predictor variables
   # here we assume that all columns in the input data frame that are NOT the response variable are potential predictor variables.

Rsquared <- function(df,responsevar="Volume"){    # univariate models only- interaction and multiple regression not implemented here
  response <- df[,responsevar]       # extract the response variable
  names <- names(df)                  
  rsq <- numeric(length(names))        # named storage vector
  names(rsq) <- names(df)               
  rsq <- rsq[names(rsq)!=responsevar]           # assume that all columns that are not the response variable are possible predictor variables
  for(i in names(rsq)){         # loop through predictors
      predictor <- df[,i]                  # extract this predictor
      model <- lm(response~predictor)       # regress response on predictor
      rsq[i] <- summary(model)$r.square       # extract R-squared statistic
  }
  return(rsq)     
}

boot_sample <- function(df,statfunc,n_samples,n_stats,responsevar="Volume"){
  indices <- c(1:nrow(df))
  output <- matrix(NA,nrow=n_samples,ncol=n_stats)        # storage object- to store a single bootstrapped sample from the original data
  
  for(i in 1:n_samples){              # for each bootstrap replicate:
    boot_rows <- sample(indices,size=nrow(df),replace=T)         # randomly sample observations with replacement
    newdf <- df[boot_rows,]                       # dataframe of bootstrapped observations
    output[i,] <- statfunc(newdf,responsevar)                 # generate statistics from the bootstrapped sample  (e.g., compute Rsquared after regressing y on all possible x variables)
  }
  return(output)
}

# boot <- boot_sample(df=trees,statfunc=Rsquared,n_samples=1000,n_stats=2)   # generate test statistics (Rsquared vals) for 1000 bootstrap samples
# confint <- apply(boot,2,function(t)  quantile(t,c(0.025,0.5,0.975)))       # summarize the quantiles to generate confidence intervals for each predictor variable
# colnames(confint) <- names(stat)
# t(confint)



#############
## exercise 5a

RegressionCoefs <- function(df=trees,responsevar="Volume"){    # univariate models only- interaction and multiple regression not implemented here
  response <- df[,responsevar]       # extract the response variable
  names <- names(df)                  
  coefs <- numeric(length(names))        # named storage vector
  names(coefs) <- names(df)               
  coefs <- coefs[names(coefs)!=responsevar]           # assume that all columns that are not the response variable are possible predictor variables
  i=names(coefs)[1]
  for(i in names(coefs)){         # loop through predictors
      predictor <- df[,i]                  # extract this predictor
      model <- lm(response~predictor)       # regress response on predictor
      coefs[i] <- coefficients(model)["predictor"]       # extract slope term
  }
  return(coefs)     
}



RegressionCoefs(df=trees,responsevar="Volume")   # should return two regression coefficients


#############
## exercise 5b


 # note: this is copied from the lecture code...
boot_sample <- function(df,statfunc,n_samples,responsevar="Volume"){
  indices <- c(1:nrow(df))
  output <- matrix(NA,nrow=n_samples,ncol=ncol(df)-1)        # storage object- to store a single bootstrapped sample from the original data
  
  for(i in 1:n_samples){              # for each bootstrap replicate:
    boot_rows <- sample(indices,size=nrow(df),replace=T)         # randomly sample observations with replacement
    newdf <- df[boot_rows,]                       # dataframe of bootstrapped observations
    output[i,] <- statfunc(newdf,responsevar)                 # generate statistics from the bootstrapped sample  (e.g., compute Rsquared after regressing y on all possible x variables)
  }
  return(output)
}


BootCoefs <- function(df=trees,statfunc=RegressionCoefs,n_samples=1000,responsevar="Volume"){    # univariate models only- interaction and multiple regression not implemented here
  boot <- boot_sample(df=df,statfunc=statfunc,n_samples=n_samples,responsevar=responsevar)   # generate test statistics (Rsquared vals) for 1000 bootstrap samples
  confint <- apply(boot,2,function(t)  quantile(t,c(0.025,0.5,0.975)))       # summarize the quantiles to generate confidence intervals for each predictor variable
  colnames(confint) <- paste("stat",c(1:(ncol(df)-1)),sep="")
  return(t(confint))     
}



BootCoefs(df=trees,statfunc=RegressionCoefs,n_samples=1000,responsevar="Volume")

df <- mtcars[,c(1,3,4,6)]
responsevar="mpg"
BootCoefs(df=df,
          statfunc=RegressionCoefs,
          n_samples=1000,
          responsevar=responsevar
)


############
# Exercise 5c

BootCoefs(df=trees,statfunc=RegressionCoefs,n_samples=1000,n_stats=2,responsevar="Volume")

confint.lm(lm(Volume~Girth,trees))    # compare with lm() 
confint.lm(lm(Volume~Height,trees))    # compare with lm() 


###########
# end of lab 1
###########

