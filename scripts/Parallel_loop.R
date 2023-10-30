# Assignment 1:  
library(tweedie) 
library(ggplot2)

simTweedieTest <-  
  function(N){ 
    t.test( 
      tweedie::rtweedie (N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

#USE MORE CORES
library(doParallel) #call for package
library(foreach)

#in my computer there are 4 cores but to run it on 
#more computers let us define a new variable Cores

maxcores<- 6
Cores<-min(parallel::detectCores(), maxcores) 
#in my case Cores will be 4
new_cl<- makeCluster(Cores) #initiate parallel environment: in my case 4
registerDoParallel (new_cl)# register 

#I use .combine='c' to concatenate results
results<- foreach(
  i=1:nrow(df), 
  .combine='c',  
  .packages= c('tweedie')
  ) %dopar% { 
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
  } 

df$share_reject<- results
stopCluster(new_cl)
