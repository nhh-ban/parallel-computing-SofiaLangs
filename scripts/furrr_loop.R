#Using furrr
# Assignment 1:  
library(tweedie) 
library(ggplot2)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
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

library(purrr)
library(furrr)
library(future)

#Let us define the number of cores
maxcores<- 6
Cores<-min(parallel::detectCores(), maxcores) 

#running the plan command as seen in class I encountered an
#error so I tried a different approach (it was due to the operating system)

if(.Platform$OS.type=="windows"){
  plan(multisession, workers = Cores)
} else {
  plan(multicore, workers = Cores)
}

df$share_reject<-
  future_map_dbl(seq_len(nrow(df)), function(i) {
    MTweedieTests(N = df$N[i], M = df$M[i], sig=.05)
  })
