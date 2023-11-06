#Modify directly the MTweedieTests function and split the M simulations in more cores 
# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(future)
library(furrr)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
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

MTweedieTests <- 
  function(N,M,sig, Cores){ 
    simManyTests<-function(chunk_size){
      sum(replicate(chunk_size, simTweedieTest(N)) < sig)
    }
    
  #Split M on many cores to replicate 
    chunk_sizes<-rep(M %/% Cores, Cores)
    chunk_sizes[Cores]<-chunk_sizes[Cores] + (M %% Cores) #handle remainder so that it is repeated M times
    
 #Compute the sum of results from each core
    total <- sum(future_map_int(chunk_sizes, simManyTests))
    total/M
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05, Cores) 
} #I left it the same just added Cores argument
