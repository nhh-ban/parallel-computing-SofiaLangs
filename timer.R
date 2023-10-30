library(tidyverse)
library(tictoc)
library(doParallel)
library(foreach)
library(tweedie)
library(furrr)
library(future)
#I use the function seen in class to store the different times and 
#compare the results
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

printTicTocLog() %>%
  knitr::kable()

#clear history
tic.clearlog()

############## Regular loop using solution as given
tic("Regular loop")
source("scripts/Regular_loop.R")
toc(log = TRUE )
printTicTocLog() %>% 
  knitr::kable()

##############Rewriting using furrr

maxcores<- 6
Cores<-min(parallel::detectCores(), maxcores) 
tic(paste0("Furrr loop, ",Cores, " cores") )
source("scripts/furrr_loop.R")
toc(log = TRUE )
printTicTocLog() %>% 
  knitr::kable()

##############Parallel loop using more cores

tic(paste0("Parallel loop, ",Cores, " cores") )
source("scripts/Parallel_loop.R")
toc(log = TRUE )
printTicTocLog() %>% 
  knitr::kable()

#Overall it appears that the fastest loop is the last, namely the parallel loop 
#In class however we saw an example where furrr loop was faster so it may depend 
#on the computer used or/and other activities/pages open on the computer

#also for furrr I had to insert another if function that maybe slows the process


#for the third requirement I tried working on the MTweedieTests function itself 
tic("Modifying directly MTweedieTest function")
source("scripts/Modify_function.R")
toc(log = TRUE )
printTicTocLog() %>% 
  knitr::kable()

#It appears to be slower but again I am not sure if it is a generasible result
