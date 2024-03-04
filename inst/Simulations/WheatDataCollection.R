library(dplyr)
library(tidyr)
library(AshHaven)

ITERATION <- 1000
FARM_SIZE <- 1:100

Sim <- expand.grid(i = seq_len(ITERATION), 
                   fs = FARM_SIZE)


library(parallel)
cl <- makeCluster(detectCores() - 1)

clusterEvalQ(cl, library(AshHaven))


Sim <- 
  Sim %>% 
  mutate(S = 
           clusterMap(
             cl,
             function(fs, i){
               S <- new_subchunk()
               S$subchunk_apply(seq_len(fs), 
                                make_block_wheat)
               while(!S$all_mature()){
                 S$assign_random_ticks()
               }
               
               now <- Sys.time()
               
               ThisBlock <- S$block_status()
               ThisBlock$crop <- "Wheat"
               ThisBlock$farm_size <- fs
               ThisBlock$iteration <- i
               ThisBlock$simulation_datetime <- now
               
               ThisTick <- S$tick_history()
               ThisTick$crop <- "Wheat"
               ThisTick$farm_size <- fs
               ThisTick$iteration <- i
               ThisTick$simulation_datetime <- now
               
               write.table(ThisBlock, 
                           "D:/SpecialStudy/Crop/WheatAt25-BlockData.csv", 
                           row.names = FALSE, 
                           col.names = TRUE, 
                           append = TRUE, 
                           sep = ",")
               write.table(ThisTick, 
                           "D:/SpecialStudy/Crop/WheatAt25-TickData.csv", 
                           row.names = FALSE, 
                           col.names = TRUE, 
                           append = TRUE, 
                           sep = ",")
               
               S
             }, 
             fs = fs, 
             i = i,
             SIMPLIFY = FALSE))
