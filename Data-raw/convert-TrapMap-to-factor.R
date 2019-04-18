library(dplyr)
library(magrittr)


TrapMap_factor <- TrapMap %>% 
  mutate_if(.,is.character, as.factor)


save(TrapMap_factor, file  ="TrapMapFactor_V0.1.Rdata")
