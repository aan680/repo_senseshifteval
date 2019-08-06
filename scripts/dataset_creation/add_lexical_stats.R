library(magrittr)
library(optparse)

source("scripts/experiment/get_correlations_simplified.R") #this contains the functions add_centrality and add_polysemy

option_list = list(
  make_option(c("--inputfile"),    action="store", default= "", help="dataset file to add centrality and polysemy columns to"),
  make_option(c("--outputfile"),    action="store", default= "", help="output file")
)
opt = parse_args(OptionParser(option_list=option_list))

df <- read.csv(opt$inputfile, header=TRUE) %>% add_centrality %>% add_polysemy
write.csv(df, file=opt$outputfile)

intersect(x, y)

#also add overlap
hw_plus <- "/ufs/aggelen/SenseShiftEval/data/HW/gold_wordpair_after_iaa.csv" %>% read.csv(header=T)  %>% .[,c("target","ref")]
wsct <- "/ufs/aggelen/SenseShiftEval/data/WSCT/gold_wordpair_after_iaa.csv" %>% read.csv(header=T)  %>% .[,c("target","ref")]
ht <- "/ufs/aggelen/SenseShiftEval/data/HT/gold_old_and_new.csv" %>% read.csv(header=T)  %>% .[,c("target","ref")]
hw <- "/ufs/aggelen/SenseShiftEval/data/HW/gold_hamilton_28.csv" %>% read.csv(header=T)  %>% .[,c("target","ref")]


all_datasets <- c(hw_plus, wsct, ht, ht)


sapply(all_datasets, function(x) sapply(all_datasets, function(y) intersect(x$target,y$target))

library(dplyr)
inner_join()
