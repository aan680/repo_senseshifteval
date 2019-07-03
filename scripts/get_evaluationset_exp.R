#this is an experiment , to see if I can simplify my code by using sql in R and embed python code

library(sqldf)
require("reticulate") #for importing python objects
library(magrittr)
require(utils)

source_python("scripts/wn_stats.py")


folder_hw <- "data/HW+"
hw_input <- file.path(folder_hw, "sourcedata.csv")

hw_output <- file.path(folder_hw, "evaluationset_wordpair_exp.csv")


df <- read.csv(hw_input,  row.names=NULL, header=TRUE) %>% unique(.)

allentries <- apply(df, 1, function(x) allinfo_word(x['target'], x['pos']) %>% do.call(rbind, .))
result <- do.call(rbind, allentries) %>% subset(., .$target != .$ref) #N=77
write.csv(result, hw_output)


###############################


