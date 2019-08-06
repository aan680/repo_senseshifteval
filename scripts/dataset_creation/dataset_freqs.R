

library(magrittr)

ht <- read.csv("/ufs/aggelen/SenseShiftEval/data/HT/gold_old_and_new.csv", sep=",", header=T, row.names=NULL) %>% unique(.)

wsct <- read.csv("/ufs/aggelen/SenseShiftEval/data/WSCT/gold_wordpair_after_iaa.csv", sep=",", header=T, row.names=NULL) %>% unique(.)

hwwn <- read.csv("/ufs/aggelen/SenseShiftEval/data/HW/gold_wordpair_after_iaa.csv", sep=",", header=T, row.names=NULL) %>% unique(.)
hwwn <- read.csv("/ufs/aggelen/SenseShiftEval/data/HW/gold_wordpair_after_iaa.csv", sep=",", header=T, row.names=NULL) %>% unique(.)


wsct$syn <- wsct$synset
hwwn$syn <- hwwn$synset

#word pair level, so gold standard as-is, with every row an entry

N_wordpair_one <- function(df){
 df <- subset(df, df$gold==1)
 targetref <- split(df, list(df$target, df$ref, df$t), drop=TRUE)
 return(length(targetref))
}

N_one <- function(df){
return(nrow(subset(df, df$gold==1)))
}

N_wordpair_minusone <- function(df){
 df <- subset(df, df$gold==-1)
 targetref <- split(df, list(df$target, df$ref, df$t), drop=TRUE)
 return(length(targetref))
}

N_zero <- function(df){
return(nrow(subset(df, df$gold==0)))
}

N_minusone <- function(df){
return(nrow(subset(df, df$gold==-1)))
}

N_total <- function(df){
	targetref <- split(df, list(df$target, df$ref, df$t), drop=TRUE)
	return(length(targetref))
	}

N_synset_target_all <- function(df){
	dfs <- split(df, list(df$syn, df$target, df$t), drop=TRUE)
	return(length(dfs))
	}

N_target_sense_in_gold <- function(df, value){
	length <- subset(df, df$gold==value) %>% split(., list(.$syn, .$target, .$t), drop=TRUE) %>% length(.)
	return(length)
	}


allstats <- function(df){
	print('one - minusone - zero for word level:')
	one <- N_one(df) %>% print(.)
	minus <- N_minusone(df) %>% print(.)
	zero <- N_zero(df) %>% print(.)
	print('TOT: ')
	print(one + minus + zero)
	print('one - minusone - zero for synset level:')
	one <- N_target_sense_in_gold(df, 1)  %>% print(.)
	minus <- N_target_sense_in_gold(df, -1)  %>% print(.)
	zero <- N_target_sense_in_gold(df, 0)  %>% print(.)
	print('TOT: ')
	print(one + minus + zero)
}

synsets <-  hwwn %>% split(., list(.$syn, .$target, .$t), drop=TRUE) 
lapply(synsets, function(x) unique(x)$gold==-1)


allstats(wsct)
allstats(hwwn)
allstats(ht)






















