require("reticulate") #for importing python objects
library(magrittr)

#pick up the vocabulary of the HistWord vector base. This is the top-100k words averaged over all decades. So this should be the same for all decades. We simply import the 1990s index
source_python("scripts/pickle_reader.py")
source_python("scripts/wn_stats.py")
source_python("scripts/hw_functions.py")
source("scripts/evaluate_target_ref_t.R")

#This section contains code already run but I add it to make this part self-contained.
freqfile<-"data/freqs_engall_unpickled"
df_freq <- read.csv(freqfile, header=TRUE) #, row.names=NULL

#we take the freqs at 1990 as the proxy for overall frequency!!! 
freqs <- df_freq[df_freq$t==1990,]
freqs <- subset(freqs, select=-c(t))
###functions#############################################

inputfile <- function(dataset, filename) {paste("/ufs/aggelen/SenseShiftEval/data/", dataset, filename, sep="")}

check_and_fix_synset_col <- function(df){ #give col correct name (synset) and if Synset('vlabla.n.01') then collect the synset name
	if(any(names(df) == 'syn')){df$synset <- df$syn}
	if(grepl("Synset", df$synset[1])){df$synset <- lapply(df$synset, function(x) unlist(strsplit(as.character(x),split="'"))[2])}
	#df$synset <- lapply(df$synset, function(x) str_trim(x))
	#df$synset <- lapply(df$synset, function(x) str_sub(x, ))
	return(df)

}


add_freq <- function(df){
	df1 <- merge(df, freqs, by.x="target", by.y="word", all.x=TRUE, all.y=FALSE)
	df <- merge(df1, freqs, by.x="ref", by.y="word", all.x=TRUE, all.y=FALSE)
	return(df)
}


add_centrality <- function(df){
	df <- check_and_fix_synset_col(df)
	df$target_centrality <- mapply(function(term, synset) synset_rank(term, synset), df$target, df$synset)
	df$ref_centrality <- mapply(function(term, synset) synset_rank(term, synset), df$ref, df$synset)
	return(df)
}


add_polysemy <- function(df){
	df$polysemy_target <- lapply(df$target, polysemy) %>% unlist(.)
	df$polysemy_ref <- lapply(df$ref, polysemy) %>% unlist(.)
	return(df)
}


add_lexical_stats <- function(dataset, filename){
	data <- inputfile(dataset, filename)
	df <- read.csv(data, sep =",", header=T) %>% unique(.) # %>% .[!is.na(.$correlation_factor),]
	print(nrow(df))
	print(sum(!is.na(df$correlation_factor)))
	df <- add_freq(df)
	df <- add_centrality(df)	
	df <- add_polysemy(df)	
	return(df)
}
################



evaluate_synset_by_average_vector<- function(df){
		if(nrow(subset(df, ! is.na(df$correct))) == 0){
			return(NA)
		}
#then the averaged synset representation method
		df <- subset(df, ! is.na(df$correct)) #!!!!! do this to prevent computations with NA values
		words <- df[,c("ref")] %>% as.list(.) #%>% print(.)
		timespan <- timespan_from_t(u(df$t))
		synset <- u(df$synset)

		if(opt$corpus=="coha"){
		timespan_vectors <- seq(1810,1990,10)
		}
		else{
		timespan_vectors <- seq(1800,1990,10)
		}

		#! some matrices have 0 rows, remove these from the list
		matrices <- lapply(words, allvectors) %>% lapply(., function(df) df<- subset(df, select = -c(w,t))) %>% Filter(function(x) nrow(x) > 0, .)
			

		if(length(matrices)>1){
			refs_average_time_vecs <- average_matrix(matrices) %>% cbind.data.frame(t = timespan_vectors, ., check.rows=TRUE) %>% cbind.data.frame(w = synset, .) #
	
			lapply(matrices, function(df) which(nrow(df)>0))
			target_time_vecs <- allvectors(u(df$target))
  			costimeseries<-cosines(u(df$target), u(df$synset), target_time_vecs, refs_average_time_vecs, timespan, 	suppress_freq_criterion=TRUE)

			df$ref <- NA
			df$sim_1990 <- NA
			entry_from_df <- df[1,] #subset(., select = -c(ref)) #cbind(u(df$target), u(df$synset), u(df$pos), u(df$gold))
			correct <- correlation(df[1,], costimeseries, timespan) %>% .$correct
			return(correct)

			#}#end if1
		}#end if length
		else{
		#print("length matrices <=1")
		return(NA)
		}#end else
}#end function


##############################

evaluate_one_synset <-function(df){
	result <- function(df, type) {
  	switch(type,
         my = df[,'correct_my']
         hw = df[,'correct_HW']
	}
	if(nrow(df)<2){return(NA,NA,NA,NA)}
	correct_by_maxcorr <- df %>% .[order(-.$corr),] %>% df[1,] %>% result(.,"my") #descending so negative
	correct_by_minp <- df %>% .[order(.$p),] %>% df[1,] %>% result(.,"my")  #ascending
	correct_by_majorityvote <- df %>%  result(.,"my") %>% modal(.,  ties='lowest', na.rm=TRUE, freq=FALSE)
	correct_by_avg <- df %>% evaluate_synset_by_average_vector(.)
	return(correct_by_maxcorr, correct_by_minp, correct_by_majorityvote, correct_by_avg)
}

#SenSeShifTEval. this is based on the criteria we use in the paper
senseshifteval <- function(df, criterion, resulttype){ #df is the data frame with the word-level outcomes, so inc. p and correct col
	df$synset <- unlist(df$synset)
	result <- data.frame()
	synset_dfs<- split(df, list(df$synset, df$target, df$t), drop=TRUE)
	allresults <- lapply(synset_dfs, function(x) evaluate_one_synset(x)) %>% rbind(.) ##TRY AND CHECK
	return(allresults)
	#result$synset <- unlist(result$synset)
	
}

###now choose representation for every synset; THIS IS FUTURE WORK we hint at in the paper
senseshifteval_improved <- function(df, summaryfile){
	df$synset <- unlist(df$synset)
	result <- data.frame()
	synset_dfs<- split(df, list(df$synset, df$target, df$t), drop=TRUE)
	for (df in synset_dfs){	
		if(nrow(df)>1){ #only if target term has more than 1 ref term
			df %>% .[order(-.$ref_centrality, -.$polysemy_ref, -.$freq.y),]
			best <- df[1,] #first row is best candidate
			print(best)
			#!!! beware: the correct column uses HW code so based on lots of zeros
			best[, c('rho', 'p', 'correct_HW')] <- rho_p_correct(best$target, best$ref, best$gold, best$t) %>% unlist(.)
			#!! I tried to add my implementation (NAs) but this does not work so I do this post-hoc
			best[,"correct_my"] <- evaluate(best$target, best$ref, best$t, best$gold) #source("scripts/evaluate_target_ref_t.R")
			result <- rbind(result, best)
		}#end if
	}#end for
	#result$synset <- unlist(result$synset)
	summary <- result[,c("target", "synset", "t", "ref", "ref_centrality", "polysemy_ref", "freq.y", "correct_HW", "correct_my", "gold")]
	write.table(as.data.frame(summary), file = summaryfile, append = FALSE, sep = ",", row.names=FALSE,col.names = TRUE) #
	return(result)
}

accuracy <- function(col){
	N_correct <- sum(col, na.rm=T)
	N_valid <- sum(!is.na(col))
	return(N_correct/N_valid)
}

#MAIN


hwwn <- add_lexical_stats("HW", "/gold_wordpair_after_iaa.csv")
ht <- add_lexical_stats("HT", "/gold_old_and_new.csv")



#EVALUATE WORD SHIFT EVAL
input <- read.csv(inputfile) #the evaluation file with the gold standard
correctcol <- apply(input, 1, function(x) evaluate(x["target"], x["ref"], as.numeric(x["t"]), as.numeric(x["gold"]))) 
results <- cbind(input, correctcol) %>% write.csv(wordshifteval_results)
print(accuracy(correctcol))


#EVALUATE SENSE SHIFT EVAL
input <- read.csv(wordshifteval_results)
correctdf <- senseshifteval(input)
summary <- result[,c("target", "synset", "t", "ref", "ref_centrality", "polysemy_ref", "freq.y", "correct_WH", "correct_myimplementation", "gold")]
	write.table(as.data.frame(summary), file = summaryfile, append = FALSE, sep = ",", row.names=FALSE,col.names = TRUE) #
	return(result)





