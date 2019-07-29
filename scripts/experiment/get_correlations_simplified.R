require("reticulate") #for importing python objects
library(magrittr)


source_python("scripts/experiment/pickle_reader.py")
source_python("scripts/dataset_creation/wn_stats.py")
source_python("scripts/experiment/hw_functions.py")
source("scripts/experiment/evaluate_target_ref_t.R")

#This section contains code already run but I add it to make this part self-contained.
freqfile<-"data/freqs_engall_unpickled"
df_freq <- read.csv(freqfile, header=TRUE) #, row.names=NULL

#we take the freqs at 1990 as the proxy for overall frequency!!! 
freqs <- df_freq[df_freq$t==1990,]
freqs <- subset(freqs, select=-c(t))



###functions#############################################

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


add_lexical_stats <- function(df){
	df <- add_freq(df)
	df <- add_centrality(df)	
	df <- add_polysemy(df)	
	return(df)
}
################

average_matrix <- function(matrices) { #input is list of data frames of length at least 2
 #calculates the matrix of the averaged t-vectors of all words in a synset. 
	#initiate the new average matrix, with the right dimensions, by simply taking the first matrix (of a random synset word) from the list
	startmatrix <- matrices[[1]]
	word_dim <- dim(startmatrix)
        sum_matrix <- as.matrix(startmatrix) #leave out first two columns, which have w (the word) and t
	valid_obs <- valid(startmatrix)
	#now sum all matrices and divide by NUMBER OF NON-NA (=not zero) observations
        for (i in 2:length(matrices)) {
                if (all(dim(matrices[[i]]) == word_dim)){ #extra check
			current_matrix <- matrices[[i]]
                	sum_matrix <- sum_matrix + as.matrix(current_matrix)
			valid_obs <- valid_obs + as.matrix(valid(current_matrix)) #keeps track of matrix to divide by
			
			
	}#end if	
		else{
		#do_some_printing(matrices, i)
		print("skipping this matrix as it is not the same size as the start matrix!")
						}#end else
        }#end for
        avg_matrix <- sum_matrix / length(matrices)               #!!!!!!! first said valid_obs 
	avg_matrix[avg_matrix[i] == NA, i] <- 0
	#print(dim(avg_matrix))
	return(avg_matrix)

}



evaluate_synset_by_average_vector<- function(df, correct_column="correct_my"){
		if(nrow(subset(df, ! is.na(df[[correct_column]])) == 0)){
			return(NA)
		}
#then the averaged synset representation method
		df <- subset(df, ! is.na(df[[correct_column]])) #!!!!! do this to prevent computations with NA values
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

evaluate_one_synset <-function(df, correct_column="correct_my"){ #df holds one synset. #correct_column is either correct_my or correct_HW
	if(nrow(df)<2){return(NA,NA,NA,NA)}
	correct_by_maxcorr <- df %>% .[order(-.$corr),] %>% df[1,] %>% .[[correct_column]] #descending so negative
	correct_by_minp <- df %>% .[order(.$p),] %>% df[1,] %>% .[[correct_column]] #ascending
	correct_by_majorityvote <- df %>%  .[[correct_column]] %>% modal(.,  ties='lowest', na.rm=TRUE, freq=FALSE)
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

##MAIN#####


#SETTINGS###

my_dataset <- "HW+"
filename_gold <- "gold_wordpair_after_iaa.csv"
#filename_gold <- "sourcedata_with_gold.csv"

#########################################################################################################
#gold standard to evaluate against
get_inputfile <- function(dataset=my_dataset, filename=filename_gold) {paste("/ufs/aggelen/repl_SenseShiftEval/data/", dataset, "/", filename, sep="")}
inputfile <- get_inputfile()

#outputfile
wordshifteval_results_file <- file.path("results", "wordlevel", paste(dataset, ".csv",sep=""))
sensehifteval_results_file <- file.path("results", "senselevel", paste(dataset, ".csv",sep=""))



#EVALUATE WORD SHIFT EVAL
input <- read.csv(inputfile) 
input <- add_lexical_stats(input)
print(head(input))
#the evaluation file with the gold standard
correct_by_me <- apply(input, 1, function(x) evaluate(x["target"], x["ref"], as.numeric(x["t"]), as.numeric(x["gold"]))) #this uses my own method, with collected vectors
correct_by_hw <- apply(input, 1, function(x) correct_by_hw(x["target"], x["ref"], as.numeric(x["gold"]), as.numeric(x["t"])))

results <- cbind(input, correct_by_me, correct_by_hw)
write.csv(results, file=wordshifteval_results_file)
print(head(results))
print(accuracy(correct_by_me))
print(accuracy(correct_by_hw))

#EVALUATE SENSE SHIFT EVAL
#input <- read.csv(wordshifteval_results_file)
#correctdf <- senseshifteval(input)
#summary <- result[,c("target", "synset", "t", "ref", "ref_centrality", "polysemy_ref", "freq.y", "correct_WH", "correct_myimplementation", "gold")]
#write.table(as.data.frame(summary), file = summaryfile, append = FALSE, sep = ",", row.names=FALSE,col.names = TRUE)) #






