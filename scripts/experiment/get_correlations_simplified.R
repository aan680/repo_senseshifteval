require("reticulate") #for importing python objects
library(magrittr)
library(optparse)

option_list = list(
  make_option(c("--dataset"),    action="store", default= "", help="how to call the results file"),
  make_option(c("--inputfile"),    action="store", default= "", help="file with the word-level gold standard"),
  make_option(c("--add_stats"),    action="store", default= TRUE, help="whether to add the lexical data stats, only possible if synset column present")
)
opt = parse_args(OptionParser(option_list=option_list))


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

#note: freqs at 1990 as the proxy for overall frequency!!! 
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

evaluate_one_synset <-function(df, correct_column="correct"){ #df holds one synset. #correct_column is either correct_my or correct_HW
	print(df)
	target <- unlist(df$target) #or just df$target?
	synset <- unlist(df$synset) #or just df$synset?
	t <- unlist(df$t)
	if(nrow(df)<2){
	print(index(df))
	return(list(NA,NA,NA,NA,NA,NA,NA))
	}
	correct_by_maxcorr <- df[order(-as.numeric(df$corr)),] %>% .[1,] %>% .[[correct_column]] #descending so negative
	correct_by_minp <- df[order(df$p),] %>% .[1,] %>% .[[correct_column]] #ascending
	correct_by_majorityvote <- df %>%  .[[correct_column]] %>% modal(.,  ties='lowest', na.rm=TRUE, freq=FALSE)
	correct_by_avg <- df %>% evaluate_synset_by_average_vector(.)
	return(list(target, synset, t, correct_by_maxcorr, correct_by_minp, correct_by_majorityvote, correct_by_avg))
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



##MAIN#####
#to test:

#WORD SHIFT EVAL
input <- read.csv(opt$inputfile) %>% .[which(.$gold!=0),]  
if(opt$add_stats){input <- add_lexical_stats(input)}


#this uses my own method, with collected vectors and with at least 5 observations (cos sim values). See R script evaluate_target_ref_t
#THESE RESULTS ARE REPORTED IN THE PAPER!
results <- apply(input, 1, function(x) evaluate_my_way(x["target"], x["ref"], as.numeric(x["t"]), as.numeric(x["gold"]))) %>% do.call(rbind, .) %>% cbind(input, .)
#apply(results,2,as.character) %>% 
write.table(results, file=paste("results/wordshifteval_", opt$dataset, ".csv",sep=""))

#numbers reported in the paper
N <- subset(results, !is.na(results$correct))%>% nrow() #nr of non NA results
N_correct <- sum(as.numeric(results[['correct']]), na.rm=T) 
pct_correct <- N_correct/ N
pct_correct_and_sig <- subset(results, results$correct==1 & results$sig==1) %>% nrow(.) / N_correct
summary <- rbind(100*pct_correct, 100*pct_correct_and_sig, N)
print(summary)
write.table(summary, file=paste("results/summary_wordshifteval_", opt$dataset, ".csv",sep=""))

accuracy <- function(col){
	N_correct <- sum(col, na,rm=T)
	N_valid <- sum(!is.na(col))
	return(100*(N_correct/N_valid))
}


#SENSE SHIFT EVAL
synset_dfs<- split(results, list(as.character(results$synset), results$target, results$t), drop=TRUE)
resultslist <- lapply(synset_dfs, function(x) evaluate_one_synset(x)) 
all <- do.call(rbind, resultslist)
summary <- rbind(accuracy(all$correct_by_maxcorr), accuracy(all$correct_by_minp), accuracy(all$correct_by_majorityvote), accuracy(all$correct_by_avg))

write.table(as.data.frame(summary), file = summaryfile, append = FALSE, sep = ",", row.names=FALSE,col.names = TRUE)) #


####################################################################
####FYI
#this is the method by HW. Beware: missing observations treated as zeros
#these results were not reported but good as a reference
results_hw <- apply(input, 1, function(x) evaluate_by_hw(x["target"], x["ref"], as.numeric(x["t"]), as.numeric(x["gold"]))) %>% do.call(rbind, .) %>% cbind(input, .)
apply(results_hw,2,as.character) %>% write.csv(., file=paste("results/hw_wordshifteval_", opt$dataset, ".csv",sep=""))





