
require("reticulate") #for importing python objects

source_python("scripts/dataset_creation/wn_stats.py")


###############################################




###functions###

)



add_polysemy_cols <- function(df){ #this uses the function polysemy from the python file
	df$polysemy_target <- lapply(df$target, polysemy)
	df$polysemy_ref <- lapply(df$ref, polysemy)
	return(df)
}

add_freq_cols <- function(df, df_freq){
	df <- subset(df, select = -c(freq.x,freq.y)) #if needed, remove old cols
	df$ref <- df$ref %>% tolower
	df$target <- df$target %>% tolower
	df1 <- merge(df,freqs, by.x="target", by.y="word", all.x=TRUE, all.y=FALSE)
	df <- merge(df1,freqs, by.x="ref", by.y="word", all.x=TRUE, all.y=FALSE)
	return(df)
}

add_centrality <- function(df){
	df$target_centrality <- mapply(function(term, synset) synset_rank(term, synset), df$target, df$synset)
	df$ref_centrality <- mapply(function(term, synset) synset_rank(term, synset), df$ref, df$synset)
	return(df)
}

summarize <- function(col, N_dataset){
	#return(table(col))
	tot <- sum(col==0, na.rm=T)+sum(col, na.rm=T) #number of entries in this category (e.g. high-low)
	nr_correct <- sum(col, na.rm=T)
	return(cbind(prop=100*(nr_correct/tot), N=100*(tot/N_dataset)))
}

#polysemy
high <- function(x, threshold) {x > threshold}
low <- function(x, threshold) {x <= threshold}
high_high <- function(df, t) {df[high(df$polysemy_target, t) & high(df$polysemy_ref, t),"correct"] %>%summarize(., N_dataset=nrow(df))}
high_low <- function(df, t) {df[high(df$polysemy_target, t) & low(df$polysemy_ref, t),"correct"] %>%summarize(., N_dataset=nrow(df))}
low_low <- function(df, t) {df[low(df$polysemy_target, t) & low(df$polysemy_ref, t),"correct"]%>%summarize(., N_dataset=nrow(df))}
low_high <- function(df, t) {df[low(df$polysemy_target, t) & high(df$polysemy_ref, t),"correct"] %>%summarize(., N_dataset=nrow(df))}


#centrality
strong <- function(x) {x <= 1}
weak <- function(x) {x > 1}
strong_strong <- function(df) {df[strong(df$target_centrality) & strong(df$ref_centrality),"correct"] %>% summarize(., N_dataset=nrow(df))}
strong_weak <- function(df) {df[strong(df$target_centrality) & weak(df$ref_centrality),"correct"] %>% summarize(., N_dataset=nrow(df))}
weak_strong<- function(df) {df[weak(df$target_centrality) & strong(df$ref_centrality),"correct"] %>% summarize(., N_dataset=nrow(df))}
weak_weak <- function(df) {df[weak(df$target_centrality) & weak(df$ref_centrality),"correct"] %>% summarize(., N_dataset=nrow(df))}

#frequency
frequent <- function(x, threshold) {x > threshold}
infrequent <- function(x, threshold) {x <= threshold}
freq_freq <- function(df, t) {df[frequent(df$freq.x, t) & frequent(df$freq.y, t),"correct"] %>% summarize(., N_dataset=nrow(df))}
freq_infreq <- function(df, t) {df[frequent(df$freq.x, t) & infrequent(df$freq.y, t),"correct"] %>% summarize(., N_dataset=nrow(df))}
infreq_infreq <- function(df, t) {df[infrequent(df$freq.x, t) & infrequent(df$freq.y, t),"correct"] %>% summarize(., N_dataset=nrow(df))}
infreq_freq <- function(df, t) {df[infrequent(df$freq.x, t) & frequent(df$freq.y, t),"correct"] %>% summarize(., N_dataset=nrow(df))}



get_stats_and_summarise <- function(file, t_poly, t_freq=10000, is_hw=FALSE, add_freqs=FALSE){ #if is_hw then drop centrality (as no synset relation)
	df <- read.csv(file, sep =",", header=T) %>% unique(.) %>% .[!is.na(.$corr),]
	N <- nrow(df)
	if(is_hw | add_freqs){ #then todo add polysemy and freq
		df <- add_polysemy_cols(df)
        	#df <- add_centrality(df) #not appliccable to HW!!!
		#now for freq
		freqfile<-"data/freqs_engall_unpickled"
		df_freq <- read.csv(freqfile, header=TRUE) #, row.names=NULL
		#we take the freqs at 1990 as the proxy for overall frequency!!! 
		freqs <- df_freq[df_freq$t==1990,]
		df <- add_freq_cols(df, freqs)
	}
	#print(df)
	polysemy <- cbind(low_low(df, t_poly), low_high(df, t_poly), high_low(df, t_poly), high_high(df, t_poly)) %>% round(., 1) 
	rownames(polysemy) <- "poly"
	colnames(polysemy)<-c("LL", "n","LH", "n","HL", "n","HH", "n")
	frequency <- cbind(infreq_infreq(df, t_freq), infreq_freq(df, t_freq), freq_infreq(df, t_freq), freq_freq(df, t_freq)) %>% round(., 1)
	rownames(frequency) <- "freq"
	colnames(frequency) <-c("LL", "n","LH", "n","HL", "n","HH", "n")
	print(polysemy)
	print(frequency)
	if(!is_hw){
	centrality <- cbind(weak_weak(df), weak_strong(df), strong_weak(df), strong_strong(df)) %>% round(., 1)
	rownames(centrality) <- "centrality"
	colnames(centrality) <-c("LL", "n","LH", "n","HL", "n","HH", "n")
	print(centrality)
	}
}



#all


resultfile <- function(corpus, dataset) {paste("/ufs/aggelen/repl_SenseShiftEval/results/", corpus, "wordshifteval_", dataset, ".csv",sep="")}

ht2 <- resultfile("SGNS", "HT") %>% get_stats_and_summarise(., t_poly=2)
hw2 <- resultfile("SGNS", "HW") %>% get_stats_and_summarise(., t_poly=2, is_hw=TRUE)
hwplus2 <- resultfile("SGNS", "HW+") %>% get_stats_and_summarise(., t_poly=2)

ht3 <- resultfile("SGNS", "HT") %>% get_stats_and_summarise(., t_poly=3)
hw3 <- resultfile("SGNS", "HW") %>% get_stats_and_summarise(., t_poly=3, is_hw=TRUE)
hwplus3 <- resultfile("SGNS", "HW+") %>% get_stats_and_summarise(., t_poly=3)

ht4 <- resultfile("SGNS", "HT") %>% get_stats_and_summarise(., t_poly=4)
hw4 <- resultfile("SGNS", "HW") %>% get_stats_and_summarise(., t_poly=4, is_hw=TRUE)
hwplus4 <- resultfile("SGNS", "HW+") %>% get_stats_and_summarise(., t_poly=4)

ht5k <- resultfile("SGNS", "HT") %>% get_stats_and_summarise(., t_poly=4, t_freq=5000)
hw5k <- resultfile("SGNS", "HW") %>% get_stats_and_summarise(., t_poly=4, is_hw=TRUE, t_freq=5000)
hwplus5k <- resultfile("SGNS", "HW+") %>% get_stats_and_summarise(., t_poly=4, t_freq=5000)

ht100k <- resultfile("SGNS", "HT") %>% get_stats_and_summarise(., t_poly=4, t_freq=100000)
hw100k <- resultfile("SGNS", "HW") %>% get_stats_and_summarise(., t_poly=4, is_hw=TRUE, t_freq=100000)
hwplus100k <- resultfile("SGNS", "HW+") %>% get_stats_and_summarise(., t_poly=4, t_freq=100000)

ht100k <- resultfile("SGNS", "HT") %>% get_stats_and_summarise(., t_poly=4, t_freq=100000, add_freqs=TRUE) #check






