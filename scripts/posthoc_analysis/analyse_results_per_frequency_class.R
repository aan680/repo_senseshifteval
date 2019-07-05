
require("reticulate") #for importing python objects

#pick up the vocabulary of the HistWord vector base. This is the top-100k words averaged over all decades. So this should be the same for all decades. We simply import the 1990s index
source_python("scripts/pickle_reader.py")
pickle_data <- read_pickle_file("embeddings/eng-all_sgns/1990-vocab.pkl")
index <- as.list(pickle_data)

#pick up the frequency file. This is a CSV file unpackaged from the HW pickle.
freqfile<-"data/freqs_engall_unpickled"
df_freq <- read.csv(freqfile, header=TRUE) #, row.names=NULL

#we take the freqs at 1990 as the proxy for overall frequency!!! 
freqs <- df_freq[df_freq$t==1990,]

#restrict freqs to the terms in the index
freqs_indexed <- freqs[freqs$word %in% index,] #N=99781. Why not 100k? I think this means that some words that were high-frequency when averaged over all decades were not in the 1990s index
#sort by decreasing freq
freqs <- freqs %>% .[order(-.$freq),]
freqs_indexed <- freqs_indexed[order(-freqs_indexed$freq),]


#N.B.: this includes stop words, articles and punctuation, e.g. most-frequent token is comma. This is unwanted as it means only about 30 words will be high-frequent. So we decide to discard the top-100 terms
freqs <- tail(freqs, -100)
freqs_indexed <- tail(freqs_indexed, -100)

#continue with this DF

#half of the corpus
half <- sum(freqs$freq)/2
half_indexed <- sum(freqs_indexed$freq)/2

#We decide to define high-frequency terms such that together they make up half of the corpus. So sort all terms by their freq (descending) and take the frequency that corresponds to the cut-off point (where the cumulative frequency == total freqs/2) as the cut-off point, i.e. the minimal frequency for the high-frequent group.

#add cumsum column
freqs$cum <- cumsum(freqs$freq)
freqs_indexed$cum <- cumsum(freqs_indexed$freq)
#
freqterms_overall <- freqs[freqs$cum<=half,]
freqterms_indexed <- freqs_indexed[freqs_indexed$cum<=half_indexed,]

nrow(freqterms)

#####################################################

#OK cut-off point is problematic due to skewedness.

#summary(freqs_indexed$freq). N=99681
 # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
 #      0      382     1753    60590     8420 13915312 

#summary(freqs$freq). N=905316
 #   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  #     0        3       16     7115       79 15313522

###############################################
#Let's just define it ourselves.

threshold <- 100000 #5000 #10000 #20000 #100000


#This section contains code already run but I add it to make this part self-contained.
df_freq <- read.csv(freqfile, header=TRUE) #, row.names=NULL

#we take the freqs at 1990 as the proxy for overall frequency!!! 
freqs <- df_freq[df_freq$t==1990,]



###functions###

resultfile <- function(dataset) {paste("/ufs/aggelen/SenseShiftEval/lsaresults_with_t_col_in_synsetresults/", dataset, "/eng-all/results_directional=FALSE_minfreqs_0-5.csv", sep="")}


add_freq_cols <- function(df, df_freq){
	df1 <- merge(df,df_freq, by.x="target", by.y="word", all.x=TRUE, all.y=FALSE)
	df <- merge(df1,df_freq, by.x="ref", by.y="word", all.x=TRUE, all.y=FALSE)
	return(df)
}

frequent <- function(x) {x > threshold}
infrequent <- function(x) {x <= threshold}
freq_freq <- function(df) {df[frequent(df$freq.x) & frequent(df$freq.y),"correct"] %>% table()}
freq_infreq <- function(df) {df[frequent(df$freq.x) & infrequent(df$freq.y),"correct"] %>% table()}
infreq_infreq <- function(df) {df[infrequent(df$freq.x) & infrequent(df$freq.y),"correct"] %>% table()}
infreq_freq <- function(df) {df[infrequent(df$freq.x) & frequent(df$freq.y),"correct"] %>% table()}

get_freqs_and_summarise <- function(dataset){
	resultfile <- resultfile(dataset)
	df <- read.csv(resultfile, sep =",", header=T) %>% unique(.) %>% .[!is.na(.$correlation_factor),]
	print(sum(!is.na(df$correlation_factor)))
	#nr_nonna <- nrow(df[!is.na(df$correlation_factor),])
	#print(nr_nonna)
	df <- add_freq_cols(df, freqs)
	print(freq_freq(df))
	print(freq_infreq(df))
	print(infreq_infreq(df))
	print(infreq_freq(df))
	#return(df)
}
################


#MAIN


get_freqs_and_summarise("hw_wn")

get_freqs_and_summarise("ht")

get_freqs_and_summarise("hw28")









