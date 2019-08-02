
require("reticulate") #for importing python objects

source_python("scripts/dataset_creation/wn_stats.py")


###############################################


threshold <- 3


###functions###

)



add_polysemy_cols <- function(df){ #this uses the function polysemy from the python file
	df$polysemy_target <- lapply(df$target, polysemy)
	df$polysemy_ref <- lapply(df$ref, polysemy)
	return(df)
}

summarize <- function(col){
	#return(table(col))
	return(100*sum(col)/(sum(col==0)+sum(col)))
}

high <- function(x) {x > threshold}
low <- function(x) {x <= threshold}
poly_poly <- function(df) {df[high(df$polysemy_target) & high(df$polysemy_ref),"correct"] %>%summarize()}
poly_nonpoly <- function(df) {df[high(df$polysemy_target) & low(df$polysemy_ref),"correct"] %>% summarize()}
nonpoly_nonpoly <- function(df) {df[low(df$polysemy_target) & low(df$polysemy_ref),"correct"] %>% summarize()}
nonpoly_poly <- function(df) {df[low(df$polysemy_target) & high(df$polysemy_ref),"correct"] %>% summarize()}


get_polysemy_and_summarise <- function(resultfile){
	df <- read.csv(resultfile, sep =",", header=T) %>% unique(.) %>% .[!is.na(.$corr),]
	N <- nrow(df)
	#df <- add_polysemy_cols(df)
	#print(df)
	print(nonpoly_nonpoly(df))
	print(nonpoly_poly(df))
	print(poly_nonpoly(df))
	print(poly_poly(df))
	#return(df)
}
################


#MAIN

resultfile <- function(corpus, dataset) {paste("/ufs/aggelen/repl_SenseShiftEval/results/", corpus, "wordshifteval_", dataset, ".csv",sep="")}

ht <- resultfile("SGNS", "HT") %>% get_polysemy_and_summarise(.)





#################################




hwwn <- get_polysemy_and_summarise("hw_wn")

ht <- get_polysemy_and_summarise("ht")

hw28 <- get_polysemy_and_summarise("hw28")

############################################################################################
#Now look at synset centrality ("quality") of the reference term: how strong a representation of the synset is it?

threshold <- 1 #max synset rank for the term to be a strong representation of the synset

resultfile <- function(dataset) {paste("/ufs/aggelen/SenseShiftEval/lsaresults_with_t_col_in_synsetresults/", dataset, "/eng-all/results_directional=FALSE_minfreqs_0-5.csv", sep="")}


check_and_fix_synset_col <- function(df){ #give col correct name (synset) and if Synset('vlabla.n.01') then collect the synset name
	if(any(names(df) == 'syn')){df$synset <- df$syn}
	if(grepl("Synset", df$synset[1])){df$synset <- lapply(df$synset, function(x) unlist(strsplit(as.character(x),split="'"))[2])}
	#df$synset <- lapply(df$synset, function(x) str_trim(x))
	#df$synset <- lapply(df$synset, function(x) str_sub(x, ))
	return(df)

}

strong <- function(x) {x <= threshold}
weak <- function(x) {x > threshold}

strong_strong_findings <- function(df) {df[strong(df$target_centrality) & strong(df$ref_centrality),"correct"] %>% table()}
strong_weak_findings <- function(df) {df[strong(df$target_centrality) & weak(df$ref_centrality),"correct"] %>% table()}
weak_strong_findings <- function(df) {df[weak(df$target_centrality) & strong(df$ref_centrality),"correct"] %>% table()}
weak_weak_findings <- function(df) {df[weak(df$target_centrality) & weak(df$ref_centrality),"correct"] %>% table()}



print_all_findings <- function(df){
	print(strong_strong_findings(df))
	print(strong_weak_findings(df))
	print(weak_strong_findings(df))
	print(weak_weak_findings(df))



}

add_centrality <- function(dataset){
	resultfile <- resultfile(dataset)
	df <- read.csv(resultfile, sep =",", header=T) %>% unique(.) %>% .[!is.na(.$correlation_factor),]
	df <- check_and_fix_synset_col(df)
	print(sum(!is.na(df$correlation_factor)))
	df$target_centrality <- mapply(function(term, synset) synset_rank(term, synset), df$target, df$synset)
	df$ref_centrality <- mapply(function(term, synset) synset_rank(term, synset), df$ref, df$synset)
	return(df)
}





}


hwwn <- add_centrality("hw_wn")
print_all_findings(hwwn)

ht <- add_centrality("ht")
print_all_findings(ht)

#example for in paper. Best example: gold ==1 and s

#ht[ht$synset=="astute.s.01",]





