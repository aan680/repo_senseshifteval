library(rlist)
library(optparse)
library(magrittr)
#library(lsa) #cosine
library(rlist)
#library(Hmisc)
library(xtable)
library(lsa)
library(dplyr)
library(raster) #implements modal, most frequent value, used for voting function
options(stringsAsFactors = FALSE)

#dir_vectors <- "/ufs/aggelen/SenseShiftEval/data/vectors/SGNS"

#pick up the frequency file. This is a CSV file unpackaged from the HW pickle.
#freqfile<-"data/freqs_engall_unpickled"
#freqs <- read.csv(freqfile, header=TRUE) #, row.names=NULL
min <- 500

#precondition for continuing
freq_above_min <- function(word, time, df_freq = freqs){ #is the freq of the word at least what is given as a parameter?
	if(time==1800 & nrow(df_freq[df_freq$t==time,]) == 0){#this must be coha; freqs/vecs only start from 1810
    	freq <- 0
    	}else{
    	freq <- df_freq[df_freq$t==time & df_freq$word==word, "freq"]
	freq %>% as.numeric(.)
    	}
    	return(freq >= min)
}



allvectors <- function(word, dir_vectors){
	word <- word %>% tolower(.) %>% trimws(.) ##!crucial! added this just now
	filepath <- word %>% paste(., "_dia.csv", sep ="") %>% file.path(dir_vectors, .) 
	if(!file.exists(filepath)){stop(filepath, " does not exist")}
	allvectors <- read.csv(filepath, header=TRUE)
	if(nrow(allvectors)<19 |ncol(allvectors)<302){
		print(word)
		#print(word, " has a vector of size ", nrow(allvectors), " and ", ncol(allvectors))
	}
	return(allvectors) #is a DF

}

select_vector<- function(df){  #from row
  v <- subset(df, select = -c(w, t))
  # v <- df[,grep('X0',colnames(df)):ncol(df), drop=FALSE]
  apply(v, 1, as.numeric)#as.numeric(v)
  return(as.vector(v))
}

vector_t <- function(allvectors, time){ 
	vector_at_t <- allvectors[which(allvectors$t == time),] %>% select_vector(.) %>% as.numeric %>% as.vector(.)
	return(vector_at_t)

}


cosines <- function(target, ref, vecs_target, vecs_ref, timespan){
  result <- data.frame(matrix(ncol = length(timespan))) %>%  setNames(., timespan)
  for(time in timespan){
    #if((freq_above_min(target, time) && freq_above_min(ref, time)) %in% TRUE){
	v_t <- vector_t(vecs_target, time)
    	v_r <- vector_t(vecs_ref, time)
    	vector_length <- length(v_t)
	
    	cos<- cosine(v_t, v_r)[[1]] #cosine similarity method from lsa, returns NA if one or both vectors are all zero
        #if(cos==0){cos <- NA}
    #else{
    	result[1,  as.character(time)] <- cos
  } #end for
 return(result)
} 

timespan_from_t <- function(t) {
  nearest_smaller_decade <- floor(t/10)*10 #round down to decade
  onset <- nearest_smaller_decade
  timespan <- seq(as.numeric(nearest_smaller_decade),1990,by=10)
  return(timespan)
}

correlation <- function(arg1, arg2, gold){ #correlate arg1 and arg2 and compare sign of correlation with gold. 
  corr <- cor.test(arg1, arg2, alternative = "two.sided", method = "spearman", exact=FALSE) #exact=TRUE gives error beccause of ties
  correlation_factor <- corr$estimate #(1,2) could also be (2,1). same thing.
  correlation_size <- abs(correlation_factor)
  p_value <- corr$p.value
  sig <- ifelse(p_value <= 0.05, 1, 0)
  direction_of_corr <-  ifelse(correlation_factor > 0, 1, -1)
  correct <- ifelse(direction_of_corr == gold, 1, 0)
  result <- list(correct, sig, p_value, correlation_size)
  names(result) <- c("correct", "sig", "p", "corr")
  return(result)
}


evaluate_my_way <- function(target, ref, t, gold, vectordir){
  target <- target %>% tolower(.) %>% trimws(.)
  ref <- ref %>% tolower(.) %>% trimws(.)
  vecs_target <- allvectors(target, vectordir)
  vecs_ref <- allvectors(ref, vectordir)
  timespan<- timespan_from_t(t)
  cos <- cosines(target, ref, vecs_target, vecs_ref, timespan)
  cosines_as_vector <- t(cos)%>% as.double %>% as.vector(.)
  #print(cosines_as_vector)
  if( sum(!is.na(cosines_as_vector))<5){
  result <- list(NA,NA,NA,NA)
  }
  else{
  result <- correlation(cosines_as_vector, timespan, gold) #exact=TRUE gives error beccause of ties
  }#end else
  names(result) <- c("correct", "sig", "p", "corr")
  return(result)
  #return(cbind(correct = correct, sig = sig, p = p_value, corr = correlation_size))
  #return(correct)
}




