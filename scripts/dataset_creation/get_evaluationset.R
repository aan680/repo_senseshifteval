#this is an experiment , to see if I can simplify my code by using dataframe merges whenever possible and embedding python code


require("reticulate") #for importing python objects
library(magrittr) #for piping
require(utils)
library(optparse)

source_python("scripts/wn_stats.py") #!!!this script depends on a python script that retrieves WN data from NLTK

option_list = list(
  make_option(c("--inputfile"), action="store", default="", help=""),#columns target,pos,(t)
  make_option(c("--outputfile_synset"),    action="store", default= "", help=""),
  make_option(c("--outputfile_wordpair"),    action="store", default= "", help=""))

opt = parse_args(OptionParser(option_list=option_list))
print(opt)

match_and_annotate_wn <- function(inputfile, outputfile_synset, outputfile_wordpair){
	df <- read.csv(inputfile,  row.names=NULL, header=TRUE) %>% unique(.)
	allentries <- apply(df, 1, function(x) allinfo_word(x['target'], x['pos']) %>% do.call(rbind, .))
	result_wordpair <- do.call(rbind, allentries) %>% unique(.) %>% subset(., .$target != .$ref) %>% merge(.,df) 
	rownames(result_wordpair) <- c()
	write.csv(result_wordpair, outputfile_wordpair)
	result_synset <- subset(result_wordpair, select = -c(ref)) %>% unique(.)
	rownames(result_synset) <- c()
	write.csv(result_synset, outputfile_synset)
	print(nrow(result_synset))
	print(nrow(result_wordpair))
}
	
match_and_annotate_wn(opt$inputfile, opt$outputfile_synset, opt$outputfile_wordpair)



#so far this is just a DF of all synonyms of the target terms in a POS 
#filter out word pairs with target == ref
#now add the metadata to the original DF. This way identical word pairs with a different t (i.e. two shifts) occur in the data twice
