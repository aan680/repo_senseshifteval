
#set working dir to source file location
library(optparse)
library(magrittr)

option_list = list(
  make_option(c("--HTUG"),    action="store", default= FALSE),
  make_option(c("--output"),    action="store", default= ""),
  make_option(c("--input_wordpair"), action="store", default=""), #the empty word-level evaluation file that needs to be annotated with gold standard labels
  make_option(c("--input_gold_synset"),    action="store", default= "") #the synset-level evaluation file that contains the gold standard labels

)

opt = parse_args(OptionParser(option_list=option_list))


make_wordlevel_gold <- function(outputfile = opt$output, input_gold_synset = opt$input_gold_synset, input_wordpair =  opt$input_wordpair){
  to_annotate <- read.csv(input_wordpair)
  if("gold" %in% colnames(to_annotate)){to_annotate <- subset(to_annotate, select=-c(gold))} #HW input file contains gold standard
  gold <- read.csv(input_gold_synset) %>% subset(., select=c("target", "synset", "pos", "t", "gold")) #no clue why it does not work without this selection
  df <- merge(gold, to_annotate)
  write.csv(df, outputfile)
  message("output written to ", outputfile)
}

 make_wordlevel_gold_ht <- function(folder, synset_gold = "gold_synset.csv", wordpair_evaluationfile =  "evaluationset_wordpair.csv", outputfile = "gold_wordpair.csv"){
  eval <- read.csv(file.path(folder, wordpair_evaluationfile))
  gold <- read.csv(file.path(folder, synset_gold))
  gold_relevant <-gold[,c("term", "syn", "pos", "t", "gold")] #added t
  df <- merge(eval, gold_relevant, by=c("target", "synset", "t"), all.x=TRUE, all.y=FALSE)
  write.csv(df, file.path(folder, outputfile))
}

make_wordlevel_gold()
if(opt$HTUG){make_wordlevel_gold_ht()}


