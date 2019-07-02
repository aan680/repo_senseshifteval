
#set working dir to source file location
library(optparse)
library(magrittr)

option_list = list(
  make_option(c("--HTUG"),    action="store", default= FALSE),
  make_option(c("--folder"),    action="store", default= ""),
  make_option(c("--file_input_wordpair"), action="store", default=""), #the empty word-level evaluation file that needs to be annotated with gold standard labels
  make_option(c("--file_input_gold_synset"),    action="store", default= "") #the synset-level evaluation file that contains the gold standard labels

)

opt = parse_args(OptionParser(option_list=option_list))


make_wordlevel_gold <- function(folder = opt$folder, synset_gold = opt$file_input_gold_synset, wordpair_evaluationfile =  opt$file_input_wordpair){
  print(wordpair_evaluationfile)
  eval <- read.csv(wordpair_evaluationfile)
  if("gold" %in% colnames(eval)){eval <- subset(eval, select=-c(gold))} #HW input file contains gold standard
  gold <- read.csv(synset_gold)
  gold_relevant <-gold[,c("target", "synset", "t", "gold")]
  df <- merge(eval, gold_relevant, by=c("target", "synset", "t"), all.x=TRUE, all.y=FALSE)
  df <- subset(df, select=-c(syn_def)) # subset(df, select = -c(a, c))
  outputfile <- gsub("synset", "wordpair", synset_gold) #%>% file.path(folder, .)
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

