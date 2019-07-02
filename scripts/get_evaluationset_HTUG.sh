
dir_newsenses="data/HT/newsenses"
dir_oldsenses="data/HT/oldsenses"

scrapedir_oldsenses="$dir_oldsenses/scrape"
scrapedir_newsenses="$dir_newsenses/scrape"

filename_evalset="evaluationfile_repl.csv"
eval_oldsenses="$dir_oldsenses/$filename_evalset"
eval_newsenses="$dir_newsenses/$filename_evalset"

echo "the scrape was done by script scrape_and_save.sh. Uncomment to run."

#./scrape_and_save.sh

echo "now extract the fields that we need from the scrape"

Rscript scripts/parse_thesaurus_scrape.R --scrapedir="$scrapedir_oldsenses/ht.ac.uk/category-selection" --outputfile="$dir_oldsenses/$filename_evalset"

Rscript scripts/parse_thesaurus_scrape.R --scrapedir="$scrapedir_newsenses/ht.ac.uk/category-selection" --outputfile="$dir_newsenses/$filename_evalset"

echo "Now create (automatically) the gold standard for the thesaurus set. This script outputs 3 files: one with all synset pairings, one with just matching synsets (called OUTPUTFILE_SYNSET followed by _filtered), and one with the wordpairs for just the matching synsets."

echo "Script aguments:[gold (-1 or 1)] [INPUTFILE_THESAURUS_SCRAPE] [outputfile_synset_all] [outputfile_wordpair_match] [outputfile_synset_match] [outputfile_wordpair_nonmatch]"


python scripts/get_evaluationset_thesaurus.py -1 $dir_oldsenses/$filename_evalset $dir_oldsenses/gold_synset_all.csv $dir_oldsenses/gold_wordpair.csv $dir_oldsenses/gold_synset.csv $dir_oldsenses/wordpairs_nomatch.csv 


python scripts/get_evaluationset_thesaurus.py 1 $dir_newsenses/$filename_evalset $dir_newsenses/gold_synset_all.csv $dir_newsenses/gold_wordpair.csv $dir_newsenses/gold_synset.csv $dir_newsenses/wordpairs_nomatch.csv 


echo "Now we merge the gold standard files for the word level version."

file1="$dir_oldsenses/gold_wordpair.csv"
file2="$dir_newsenses/gold_wordpair.csv"
file2_noheader="$dir_newsenses/gold_wordpair_noheader.csv"
res="data/HT/gold_old_and_new.csv"

awk 'FNR > 1' $file2 > $file2_noheader

cat $file1 $file2_noheader > $res

















