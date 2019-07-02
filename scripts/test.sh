#input files with columns [target, POS, t]. Save as targets.csv in the corresponding dataset folder (data/WSE or data/HW). t expresses time of change.

folder_hw="data/HW+"
folder_hw28="data/HW"
folder_wsct="data/WSCT"
folder_logs="data" #for extra output, for reference
hw_input="$folder_hw/sourcedata.csv"
wsct_input="$folder_wsct/sourcedata.csv"
gold_hwplus_synset="$folder_hw/gold_synset_after_iaa.csv"
gold_wsctplus_synset="$folder_wsct/gold_synset_after_iaa.csv"




###################################################################################

###################################################################################
echo "Now running:"
echo "python scripts/get_evaluationset_wsct+_hw+.py $hw_input $folder_hw/evaluationset_synset.csv $folder_hw/evaluationset_wordpair.csv"
echo "python scripts/get_evaluationset_wsct+_hw+.py $wsct_input $folder_hw/evaluationset_synset $folder_wsct/evaluationset_wordpair"

python scripts/get_evaluationset.py $hw_input $folder_hw/evaluationset_synset.csv $folder_hw/evaluationset_wordpair.csv

#python scripts/get_evaluationset_wsct+_hw+.py $hw_input $folder_hw/evaluationset_synset.csv $folder_hw/evaluationset_wordpair.csv
#python scripts/get_evaluationset_wsct+_hw+.py $wsct_input $folder_wsct/evaluationset_synset.csv $folder_wsct/evaluationset_wordpair.csv

###################################################################################


read -p "The following script transfers the synset-level gold standard onto the word-level file. The name of the output file is based on the input file with sed("synset", "wordpair"), e.g. gold_wordpair_after_iaa.csv and is saved to the same folder as the synset-level gold standard. Press enter to run."

echo "Now running: "
echo Rscript scripts/make_wordlevel_gold_from_synsetlevel_gold.R --folder=$folder_hw --file_input_gold_synset=$gold_hwplus_synset --file_input_wordpair="evaluationset_wordpair.csv"
echo Rscript scripts/make_wordlevel_gold_from_synsetlevel_gold.R --folder=$folder_wsct --file_input_gold_synset=$gold_wsctplus_synset --file_input_wordpair="evaluationset_wordpair.csv"

#Rscript scripts/make_wordlevel_gold_from_synsetlevel_gold.R --folder=$folder_hw --file_input_gold_synset=$gold_hwplus_synset --file_input_wordpair=$folder_hw/"evaluationset_wordpair.csv"
#Rscript scripts/make_wordlevel_gold_from_synsetlevel_gold.R --folder=$folder_wsct --file_input_gold_synset=$gold_wsctplus_synset --file_input_wordpair=$folder_wsct/"evaluationset_wordpair.csv"

###################################################################################
