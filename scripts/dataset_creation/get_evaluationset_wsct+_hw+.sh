#input files with columns [target, POS, t]. Save as targets.csv in the corresponding dataset folder (data/WSE or data/HW). t expresses time of change.

folder_hw="data/HW+"
folder_hw28="data/HW28"
folder_wsct="data/WSCT"
folder_logs="data" #for extra output, for reference
hw_input="$folder_hw/sourcedata.csv"
wsct_input="$folder_wsct/sourcedata.csv"

###################################################################################

echo "This script does the automated part of the generation process of WSCT+ and HW+, i.e. before the manual gold standard annotations. It simply permutates the target terms from from WSCT and HistWords with all their WordNet synonyms. We generate a wordpair level file (word shift eval) and a synset level file (sense shift eval). The latter will be used to add the gold standard. Just to be clear: not all WordNet synsets are a valid match, hence the resulting synsets/word pairs do not all stand for a semantic shift. This is what the gold standard annotation (and inter-annotator evaluation) is for." 

read -p "Make sure that files $hw_input and $wsct_input reflect the source data (HistWords and WSE) in columns [target, POS, t]: the target term part of speech (given or inferred otherwise) and the start time of the change. Press enter to continue."


###################################################################################
echo "Now running:"
echo "python scripts/get_evaluationset_wsct+_hw+.py $hw_input $folder_hw/evaluationset_synset.csv $folder_hw/evaluationset_wordpair.csv"
echo "python scripts/get_evaluationset_wsct+_hw+.py $wsct_input $folder_hw/evaluationset_synset $folder_wsct/evaluationset_wordpair"

Rscript scripts/get_evaluationset.R --inputfile=$hw_input --outputfile_synset=$folder_hw/evaluationset_synset.csv --outputfile_wordpair=$folder_hw/evaluationset_wordpair.csv

Rscript scripts/get_evaluationset.R --inputfile=$wsct_input --outputfile_synset=$folder_wsct/evaluationset_synset.csv --outputfile_wordpair=$folder_wsct/evaluationset_wordpair.csv


###################################################################################

echo "Now Upload all output files evaluationset_synset.csv to Google Docs and annotate them with the gold standard under a column with the annotator name in the column name. Finally, after IAA, add a column “gold” with the agreed-upon gold standard: +1. 0, -1."
read -p "Press enter to confirm this is done."

###################################################################################

googlelink_gold_hw="https://docs.google.com/spreadsheets/d/1f7d2sbBIvVuuOVK0fBmLOzU16X2w20eglrwS5-66W6Y/edit?usp=sharing"
googlelink_gold_wsct="https://docs.google.com/spreadsheets/d/1f7d2sbBIvVuuOVK0fBmLOzU16X2w20eglrwS5-66W6Y/edit?usp=sharing"

gold_hwplus_synset="$folder_hw/gold_synset_after_iaa.csv"
gold_wsctplus_synset="$folder_wsct/gold_synset_after_iaa.csv"

read -p "We are now going to assume the gold standard files have been filled out; they can be found at $googlelink_gold_hw for HW and $googlelink_gold_wsct for WSCT.  Make sure these files have been saved to, respectively, the following files: $gold_hwplus_synset and $gold_wsctplus_synset. Press enter to confirm and to continue."


###################################################################################


read -p "The following script transfers the synset-level gold standard onto the word-level file. The name of the output file is based on the input file with sed("synset", "wordpair"), e.g. gold_wordpair_after_iaa.csv and is saved to the same folder as the synset-level gold standard. Press enter to run."

echo "Now running: "
echo Rscript scripts/make_wordlevel_gold_from_synsetlevel_gold.R --input_gold_synset=$gold_hwplus_synset --input_wordpair="evaluationset_wordpair.csv" --output=$folder_hw/gold_wordpair_after_iaa.csv

echo Rscript scripts/make_wordlevel_gold_from_synsetlevel_gold.R --input_gold_synset=$gold_wsctplus_synset --input_wordpair="evaluationset_wordpair.csv" --output=$folder_wsct/gold_wordpair_after_iaa.csv






