
datafolder="/ufs/aggelen/SenseShiftEval/data/"


Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW+" --inputfile="$datafolder/HW/gold_wordpair_after_iaa.csv"
#Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW" --inputfile="$datafolder/HW/gold_hamilton_28.csv"
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HT" --inputfile="$datafolder/HT/evaluationset_wordpair.csv"
Rscript scripts/experiment/get_correlations_simplified.R --dataset="WSCT" --inputfile="$datafolder/WSCT/gold_wordpair_after_iaa.csv"


