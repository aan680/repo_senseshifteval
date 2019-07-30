
datafolder="/ufs/aggelen/SenseShiftEval/data/"

mkdir ./results

Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW+" --inputfile="$datafolder/HW/gold_wordpair_after_iaa.csv"
#Rscript scripts/experiment/get_correlations_simplified.R --dataset="HT" --inputfile="$datafolder/HT/gold_old_and_new.csv" 
#Rscript scripts/experiment/get_correlations_simplified.R --dataset="WSCT" --inputfile="$datafolder/WSCT/gold_wordpair_after_iaa.csv"
#Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW" --inputfile="$datafolder/HW/gold_hamilton_28.csv" --add_stats=FALSE #do not add_stats because no WN annotations

