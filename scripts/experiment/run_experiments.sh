
datafolder="/ufs/aggelen/SenseShiftEval/data/"
#sgns-settings are the default settings in the R script!
cohasettings="--corpus="COHA" --dir_downloaded_vectors="/ufs/aggelen/SenseShiftEval/data/vectors/SGNS-coha" --dir_hw_vectors="embeddings/sgns-coha""
engficsettings="--corpus="engfic" --dir_downloaded_vectors="/ufs/aggelen/SenseShiftEval/data/vectors/SGNS_eng-fiction-all" --dir_hw_vectors="embeddings/eng-fiction-all""


mkdir ./results


#sgns
Rscript scripts/experiment/get_correlations_simplified.R --dataset="WSCT" --inputfile="$datafolder/WSCT/gold_wordpair_after_iaa.csv"
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HT" --inputfile="$datafolder/HT/gold_old_and_new.csv" 
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW+" --inputfile="$datafolder/HW/gold_wordpair_after_iaa.csv"
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW" --inputfile="$datafolder/HW/gold_hamilton_28.csv" --add_stats=FALSE #do not add_stats because no WN annotations

#coha
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW" --inputfile="$datafolder/HW/gold_hamilton_28.csv" --add_stats=FALSE $cohasettings
Rscript scripts/experiment/get_correlations_simplified.R --dataset="WSCT" --inputfile="$datafolder/WSCT/gold_wordpair_after_iaa.csv" $cohasettings
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW+" --inputfile="$datafolder/HW/gold_wordpair_after_iaa.csv" $cohasettings
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HT" --inputfile="$datafolder/HT/gold_old_and_new.csv" $cohasettings

#engfic
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW" --inputfile="$datafolder/HW/gold_hamilton_28.csv" --add_stats=FALSE $engficsettings 
Rscript scripts/experiment/get_correlations_simplified.R --dataset="WSCT" --inputfile="$datafolder/WSCT/gold_wordpair_after_iaa.csv" $engficsettings
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HW+" --inputfile="$datafolder/HW/gold_wordpair_after_iaa.csv" $engficsettings
Rscript scripts/experiment/get_correlations_simplified.R --dataset="HT" --inputfile="$datafolder/HT/gold_old_and_new.csv" $engficsettings
