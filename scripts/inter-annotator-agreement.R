install.packages("irr")
library(irr)


#HT
iaa <- read.csv("./data/IAA/HT_IAA_Astrid_Jacco.csv", header=TRUE) #this is a download of sheet "IAA_Astrid_Jacco" in https://docs.google.com/spreadsheets/d/15YoemXPouNFf6Dp5sMm97VKGE6pNIb6DdE0tyKviyRo/edit#gid=82129764 
iaa_aj <- iaa[,c("recoded_Astrid","recoded_jacco")]
agreement <- agree(iaa_aj, tolerance=0)
kappa2(iaa_aj, "unweighted", sort.levels = FALSE)

#WSE
iaa <- read.csv("./data/IAA/WSE_IAA_Astrid_Jacco.csv", header=TRUE) #this is a download of sheet "IAA_Astrid_Jacco" in https://docs.google.com/spreadsheets/d/15YoemXPouNFf6Dp5sMm97VKGE6pNIb6DdE0tyKviyRo/edit#gid=82129764 
iaa_aj <- iaa[,c("gold_Astrid","gold_Jacco")]
agreement <- agree(iaa_aj, tolerance=0)
kappa2(iaa_aj, "unweighted", sort.levels = FALSE)

#HW. BEware: 3 raters.
iaa <- read.csv("./data/IAA/HW_IAA_Astrid_Jacco_Laura.csv", header=TRUE) #this is a download of sheet "IAA_Astrid_Jacco" in https://docs.google.com/spreadsheets/d/15YoemXPouNFf6Dp5sMm97VKGE6pNIb6DdE0tyKviyRo/edit#gid=82129764 
iaa_ajl <- iaa[,c("gold_Astrid","gold_Jacco", "gold_Laura")]
agreement <- agree(iaa_ajl, tolerance=0)
kappam.fleiss(iaa_ajl)
