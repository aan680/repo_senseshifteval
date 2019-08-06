#library(reshape)
#library(ggplot2)
library(dplyr)
library(Hmisc)
library(magrittr)
library(plotly)

resultfile <- "/ufs/aggelen/repl_SenseShiftEval/results/SGNSwordshifteval_HT.csv"

output <- read.csv(resultfile, header=T) %>% unique(.) #, stringsAsFactors=FALSE) # load file

synset_dfs<- split(output, list(output$syn, output$target, output$t, output$gold), drop=TRUE) #or should I do group_by ?

all.na <- function(vector){
  length(vector) == sum(is.na(vector))
}


##############################################################################
#collect data meeded, simpler way

vector_correct <- c()
vector_total <- c()


for (df in synset_dfs){	
	#print(df)
	if(nrow(df)>1 & !all.na(df$p)){ #only if target term has more than 1 reference term
		nr_correct <- nrow(subset(df, df$correct==1))
		nr_false <- nrow(subset(df, df$correct==0))
		total <- nr_correct + nr_false
		vector_correct <- append(vector_correct, nr_correct)
		vector_total <- append(vector_total, total)
}}




#figure 1 (pie)


proportion_correct<-vector_correct/vector_total
z <- cut(proportion_correct, c(-0.1,0.1,0.49,0.51,0.9,1.1))

png(file = "/ufs/aggelen/repl_SenseShiftEval/figures/pie.png")

labels <- c("none correct","mostly false", "50% correct", "mostly correct", "all correct")
pcts <- round(table(z)*100/sum(table(z)),0)
label_and_pct <-  paste(labels, ": ", pcts, "%",sep="") # ad % to labels


pie(table(z), labels = label_and_pct)



dev.off()



























##############################################################################
#outcome combinations for different ways of aggregating

freqs <- count(t2, c('correct_by_p', 'correct_by_max', 'correct_by_mostfrequent', 'correct_by_voting', 'correct_by_avg_vector'))
aliases<-c('min.p', 'max.abs.corr.', 'mostfreq.refterm', 'mostfreq.corr.sign', 'avg.vector')
df <- freqs[order(-freqs$freq),]

df_values <- df[,c(1:5)] %>% round(.,0)
labs <- df[,"freq"]

png(file = paste(dir, "table_aggregations.png", sep=""))


library(plotrix)
#par(mar = c(0.5, 8, 3.5, 0.5))
par(mar=c(3,3,12,6)+.1)
color2D.matplot(df_values, 
                show.values = FALSE,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 2,
                vcol = "black",
                extremes = c("red", "green"))
#axis(3, at = seq_len(ncol(df_values)) - 0.5,
 #    tick = FALSE, las=1, cex.axis = 1)

mtext(aliases, at = seq_len(ncol(df_values)) - 0.5, side=3, las=2)

axis(4, at = seq_len(nrow(df_values)) -0.5,
     labels = rev(labs), tick = FALSE, las = 1, cex.axis = 1) #freqs

mtext("Agreement between different", side=3, line=9, cex=1.5)
mtext("synset-level aggregations", side=3, line=7, cex=1.5)
mtext("Frequencies", side=4, line=4, cex=2)
#mtext("Correct by", side=3, line=2)

dev.off()


##############################################################################

#collect data needed 

###TODO: better just collect [target, synset, t, p(success)], then merge with synset-level outcome df
vector_correct <- c()
vector_total <- c()
vector_syns <- c()


df_p<-c(NULL, NULL, NULL, NULL, NULL)
df_avg<-c(NULL, NULL, NULL, NULL, NULL)
df_vote<-c(NULL, NULL, NULL, NULL, NULL)
df_max<-c(NULL, NULL, NULL, NULL, NULL)
df_mostfrequent<-c(NULL, NULL, NULL, NULL, NULL)

for (df in synset_dfs){	
	if(nrow(df)>1 & !all.na(df$p_value)){ #only if target term has more than 1 reference term
		nr_correct <- nrow(subset(df, df$correct==1))
		nr_false <- nrow(subset(df, df$correct==0))
		total <- nr_correct + nr_false
		vector_correct <- append(vector_correct, nr_correct)
		vector_total <- append(vector_total, total)
		vector_syns <- append(vector_syns, unique(as.character(df$syn)))

		#now for plot number 2
		target <- unique(as.character(df$target))
		syn <- unique(as.character(df$syn))
		t <- unique(as.character(df$t))

		df2<- t2[which(t2$target==target&t2$synset==syn&t2$t==t),] #has the synset assessments


		if(nrow(df2)>1){stop("not a unique synset result")}

		if(!is.na(df2$correct_by_p)){

			p_correct <- nr_correct/(nr_correct + nr_false) #ifelse(nr_false==0, 1, 
			
			
			df_p <- rbind(df_p, c(term = target, synset = syn, time = t, p_success= p_correct, success = df2$correct_by_p))#stringsAsFactors = FALSE)
			df_avg <- rbind(df_avg,  c(term = target, synset = syn, time = t, p_success= p_correct, success = df2$correct_by_avg_vector))
			df_vote <- rbind(df_vote,  c(term = target, synset = syn, time = t, p_success= p_correct, success = df2$correct_by_voting))
			df_max <- rbind(df_vote,  c(term = target, synset = syn, time = t, p_success= p_correct, success = df2$correct_by_max))		
			df_mostfrequent <- rbind(df_vote,  c(term = target, synset = syn, time = t, p_success= p_correct, success = df2$correct_by_mostfrequent))		
		}

		#}#end if look at aggregation


		}#end if
	}#end for

###############################################################################

correct_by_any <- t2$correct_by_voting==1|t2$correct_by_p==1|t2$correct_by_max==1|t2$correct_by_avg_vector==1)

#figure 0 (freq and p vs correctness)

colnames(output) #word level
colnames(t2) #syn level

names(t2)[names(t2) == "synset"] <- "syn"

all <- merge(t2,output, all.x=FALSE,all.y=FALSE)



#add freq at 1990 to all
f <- read.csv("/ufs/aggelen/SenseShiftEval/data/freqs_engall_unpickled")

#freq_1990 <- lapply(as.character(all$ref), function(x) f[f$word==x & f$t==1990, "freq"]) #uncomment when running from terminal; takes time

all$freq_1990 <- freq_1990

head(all)

plot(x=all$p_value,y=all$freq_1990, xlim=c(0,1), ylim=c(0,100000),  col = "red", pch=16)

abline(lm(all$freq_1990 ~ all$p_value), asp=1,  col = "blue", lwd=3)

###############################################################################






#figure 1 (pie)


proportion_correct<-vector_correct/vector_total
z <- cut(proportion_correct, c(-0.1,0.1,0.49,0.51,0.9,1.1))

png(file = paste(dir, "pie.png", sep=""))

labels <- c("none correct","under 50% correct", "50% correct", "over 50% correct", "all correct")
pcts <- round(table(z)*100/sum(table(z)),0)
label_and_pct <-  paste(labels, ": ", pcts, "%",sep="") # ad % to labels


pie(table(z), labels = label_and_pct)



dev.off()

###############################################################################

#figure 2

p_success=as.numeric(df_p[,c("p_success")])
success=as.numeric(df_p[,c("success")])


df1 <- aggregate(cbind(p_success = p_success, success = success), list(df_p[,"p_success"]), mean)

#as.data.frame(tups_p[,c("p_success", "success")]), stringsAsFactors = FALSE)
#tups_avg_df <-as.data.frame(tups_avg[,c("p_success", "success")], stringsAsFactors = FALSE)

plot(x=jitter(p_success),y=jitter(success), asp=1, cex = 1.3, xlim=c(0,1), ylim=c(0,1),  col = "blue", pch=16,xlab="probability of success (correct vs. total observations per synset)", ylab="observed success rate (by aggregation)")
abline(lm(df1$success ~ df1$p_success), asp=1,  col = "blue", lwd=3)





df2 <- aggregate(tups_avg, list(tups_avg[,"p_success"]), mean)



png(file = paste(dir, "aggregation.png", sep=""))

plot(x=jitter(df1$p_success),y=jitter(df1$success), asp=1, cex = 1.3, xlim=c(0,1), ylim=c(0,1),  col = "blue", pch=16,
	xlab="probability of success (correct vs. total observations per synset)", ylab="observed success rate (by aggregation)")
abline(lm(df1$success ~ df1$p_success), asp=1,  col = "blue", lwd=3)

abline(0,1,col="green", lwd=2) #baseline
#par(new=TRUE)
points(x=jitter(df2$p_success),y=jitter(df2$success), asp=1,  col = "red", pch=16)
abline(lm(df2$success ~ df2$p_success), asp=1,  col = "red", lwd=3)

legend(0.4,0.3, 
       c("vote by most smallest p","by average vector","baseline (chance-level)"), 
       lty=c(1,1,1), 
       lwd=c(1,1,1),col=c("blue","red","green"))

title("Added value of synset-level aggregation \n(HT+, eng-all)")


dev.off()
################################################

##figure 3: not averaged; bar chart

plot.new()




transform <- function(df){ #change into df with unique p_success values and columns freq_0, freq_1
	df <- as.data.frame(df)
	#df <- subset(df, df$p_success<1) #!!! DROP P=1
	df_alt <- NULL
	p_success <- unique(df$p_success)
	freq_0 <- NULL
	freq_1 <- NULL
	#df_alt$freq_0 <- lapply(df_alt$p_success, nrow(subset(df, df$p_success==. & df$success==0)))

	for (i in p_success){
		freq_0 <- append(freq_0,nrow(subset(df, df$p_success==i & df$success==0)))
		freq_1 <- append(freq_1,nrow(subset(df, df$p_success==i & df$success==1)))
	}

	return(cbind(p_success, freq_0, freq_1) %>% as.data.frame(., stringsAsFactors=FALSE))

}

df_p <- transform(tups_p)
df_avg <- transform(tups_avg)

#make numeric
nrow <- nrow(df_p)
df_p.n <- data.frame(p_success=double(nrow), freq_0=integer(nrow), freq_1=integer(nrow))
df_p.n$p_success <- as.numeric(df_p$p_success)


ax <- list(
  range=c(0,1),
  dtick = 0.1,
  title = "chance of correct assessment (number correct on total word pairs in synset)",
  zeroline = TRUE,
  showline = TRUE,
  showticklabels = TRUE,
  showgrid = TRUE
)

ay <- list(
  range=c(0,50),
  dtick = 10,
  title = "observed frequency of correct (green) and incorrect (yellow) assessment",
  zeroline = TRUE,
  showline = TRUE,
  showticklabels = TRUE,
  showgrid = TRUE
)



p <- plot_ly() %>%
  add_bars(
    x = p_success,
    y = freq_0,
    base = 0, #-freq_0,
    text=freq_0,
    textposition = 'auto',
    xaxis = ax,
    yaxis=ay,
    textfont = list(color = '#000000', size = 30),
    width = 0.1, #0.015,
    marker = list(
      color = 'yellow'

    ),
    name = 'false shift assessments'
  ) %>%
  add_bars(
    x = p_success,
    y = freq_1,
    base = 0,
    width = 0.1, #0.015,
    text=freq_1,
    textposition = 'auto',
    xaxis = ax,
    yaxis=ay,
    textfont = list(color = '#000000', size = 16),
   # width = 0.010, #0.015,
    marker = list(
      color = 'green'
    ),
    name = 'correct shift assessments'
  ) #%>%   layout(title = 'Primates Brain and Body Weight', xaxis = ax, yaxis=ay)


#https://plot.ly/r/bar-charts/
p <- plot_ly(df_p, x = ~p_success, y = ~freq_1, type = 'bar', name = 'success',
	text=freq_1,
        xaxis = ax,
        yaxis=ay,
        marker = list(color = 'green')) %>%
  add_trace(y = ~freq_0, name = 'error', marker = list(color = 'false'),xaxis = ax, yaxis=ay,
) %>%layout(title = 'Added value of synset-level aggregation \n(HT+, eng-all, aggregated by argmin(p))',
         xaxis = list(
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'USD (millions)',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', bargap = 0.15, bargroupgap = 0.1)

##################################

#df <- as.data.frame(df_p, stringsAsFactors = FALSE)
#df <- df_p[order(p_success),]

df <- mutate_all(df_p, function(x) as.numeric(as.character(x))) %>% as.matrix(.)

barplot(
    #as.matrix(t(df[c('freq_0','freq_1')])),
    df[,2:3],
    beside=T,
    #ylim=ylim,
    #border=cols,
    col='white',
    #names.arg=df[,"p_success"],
    xlab='p_success',
    ylab='observed outcome frequencies',
    legend.text=c('error frequency','success frequency')
    #args.legend=list(text.col=cols,col=cols,border=cols,bty='n')
);
box();



##################################

#first on the p df
freq_0 <- df_p$freq_0
freq_1 <- df_p$freq_1
p_success <- df_p$p_success

plot1<- p %>%   layout(title = 'Added value of synset-level aggregation \n(HT+, eng-all, aggregated by argmin(p))')#, xaxis = ax, yaxis=ay)

plot1
p

#plotly_IMAGE(plot1, width = 500, height = 500, format = "png", scale = 2, #needs API or simething
             #out_file = "/ufs/aggelen/SenseShiftEval/setting2/ht/eng-all/barchart.png")

#orca(plot1, file="/ufs/aggelen/SenseShiftEval/setting2/ht/eng-all/barchart.png") #needs something installed!


####################

#attach(df_avg)

#p %>%   layout(title = 'Added value of synset-level aggregation \n(HT+, eng-all)', xaxis = ax, yaxis=ay)

#detach(df_avg)


########################################################################################


#png(file = "/ufs/aggelen/SenseShiftEval/setting2/ht/eng-all/aggregation_not_averaged.png")

#plot(x=df1[,"p_success"],y=df1[,"success"], asp=1, cex = 1.3, xlim=c(0,1), ylim=c(0,1),  col = "blue", pch=16,
	#xlab="probability of success (correct vs. total observations per synset)", ylab="observed success rate (by aggregation)")

#abline(lm(df1$success ~ df1$p_success), asp=1,  col = "blue", lwd=3)


