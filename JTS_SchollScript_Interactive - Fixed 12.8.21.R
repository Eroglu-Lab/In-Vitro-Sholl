#authors Krissy Sakers and Justin Savage
#kristina.sakers@duke.edu
#Version 2.0
#3/6/20



#install all of these first with install.packages('package name')
library(plyr)
library(stringr)
library(ggplot2)
library(reshape)
library(car)
library(agricolae)
library(xlsx)
library(lme4)
library(nlme)
require(multcomp) #require is used sometimes for use within a package.
library(rstudioapi)

dataFile <- selectDirectory(
  caption = "Select Directory",
  label = "Select",
  path = getActiveProject()
)
setwd(dataFile)

a <- list.files(pattern='Sholl')
keyFiles <- list.files(pattern = 'Key')
c <- lapply(a, read.csv)

for(i in 1:length(c)){
  tmp <- c[[i]]
  names(tmp)[1] <- 'Radius'
  c[[i]] <- tmp
}


h <- melt(c, id='Radius') #This creates a data frame now with all of the data. Column L1 is the list #.
                    #As long as your data is saved in order (i.e. merged condition 1 is in position 1), it will work.
                    # number 10 is now number one because numbers.

h <- h[!h$variable == 'X',] #this will remove any values that are introduced by having row names. 
                            #if you open in excel you will force row names, even if the file isn't saved.

f <- unique(h$variable)
ff <- data.frame(f, c(1:length(f)))
names(ff) <- c('name', 'num')

h$cellNum <- c()
k<-c()
for (i in 1:length(f)){
  tmp1 <- as.character(ff[ff$num == i, 1][1])
  tmp <- h[h$variable == tmp1, ]
  y <- rep(i, times=nrow(tmp))
  k <- c(k,y)
}
h$cellNum <- k

###THIS PART HERE IS THE PROBLEM AREA, ITS MAKING MORE OUTPUTS THAN THERE IS ROOM FOR IN THE ARRAY

key <- read.xlsx(keyFiles[1], 1, header=F) #change your key info
colnames(key) <- c('Blinded', 'Condition')
key$L1 <- 1:length(c)
#casks user for the number of replicates
repNum <- showPrompt('Replicates', 'How many replicates per condition?', default = 3)
#if number of replicates is greater than one, does appropriate statistics
key$rep <- rep(1:repNum, each=length(unique(key$Condition)))


h <- merge(h, key, 'L1')
names(h) <- c('List', 'Radius', 'ImageName', 'Intersections', 'cellNum', 'Blinded', 'Condition', 'rep')


#####################################################################
############               STATISTICS                     ###########
#####################################################################

h$Condition <- as.factor(h$Condition) #added to add compatiblity with R 4.0

cond_test_corAR <- nlme::lme(
  Intersections ~  1 + Condition,
  data = h,
  random = ~ 1 | cellNum/Radius, #accounts for the repeated measures and that some cells are maybe at different stages of complexity regardless of treatment
  correlation = corAR1() #0 autocorrelation, no porportional changes in a time series
)

cond_test_corAR
summary(cond_test_corAR)
anova(cond_test_corAR)

tukeys <- summary(glht(cond_test_corAR, linfct=mcp(Condition="Tukey")))
tukeysPH <- data.frame(as.character(row.names(tukeys$linfct)), tukeys$test$pvalues)
write.csv(h, 'MergedData.csv', row.names=F, quote=F) #This is the raw data file DONT GET RID OF IT
write.csv(tukeysPH, 'TukeyPostHoc.csv', row.names=F, quote=F) #saves the tukey output table
write.csv(anova(cond_test_corAR), "Anova_results.csv", row.names=T, quote=F)

#visualize individual replicates.
ggplot(h[h$rep == 1, ], aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess', span=0.1, alpha=0.3) + 
       ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle('Replicate 1')
ggplot(h[h$rep == 2, ], aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess', span=0.1, alpha=0.3) + 
       ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle('Replicate 2')
ggplot(h[h$rep == 3, ], aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess', span=0.1, alpha=0.3) + 
       ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle('Replicate 3')


#plot used for figure
mer <- aggregate(h$Intersections, by=list(h$Condition, h$rep, h$Radius), mean)
colnames(mer) <- c('Condition', 'Replicate', 'Radius', 'Intersections')

ggplot(mer, aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess') + 
  ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle('this is my final figure')
#colored area is standard error