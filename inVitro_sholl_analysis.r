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
require(multcomp)
library(rstudioapi)

dataFile <- selectDirectory(
  caption = "Select Directory",
  label = "Select",
  path = getActiveProject()
)
setwd(dataFile)


#all of your files of merged sholl data should have Radius as the first column, followed by individual cells with number of intersections in the subsequent columns
a <- list.files(pattern='Cond') #this should be any common string among all of your sholl files
keyFiles <- list.files(pattern = 'Key', ignore.case=T)
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

#### The next 13 lines create a unique cell id.
## CRITICAL - YOU MUST HAVE UNIQUE IMAGE NAMES ACROSS ALL CONDITIONS AND ALL REPLICATES
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


key <- read.csv(keyFiles[1])
print(key) # CRTIICAL the order of the file names in the 'Name' column MUST be identical to the order of the variable 'a'
print(a)

h <- merge(h, key, 'L1')
names(h) <- c('List', 'Radius', 'ImageName', 'Intersections', 'cellNum', 'Condition', 'FileName', 'rep')
head(h)

#####################################################################
############               STATISTICS                     ###########
#####################################################################

h$Condition <- as.factor(h$Condition) #added to add compatiblity with R 4.0

cond_test_corAR <- nlme::lme(
  Intersections ~  1 + Condition,
  data = h,
  random = ~ 1 | cellNum/Radius, #accounts for the repeated measures and that some cells are maybe at different stages of complexity regardless of treatment
  control = lmeControl(opt = "optim")
)

cond_test_corAR
summary(cond_test_corAR)
print(anova(cond_test_corAR))

tukeys <- summary(glht(cond_test_corAR, linfct=mcp(Condition="Tukey")))
tukeysPH <- data.frame(as.character(row.names(tukeys$linfct)), tukeys$test$pvalues)
print(tukeysPH) #p value = 0 means the p value was less than 2.2E-16

#save raw data and statistics files
write.csv(h, 'MergedData.csv', row.names=F, quote=F) #This is the raw data file
write.csv(tukeysPH, 'TukeyPostHoc.csv', row.names=F, quote=F) #saves the tukey output table
write.csv(anova(cond_test_corAR), "Anova_results.csv", row.names=T, quote=F) #saves ANOVA results

#visualize individual replicates.
ggplot(h, aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess', span=0.1, alpha=0.3) + 
       ylab('# Intersections') + xlab ('Distance from Soma (um)') + facet_grid(~rep)

#plot used for figure
mer <- aggregate(h$Intersections, by=list(h$Condition, h$rep, h$Radius), mean) #takes mean of each radius by replicate and condition
colnames(mer) <- c('Condition', 'Replicate', 'Radius', 'Intersections')

ggplot(mer, aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess') + 
  ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle('this is my final figure')
