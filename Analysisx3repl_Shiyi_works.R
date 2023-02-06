#authors Krissy Sakers and Justin Savage
#kristina.sakers@duke.edu
#Version 2.0
#3/11/20

#This script must be run in RStudio. Prepare your data by moving
#all of your merged sholl profiles into a folder and naming them
#following the convention of "Sholl_11" for prep 1 condition 1.
#Also make an excel sheet named "Key" (saved as .xlsx) with the first 
#column as each merged profile (i.e. Sholl_11) and the
#second column having the treatment for that condition
#(i.e. shScramble). Next select all of this script and click
#the run button. You will then be asked to select the folder with
#with all of your sholl profiles and your Key. Next you will be
#asked how many replicates you have. Enter this number and 
#hit enter. The program will then take a few moments. Click
#the "Plots" window in RStudio and you should see a plot labeled
#"this is my final figure once the script has finished. The script
#will then ask if you'd like to change the tick marks for the axes 
#and to give the plot a title. This plot will also be saved as a png file.

#installs necessary packages if they aren't installed already

if("plyr" %in% rownames(installed.packages()) == FALSE) 
{install.packages("plyr")}
library(plyr)
if("stringer" %in% rownames(installed.packages()) == FALSE) 
{install.packages("stringr")}
library(stringr)
if("ggplot2" %in% rownames(installed.packages()) == FALSE) 
{install.packages("ggplot2")}
library(ggplot2)
if("reshape" %in% rownames(installed.packages()) == FALSE) 
{install.packages("reshape")}
library(reshape)
if("car" %in% rownames(installed.packages()) == FALSE) 
{install.packages("car")}
library(car)
if("agricolae" %in% rownames(installed.packages()) == FALSE) 
{install.packages("agricolae")}
library(agricolae)
if("xlsx" %in% rownames(installed.packages()) == FALSE) 
{install.packages("xlsx")}
library(xlsx)
if("lme4" %in% rownames(installed.packages()) == FALSE) 
{install.packages("lme4")}
library(lme4)
if("nlme" %in% rownames(installed.packages()) == FALSE) 
{install.packages("nlme")}
library(nlme)
if("multcomp" %in% rownames(installed.packages()) == FALSE) 
{install.packages("multcomp")}
require(multcomp) #require is used sometimes for use within a package.

#loaded RStudioAPI to use for selecting working directory
if("rstudioapi" %in% rownames(installed.packages()) == FALSE) 
{install.packages("rstudioapi")}
library(rstudioapi)

dataFile <- selectDirectory(
  caption = "Select Directory",
  label = "Select",
  path = getActiveProject()
)
setwd(dataFile)

#a is a list of the location of each sholl file
a <- list.files(pattern='Sholl')
#added keyFiles variable so user doesn't have to enter it manually
keyFiles <- list.files(pattern = 'Key')
#c is each Sholl file read into R
c <- lapply(a, read.csv, stringsAsFactors = FALSE)
h <- melt(c, id='Radius') #This creates a data frame now with all of the data. Column L1 is the list #.
                    #As long as your data is saved in order (i.e. merged condition 1 is in position 1), it will work.
                    # number 10 is now number one because numbers.

h <- h[!h$variable == 'X',] #this will remove any values that are introduced by having row names. 
                            #if you open in excel you will force row names, even if the file isn't saved.

#the following loop processes produces the cellNum vector k for each
#Sholl file (L1) by assuming sholl analysis started at radius of 10 
#and using this to loop through and count until it reaches the 
#second radius 10 in the subset (the second image) it then saves 
#this number as radiusCount and uses this to populate the 
#appropriate cellNum data instead of relying on unique variable names

radiusCount <- 0
i = 1

h$cellNum <- 0
currentList <- 1
currentRow <- 1
lastStop <- 1
currentCell <- 1
#for each Sholl File
for(i in 1:length(a)){
#L1 is each Sholl File source with the same max radius
  #find the length of the max Radius for that list 
  activeRows <- subset(h, L1 == currentList)
  #loops through 500 rows of h to find the end of the radius measurements
  for(i in 1:500) {
    currentRadius <- activeRows$Radius[i]
    if(currentRadius == 10 && i != 1){
      break
    }
      radiusCount <- activeRows$Radius[i]
  }
  #subsets the rows in the current list and populates their cellNum
  k<-c()
  for (i in 1:(length(activeRows$variable)/(radiusCount-9))) {
    y <- rep(currentCell, times = (radiusCount-9))
    k <- c(k,y)
    currentCell <- currentCell + 1
  }
  h$cellNum[lastStop:(lastStop + length(activeRows$variable)-1)] <- k
  
  lastStop <- lastStop + length(activeRows$L1)
  currentList <- currentList + 1
}
#used keyFiles variable so you don't have to change the following line
key <- read.xlsx(keyFiles[1], 1, header=F)
key <- key[complete.cases(key[,1]), ]
key <- key[,c(1:2)]
colnames(key) <- c('Blinded', 'Condition')
key$L1 <- 1:length(c)

#asks user for the number of replicates and uses this for key$rep
repNum <- showPrompt('Replicates', 'How many replicates per condition?', default = 3)
key$rep <- rep(1:repNum, each=length(unique(key$Condition)))

h <- merge(h, key, 'L1')
names(h) <- c('List', 'Radius', 'ImageName', 'Intersections', 'cellNum', 'Blinded', 'Condition', 'rep')

h$Radius <- as.factor(h$Radius)
h$cellNum <- as.factor(h$cellNum)
h$Condition <- as.factor(h$Condition)

#####################################################################
############               STATISTICS                     ###########
#####################################################################

cond_test_corAR <- nlme::lme(
  Intersections ~  0 + Condition,
  data = h,
  random = ~ 1 | cellNum/Radius, #accounts for the repeated measures and that some cells are maybe at different stages of complexity regardless of treatment
  na.action=na.exclude,
  correlation = corAR1(),
  control = lmeControl(opt = "optim")#0 autocorrelation, no porportional changes in a time series
)

cond_test_corAR
summary(cond_test_corAR)
anova(cond_test_corAR)

tukey <- summary(glht(cond_test_corAR, linfct=mcp(Condition="Tukey")))
tukeysPH <- data.frame(as.character(row.names(tukey$linfct)), tukey$test$pvalues)
write.csv(h, 'MergedData.csv', row.names=F, quote=F)
write.csv(tukeysPH, 'TukeyPostHoc.csv', row.names=F, quote=F)


#visualize individual replicates.
repNum <- strtoi(repNum, base = 10)
plotRep <- function(x)
{
    ggplot(h[h$rep == x, ], aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + 
    stat_smooth(col='black', method='loess', span=0.1, alpha=0.3) + 
    ylab('# Intersections') + xlab ('Distance from Soma (um)') + 
    ggtitle(paste('Replicate', x, sep = " "))
}

for (i in 1:repNum){
  currentPlot <- plotRep(i)
  print(currentPlot)
}


#plot used for figure
mer <- aggregate(h$Intersections, by=list(h$Condition, h$rep, h$Radius), mean)
colnames(mer) <- c('Condition', 'Replicate', 'Radius', 'Intersections')
ggplot(mer, aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess') + 
  ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle('this is my final figure') + scale_x_discrete(breaks=c(0,40, 80, 120, 160))


#asks the user if they'd like to change the axes or title
xmax <- showPrompt("xmax", "Set xmax for x-axis labels", default = 150)
xmax <- strtoi(xmax, base = 10)
xstep <- showPrompt("xstep", "Set interval between x-axis tick marks", default = 25)
xstep <- strtoi(xstep, base = 10)
ymax <- showPrompt("ymax", "Set ymax for y-axis labels", default = 30)
ymax <- strtoi(ymax, base = 10)
ystep <- showPrompt("ystep", "Set interval between y-axis tick marks", default = 10)
ystep <- strtoi(ystep, base = 10)
graphTitle <- showPrompt("Graph Title", "Enter graph title", default = "title")

ggplot(mer, aes(x=Radius, y=Intersections, group=Condition, fill=Condition)) + stat_smooth(col='black', method='loess') + 
  ylab('# Intersections') + xlab ('Distance from Soma (um)') + ggtitle(graphTitle) + 
  scale_x_continuous(breaks = seq(0,xmax,xstep)) + scale_y_continuous(breaks = seq(0, ymax, ystep))

#saves the final plot as png file
ggsave("finalplot.png", width = 8, height = 5)


