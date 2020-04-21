################################################################################
# Calculate historical DBH time series from tree rings
################################################################################

rm(list=ls())

# libraries
library(dplR) # see: https://rpubs.com/andybunn/dplR-basics
library(dplyr)
library(naniar) # missing values ploting
library(imputeTS) # gapfill missing values

library(ggplot2)
library(grid)
library(gtable)
library(gridExtra)
library(scales)

trw_plotA_gapfilled = read.csv("trw_plotA.csv", header=T)
rownames(trw_plotA_gapfilled) = trw_plotA_gapfilled$Year
trw_plotA_gapfilled=trw_plotA_gapfilled[,-1]
class(trw_plotA_gapfilled)="data.frame"

trw_plotB_gapfilled = read.csv("trw_plotB.csv", header=T)
rownames(trw_plotB_gapfilled) = trw_plotB_gapfilled$Year
trw_plotB_gapfilled=trw_plotB_gapfilled[,-1]
class(trw_plotB_gapfilled)="data.frame"


# read metadata for the tree rings collected at the two plots          
plotA = read.csv("TRW_metadata_plotA.csv", stringsAsFactors=F)
plotA = plotA[,1:3]
plotA = plotA[,c("ID", "DBH..cm.", "Species")]
colnames(plotA) = c("ID", "DBH_cm", "Species") 

plotB = read.csv("TRW_metadata_plotB.csv", stringsAsFactors=F)
plotB = plotB[,1:3]
plotB = plotB[,c("ID", "DBH..cm.", "Species")]
colnames(plotB) = c("ID", "DBH_cm", "Species")

# Double bark thickness from the measured DBH
plotA$DBT_cm = (0.30 + 0.02 * ((plotA$DBH_cm)*0.393701))*2.54
plotA$iDBH = plotA$DBH_cm - plotA$DBT_cm 

plotB$DBT_cm = (0.30 + 0.02 * ((plotB$DBH_cm)*0.393701))*2.54
plotB$iDBH = plotB$DBH_cm - plotB$DBT_cm
 

# create an empty matrix for plot A
DBH_plot_A = matrix(NA, nrow=nrow(trw_plotA_gapfilled)+1, ncol=ncol(trw_plotA_gapfilled))
colnames(DBH_plot_A) =colnames(trw_plotA_gapfilled)
rownames(DBH_plot_A) = c(as.numeric(rownames(trw_plotA_gapfilled))-1, "2015")

for (i in 1:ncol(trw_plotA_gapfilled)){ # loop through trees
	
	# Stem diameter in 2015 (measured in the field; May 2016)
	iDBH_2015 = plotA[i,"iDBH"]
	
	tree = data.frame(year = as.numeric(rownames(trw_plotA_gapfilled)),
	                  ringwidth = trw_plotA_gapfilled[,i])
	tree_cc = tree[complete.cases(tree$ringwidth),]
	tree_cc$cumringwidth = rev(cumsum(rev(tree_cc$ringwidth)))/10 # cumulative ring width in [cm]
	
	# Reconstructing historical diameters with the proportional method of Bakker 2005
	tree_cc$iDBH = iDBH_2015 * (1-(2*tree_cc$cumringwidth)/iDBH_2015)
	
  tree_cc$DBH = tree_cc$iDBH + (0.30 + 0.02 * ((tree_cc$iDBH)*0.393701))*2.54               
	dummy = merge(tree, tree_cc, by="year", all.x=T)
	
	DBH_plot_A[,i] = c(dummy$DBH,  plotA[i,"DBH_cm"])
	 
}

matplot(DBH_plot_A, type="l")

# create an empty matrix for plot B
DBH_plot_B = matrix(NA, nrow=nrow(trw_plotB_gapfilled)+1, ncol=ncol(trw_plotB_gapfilled))
colnames(DBH_plot_B) =colnames(trw_plotB_gapfilled)
rownames(DBH_plot_B) = c(as.numeric(rownames(trw_plotB_gapfilled))-1, "2015")


for (i in 1:ncol(trw_plotB_gapfilled)){ # loop through trees
	
	# Stem diameter in 2015 (measured in the field; May 2016)
	iDBH_2015 = plotB[i,"iDBH"]
	
	tree = data.frame(year = as.numeric(rownames(trw_plotB_gapfilled)),
	                  ringwidth = trw_plotB_gapfilled[,i])
	tree_cc = tree[complete.cases(tree$ringwidth),]
	tree_cc$cumringwidth = rev(cumsum(rev(tree_cc$ringwidth)))/10
	
	# Reconstructing historical diameters with the proportional method of Bakker 2005
	tree_cc$iDBH = iDBH_2015 * (1-(2*tree_cc$cumringwidth)/iDBH_2015)

  tree_cc$DBH = tree_cc$iDBH + (0.30 + 0.02 * ((tree_cc$iDBH)*0.393701))*2.54       
  
	dummy = merge(tree, tree_cc, by="year", all.x=T)
	
	DBH_plot_B[,i] = c(dummy$DBH,  plotB[i,"DBH_cm"])
	 
}

matplot(DBH_plot_B, type="l")

# Save data --------------------------------------------------------------------
save(DBH_plot_A, DBH_plot_B, plotA, plotB, file = "DBH_timeseries.RData")

write.csv(DBH_plot_A,
          file="DBH_plot_A.csv",
          quote=F, row.names=T)
          
write.csv(DBH_plot_B,
          file="DBH_plot_B.csv",
          quote=F, row.names=T)
          