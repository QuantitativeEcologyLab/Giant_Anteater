#An example of an MCI analysis based on the corrMove package

setwd("/Path/to/your/files")

#Install corrMove from GitHub. This requires the devtools package.
#The GitHub versionof corrMove will be periodically updated.
library(devtools)
devtools::install_github("jmcalabrese/corrMove")

#Install corrMove from archival source package included with paper.
#Windows users will first need to install and load the Rtools package to install corrMove from the source .tar.gz.
#This assumes the .tar.gz file is in the directory you set above
install.packages("corrMove_0.1.0.tar.gz", repos=NULL, type="source")

#Load corrMove
library(corrMove)

#Help files available for main functions in corrMove
?as.corrData
?findPrts
?corrMove
?plot.corrMove

#Khulan example
dataKhulan <- read.csv("khulan_data.csv", header=TRUE)

#Create corrData object.
cdKhulan <- as.corrData(dataKhulan, timeformat = "%m/%d/%y")

#Estimate the partition points for the khulan data, with W=25
prtsKhulan <- findPrts(cdKhulan, W=25)

#Get the MCI estmates and selected model conditional on the data and partition points
cmKhulan <- corrMove(cdKhulan, prtsKhulan)

#3-panel plot of the MCIs over time
plot(cmKhulan)
