##########
#Schneider/Wagemann (2012): Set-Theoretic Methods for the Social Sciences, Cambridge University Press
##R script of analytic steps described in chapters 1-7
#Author: Carsten Q. Schneider

# Remove everything that might be in your R working space from previous analyses
rm(list = ls())

# Load the packages relevant for doing set-theoretic analyses 
# NB: loading these packages will automatically load additional packages
library(QCA); library(SetMethods); library(lattice); library(foreign)

# Tell R where your data files are and where you would like to save
# this R script or any other file that you produce in the course of this session
#NB: In RStudio, go to the lower-right window and there the "Files" tab
#find your folder, then click "More" and"Set as Working Directory"
#Then, from the lower-left window (the "Console")copy and paste the path into
#the upper-left window (i.e. this R script file)

setwd("~/Dropbox/QCA/QCA Workshops/Vienna_14/lab_sessions")


########
#CHAPTER 1
########

#DIRECT CALIBRATION
#load data and store it in an object
FS <- read.csv("FreitagSchlicht_2010_raw.csv")
#quick check how the first rows of the data look like
head(FS)
#You can also make the data visible by clicking on the data file name in the upper-right window
#This opens a tab in the upper-left window

#Direct calibration of crisp set "high provision of full day schools" using variable "full_day_schools"
#as the base variable
FS$fulldayschools_cs <- calibrate (FS$full_day_schools, type = "crisp", thresholds = c (8.3))

#Direct calibration of fuzzy set "high provision of full day schools"
#we stack this new condition into the data set "FS" using this dollar sign
#We tell R to use the logistic function ("logistic = TRUE") in order to obtain the same set
#membership scores as when doing the calibration with the fsQCA software.
#In addition to the logistic function, R offers additional functional forms.
FS$fulldayschools_fz <- calibrate (FS$full_day_schools, type = "fuzzy", thresholds = c(3, 8.3 , 20), logistic = TRUE)


#INDIRECT CALIBRATION
#Notice that the indirect calibration function exists only in the package "SetMethods"
#Since we have loaded it at the beginning of our session, R understands the command that we will
#write in a second (directCalibration).

#first create the precalibrated set ("fulldayschool_precab") and stack it into data set "FS"
FS$fulldayschool_precab[FS$full_day_schools >0 & FS$full_day_schools <=5] <- 0
FS$fulldayschool_precab[FS$full_day_schools >5 & FS$full_day_schools <=7] <- 0.2
FS$fulldayschool_precab[FS$full_day_schools >7 & FS$full_day_schools <=9] <- 0.4
FS$fulldayschool_precab[FS$full_day_schools >9 & FS$full_day_schools <=13] <- 0.6
FS$fulldayschool_precab[FS$full_day_schools >13 & FS$full_day_schools <=18] <- 0.8
FS$fulldayschool_precab[FS$full_day_schools >18 & FS$full_day_schools <=30] <- 1

#Make sure this new condition is recognized as a numeric vector by R
FS$fulldayschool_precab <- as.numeric(FS$fulldayschool_precab)

#let's plot the precalibrated set against the base variable
plot(FS$full_day_schools, FS$fulldayschool_precab)

#now we use both the variable on the percentage of schools and the precalibrated set
#in order to produce the membership of each case in the set of "high provision of full day schools"
FS$fulldayschool_indir <- indirectCalibration(FS$full_day_schools, FS$fulldayschool_precab, binom = TRUE)

#Now, let's plot the indirectly calibrated set against the base variable
plot(FS$full_day_schools, FS$fulldayschool_indir)

#if you want to save the data with all the additional variables you have added
save(FS,file="FS.Rdata")

#####
#CHAPTER 2
#OPERATIONS ON SETS
#####

S.data <- read.csv("Samford_2010_fs.csv")
head(S.data)

#Let's create a new set which is the logical negation of set "raplib"
S.data$not_raplib <- 1 - S.data$raplib

#We print the two conditions and see that the vakues in column 2 are the values 
#from column 1 subtracted from 1
cbind(S.data$raplib, S.data$not_raplib) # 

#We can also perform logical operations on complex Boolean expression
#What is the negation of expression AC
deMorgan("AC")
#and of AC + B~C
deMorgan("AC + B~C")
#or of WS + ES*WM + QU*LP + es*LP
deMorgan("WS + ES*WM + QU*LP + es*LP", prod.split = "*")

#########
#CHAPTERS 3+5
#SET RELATIONS and parameters of fit
#########
#ANALYSIS OF NECESSITY
#Let's first get Freitag and Schlicht's data set with all conditions and the outcome calibrated
FSC <- read.csv("FreitagSchlicht_2009_fs.csv", row.names = 1)
head(FSC)

names(FSC)[1] <- 'OUTCOME'

#We use the pof function in the QCA package to figure out whether 
# condition "lateeduc" is necessary for outcome "socunequal"
pof(FSC$lateeduc, outcome = "OUTCOME", FSC, relation = "nec")
#if we wanted to test if the negation of condition "lateeduc" is necessary
#we write
pof(1 - FSC$lateeduc, outcome = "OUTCOME", FSC, relation = "nec")

#We can also use the QCAfit function in the SetMethod package for the same purpose
##The advantage is that QCAfit reports one additional parameter of fit, an, I think,
#improved measure of relevance of necessity (RoN)
QCAfit(FSC$lateeduc, FSC$OUTCOME, necessity = TRUE)
QCAfit(1- FSC$lateeduc, FSC$OUTCOME, necessity = TRUE)

#IF we want to perform tests of necessity for each condition separately but in one command
#line, we write
QCAfit(FSC[, 3:5], FSC$OUTCOME, cond.lab = names(FSC)[ 3:5],  necessity = TRUE)
QCAfit(1 - FSC[, 3:5], FSC$OUTCOME, cond.lab = paste('not', names(FSC)[ 3:5]),  necessity = TRUE)

#ANALYSIS OF SUFFICIENCY
#If we have a hunch that a specific single condition is sufficient for the outcome
# we can test it using pof. Let's try with condition "earlytrack"
pof(FSC$earlytrack, outcome = "OUTCOME", FSC, relation = "suf")

#if we think a specific combination of conditions is sufficient, we first need to 
#create this combination and then use pof again
#let's create the logical AND combination of lateeduc and earlytrack and 
#call the new set Q and stack it into data set FSC
FSC$Q <- pmin(FSC$lateeduc, FSC$earlytrack)

pof(FSC$Q, outcome = "OUTCOME", FSC, relation = "suf")


######
#CHAPTER 4-7
#TRUTH TABLES, LIMITED DIVERSITY, LOGICAL MINIMIZATION
#####

#The common way to identify sufficient conditions, though, is to test 
#all logical possible combinations, i.e truth table rows.
#So we first sort our data into a truth table and call it TT

TT <- truthTable(FSC, outcome = "OUTCOME", 
                 conditions = c("lateeduc", "hdayschool", "earlytrack", "strongtripart"),
                 incl.cut1 = 0.8, complete = TRUE, show.cases = TRUE, sort.by = c("n", "incl"))
#to see TT type
TT

#to logically minimize truth table TT

#conservative solution
sol_c <- eqmcc(TT, details = TRUE, show.cases = TRUE)
sol_c

#most parsimonious solution
sol_p <- eqmcc(TT, details = TRUE, show.cases = TRUE, include = "?")
sol_p
#Which simplifying assumptions have been made
sol_p$SA

#intermediate solution
sol_i <-eqmcc(TT, details = TRUE, show.cases = TRUE, include = "?", dir.exp = c(1, 1, 0, 0))
sol_i
#Which easy counterfactuals have been made
sol_i$i.sol$C1P1$EC
# END
