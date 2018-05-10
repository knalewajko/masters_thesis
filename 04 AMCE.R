# Clean environment
rm(list=ls())

# Set directory
getwd()
setwd("/Users/knalewajko/Documents/HSoG/04 Viertes Semester/03 Thesis/DATA")

# Load packages
library(readxl)
library(cjoint)

# Make sure R reads Polish characters
Sys.setlocale("LC_CTYPE", "pl_PL")

# Load dataset
db <- read_excel("conjoint_05042018.xls", sheet = 1)

# Perform analysis
results <- amce(Selected ~  relevel(as.factor(Attribute1),"1") + 
                  relevel(as.factor(Attribute2),"2") + 
                  relevel(as.factor(Attribute3),"2") + 
                  relevel(as.factor(Attribute4),"2"),
                  data=db, respondent.id="ID")
summary(results)