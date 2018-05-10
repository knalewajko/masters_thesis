# Clean environment
rm(list=ls())

# Set directory
getwd()
setwd("/Users/knalewajko/Documents/HSoG/04 Viertes Semester/03 Thesis/DATA")

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# Make sure R reads Polish characters
Sys.setlocale("LC_CTYPE", "pl_PL")

###########################################################################

# DESCRIPTIVE ANALYSIS

###########################################################################

# Load data

db <- read_excel("Thesis4.xlsx", sheet = 1)

db$Voted <- as.numeric(db$Voted)
db$LeftRight <- as.numeric(db$LeftRight)
db$Sex <- as.numeric(db$Sex)
db$Education <- as.numeric(db$Education)
db$Income <- as.numeric(db$Income)
db$Treatment <- as.character(db$Treatment)

###########################################################################

# Create the codebook

# Create the codebook
variables <- c('ID','SumPop','PopBinary','SumEconWonk','EconWonkBinary','Treatment','EconState','Sex','Education','EmplStatus','Income','PolInt','LeftRight','Voted','Party2015','PiD','NationalID','Age','Province','Unemp201615','LTUNemp201615','YouthUnemp201615','Unemp2016','LTUnemp2016','YouthUnemo2016','Seconds','YearBorn','Origin','PLindisp','PLconnect','PLborn','PLproud','PLbetter')
description <- c('(chr) Unique IDs of respondents','(int) Number of times a populist candidate was chosen (0-5)','(int) Dummy for preference for populist candidates, i.e. SumPop >= 3 (0,1)','(int) Number of times a mainstream candidate was chosen (0-5)','(int) Dummy for preference for mainstream candidates, i.e. SumEconWonk >=3 (0,1)','(chr) Type or lack of treatment assigned at random to the respondent (\'A\': German newspaper, \'B\': Polish newspaper, \'control\': no treatment)','(chr) Evaluation of the state of Poland\'s economy (\'very bad\', \'bad\', \'good\', \'very good\')', '(int) Dummy for respondents\' gender (\'0\': male, \'1\': female)','(int) Highest achieved level of formal education (1-8: corresponding to 6-22 years)','(chr) Employment status (\'Employed full-time in private sector\', \'Employed full-time in public sector\', \'Employed part-time in private sector\', \'Employed part-time in public sector\', \'Small business owner\', \'Freelancer\', \'On pension\', \'Unemployed\', \'Student\')','(int) Yearly gross income (1-9: corresponding to approx. less than 2,800-more than 24,000 EUR)','(int) Degree of overall interest in politics (1-8)','(int) Personal political placement on Left-Right spectrum (1:8 & NA, where 1 = \'extreme left\', \'8\' = \'extreme right\', \'NA\' = \'don\'t know\')','(int) Dummy for having voted in 2015 parliamentary elections (0,1)','(chr) Party chosen in 2015 parliamentary elections if Voted = 1 (\'Kukiz\'15\', \'Nowoczesna\', \'PO\', \'KORWiN\', \'Lewica\', \'PiS\', \'Razem\', \'Grzegorz Braun\', \'PSL\')','(int) Degree of personal identification with the chosen party (1-8, where 1 = \'not at all\', \'8\' = \'very much\')','(int) Degree of national identification, i.e. the sum of PLindisp, PLconnect, PLborn, PLproud and PLbetter (5-25)','(int) Age of the respondent, i.e. the difference between 2018 and YearBorn (22-67)','(chr) Voivodeship of origin (\'DŚ\', \'KP\', \'LB\', \'LD\', \'LS\', \'MP\', \'MZ\', \'OP\', \'PK\', \'PL\' \'PM\', \'SL\', \'SK\', \'WM\', \'WP\', \'ZP\')','(dbl) Change in unemployment rate per voivodeship between years 2016 and 2015, source: Eurostat (min.: -2.1, max.: -0.5, mean: -1.2)','(dbl) Change in long-term unemployment rate per voivodeship between years 2016 and 2015 - as proportion of Unemp201615, source: Eurostat (min.: -15, max.: 0.3, mean: -4.7)','(dbl) Change in youth unemployment rate per voivodeship between years 2016 and 2015 - as proportion of Unemp201615, source: Eurostat (min.: -7.9, max.: 1.4, mean: -2.8)', '(dbl) 2016 unemployment rate per voivodeship, source: Eurostat (min.: 4.8, max.: 9.2, mean: 6.4)','(dbl) 2016 long-term unemployment rate per voivodeship - as proportion of Unemp2016, source: Eurostat (min.: 26.8, max.: 42, mean: 35)','(dbl) 2016 youth unemployment rate per voivodeship - as proportion of Unemp2016, source: Eurostat (min.: 10.6, max.: 27.5, mean: 18.23)','(int) Number of seconds taken to complete the survey (min.: 200, max.: 785, mean: 346)','(dbl) Year in which respondent was born (1951-1996)','(chr) Origin of the respondent', '(int) Degree of agreement with the statement \'My Polish identity is an integral part of me\' (1-5)', '(int) Degree of agreement with the statement \'I feel truly connected with other Poles\' (1-5)', '(int) Degree of agreement with the statement \'I am happy to have been born in Poland\' (1-5)', '(int) Degree of agreement with the statement \'I am proud to be Polish\' (1-5)', '(int) Degree of agreement with the statement \'Poland is a better country than other countries in the world\' (1-5)')
codebookFULL <- data.frame(variables, description)
openxlsx::write.xlsx(codebookFULL, 'codebook.xlsx')

###########################################################################

# Summary statistics table per group

tapply(db$Age, db$Treatment, summary)

psych::describeBy(x = list(db$Age), group = db$Treatment, mat = FALSE, type = 1)

db %>%
  group_by(Treatment) %>%
  summarise_all(funs(mean()), db$Age, db$Education, db$Income) %>%
  ungroup

# Check sample for distributions and randomizations

# Gender
ggplot(data = db, mapping = aes(x = as.factor(Sex))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = 'dodge') +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "male","1" = "female")) +
  theme_bw()
# Age
ggplot(data = db, mapping = aes(x = Age)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 5) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot(data = db, mapping = aes(x = Age)) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggplot(data = db, mapping = aes(x = log(Age))) +
  geom_density(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()
# Province
ggplot(data = db, mapping = aes(x=reorder(Province, -table(Province)[Province]))) + 
  geom_bar() +
  labs(x="Province", y="Count of respondents") +
  coord_flip() +
  theme_bw()

# Province per treatment group (for ANNEX)
ggplot(data = db, mapping = aes(x=reorder(Province, -table(Province)[Province]))) + 
  geom_bar() +
  facet_grid(~Treatment) +
  labs(x="Province", y="Count of respondents") +
  coord_flip() +
  theme_bw()

# Employment status
ggplot(data = db, mapping = aes(x=reorder(EmplStatus, -table(EmplStatus)[EmplStatus]))) + 
  geom_bar() +
  labs(x="Employment Status", y="Count of respondents") +
  coord_flip() +
  theme_bw()

# Employment status per treatment group (for ANNEX)
ggplot(data = db, mapping = aes(x=reorder(EmplStatus, -table(EmplStatus)[EmplStatus]))) + 
  geom_bar() +
  facet_grid(~Treatment) +
  labs(x="Employment Status", y="Count of respondents") +
  coord_flip() +
  theme_bw()

# Education
ggplot(data = db, mapping = aes(x = Education)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()
# Income
db2 <- db$Income
db2 <- as.data.frame(db2)
db2$Income <- db$Income
db2$db2 <- NULL

db2$Income[db2$Income == 1] <- "less than 2,900 EUR"
db2$Income[db2$Income == 2] <- "2,900-5,699 EUR"
db2$Income[db2$Income == 3] <- "5,700-8,599 EUR"
db2$Income[db2$Income == 4] <- "8,600-11,499 EUR"
db2$Income[db2$Income == 5] <- "11,500-14,399 EUR"
db2$Income[db2$Income == 6] <- "14,400-17,199 EUR"
db2$Income[db2$Income == 7] <- "17,200-22,999 EUR"
db2$Income[db2$Income == 8] <- "23,000-28,800 EUR"
db2$Income[db2$Income == 9] <- "more than 28,800 EUR"

ggplot(data = db2, mapping = aes(x=factor(Income, levels = c("less than 2,900 EUR", "2,900-5,699 EUR", "5,700-8,599 EUR",
                                                             "8,600-11,499 EUR", "11,500-14,399 EUR", "14,400-17,199 EUR",
                                                             "17,200-22,999 EUR", "23,000-28,800 EUR", "more than 28,800 EUR")))) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1, stat = "count") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Yearly gross salary", y="Proportion of respondents") +
  coord_flip() +
  theme_bw()

# Political ideology
ggplot(data = db, mapping = aes(x = LeftRight)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

# Voting behaviour
ggplot(data = db, mapping = aes(x=reorder(Party2015, -table(Party2015)[Party2015]))) + 
  geom_bar() +
  labs(x="Party chosen in 2015 elections", y="Count of respondents") +
  coord_flip() +
  theme_bw()

ggplot(data = db, mapping = aes(x=reorder(Party2015, -table(Party2015)[Party2015]), y=Age)) + 
  geom_boxplot() +
  labs(x="Party chosen in 2015 elections", y="Age of respondents") +
  coord_flip() +
  theme_bw()

# Voting behaviour per treatment group
ggplot(data = db, mapping = aes(x=reorder(Party2015, -table(Party2015)[Party2015]))) + 
  geom_bar() +
  labs(x="Party chosen in 2015 elections", y="Count of respondents") +
  facet_grid(~ Treatment) +
  coord_flip() +
  theme_bw()

# Voting behaviour per province
ggplot(data = subset(db,Party2015 %in% c("PO","PiS","non-voter")), mapping = aes(x=reorder(Party2015, -table(Party2015)[Party2015]), fill = Treatment)) + 
  geom_bar() +
  labs(x="Party chosen in 2015 elections", y="Count of respondents") +
  facet_grid(~ Province) +
  coord_flip() +
  theme_bw()

# Voting behaviour per gender
ggplot(data = db, mapping = aes(x=reorder(Party2015, -table(Party2015)[Party2015]))) + 
  geom_bar() +
  labs(x="Party chosen in 2015 elections", y="Count of respondents") +
  facet_grid(~ Sex) +
  coord_flip() +
  theme_bw()

###########################################################################

# Check whether the treatment worked #1. Treatment and EconState

ggplot(db, aes(x=factor(EconState, levels = c("very bad","bad","good","excellent")), fill=Treatment)) +
  geom_bar(aes(group=factor(Treatment), y = ..prop..), stat = "count", position = "dodge") +
  labs(title="Respondents' opinions on the state of Poland's economy per treatment group",
       subtitle="'A' relates to the 'German newspaper'-treatment, 'B' to 'Polish newspaper'-treatment, and 'control' to the control group",
       caption="",
       x="State of the economy",
       y="Proportion of answers",
       fill="Treatment")
  
chisq.test(db$Treatment, db$EconState) # X-squared = 4.167, df = 6, p-value = 0.6541  ==> p>0.05, so we can't reject the null hypothesis and conclude that the treatments don't have any effect on the evaluation of the state of the economy

db$Treatment <- factor(db$Treatment, levels = c("A","B","control"))
ggplot(db, aes(x=NationalID, fill=Treatment)) +
  geom_density(aes(x=NationalID, y = ..count..), alpha = 0.5) +
  labs(title=" ",
       subtitle="x",
       caption="",
       x="Strength of national identification",
       y="Count of answers",
       fill="Treatment") +
  scale_fill_grey(start = 0.1, end = .9) +
  scale_color_grey(start = 0.1, end = .9) +
  theme_bw()
  scale_y_continuous(labels = scales::percent)

ggplot(db, aes(x=NationalID,fill=Treatment)) +
  geom_area(aes(x=NationalID, y = ..density..), position = "dodge", binwidth = 5) +
  labs(title=" ",
       subtitle="x",
       caption="",
       x="Strength of national identification",
       y="Count of answers",
       fill="Treatment") +
  scale_fill_grey(start = 0.3, end = .9) +
  scale_color_grey(start = 0.3, end = .9) +
  theme_bw()
  #scale_y_continuous() +
  #scale_x_continuous(breaks = c(5,10,15,20,25))

ols <- lm(data = db, formula = NationalID ~ relevel(as.factor(db$Treatment), "control"))
summary(ols) #difference not significant

ggplot(db, aes(x=factor(Treatment, levels = c("A","B","control")), fill=as.factor(PopBinary))) +
  geom_bar(aes(group=factor(PopBinary), y = ..prop..), position = "fill") +
  labs(title="Respondents' choice of party types per treatment group",
       subtitle="'A' relates to the 'German newspaper'-treatment, 'B' to 'Polish newspaper'-treatment, and 'control' to the control group",
       caption="",
       x="Experiment Groups",
       y="Proportion of answers",
       fill="PopBinary") +
  scale_y_continuous(labels=percent) +
  scale_fill_grey(start = 0.7, end = 0.1) +
  scale_color_grey(start = 0.7, end = 0.1) +
  theme_bw()

chisq.test(db$PopBinary, db$Treatment) # X-squared = 3.0211, df = 2, p-value = 0.2208

db$EconState <- factor(db$EconState, 
                       levels=c("very bad","bad","good","excellent"), 
                       labels=c("very bad","bad","good","excellent"))

ggplot(data = db, mapping = aes(x=NationalID, fill=EconState, color=EconState)) + 
  geom_area(db, mapping = aes(x=NationalID, fill=EconState, color = EconState), stat = "bin", binwidth = 5, alpha = 0.3) +
  facet_grid(~ Treatment) +
  scale_fill_grey(start = 0, end = .9) +
  scale_color_grey(start = 0, end = .9) +
  theme_bw()

# Check whether the non-voters are the least educated
  
ggplot(db, aes(x=factor(Party2015), y = Education)) +
  geom_boxplot()

###########################################################################

# Check whether the treatment worked #2. Treatment, EconState, and actual Unemployment

ggplot(db, aes(x=factor(EconState, levels = c("very bad","bad","good","excellent")), y=Unemp201615, fill=Treatment)) +
  geom_bar(aes(y = Unemp201615), stat = "identity", position = "dodge")

chisq.test(db$Unemp201615, db$EconState) # X-squared = 28.106, df = 33, p-value = 0.7095

###########################################################################

# PLOT AND TEST DEPENDENT VARIABLE AGAINST OTHER ATTRIBUTES

# Voting for populists by treatment

ggplot(db, aes(x=factor(PopBinary), fill=as.factor(Treatment))) +
  geom_bar(aes(group=factor(Treatment), y = ..prop..), stat = "count", position = "dodge")

chisq.test(db$PopBinary, db$Treatment) # X-squared = 3.0211, df = 2, p-value = 0.2208

anova <- aov(data = db, PopBinary ~ Treatment)
summary(anova2) # F-statistic = 1.511 and p-value = 0.225, so we can't reject the null hypothesis

anova <- aov(data = db, SumPop ~ Treatment)
summary(anova) # F = 2.484 and p = 0.0873, so we can't reject the null hypothesis at 95% confidence level, only at 90% level

# Voting for populists per sex

ggplot(db, aes(x=factor(PopBinary), fill=as.factor(Sex))) +
  geom_bar(aes(group=factor(Sex), y = ..prop..), stat = "count", position = "dodge")

chisq.test(db$PopBinary, db$Sex) # X-squared = 0.22509, df = 1, p-value = 0.6352
anova <- aov(data = db, PopBinary ~ Sex)
summary(anova)  # F = 0.419  p = 0.519
  
# Voting for populists by econstate

ggplot(db, aes(x=factor(PopBinary), fill=EconState)) +
  geom_bar(aes(group=factor(EconState), y = ..prop..), stat = "count", position = "dodge")

chisq.test(db$PopBinary, db$EconState) # X-squared = 1.7106, df = 3, p-value = 0.6346
anova <- aov(data = db, PopBinary ~ EconState)
summary(anova)  # F = 0.56  p = 0.642

# Voting for populists by education

ggplot(db, aes(x=factor(PopBinary), y=Education)) +
  geom_boxplot() # The median of education for the populist voters is lower (6 vs 4.5)

chisq.test(db$Education, db$PopBinary) # X-squared = 10.594, df = 7, p-value = 0.1573  ==> p>0.05, so we can't reject the null hypothesis and we need to conclude that the level of education doesn't influence the respondents' voting choice

anova <- aov(data = db, Education ~ PopBinary)
summary(anova) # F-statistic is 0.733 and p-value 0.394, so we can't reject the null hypothesis at 95% confidence level

ggplot(db, aes(x=SumPop, y=Education)) +
  geom_jitter() + geom_smooth() # No discerneable pattern

# Voting for populists by income

ggplot(db, aes(x=factor(PopBinary), y=Income)) +
  geom_boxplot() # The median income for the populist voters is lower (5 vs 4.75 level), the interquartile range is more spread (upper limit for populist voters is level 6, that of non-populists: 7)

ggplot(db, aes(x=SumPop, y=Income)) +
  geom_jitter() + geom_smooth() 

chisq.test(db$Income, db$PopBinary) # X-squared = 9.9984, df = 8, p-value = 0.2651
anova <- aov(data = db, Income ~ PopBinary)
summary(anova) # F = 0.495  p = 0.483

# Voting for populists by interest in politics

ggplot(db, aes(x=factor(PopBinary), y=PolInt)) +
  geom_boxplot() 

ggplot(db, aes(x=SumPop, y=PolInt)) +
  geom_jitter() + geom_smooth() 

# Voting for populists by left-right positioning

ggplot(db, aes(x=factor(PopBinary), y=LeftRight)) +
  geom_boxplot() 

ggplot(db, aes(x=SumPop, y=LeftRight)) +
  geom_jitter() + geom_smooth() 

# Voting for populists by election participation

ggplot(db, aes(x=factor(PopBinary), fill=factor(Voted))) +
  geom_bar(aes(group=factor(Voted), y = ..prop..), stat = "count", position = "dodge")

chisq.test(db$PopBinary, db$Voted) # X-squared = 0.2441, df = 6, p-value = 0.6213  ==> p>0.05, so we can't reject the null hypothesis
anova2 <- aov(data = db, PopBinary ~ Voted)
summary(anova2) # F-statistic is 0.502 and p-value 0.48, so we can't reject the null hypothesis

ggplot(db, aes(x=factor(Voted), y=SumPop)) +
  geom_boxplot()

# Voting for populists by party voted in 2015

ggplot(db, aes(x=factor(PopBinary), fill=Party2015)) +
  geom_bar(aes(group=factor(Party2015), y = ..prop..), stat = "count", position = "dodge")

chisq.test(db$PopBinary, db$Party2015) # X-squared = 9.5554, df = 6, p-value = 0.2976  ==> p>0.05, so we can't reject the null hypothesis
anova2 <- aov(data = db, PopBinary ~ Party2015)
summary(anova2) # F-statistic is 1.201 and p-value 0.306, so we can't reject the null hypothesis

# Voting for populists by party identification

ggplot(db, aes(x=factor(PopBinary), y=PiD)) +
  geom_boxplot() 

# Voting for populists by degree of nationalism

ggplot(db, aes(x=factor(PopBinary), y=NationalID)) +
  geom_boxplot() 

################################################################
# HERE A TEST MISSING DUMMY x CONTINUOUS
################################################################

ggplot(db, aes(x=SumPop, y=NationalID)) +
  geom_jitter() + geom_smooth() 

################################################################
# HERE A TEST MISSING CONTINUOUS x CONTINUOUS
################################################################

# Voting for populists by age

ggplot(db, aes(x=factor(PopBinary), y=Age)) +
  geom_boxplot() 

################################################################
# HERE A TEST MISSING DUMMY x CONTINUOUS
################################################################

ggplot(db, aes(x=SumPop, y=Age)) +
  geom_jitter() + geom_smooth() 

################################################################
# HERE A TEST MISSING CONTINUOUS x CONTINUOUS
################################################################

# Voting for populists by employment status

ggplot(db, aes(x=factor(PopBinary), fill=EmplStatus)) +
  geom_bar(aes(group=factor(EmplStatus), y = ..prop..), stat = "count", position = "dodge")

chisq.test(db$PopBinary, db$EmplStatus) # X-squared = 13.352, df = 8, p-value = 0.1003  ==> p>0.05, so we can't reject the null hypothesis
anova <- aov(data = db, PopBinary ~ EmplStatus)
summary(anova) # F-statistic is 1.73 and p-value 0.0979 -- significant at 90% confidence level

# Did the treatment influence the feeling of national ID? Check mean national ID by treatment

ggplot(db, aes(x=factor(Treatment), y=NationalID)) +
  geom_boxplot() 

chisq.test(db$Treatment, db$NationalID) #X-squared = 39.108, df = 40, p-value = 0.5103

################################################################
# HERE A TEST MISSING CATEGORICAL x CONTINUOUS
################################################################

ggplot(db, aes(x=factor(Treatment), y=LeftRight)) +
  geom_boxplot() 

chisq.test(db$Treatment, db$LeftRight) # X-squared = 18.907, df = 14, p-value = 0.1685

################################################################
# HERE A TEST MISSING CATEGORICAL x CONTINUOUS
################################################################


