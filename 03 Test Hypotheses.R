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
library(mice) # to uncover the pattern of missing values
library(car) # to perform multicollinearity tests
library(VIM)
library(mi)
library(stargazer)
library(Hmisc)
library(lfe)
library(survey)

# Make sure R reads Polish characters
Sys.setlocale("LC_CTYPE", "pl_PL")

###########################################################################

# INFERENTIAL ANALYSIS

###########################################################################

# Load data

db <- read_excel("Thesis5.xlsx", sheet = 1)

###########################################################################

# Multiple imputation of missing values for maximum number of degrees of freedom in later analysis

names(which(sapply(db, anyNA)))
mice::md.pattern(db)
Hmisc::describe(db$PiD)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(db,2,pMiss)

# Check percentage of missing values in the columns
names(which(sapply(db, anyNA)))
Hmisc::describe(db$YouthUnemp2016)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(db,2,pMiss)
mice::md.pattern(db)
aggr_plot <- VIM::aggr(db, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                       labels=names(data), cex.axis=.7, gap=3, 
                       ylab=c("Histogram of missing data","Pattern"))
# 65% of obs are complete; 16% missing values in PiD, 9% in Party2015 and 5% in LeftRight

# Test for MAR of LeftRight and Age (younger people might not have political opinions), Education, and Voted, and PolInt

missData <- db$LeftRight
missData <- as.data.frame(missData)
missData$miss <- ifelse(missData$missData == db$LeftRight, 1, 0)
missData$age <- db$Age
missData$education <- db$Education
missData$polint <- db$PolInt
missData$voted <- db$Voted
summary(glm(miss ~ age, data=missData, family=binomial)) # Stat insignificant
summary(glm(miss ~ education, data=missData, family=binomial)) # Stat insignificant
summary(glm(miss ~ polint, data=missData, family=binomial)) # Stat insignificant
summary(glm(miss ~ voted, data=missData, family=binomial)) # Stat insignificant

# Proceed to multiple imputation of LeftRight variable
methods(mice)
db$ID <- NULL
tempData <- mice(db,m=5,maxit=50,meth='pmm')
summary(tempData)
tempData$imp$LeftRight
completeData <- mice::complete(tempData,1)

db2$LeftRight <- completeData$LeftRight # add imputed values to the data set where 'ID' variable wasn't removed from
db2$LTUnemp201615 <- completeData$LTUnemp201615
db2$YouthUnemp201615 <- completeData$YouthUnemp201615
db2$YouthUnemp2016 <- completeData$YouthUnemp2016

openxlsx::write.xlsx(db2, 'Thesis3.xlsx', showNA=TRUE)

###########################################################################

db <- read_excel("Thesis5.xlsx", sheet = 1)

###########################################################################

summary(db)

# Transform variable 'Education' so that it approximates the normal distribution (this makes theoretical sense too, because levels 4 & 5 correspond to the same number of years of education)
db$Education[db$Education == 5] <- 4
db$Education[db$Education == 6] <- 5
db$Education[db$Education == 7] <- 6
db$Education[db$Education == 8] <- 7

# HYPOTHESIS 1: Polish voters exposed to foreign media calling them to choose a particular candidate, are more prone to choose the opposite (populist)

# OLS

db$Treatment <- relevel(as.factor(db$Treatment), "control")

ols1 <- lm(data=db, formula = SumPop ~ Treatment, weights = weights_vot)
ols2 <- lm(SumPop ~ Treatment + Sex + Age + Education + Income +
                    PolInt + NationalID + LeftRight + Province, db, weights = weights_vot)
ols3 <- lm(SumPop ~ Treatment + Sex + Age + Education + Income + 
                    PolInt + NationalID + LeftRight + PiD + Province + Party2015, db, weights = weights_vot)
ols4 <- lm(SumPop ~ Treatment + Sex + Age + Education + Income +
                    PolInt + NationalID + LeftRight + PiD + Treatment:NationalID + Province + Party2015, db,
           weights = weights_vot)
ols5 <- lm(SumPop ~ Treatment + Sex + Age + Education + Income +
             PolInt + NationalID + LeftRight + PiD + Treatment:Income + Province + Party2015, db, weights = weights_vot)
ols6 <- lm(SumPop ~ Treatment + Sex + Age + Education + Income +
             PolInt + NationalID + LeftRight + PiD + Treatment:Education + Province + Party2015, db, weights = weights_vot)
ols7 <- lm(SumPop ~ Treatment + Sex + Age + Education + Income +
             PolInt + NationalID + LeftRight + PiD + Treatment:LeftRight + Province + Party2015, db, weights = weights_vot)

summary(ols6)

############################################

# Other regressions checked, not included in the final version of the thesis, because they were not theoretically motivated

ols7 <- lfe::felm(SumPop ~ relevel(as.factor(Treatment), 'control') + Sex + log(Age) + Education + Income +
                    PolInt + NationalID + LeftRight + relevel(as.factor(Treatment), 'control'):LeftRight | Province + Party2015, db)
summary(ols7, robust = TRUE)

ols8 <- lfe::felm(SumPop ~ relevel(as.factor(Treatment), 'control') + Sex + log(Age) + Education + Income + PiD +
                    PolInt + NationalID + LeftRight + relevel(as.factor(Treatment), 'control'):Sex | Province + Party2015, db)
summary(ols8, robust = TRUE)

ols9 <- lfe::felm(SumPop ~ relevel(as.factor(Treatment), 'control') + Sex + log(Age) + Education + Income + PiD +
                    PolInt + NationalID + LeftRight + relevel(as.factor(Treatment), 'control'):log(Age) | Province + Party2015, db)
summary(ols9, robust = TRUE)

############################################

summary(ols1) # 130 Df

cov.ols1 <- sandwich::vcovHC(ols1, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err1 <- sqrt(diag(cov.ols1))
naive.std.err1 <- summary(ols1)$coefficients[,2]

cov.ols2 <- sandwich::vcovHC(ols2, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err2 <- sqrt(diag(cov.ols2))
naive.std.err2 <- summary(ols2)$coefficients[,2]

cov.ols3 <- sandwich::vcovHC(ols3, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err3 <- sqrt(diag(cov.ols3))
naive.std.err3 <- summary(ols3)$coefficients[,2]

cov.ols4 <- sandwich::vcovHC(ols4, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err4 <- sqrt(diag(cov.ols4))
naive.std.err4 <- summary(ols4)$coefficients[,2]

cov.ols5 <- sandwich::vcovHC(ols5, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err5 <- sqrt(diag(cov.ols5))
naive.std.err5 <- summary(ols5)$coefficients[,2]

cov.ols6 <- sandwich::vcovHC(ols6, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err6 <- sqrt(diag(cov.ols6))
naive.std.err6 <- summary(ols6)$coefficients[,2]

cov.ols7 <- sandwich::vcovHC(ols7, type = "HC") # Robust standard errors can be thus calculated only for lm package regressions
rob.std.err7 <- sqrt(diag(cov.ols7))
naive.std.err7 <- summary(ols7)$coefficients[,2]

stargazer(list(ols1,ols1,ols2,ols2,ols3,ols3,ols4,ols4,ols5,ols5,ols6,ols6,ols7,ols7), 
          se = list(naive.std.err1, rob.std.err1,naive.std.err2, rob.std.err2,naive.std.err3, rob.std.err3,naive.std.err4, rob.std.err4,naive.std.err5, rob.std.err5,naive.std.err6, rob.std.err6,naive.std.err7, rob.std.err7),
          column.labels=c("default","robust","default","robust","default","robust","default","robust","default","robust","default","robust","default","robust"),
          omit.stat=c("LL","ser","f"),
          type="html",
          out = "ols_pop.html")

stargazer(list(ols1,ols1,ols2,ols2,ols3,ols3,ols4,ols4,ols5,ols5,ols6,ols6,ols7,ols7), 
          se = list(naive.std.err1, rob.std.err1,naive.std.err2, rob.std.err2,naive.std.err3, rob.std.err3,naive.std.err4, rob.std.err4,naive.std.err5, rob.std.err5,naive.std.err6, rob.std.err6, naive.std.err7, rob.std.err7),
          column.labels=c("default","robust","default","robust","default","robust","default","robust","default","robust","default","robust","default","robust"),
          omit.stat=c("LL","ser","f"),
          type="html",
          out = "ols_vot.html")

summary(ols2, robust = TRUE) # 108 Df
summary(ols3, robust = TRUE) # 68 Df
summary(ols4, robust = TRUE) # 106 Df

############################################

# Check for heteroscedasticity, normality and influential observations
layout(matrix(c(1,2,3,4),2,2))
plot()

outlierTest(fit)

alias(ols2) # adding Voted variable to the model results in perfect multicollinearity with a few other variables
car::vif(ols2)
car::vif(ols5)

waldtest(ols1, 
         R = "relevel(as.factor(Treatment), \"control\")A",
         type = c("robust"))

waldtest(ols2, 
         R = "relevel(as.factor(Treatment), \"control\")A",
         type = c("robust"))

waldtest(ols3, 
         R = "relevel(as.factor(Treatment), \"control\")A",
         type = c("robust"))

waldtest(ols4, 
         R = "relevel(as.factor(Treatment), \"control\")A",
         type = c("robust"))

############################################

# Create final regression output

stargazer(list(ols1, ols2, ols3, ols4),
          se = list(rob.std.err1),
          column.labels=c("Simple","Province-FE","Party2015","Treatment:NationalID"),
          covariate.labels=c("Treatment A","Treatment B","Female","log(Age)","Education level","Income level","Level of interest in politics","Degree of national ID","Left-right placement","Party identification","Treatment A: National ID","Treatment B: National ID","Constant"), 
          title="Table 1: The effects of the experiment on party choice",
          model.numbers=TRUE,
          dep.var.labels=c("Populist party choice (0-5)"),
          omit.stat=c("LL","ser","f"),
          #notes.append = TRUE,
          #notes = "The control group is the baseline excluded condition in the Treatment variable. Entries under model (1) are OLS coefficients with Huber???White robust standard errors in parentheses. Entries under models (2)-(4) are felm coefficients with normal standard errors in parentheses.",
          #no.space = TRUE,
          #single.row = TRUE,
          type="html",
          out = "ols.html")

############################################

# Plot the results

# Lm models are needed for plotting. The coefficients on variables are of the same size and stadard errors.

ols1 <- lm(data=db, formula = SumPop ~ relevel(as.factor(Treatment), 'control'))
ols2 <- lm(SumPop ~ relevel(as.factor(Treatment), 'control') + Sex + log(Age) + Education + Income +
                    PolInt + NationalID + LeftRight + Province, db)
ols3 <- lm(SumPop ~ relevel(as.factor(Treatment), 'control') + Sex + log(Age) + Education + Income + 
                    PolInt + NationalID + LeftRight + PiD + Province + Party2015, db)
ols4 <- lm(SumPop ~ relevel(as.factor(Treatment), 'control') + Sex + log(Age) + Education + Income +
                    PolInt + NationalID + LeftRight + PiD + relevel(as.factor(Treatment), 'control'):NationalID + Province + Party2015, db)


ols1_tidy <- broom::tidy(ols1, conf.int = TRUE, conf.level = 0.90)
ols1_tidy$term[ols1_tidy$term == "relevel(as.factor(Treatment), \"control\")A"] <- "Treatment A"
ols1_tidy$term[ols1_tidy$term == "relevel(as.factor(Treatment), \"control\")B"] <- "Treatment B"

kolor = rep('grey', nrow(ols1_tidy))
kolor[ols1_tidy$conf.low > 0 & ols1_tidy$conf.high > 0] = 'black'

p1 <- ggplot(ols1_tidy, mapping = aes(x = term,
                                      y = estimate,
                                      ymin = conf.low,
                                      ymax = conf.high)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_pointrange(color = kolor) +
  coord_flip() +
  labs(x="", y="OLS Estimate")
p1

ols2_tidy <- broom::tidy(ols2, conf.int = TRUE, conf.level = 0.90)
ols2_tidy$term[ols2_tidy$term == "relevel(as.factor(Treatment), \"control\")A"] <- "Treatment A"
ols2_tidy$term[ols2_tidy$term == "relevel(as.factor(Treatment), \"control\")B"] <- "Treatment B"
ols2_tidy <- ols2_tidy[-11:-25, ]
kolor = rep('grey', nrow(ols2_tidy))
kolor[ols2_tidy$conf.low > 0 & ols2_tidy$conf.high > 0 | ols2_tidy$conf.low < 0 & ols2_tidy$conf.high < 0] = 'black'
p2 <- ggplot(ols2_tidy, mapping = aes(x = reorder(term, -estimate),
                                      y = estimate,
                                      ymin = conf.low,
                                      ymax = conf.high)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_pointrange(color = kolor) +
  coord_flip() +
  labs(title ="OLS estimates by variable, in ascending order",
       subtitle = "Modelled on results from OLS model (2)",
       x = "", 
       y = "",
       caption = expression(atop("Method: OLS with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.")))
p2

ols3_tidy <- broom::tidy(ols3, conf.int = TRUE, conf.level = 0.90)
ols3_tidy$term[ols3_tidy$term == "relevel(as.factor(Treatment), \"control\")A"] <- "Treatment A"
ols3_tidy$term[ols3_tidy$term == "relevel(as.factor(Treatment), \"control\")B"] <- "Treatment B"
ols3_tidy <- ols3_tidy %>%
  mutate(term = gsub("Party2015", "", term))
ols3_tidy <- ols3_tidy %>%
  mutate(term = gsub("Province", "", term))
ols3_tidy <- ols3_tidy[-12:-26, ]

kolor = rep('grey', nrow(ols3_tidy))
kolor[ols3_tidy$conf.low > 0 & ols3_tidy$conf.high > 0 | ols3_tidy$conf.low < 0 & ols3_tidy$conf.high < 0] = 'black'

p3 <- ggplot(ols3_tidy, mapping = aes(x = reorder(term, -estimate),
                                      y = estimate,
                                      ymin = conf.low,
                                      ymax = conf.high)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_pointrange(color = kolor) +
  coord_flip() +
  labs(title ="OLS estimates by variable, in ascending order",
       subtitle = "Modelled on results from OLS model (3)",
       x = "", 
       y = "",
       caption = expression(atop("Method: OLS with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.")))
p3

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

min_ID <- min(db$NationalID)
max_ID <- max(db$NationalID)

pred_ols4 <- expand.grid(NationalID = (seq(from = min_ID,
                                           to = max_ID)),
                         Treatment = c("A", "B", "control"),
                         Income = median(db$Income),
                         Education = median(db$Education),
                         Sex = median(db$Sex),
                         PolInt = median(db$PolInt),
                         LeftRight = median(db$LeftRight),
                         Voted = 1,
                         PiD = median(db$PiD, na.rm = TRUE),
                         Age = median(db$Age),
                         Province = getmode(db$Province),
                         Party2015 = getmode(db$Party2015))

pred_outols4 <- predict(object = ols4, 
                        newdata = pred_ols4,
                        # interval = 'prediction',
                        level=.90,
                        type = 'response',
                        se.fit = TRUE)

pred_outols4$upr = pred_outols4$fit + 1.96 * pred_outols4$se.fit
pred_outols4$lwr = pred_outols4$fit - 1.96 * pred_outols4$se.fit

pred_fullols4 <- cbind(pred_ols4, pred_outols4)

p4 <- ggplot(data = pred_fullols4,
             aes(x = NationalID, y = fit, ymin = lwr, ymax = upr,
                 color = Treatment, fill = Treatment, group = Treatment)) +
  geom_point(data = pred_fullols4, 
             aes(x = NationalID, y = fit,
                 shape = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_line() +
  labs(title ="Plot of interaction between Treatment and NationalID",
       subtitle = "Modelled on results from OLS model (4)",
       x = "Degree of national identification", 
       y = "Predicted probability of voting for a populist party",
       caption = expression(atop("Method: OLS with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election."))) +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw()

p4

min_LeftRight <- min(db$LeftRight)
max_LeftRight <- max(db$LeftRight)

pred_ols5 <- expand.grid(LeftRight = (seq(from = min_LeftRight,
                                           to = max_LeftRight)),
                         Treatment = c("A", "B", "control"),
                         Income = median(db$Income),
                         Education = median(db$Education),
                         Sex = median(db$Sex),
                         PolInt = median(db$PolInt),
                         Voted = 1,
                         PiD = median(db$PiD, na.rm = TRUE),
                         Age = median(db$Age),
                         NationalID = median(db$NationalID),
                         Province = getmode(db$Province),
                         Party2015 = getmode(db$Party2015))

pred_outols5 <- predict(object = ols5, 
                        newdata = pred_ols5,
                        # interval = 'prediction',
                        # level=.95,
                        type = 'response',
                        se.fit = TRUE)

pred_outols5$upr = pred_outols5$fit + 1.96 * pred_outols5$se.fit
pred_outols5$lwr = pred_outols5$fit - 1.96 * pred_outols5$se.fit

pred_fullols5 <- cbind(pred_ols5, pred_outols5)

p5 <- ggplot(data = pred_fullols5,
             aes(x = LeftRight, y = fit, ymin = lwr, ymax = upr,
                 color = Treatment, fill = Treatment, group = Treatment)) +
  geom_point(data = pred_fullols5, 
             aes(x = LeftRight, y = fit,
                 shape = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw()

p5

##############################################################

# Logit

log1 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control"), 
            family=binomial(),
            weights = weights)
log2 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") + 
              Sex + Age + Education + Income + PolInt + NationalID + LeftRight + Voted + Province, 
            family=binomial(),
            weights = weights)
log3 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") +
              Sex + Age + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015, 
            family=binomial(),
            weights = weights)
summary(log1)
cov.log1 <- sandwich::vcovHC(log1, type = "HC")
rob.std.err1 <- sqrt(diag(cov.log1))
naive.std.err1 <- summary(log1)$coefficients[,2]

cov.log2 <- sandwich::vcovHC(log2, type = "HC")
rob.std.err2 <- sqrt(diag(cov.log2))
naive.std.err2 <- summary(log2)$coefficients[,2]

cov.log3 <- sandwich::vcovHC(log3, type = "HC")
rob.std.err3 <- sqrt(diag(cov.log3))
naive.std.err3 <- summary(log3)$coefficients[,2]

stargazer(list(log1, log2, log3),
          title="Table 2: The effects of the experiment on party choice",
          se = list(rob.std.err1,rob.std.err2,rob.std.err3),
          column.labels=c("Simple","Province FE","Party2015"),
          covariate.labels=c("Treatment A","Treatment B","Female","log(Age)","Education level","Income level","Level of interest in politics","Degree of national ID","Left-right placement","Dummy for vote in 2015","Party identification","Constant"), 
          model.numbers=TRUE,
          dep.var.labels=c("Populist party choice (0-1)"),
          omit.stat=c("LL","ser","f"),
          omit=c("Party2015KORWiN","Party2015Kukiz'15","Party2015Lewica","Party2015Nowoczesna","Party2015PO","Party2015PSL","Party2015PiS","Party2015Razem","ProvinceKP","ProvinceLB","ProvinceLS","ProvinceMP","ProvinceMZ","ProvinceOP","ProvincePK","ProvincePL","ProvincePM","ProvinceWM","ProvinceWP","ProvinceZP","Province??D","Province??K","Province??L"),
          #notes.append = TRUE,
          #notes = "The control group is the baseline excluded condition in the Treatment variable. Entries are logit coefficients with Huber???White robust standard errors in parentheses.",
          no.space = TRUE,
          single.row = TRUE,
          type="html",
          out = "log.html")

# Interaction with level of education

log4 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") +
              Sex + log(Age) + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              relevel(as.factor(Treatment), "control"):Education, 
            family=binomial())
summary(log4)

# Interaction with national ID

log5 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") +
              Sex + log(Age) + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              relevel(as.factor(Treatment), "control"):NationalID, 
            family=binomial())
summary(log5)

# Interaction with income

log6 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") +
              Sex + log(Age) + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              relevel(as.factor(Treatment), "control"):Income, 
            family=binomial())
summary(log6)

# Interaction with Sex

log7 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") +
              Sex + log(Age) + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              relevel(as.factor(Treatment), "control"):Sex, 
            family=binomial())
summary(log7)

# Interaction with PiD (all is insignificant)

log8 <- glm(data=db, formula = PopBinary ~ relevel(as.factor(Treatment), "control") +
              Sex + log(Age) + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              relevel(as.factor(Treatment), "control"):PiD, 
            family=binomial())
summary(log8)

##############################################################

# PLOT PREDICTED PROBABILITIES

##############################################################

# Plot predicted probabilities for Treatments log 2 & 3
pred_dfT <- expand.grid(Treatment = c("A", "B", "control"),
                        Income = median(db$Income),
                        Education = median(db$Education),
                        Sex = median(db$Sex),
                        PolInt = median(db$PolInt),
                        LeftRight = median(db$LeftRight),
                        Voted = 1,
                        PiD = median(db$PiD, na.rm = TRUE),
                        NationalID = median(db$NationalID),
                        Age = median(db$Age),
                        Province = getmode(db$Province),
                        Party2015 = getmode(db$Party2015)
                        )

pred_outT2 <- predict(object = log2,
                        newdata = pred_dfT,
                        # interval = 'prediction',
                        level=.9,
                        type = 'response',
                        se.fit = TRUE)

pred_outT2$upr = pred_outT2$fit + 1.96 * pred_outT2$se.fit
pred_outT2$lwr = pred_outT2$fit - 1.96 * pred_outT2$se.fit

pred_fullT2 <- cbind(pred_dfT, pred_outT2)

TOnVote2 <- ggplot(data = pred_fullT2,
                     aes(x = Treatment, y = fit, ymin = lwr, ymax = upr)) +
  geom_point(data = subset(pred_fullT2, Treatment %in% c("A", "B", "control")), 
             aes(x = Treatment, y = fit), 
             alpha = 0.5,
             inherit.aes = FALSE,
             size = 4,
             fill = "black") + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), 
                lwd=1, width=0.2, lty=1) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  labs(title ="Plot of predicted probabilities of populist vote choice by treatment",
       subtitle = "Modelled on results from logit model (2)",
       x = "Treatment", 
       y = "Predicted probability of voting for a populist party") +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw()

TOnVote2

pred_outT3 <- predict(object = log3,
                      newdata = pred_dfT,
                      # interval = 'prediction',
                      level=.9,
                      type = 'response',
                      se.fit = TRUE)

pred_outT3$upr = pred_outT3$fit + 1.96 * pred_outT3$se.fit
pred_outT3$lwr = pred_outT3$fit - 1.96 * pred_outT3$se.fit

pred_fullT3 <- cbind(pred_dfT, pred_outT3)

TOnVote3 <- ggplot(data = pred_fullT3,
                   aes(x = Treatment, y = fit, ymin = lwr, ymax = upr)) +
  geom_point(data = subset(pred_fullT3, Treatment %in% c("A", "B", "control")), 
             aes(x = Treatment, y = fit), 
             alpha = 0.5,
             inherit.aes = FALSE,
             size = 4,
             fill = "black") + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), 
                lwd=1, width=0.2, lty=1) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  labs(title ="Plot of predicted probabilities of populist vote choice by treatment",
       subtitle = "Modelled on results from logit model (3)",
       x = "Treatment", 
       y = "Predicted probability of voting for a populist party") +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw()

TOnVote3

# Plot predicted probabilities for Income (log6)

min_income <- min(db$Income)
max_income <- max(db$Income)

pred_dfI <- expand.grid(Income =
                         (seq(from = min_income,
                              to = max_income)),
                       Treatment = c("A", "B", "control"),
                       Education = median(db$Education),
                       Sex = 0,
                       PolInt = median(db$PolInt),
                       LeftRight = median(db$LeftRight),
                       Voted = 1,
                       PiD = median(db$PiD, na.rm = TRUE),
                       NationalID = median(db$NationalID),
                       Age = median(db$Age),
                       Party2015 = getmode(db$Party2015),
                       Province = getmode(db$Province))

pred_outIbyT <- predict(object = log2, 
                        newdata = pred_dfI,
                        # interval = 'prediction',
                        # level=.95,
                        type = 'response',
                        se.fit = TRUE)

pred_outIbyT$upr = pred_outIbyT$fit + 1.96 * pred_outIbyT$se.fit
pred_outIbyT$lwr = pred_outIbyT$fit - 1.96 * pred_outIbyT$se.fit

pred_fullIbyT <- cbind(pred_dfI, pred_outIbyT)

IOnVoteByT <- ggplot(data = subset(pred_fullIbyT, 
                                   Treatment %in% c("A", "B", "control")),
                     aes(x = Income, y = fit, ymin = lwr, ymax = upr,
                         color = Treatment, fill = Treatment,
                         group = Treatment)) +
  geom_point(data = subset(pred_fullIbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = Income, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE)

IOnVoteByT

pred_outIxT <- predict(object = log8, 
                    newdata = pred_dfI,
                    # interval = 'prediction',
                    # level=.95,
                    type = 'response',
                    se.fit = TRUE)

pred_outIxT$upr = pred_outIxT$fit + 1.96 * pred_outIxT$se.fit
pred_outIxT$lwr = pred_outIxT$fit - 1.96 * pred_outIxT$se.fit

pred_fullIxT <- cbind(pred_dfI, pred_outIxT)

IxTOnVote <- ggplot(data = subset(pred_fullIxT, 
                                Treatment %in% c("A", "B", "control")),
                  aes(x = Income, y = fit, ymin = lwr, ymax = upr,
                      color = Treatment, fill = Treatment,
                      group = Treatment)) +
  geom_point(data = subset(pred_fullIxT, Treatment %in% c("A", "B", "control")), 
             aes(x = Income, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE)

IxTOnVote

# ! Plot predicted probabilities for National ID (logs 5)
min_ID <- min(db$NationalID)
max_ID <- max(db$NationalID)

pred_dfID <- expand.grid(NationalID =
                          (seq(from = min_ID,
                               to = max_ID)),
                        Treatment = c("A", "B", "control"),
                        Income = median(db$Income),
                        Education = median(db$Education),
                        Sex = 0,
                        PolInt = median(db$PolInt),
                        LeftRight = median(db$LeftRight),
                        Voted = 1,
                        PiD = median(db$PiD, na.rm=TRUE),
                        Age = median(db$Age),
                        Party2015 = getmode(db$Party2015),
                        Province = getmode(db$Province))

pred_outIDbyT <- predict(object = log3, 
                        newdata = pred_dfID,
                        # interval = 'prediction',
                        level=.90,
                        type = 'response',
                        se.fit = TRUE)

pred_outIDbyT$upr = pred_outIDbyT$fit + 1.96 * pred_outIDbyT$se.fit
pred_outIDbyT$lwr = pred_outIDbyT$fit - 1.96 * pred_outIDbyT$se.fit

pred_fullIDbyT <- cbind(pred_dfID, pred_outIDbyT)

IDOnVoteByT <- ggplot(data = pred_fullIDbyT,
                      aes(x = NationalID, y = fit, ymin = lwr, ymax = upr,
                         color = Treatment, fill = Treatment,
                         group = Treatment)) +
  geom_point(data = subset(pred_fullIDbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = NationalID, y = fit,
                 color = Treatment,
                 shape = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw() +
  labs(title ="Effect of increasing national ID on populist choice by treatment",
       subtitle = "Modelled on results from logit model (3)",
       x = "Degree of national identification", 
       y = "Predicted probability of voting for a populist party",
       caption = "Method: Logit with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.")

IDOnVoteByT

pred_outIDxT <- predict(object = log5, 
                       newdata = pred_dfID,
                       # interval = 'prediction',
                       level=.90,
                       type = 'response',
                       se.fit = TRUE)

pred_outIDxT$upr = pred_outIDxT$fit + 1.96 * pred_outIDxT$se.fit
pred_outIDxT$lwr = pred_outIDxT$fit - 1.96 * pred_outIDxT$se.fit

pred_fullIDxT <- cbind(pred_dfID, pred_outIDxT)

IDxTOnVote <- ggplot(data = pred_fullIDxT,
                    aes(x = NationalID, y = fit, ymin = lwr, ymax = upr,
                        color = Treatment, fill = Treatment,
                        group = Treatment)) +
  geom_point(data = subset(pred_fullIDxT, Treatment %in% c("A", "B", "control")), 
             aes(x = NationalID, y = fit,
                 color = Treatment,
                 shape = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw() +
  labs(title ="Effect of interaction national ID:treatment",
       subtitle = "Modelled on results from logit model (5)",
       x = "Degree of national identification", 
       y = "Predicted probability of voting for a populist party",
       caption = "Method: Logit with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.")


IDxTOnVote # INTERESTING FINDING!

# Plot predicted probabilities for PiD (logs 15 & 16)

min_PiD <- min(db$PiD, na.rm = TRUE)
max_PiD <- max(db$PiD, na.rm = TRUE)
pred_dfPiD <- expand.grid(PiD =
                           (seq(from = min_PiD,
                                to = max_PiD)),
                         Treatment = c("A", "B", "control"),
                         Income = median(db$Income),
                         Education = median(db$Education),
                         Sex = 0,
                         PolInt = median(db$PolInt),
                         LeftRight = median(db$LeftRight),
                         Voted = 1,
                         Age = median(db$Age),
                         NationalID = median(db$NationalID),
                         Party2015 = getmode(db$Party2015),
                         Province = getmode(db$Province),
                         NationalID = median(db$NationalID))

pred_outPiDbyT <- predict(object = log3, 
                         newdata = pred_dfPiD,
                         # interval = 'prediction',
                         # level=.95,
                         type = 'response',
                         se.fit = TRUE)

pred_outPiDbyT$upr = pred_outPiDbyT$fit + 1.96 * pred_outPiDbyT$se.fit
pred_outPiDbyT$lwr = pred_outPiDbyT$fit - 1.96 * pred_outPiDbyT$se.fit

pred_fullPiDbyT <- cbind(pred_dfPiD, pred_outPiDbyT)

PiDOnVoteByT <- ggplot(data = subset(pred_fullPiDbyT, 
                                    Treatment %in% c("A", "B", "control")),
                      aes(x = PiD, y = fit, ymin = lwr, ymax = upr,
                          color = Treatment, fill = Treatment,
                          group = Treatment)) +
  geom_point(data = subset(pred_fullPiDbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = PiD, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE)

PiDOnVoteByT

pred_outPiDxT <- predict(object = log8, 
                        newdata = pred_dfPiD,
                        # interval = 'prediction',
                        level=.90,
                        type = 'response',
                        se.fit = TRUE)

pred_outPiDxT$upr = pred_outPiDxT$fit + 1.96 * pred_outPiDxT$se.fit
pred_outPiDxT$lwr = pred_outPiDxT$fit - 1.96 * pred_outPiDxT$se.fit

pred_fullPiDxT <- cbind(pred_dfPiD, pred_outPiDxT)

PiDxTOnVote <- ggplot(data = subset(pred_fullPiDxT, 
                                   Treatment %in% c("A", "B", "control")),
                     aes(x = PiD, y = fit, ymin = lwr, ymax = upr,
                         color = Treatment, fill = Treatment,
                         group = Treatment)) +
  geom_point(data = subset(pred_fullPiDxT, Treatment %in% c("A", "B", "control")), 
             aes(x = PiD, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE)

PiDxTOnVote # potentially OK, but statistically insignificant

# Plot predicted probabilities for Education
min_edu <- min(db$Education)
max_edu <- max(db$Education)
pred_dfE <- expand.grid(Education =
                          (seq(from = min_edu,
                               to = max_edu)),
                        Treatment = c("A", "B", "control"),
                        Income = median(db$Income),
                        Sex = 0,
                        PolInt = median(db$PolInt),
                        LeftRight = median(db$LeftRight),
                        Voted = 1,
                        PiD = median(db$PiD, na.rm = TRUE),
                        NationalID = median(db$NationalID),
                        Age = median(db$Age),
                        Party2015 = getmode(db$Party2015),
                        Province = getmode(db$Province))
pred_outEbyT <- predict(object = log3, 
                     newdata = pred_dfE,
                     # interval = 'prediction',
                     # level=.95,
                     type = 'response',
                     se.fit = TRUE)
pred_outEbyT$upr = pred_outEbyT$fit + 1.96 * pred_outEbyT$se.fit
pred_outEbyT$lwr = pred_outEbyT$fit - 1.96 * pred_outEbyT$se.fit
pred_fullEbyT <- cbind(pred_dfE, pred_outEbyT)
EOnVotebyT <- ggplot(data = subset(pred_fullEbyT, 
                                                   Treatment %in% c("A", "B", "control")),
                                     aes(x = Education, y = fit, ymin = lwr, ymax = upr,
                                         color = Treatment, fill = Treatment,
                                         group = Treatment)) +
  geom_point(data = subset(pred_fullEbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = Education, y = fit,
                 color = Treatment,
                 shape = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw() +
  labs(title ="Effect of education by treatment",
       subtitle = "Modelled on results from logit model (3)",
       x = "Level of formal education", 
       y = "Predicted probability of voting for a populist party",
       caption = "Method: Logit with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.")

EOnVotebyT

pred_outExT <- predict(object = log4, 
                    newdata = pred_dfE,
                    # interval = 'prediction',
                    level=.90,
                    type = 'response',
                    se.fit = TRUE)
pred_outExT$upr = pred_outExT$fit + 1.96 * pred_outExT$se.fit
pred_outExT$lwr = pred_outExT$fit - 1.96 * pred_outExT$se.fit
pred_fullExT <- cbind(pred_dfE, pred_outExT)

ExTOnVote <- ggplot(data = subset(pred_fullExT, 
                                  Treatment %in% c("A", "B", "control")),
                                  aes(x = Education, y = fit, ymin = lwr, ymax = upr,
                                      color = Treatment, fill = Treatment,
                                      group = Treatment)) +
  geom_point(data = subset(pred_fullExT, Treatment %in% c("A", "B", "control")), 
             aes(x = Education, y = fit,
                 color = Treatment,
                 shape = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw() +
  labs(title ="Effect of interaction education:treatment",
       subtitle = "Modelled on results from logit model (4)",
       x = "Level of formal education", 
       y = "Predicted probability of voting for a populist party",
       caption = "Method: Logit with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.")

ExTOnVote

# Plot predicted probabilities for Sex
min_sex <- min(db$Sex)
max_sex <- max(db$Sex)
pred_dfS <- expand.grid(Sex =
                          (seq(from = min_sex,
                               to = max_sex)),
                        Treatment = c("A", "B", "control"),
                        Income = median(db$Income),
                        Education = median(db$Education),
                        PolInt = median(db$PolInt),
                        LeftRight = median(db$LeftRight),
                        Voted = 1,
                        PiD = median(db$PiD, na.rm = TRUE),
                        NationalID = median(db$NationalID),
                        Age = median(db$Age),
                        Party2015 = getmode(db$Party2015),
                        Province = getmode(db$Province))
pred_outSbyT <- predict(object = log3, 
                        newdata = pred_dfS,
                        # interval = 'prediction',
                        level=.90,
                        type = 'response',
                        se.fit = TRUE)
pred_outSbyT$upr = pred_outSbyT$fit + 1.96 * pred_outSbyT$se.fit
pred_outSbyT$lwr = pred_outSbyT$fit - 1.96 * pred_outSbyT$se.fit
pred_fullSbyT <- cbind(pred_dfS, pred_outSbyT)
SOnVotebyT <- ggplot(data = subset(pred_fullSbyT, 
                                   Treatment %in% c("A", "B", "control")),
                     aes(x = Sex, y = fit, ymin = lwr, ymax = upr,
                         color = Treatment, fill = Treatment,
                         group = Treatment)) +
  geom_point(data = subset(pred_fullSbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = Sex, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE)

SOnVotebyT

pred_outSxT <- predict(object = log7, 
                        newdata = pred_dfS,
                        # interval = 'prediction',
                        level=.90,
                        type = 'response',
                        se.fit = TRUE)
pred_outSxT$upr = pred_outSxT$fit + 1.96 * pred_outSxT$se.fit
pred_outSxT$lwr = pred_outSxT$fit - 1.96 * pred_outSxT$se.fit
pred_fullSxT <- cbind(pred_dfS, pred_outSxT)
SOnVoteSxT <- ggplot(data = subset(pred_fullSxT, 
                                   Treatment %in% c("A", "B", "control")),
                     aes(x = Sex, y = fit, ymin = lwr, ymax = upr,
                         color = Treatment, fill = Treatment,
                         group = Treatment)) +
  geom_point(data = subset(pred_fullSxT, Treatment %in% c("A", "B", "control")), 
             aes(x = Sex, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) + 
  scale_x_discrete() +
  geom_errorbar()

SOnVoteSxT # Women potentially less prone to vote for pops (but stat insignificant)

###########################################################

# (Don't show) Plot predicted probabilities for Party chosen in 2015 (logs 9 & 10)

pred_dfPar <- expand.grid(Party2015 = unique(db$Party2015),
                          Treatment = c("A", "B", "control"),
                          Income = median(db$Income),
                          Education = mean(db$Education),
                          PolInt = median(db$PolInt),
                          LeftRight = median(db$LeftRight),
                          Sex = median(db$Sex),
                          Voted = 1,
                          PiD = median(db$PiD, na.rm=TRUE),
                          NationalID = median(db$NationalID),
                          Age = median(db$Age),
                          Province = "MZ")
pred_outParbyT <- predict(object = log2, 
                          newdata = pred_dfPar,
                          # interval = 'prediction',
                          # level=.95,
                          type = 'response',
                          se.fit = TRUE)
pred_outParbyT$upr = pred_outParbyT$fit + 1.96 * pred_outParbyT$se.fit
pred_outParbyT$lwr = pred_outParbyT$fit - 1.96 * pred_outParbyT$se.fit
pred_fullParbyT <- cbind(pred_dfPar, pred_outParbyT)
ParOnVotebyT <- ggplot(data = subset(pred_fullParbyT, 
                                     Treatment %in% c("A", "B", "control")),
                       aes(x = Party2015, y = fit, ymin = lwr, ymax = upr,
                           color = Treatment, fill = Treatment,
                           group = Treatment)) +
  geom_point(data = subset(pred_fullParbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = Party2015, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_errorbar()
ParOnVotebyT

pred_outParxT <- predict(object = log10, 
                         newdata = pred_dfPar,
                         # interval = 'prediction',
                         # level=.95,
                         type = 'response',
                         se.fit = TRUE)
pred_outParxT$upr = pred_outParxT$fit + 1.96 * pred_outParxT$se.fit
pred_outParxT$lwr = pred_outParxT$fit - 1.96 * pred_outParxT$se.fit
pred_fullParxT <- cbind(pred_dfPar, pred_outParxT)
ParxTOnVote <- ggplot(data = subset(pred_fullParxT, 
                                    Treatment %in% c("A", "B", "control")),
                      aes(x = Party2015, y = fit, ymin = lwr, ymax = upr,
                          color = Treatment, fill = Treatment,
                          group = Treatment)) +
  geom_point(data = subset(pred_fullParxT, Treatment %in% c("A", "B", "control")), 
             aes(x = Party2015, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_errorbar()
ParxTOnVote

# (Don't show) Plot predicted probabilities for Province (logs 13 & 14)

pred_dfP <- expand.grid(Province = unique(db$Province),
                        Treatment = c("A", "B", "control"),
                        Income = median(db$Income),
                        Education = median(db$Education),
                        PolInt = median(db$PolInt),
                        LeftRight = median(db$LeftRight),
                        Voted = 1,
                        PiD = median(db$PiD, na.rm = TRUE),
                        NationalID = median(db$NationalID),
                        Age = median(db$Age),
                        Party2015 = getmode(db$Party2015),
                        Sex = median(db$Sex))
pred_outPbyT <- predict(object = log2, 
                        newdata = pred_dfP,
                        # interval = 'prediction',
                        # level=.95,
                        type = 'response',
                        se.fit = TRUE)
pred_outPbyT$upr = pred_outPbyT$fit + 1.96 * pred_outPbyT$se.fit
pred_outPbyT$lwr = pred_outPbyT$fit - 1.96 * pred_outPbyT$se.fit
pred_fullPbyT <- cbind(pred_dfP, pred_outPbyT)
POnVotebyT <- ggplot(data = subset(pred_fullPbyT, 
                                   Treatment %in% c("A", "B", "control")),
                     aes(x = Province, y = fit, ymin = lwr, ymax = upr,
                         color = Treatment, fill = Treatment,
                         group = Treatment)) +
  geom_point(data = subset(pred_fullPbyT, Treatment %in% c("A", "B", "control")), 
             aes(x = Province, y = fit, ymin = lwr, ymax = upr,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), lwd=1, width=0.2, lty=1, alpha = 0.5)

POnVotebyT

pred_outPxT <- predict(object = log12, 
                       newdata = pred_dfP,
                       # interval = 'prediction',
                       # level=.95,
                       type = 'response',
                       se.fit = TRUE)
pred_outPxT$upr = pred_outPxT$fit + 1.96 * pred_outPxT$se.fit
pred_outPxT$lwr = pred_outPxT$fit - 1.96 * pred_outPxT$se.fit
pred_fullPxT <- cbind(pred_dfP, pred_outPxT)
PxTOnVote <- ggplot(data = subset(pred_fullPxT, 
                                  Treatment %in% c("A", "B", "control")),
                    aes(x = Province, y = fit, ymin = lwr, ymax = upr,
                        color = Treatment, fill = Treatment,
                        group = Treatment)) +
  geom_point(data = subset(pred_fullPxT, Treatment %in% c("A", "B", "control")), 
             aes(x = Province, y = fit,
                 color = Treatment), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), lwd=1, width=0.2, lty=1, alpha = 0.5)
PxTOnVote

########################### MARGINAL EFFECTS PLOTS

# Marginal effects plots for significant interactions

emp <- c(min(fulldf$ftptemp, na.rm = TRUE),
         median(fulldf$ftptemp, na.rm = TRUE),
         max(fulldf$ftptemp, na.rm = TRUE))

# Create a data frame to plot

dat <- tot_asyl_int # Model 
coefs <- c(dat$coefficients["asylumseeker"] + dat$coefficients["asylumseeker:ftptemp"]*emp[1], 
           dat$coefficients["asylumseeker"] + dat$coefficients["asylumseeker:ftptemp"]*emp[2],
           dat$coefficients["asylumseeker"] + dat$coefficients["asylumseeker:ftptemp"]*emp[3])

ci_upper <- c(coefs[1] + 1.96*sqrt(dat$vcov["asylumseeker","asylumseeker"] + 
                                     dat$vcov["asylumseeker:ftptemp","asylumseeker:ftptemp"]*emp[1]^2 +
                                     2*emp[1]*dat$vcov["asylumseeker","asylumseeker:ftptemp"]),
              coefs[2] + 1.96*sqrt(dat$vcov["asylumseeker","asylumseeker"] + 
                                     dat$vcov["asylumseeker:ftptemp","asylumseeker:ftptemp"]*emp[2]^2 +
                                     2*emp[2]*dat$vcov["asylumseeker","asylumseeker:ftptemp"]),
              coefs[3] + 1.96*sqrt(dat$vcov["asylumseeker","asylumseeker"] + 
                                     dat$vcov["asylumseeker:ftptemp","asylumseeker:ftptemp"]*emp[3]^2 +
                                     2*emp[3]*dat$vcov["asylumseeker","asylumseeker:ftptemp"]))

ci_lower <- c(coefs[1] - 1.96*sqrt(dat$vcov["asylumseeker","asylumseeker"] + 
                                     dat$vcov["asylumseeker:ftptemp","asylumseeker:ftptemp"]*emp[1]^2 +
                                     2*emp[1]*dat$vcov["asylumseeker","asylumseeker:ftptemp"]),
              coefs[2] - 1.96*sqrt(dat$vcov["asylumseeker","asylumseeker"] + 
                                     dat$vcov["asylumseeker:ftptemp","asylumseeker:ftptemp"]*emp[2]^2 +
                                     2*emp[2]*dat$vcov["asylumseeker","asylumseeker:ftptemp"]),
              coefs[3] - 1.96*sqrt(dat$vcov["asylumseeker","asylumseeker"] + 
                                     dat$vcov["asylumseeker:ftptemp","asylumseeker:ftptemp"]*emp[3]^2 +
                                     2*emp[3]*dat$vcov["asylumseeker","asylumseeker:ftptemp"]))

plotdf <- as.data.frame(cbind(coefs, ci_upper, ci_lower))
summary(plotdf)

ggplot(data = plotdf) + geom_line(aes(x = emp, y = coefs, group = 1)) + 
  geom_ribbon(aes(x = emp, ymin=ci_lower, ymax=ci_upper), alpha=0.3)

# HYPOTHESIS 2: Polish voters are motivated in their choices by in and out-group-identity (populist) rhetoric more than by concrete policy proposals
library(cjoint)

# Load dataset
conjoint <- read_excel("conjoint_05042018.xls", sheet = 1)

# Keep only correct, full responses as per main dataset "db"
conjoint <- conjoint[conjoint$ID %in% db$ID, ]

# Perform analysis
conjoint[,4:7] <- lapply(conjoint[,4:7], factor)
baselines <- list()
baselines$Attribute1 <- "2"
baselines$Attribute2 <- "2"
baselines$Attribute3 <- "2"
baselines$Attribute4 <- "2"

results <- cjoint::amce(Selected ~ Attribute1 + Attribute2 + Attribute3 + Attribute4,
                        data=conjoint, 
                        cluster=TRUE,
                        respondent.id='ID',
                        baselines = baselines)
summary(results)
names <- list()
names$Attribute1 <- c("Ensure full employment", "Ensure economic competitiveness", "Defend interests of the real Poles", "Deal with elite corruption")
names$Attribute2 <- c("Is for staying with the current energy policy", "Is for investing in renewable energies", "Is against investing in renewable energies")
names$Attribute3 <- c("Is for staying with the current taxation on the wealthy", "Is for progressive taxation", "Is for flat taxation")
names$Attribute4 <- c("Is for staying with the current trade policy", "Is for more free trade and economic openness", "Is for less free trade and economic openness")
plot.amce(results, 
          attribute.names = c("Political priority", "Position on energy", "Position on taxation", "Position on free trade"),
          level.names = names,
          label.baseline = TRUE,
          ci = 0.95,
          main = "(All)",
          xlab = "Change in E[Y]", 
          plot.theme = theme_bw(), 
          colors = "black",
          cluster = TRUE,
          legend.position="bottom")


# AMCE analysis for subset == Treatment A
conjointA <- merge(x = conjoint, y = db[ , c("ID", "Treatment")], by = "ID", all.x=TRUE)
conjointA <- conjointA %>% 
  filter(Treatment == "A")
#conjointA$Treatment <- NULL
A <- cjoint::amce(Selected ~ Attribute1 + Attribute2 + Attribute3 + Attribute4,
                        data=conjointA, 
                        cluster=TRUE,
                        respondent.id='ID',
                        baselines = baselines)
summary(A)
plot.amce(A, 
          attribute.names = c("Political priority", "Position on energy", "Position on taxation", "Position on free trade"),
          level.names = names,
          label.baseline = TRUE,
          ci = 0.95,
          main = "(A)",
          xlab = "Change in probability that party will be preferred (in percentage points)",
          plot.theme = theme_bw(),
          colors = "black",
          cluster = TRUE)

# AMCE analysis for subset == Treatment B
conjointB <- merge(x = conjoint, y = db[ , c("ID", "Treatment")], by = "ID", all.x=TRUE)
conjointB <- conjointB %>% 
  filter(Treatment == "B")
#conjointB$Treatment <- NULL
B <- cjoint::amce(Selected ~ Attribute1 + Attribute2 + Attribute3 + Attribute4,
                  data=conjointB, 
                  cluster=TRUE,
                  respondent.id='ID',
                  baselines = baselines)
summary(B)
plot.amce(B, 
          attribute.names = c("Political priority", "Position on energy", "Position on taxation", "Position on free trade"),
          level.names = names,
          label.baseline = TRUE,
          ci = 0.95,
          main = "(B)",
          xlab = "Change in probability that party will be preferred (in percentage points)",
          plot.theme = theme_bw(),
          colors = "black",
          cluster = TRUE)

# AMCE analysis for subset == control
conjointC <- merge(x = conjoint, y = db[ , c("ID", "Treatment")], by = "ID", all.x=TRUE)
conjointC <- conjointC %>% 
  filter(Treatment == "control")

C <- cjoint::amce(Selected ~ Attribute1 + Attribute2 + Attribute3 + Attribute4,
                  data=conjointC, 
                  cluster=TRUE,
                  respondent.id='ID',
                  baselines = baselines)
summary(C)
plot.amce(C, 
          attribute.names = c("Political priority", "Position on energy", "Position on taxation", "Position on free trade"),
          level.names = names,
          label.baseline = TRUE,
          ci = 0.95,
          main = "(Control)",
          xlab = "Change in probability that party will be preferred (in percentage points)",
          plot.theme = theme_bw(),
          colors = "black",
          cluster = TRUE)
          
# HYPOTHESIS 3: Polish voters who are exposed to foreign press calling them to vote for a particular candidate are more likely to vote for the opposite candidate.

ols1 <- lm(data = db, SumEconWonk ~ relevel(as.factor(Treatment), 'control'))

ols2 <- lm(data = db, SumEconWonk ~ relevel(as.factor(Treatment), 'control') + 
             Sex + Education + Income + NationalID + Age + LeftRight + Province)

ols3 <- lm(data = db, SumEconWonk ~ relevel(as.factor(Treatment), 'control') + 
             Sex + Education + Income + NationalID + Age + LeftRight + Party2015 + PiD + Province)

ols4 <- lm(data = db, SumEconWonk ~ relevel(as.factor(Treatment), 'control') + 
             Income + NationalID + Party2015 + Education + Age + Province)
ols4$
cov.ols1 <- sandwich::vcovHC(ols1, type = "HC") # Robust standard errors
rob.std.err1 <- sqrt(diag(cov.ols1))
naive.std.err1 <- summary(ols1)$coefficients[,2]

cov.ols2 <- sandwich::vcovHC(ols2, type = "HC") # Robust standard errors
rob.std.err2 <- sqrt(diag(cov.ols2))
naive.std.err2 <- summary(ols2)$coefficients[,2]

cov.ols3 <- sandwich::vcovHC(ols3, type = "HC") # Robust standard errors
rob.std.err3 <- sqrt(diag(cov.ols3))
naive.std.err3 <- summary(ols3)$coefficients[,2]

cov.ols4 <- sandwich::vcovHC(ols4, type = "HC") # Robust standard errors
rob.std.err4 <- sqrt(diag(cov.ols4))
naive.std.err4 <- summary(ols4)$coefficients[,2]

stargazer(list(ols1, ols1, ols2, ols2, ols3, ols3, ols4, ols4),
          se = list(naive.std.err1,rob.std.err1, naive.std.err2,rob.std.err2, naive.std.err3,rob.std.err3),
          column.labels=c("default","robust"),
          title="Regression Results",
          model.numbers=TRUE,
          dep.var.labels=c("x","x"),
          omit.stat=c("LL","ser","f"),
          type="html",
          out = "OLSecon.html")

log1 <- glm(data = db, formula = EconWonkBinary ~ relevel(as.factor(Treatment), 'control') +
              Income + NationalID + Party2015 + Province, family = 'binomial')
log2 <- glm(data = db, formula = EconWonkBinary ~ relevel(as.factor(Treatment), 'control') +
              Income + NationalID + Party2015 + Province + Education + Age, family = 'binomial')

#################################################################

# GOODNESS-OF-FIT CHECKS OF LOGIT MODELS 5 AND 6

#################################################################
db$Treatment <- relevel(as.factor(db$Treatment), "control")
db$Party2015[is.na(db$Party2015)] <- "non-voter"
db$Party2015 <- relevel(as.factor(db$Party2015), "non-voter")
log3p <- glm(PopBinary ~ Treatment + Sex + Age + Education + Income + PolInt + NationalID + LeftRight + Province + Party2015, db, weights = weights_pop, family=binomial())


# Hosmer-Lemeshow tests

hosmer <- function(y, fv, groups=10, table=TRUE, type=2) {
  q <- quantile(fv, seq(0,1,1/groups), type=type)
  fv.g <- cut(fv, breaks=q, include.lowest=TRUE)
  obs <- xtabs( ~ fv.g + y)
  fit <- cbind( e.0 = tapply(1-fv, fv.g, sum), e.1 = tapply(fv, fv.g, sum))
  if(table) print(cbind(obs,fit))
  chi2 <- sum((obs-fit)^2/fit)
  pval <- pchisq(chi2, groups-2, lower.tail=FALSE)
  data.frame(test="Hosmer-Lemeshow",groups=groups,chi.sq=chi2,pvalue=pval)
}

HL5 <- hosmer(db$PopBinary, fitted(log3p))
HL5

db2 <- db %>% 
  filter(Voted == 1)
sum(is.na(db2))
names(which(sapply(db2, anyNA)))

db2 <- db2[complete.cases(db2), ]

log3v <- glm(data=db2, PopBinary ~ Treatment + Sex + Age + Education + Income + PolInt + NationalID + LeftRight + Province + PiD + Party2015, weights = weights_vot, family=binomial())

HL6 <- hosmer(db2$PopBinary, fitted(log3v))
HL6
