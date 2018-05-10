# Population weights
db2 <- db
db2$Party2015[is.na(db2$Party2015)] <- "something"
db2$AgeCat <- cut(db2$Age, breaks=c(19,29,39,49,59,69), labels=c("20s","30s","40s","50s","60s"))
db.svy.unweighted <- survey::svydesign(ids=~1, data=db2)
vote.dist <- data.frame(Party2015 = c("something","Kukiz'15","Nowoczesna","PO","KORWiN","Lewica","PiS","Razem","Grzegorz Braun","PSL"),
                       Freq = nrow(db) * c(0.5,0.04405,0.038,0.12045,0.0238,0.03775,0.1879,0.0181,0.004,0.02565))
#vote.dist <- vote.dist[-1,]
age.dist <- data.frame(AgeCat=c("20s","30s","40s","50s","60s"),
                       Freq=c(0.22317593,0.228070534,0.181643757,0.214743542,0.152366237))
prov.dist <- data.frame(Province=c("D??","KP","LB","LS","??D","MP","MZ","OP","PK","PL","PM","??L","??K","WM","WP","ZP"),
                        Freq=c(0.075557118,0.054275751,0.055668046,0.026486684,0.064874665,0.087743503,0.139164886,0.025912657,0.055354054,0.03092834,0.060038391,0.118917204,0.032707318,0.037455214,0.090415521,0.044500647))
sex.dist <- data.frame(Sex=c(0,1),
                       Freq=c(0.484028212,0.515971788))
db.svy.rake <- rake(design = db.svy.unweighted,
                    sample.margins = list(~Province, ~AgeCat, ~Sex, ~Party2015),
                    population.margins = list(prov.dist, age.dist, sex.dist, vote.dist))
summary(weights(db.svy.rake))
weights <- weights(db.svy.rake)
weights <- as.data.frame(weights)
openxlsx::write.xlsx(weights, 'weights.xlsx')

# Electorate's weights
vote.dist <- data.frame(Party2015 = c("something","Kukiz'15","Nowoczesna","PO","KORWiN","Lewica","PiS","Razem","Grzegorz Braun","PSL"),
                        Freq = nrow(db) * c(0.0001,0.0881, 0.076, 0.2409, 0.0476, 0.0755, 0.3758, 0.0362, 0.008, 0.0513))
#vote.dist <- vote.dist[-1,]
age.dist <- data.frame(AgeCat=c("20s","30s","40s","50s","60s"),
                       Freq=c(0.14,0.15,0.19,0.25,0.27))
prov.dist <- data.frame(Province=c("ZP","WP","LB","PK","MP","OP","KP","MZ","??D","D??","??L","??K","PM","WM","LS","PL"),
                        Freq=c(0.039308528,0.086886099,0.054432202,0.054874025,0.093183189,0.022252176,0.048470689,0.166875199,0.066742448,0.073302356,0.121927841,0.030833507,0.058802602,0.030691277,0.022776495,0.028641367))
sex.dist <- data.frame(Sex=c(0,1),
                       Freq=c(0.52,0.48))
db.svy.rake <- rake(design = db.svy.unweighted,
                    sample.margins = list(~Province, ~AgeCat, ~Sex, ~Party2015),
                    population.margins = list(prov.dist, age.dist, sex.dist, vote.dist))
summary(weights(db.svy.rake))
weights <- weights(db.svy.rake)
weights <- as.data.frame(weights)
openxlsx::write.xlsx(weights, 'weights.xlsx')



#db2 <- na.omit(db2, cols=c("Party2015"))



db2 <- read_excel("kurz.xlsx", sheet = 1)
db <- left_join(db, db2, by = "ID")

openxlsx::write.xlsx(db, 'db_weights_incomplete.xlsx')
db2 <- read_excel("db_weights_incomplete.xlsx", sheet = 1)

# Proceed to multiple imputation of weights variable
methods(mice)
db$ID <- NULL
tempData <- mice(db,m=5,maxit=50,meth='pmm')
summary(tempData$imp$weights)
completeData <- mice::complete(tempData,1)

db2$weights <- completeData$weights # add imputed values to the data set where 'ID' variable wasn't removed from

openxlsx::write.xlsx(db2, 'Thesis5.xlsx', showNA=TRUE)









weights <- as.data.frame(weights(db.svy.rake))

print(db.svy.rake$weights)

log1 <- svyglm(data=db2, design = db.svy.rake, 
               formula = PopBinary ~ Treatment + Sex + Age + Education + Income + PolInt + 
                 NationalID + LeftRight + PiD + Province + Party2015, family=binomial())
summary(log1)




db.svy.rake <- rake(design = db.svy.unweighted,
                    sample.margins = list(~Party2015, ~Age),
                    population.margins = list(vote.dist, age.dist))
svymean(db2, db.svy.rake, na.rm=FALSE)




summary(weights(db.svy.rake))
summary(db.svy.rake)

db.svy.rake <- as.data.frame(db.svy.rake)

Age <- db %>% 
  select(Age) %>% 
  arrange(desc(Age))



22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 50, 51, 53, 54, 55, 56, 57, 58, 59, 62, 65, 67),
1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2,2,2,2,2,2,2,2,2,2.78,2.78,2.78,2.78,2.78,2.78,2.78,2.78,2.78,9.33,9.33,9.32


load(url("http://knutur.at/wsmt/R/RData/small.RData"))
0.5,0.04405,0.038,0.12045,0.0238,0.03775,0.1879,0.0181,0.004,0.02565
0.0881, 0.076, 0.2409, 0.0476, 0.0755, 0.3758, 0.0362, 0.008, 0.0513


prov.dist <- data.frame(Province=c("ZP","WP","LB","PK","MP","OP","KP","MZ","??D","D??","??L","??K","PM","WM","LS","PL"),
                        Freq=c(0.039308528,0.086886099,0.054432202,0.054874025,0.093183189,0.022252176,0.048470689,0.166875199,0.066742448,0.073302356,0.121927841,0.030833507,0.058802602,0.030691277,0.022776495,0.028641367))
