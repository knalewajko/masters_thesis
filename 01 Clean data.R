# Set directory
getwd()
setwd("/Users/knalewajko/Documents/HSoG/04 Viertes Semester/03 Thesis/DATA")

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Make sure R reads Polish characters
Sys.setlocale("LC_CTYPE", "pl_PL")

###########################################################################

# CREATE RANDOMIZED CONJOINT DESIGN TO UPLOAD ON QUESTIONPRO WEBPAGE

# Open template
db3 <- read_excel("Conjoint.xls", sheet = 1)

# Randomize integers (=levels) 0-2 for Attributes 2-4
A <- 0:2
db3$Attribute2 <- sample(A, size = 1000, replace = TRUE)
db3$Attribute3 <- sample(A, size = 1000, replace = TRUE)
db3$Attribute4 <- sample(A, size = 1000, replace = TRUE)

# Save dataset db3
openxlsx::write.xlsx(db3, 'Attributes2-4.xlsx')

# For Attribute1 (0-3) I need pairs of 0/1 & 2/3 OR 2/3 & 0/1
pair1 <- c(5.2)
pair2 <- c(5.3)
pair3 <- c(1.2)
pair4 <- c(1.3)
pair5 <- c(2.5)
pair6 <- c(2.1)
pair7 <- c(3.5)
pair8 <- c(3.1)

db4 <- data.frame(1:500)
db4$x <- sample(c(pair1, pair2, pair3, pair4, pair5, pair6, pair7, pair8), size = 500, replace = TRUE)
db4$x <- as.character(db4$x)
db4 <- tidyr::separate(db4, x, into = c("A", "B"))
db4[,2:3][db4[,2:3]==5] <- 0

# Save dataset db4 and interwave column A with column B in excel (in order to create pairs of candidates after one another)
openxlsx::write.xlsx(db4, 'Attribute1.xlsx')

# Create Task variable 
db <- read_excel("Conjoint-Filled.xlsx", sheet = 2)
sdb <- stack(db)
openxlsx::write.xlsx(sdb, 'Task.xlsx')


###########################################################################

# CREATE A LIST OF POLISH CITIES FOR THE EXPERIMENT that will be matchable with data on unemployment

# Make sure R reads Polish characters
Sys.setlocale("LC_CTYPE", "pl_PL")

# Open excel sheet
db <- read.csv("Cities.txt", encoding = "UTF-8")
db2 <- read_excel("Eurostat.xlsx", sheet = 3) # this one's better

# Trim leading and trailing spaces
db2$CITY <- stringr::str_trim(db2$CITY)

# Save doc
openxlsx::write.xlsx(db2, 'Cities_trimmed.xlsx')

###########################################################################

# FROM THE DATASET, CHOOSE ONLY RELEVANT ROWS AND MERGE THEM WITH CONJOINT RESULTS

system.time(conjoint <- read.csv("Conjoint.csv", sep=';')) # The file is large, so I translated it to csv

clean <- conjoint %>% 
  filter(ID==1604082923&Task==70|
           ID==1604082923&Task==187|
           ID==1604082923&Task==309|
           ID==1604082923&Task==429|
           ID==1604082923&Task==441|
           ID==1604082925&Task==39|
           ID==1604082925&Task==62|
           ID==1604082925&Task==147|
           ID==1604082925&Task==298|
           ID==1604082925&Task==380|
           ID==1604082926&Task==79|
           ID==1604082926&Task==117|
           ID==1604082926&Task==119|
           ID==1604082926&Task==287|
           ID==1604082926&Task==440|
           ID==1604082927&Task==51|
           ID==1604082927&Task==270|
           ID==1604082927&Task==329|
           ID==1604082927&Task==377|
           ID==1604082927&Task==484|
           ID==1604082929&Task==21|
           ID==1604082929&Task==56|
           ID==1604082929&Task==114|
           ID==1604082929&Task==279|
           ID==1604082929&Task==474|
           ID==1604082931&Task==13|
           ID==1604082931&Task==158|
           ID==1604082931&Task==253|
           ID==1604082931&Task==291|
           ID==1604082931&Task==481|
           ID==1604082932&Task==139|
           ID==1604082932&Task==186|
           ID==1604082932&Task==215|
           ID==1604082932&Task==364|
           ID==1604082932&Task==453|
           ID==1604082933&Task==198|
           ID==1604082933&Task==211|
           ID==1604082933&Task==231|
           ID==1604082933&Task==293|
           ID==1604082933&Task==428|
           ID==1604082934&Task==85|
           ID==1604082934&Task==125|
           ID==1604082934&Task==129|
           ID==1604082934&Task==288|
           ID==1604082934&Task==369|
           ID==1604082935&Task==76|
           ID==1604082935&Task==186|
           ID==1604082935&Task==206|
           ID==1604082935&Task==354|
           ID==1604082935&Task==410|
           ID==1604082936&Task==209|
           ID==1604082936&Task==238|
           ID==1604082936&Task==276|
           ID==1604082936&Task==346|
           ID==1604082936&Task==429|
           ID==1604082937&Task==17|
           ID==1604082937&Task==51|
           ID==1604082937&Task==228|
           ID==1604082937&Task==440|
           ID==1604082937&Task==470|
           ID==1604082938&Task==204|
           ID==1604082938&Task==268|
           ID==1604082938&Task==340|
           ID==1604082938&Task==461|
           ID==1604082938&Task==466|
           ID==1604082940&Task==184|
           ID==1604082940&Task==200|
           ID==1604082940&Task==368|
           ID==1604082940&Task==381|
           ID==1604082940&Task==399|
           ID==1604082941&Task==200|
           ID==1604082941&Task==248|
           ID==1604082941&Task==306|
           ID==1604082941&Task==407|
           ID==1604082941&Task==428|
           ID==1604082942&Task==208|
           ID==1604082942&Task==212|
           ID==1604082942&Task==251|
           ID==1604082942&Task==400|
           ID==1604082942&Task==456|
           ID==1604082943&Task==4|
           ID==1604082943&Task==167|
           ID==1604082943&Task==347|
           ID==1604082943&Task==373|
           ID==1604082943&Task==473|
           ID==1604082944&Task==51|
           ID==1604082944&Task==130|
           ID==1604082944&Task==252|
           ID==1604082944&Task==300|
           ID==1604082944&Task==449|
           ID==1604082946&Task==34|
           ID==1604082946&Task==80|
           ID==1604082946&Task==153|
           ID==1604082946&Task==169|
           ID==1604082946&Task==315|
           ID==1604082947&Task==178|
           ID==1604082947&Task==296|
           ID==1604082947&Task==351|
           ID==1604082947&Task==410|
           ID==1604082947&Task==491|
           ID==1604082950&Task==24|
           ID==1604082950&Task==35|
           ID==1604082950&Task==37|
           ID==1604082950&Task==38|
           ID==1604082950&Task==110|
           ID==1604082951&Task==15|
           ID==1604082951&Task==163|
           ID==1604082951&Task==225|
           ID==1604082951&Task==238|
           ID==1604082951&Task==365|
           ID==1604082954&Task==19|
           ID==1604082954&Task==73|
           ID==1604082954&Task==169|
           ID==1604082954&Task==192|
           ID==1604082954&Task==334|
           ID==1604082956&Task==113|
           ID==1604082956&Task==300|
           ID==1604082956&Task==305|
           ID==1604082956&Task==351|
           ID==1604082956&Task==429|
           ID==1604082961&Task==115|
           ID==1604082961&Task==166|
           ID==1604082961&Task==223|
           ID==1604082961&Task==444|
           ID==1604082961&Task==485|
           ID==1604082963&Task==11|
           ID==1604082963&Task==299|
           ID==1604082963&Task==342|
           ID==1604082963&Task==375|
           ID==1604082963&Task==441|
           ID==1604082964&Task==12|
           ID==1604082964&Task==256|
           ID==1604082964&Task==296|
           ID==1604082964&Task==333|
           ID==1604082964&Task==455|
           ID==1604082976&Task==45|
           ID==1604082976&Task==76|
           ID==1604082976&Task==323|
           ID==1604082976&Task==344|
           ID==1604082976&Task==414|
           ID==1604082978&Task==10|
           ID==1604082978&Task==247|
           ID==1604082978&Task==295|
           ID==1604082978&Task==330|
           ID==1604082978&Task==452|
           ID==1604082980&Task==98|
           ID==1604082980&Task==110|
           ID==1604082980&Task==240|
           ID==1604082980&Task==305|
           ID==1604082980&Task==464|
           ID==1604082987&Task==135|
           ID==1604082987&Task==148|
           ID==1604082987&Task==308|
           ID==1604082987&Task==320|
           ID==1604082987&Task==341|
           ID==1604082989&Task==109|
           ID==1604082989&Task==222|
           ID==1604082989&Task==331|
           ID==1604082989&Task==440|
           ID==1604082989&Task==496|
           ID==1604082993&Task==179|
           ID==1604082993&Task==198|
           ID==1604082993&Task==254|
           ID==1604082993&Task==343|
           ID==1604082993&Task==411|
           ID==1604087597&Task==15|
           ID==1604087597&Task==142|
           ID==1604087597&Task==159|
           ID==1604087597&Task==312|
           ID==1604087597&Task==460|
           ID==1604087602&Task==20|
           ID==1604087602&Task==31|
           ID==1604087602&Task==132|
           ID==1604087602&Task==250|
           ID==1604087602&Task==318|
           ID==1604087604&Task==8|
           ID==1604087604&Task==94|
           ID==1604087604&Task==120|
           ID==1604087604&Task==222|
           ID==1604087604&Task==349|
           ID==1604087605&Task==106|
           ID==1604087605&Task==151|
           ID==1604087605&Task==280|
           ID==1604087605&Task==394|
           ID==1604087605&Task==497|
           ID==1604087607&Task==110|
           ID==1604087607&Task==290|
           ID==1604087607&Task==417|
           ID==1604087607&Task==444|
           ID==1604087607&Task==485|
           ID==1604087609&Task==126|
           ID==1604087609&Task==295|
           ID==1604087609&Task==333|
           ID==1604087609&Task==349|
           ID==1604087609&Task==485|
           ID==1604087610&Task==111|
           ID==1604087610&Task==190|
           ID==1604087610&Task==249|
           ID==1604087610&Task==382|
           ID==1604087610&Task==410|
           ID==1604087612&Task==10|
           ID==1604087612&Task==58|
           ID==1604087612&Task==326|
           ID==1604087612&Task==439|
           ID==1604087612&Task==455|
           ID==1604087614&Task==123|
           ID==1604087614&Task==160|
           ID==1604087614&Task==253|
           ID==1604087614&Task==431|
           ID==1604087614&Task==489|
           ID==1604087615&Task==147|
           ID==1604087615&Task==162|
           ID==1604087615&Task==196|
           ID==1604087615&Task==365|
           ID==1604087615&Task==373|
           ID==1604087617&Task==17|
           ID==1604087617&Task==237|
           ID==1604087617&Task==343|
           ID==1604087617&Task==349|
           ID==1604087617&Task==408|
           ID==1604087618&Task==127|
           ID==1604087618&Task==142|
           ID==1604087618&Task==245|
           ID==1604087618&Task==445|
           ID==1604087618&Task==487|
           ID==1604087619&Task==133|
           ID==1604087619&Task==149|
           ID==1604087619&Task==194|
           ID==1604087619&Task==245|
           ID==1604087619&Task==415|
           ID==1604087620&Task==3|
           ID==1604087620&Task==271|
           ID==1604087620&Task==405|
           ID==1604087620&Task==427|
           ID==1604087620&Task==464|
           ID==1604087621&Task==55|
           ID==1604087621&Task==242|
           ID==1604087621&Task==248|
           ID==1604087621&Task==287|
           ID==1604087621&Task==414|
           ID==1604087624&Task==21|
           ID==1604087624&Task==62|
           ID==1604087624&Task==171|
           ID==1604087624&Task==268|
           ID==1604087624&Task==341|
           ID==1604087625&Task==84|
           ID==1604087625&Task==221|
           ID==1604087625&Task==258|
           ID==1604087625&Task==372|
           ID==1604087625&Task==486|
           ID==1604087626&Task==127|
           ID==1604087626&Task==226|
           ID==1604087626&Task==273|
           ID==1604087626&Task==323|
           ID==1604087626&Task==402|
           ID==1604087627&Task==51|
           ID==1604087627&Task==172|
           ID==1604087627&Task==327|
           ID==1604087627&Task==367|
           ID==1604087627&Task==374|
           ID==1604087628&Task==29|
           ID==1604087628&Task==200|
           ID==1604087628&Task==329|
           ID==1604087628&Task==400|
           ID==1604087628&Task==401|
           ID==1604087629&Task==55|
           ID==1604087629&Task==207|
           ID==1604087629&Task==237|
           ID==1604087629&Task==249|
           ID==1604087629&Task==324|
           ID==1604087630&Task==26|
           ID==1604087630&Task==208|
           ID==1604087630&Task==275|
           ID==1604087630&Task==360|
           ID==1604087630&Task==471|
           ID==1604087631&Task==128|
           ID==1604087631&Task==173|
           ID==1604087631&Task==303|
           ID==1604087631&Task==369|
           ID==1604087631&Task==449|
           ID==1604087636&Task==34|
           ID==1604087636&Task==118|
           ID==1604087636&Task==145|
           ID==1604087636&Task==410|
           ID==1604087636&Task==496|
           ID==1604087637&Task==29|
           ID==1604087637&Task==126|
           ID==1604087637&Task==144|
           ID==1604087637&Task==173|
           ID==1604087637&Task==393|
           ID==1604087638&Task==110|
           ID==1604087638&Task==149|
           ID==1604087638&Task==240|
           ID==1604087638&Task==293|
           ID==1604087638&Task==309|
           ID==1604087639&Task==38|
           ID==1604087639&Task==60|
           ID==1604087639&Task==158|
           ID==1604087639&Task==195|
           ID==1604087639&Task==405|
           ID==1604087640&Task==3|
           ID==1604087640&Task==153|
           ID==1604087640&Task==273|
           ID==1604087640&Task==349|
           ID==1604087640&Task==437|
           ID==1604087641&Task==10|
           ID==1604087641&Task==67|
           ID==1604087641&Task==73|
           ID==1604087641&Task==85|
           ID==1604087641&Task==324|
           ID==1604087643&Task==191|
           ID==1604087643&Task==247|
           ID==1604087643&Task==250|
           ID==1604087643&Task==269|
           ID==1604087643&Task==309|
           ID==1604087644&Task==2|
           ID==1604087644&Task==174|
           ID==1604087644&Task==365|
           ID==1604087644&Task==369|
           ID==1604087644&Task==380|
           ID==1604087645&Task==85|
           ID==1604087645&Task==119|
           ID==1604087645&Task==157|
           ID==1604087645&Task==294|
           ID==1604087645&Task==297|
           ID==1604087646&Task==28|
           ID==1604087646&Task==181|
           ID==1604087646&Task==256)

# Save doc
openxlsx::write.xlsx(clean, 'Conjoint_clean.xlsx')

###########################################################################

# FILTER INCOMPLETE RESPONSES

db <- read_excel("data_04042018.xlsx", sheet = 3)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

db <- completeFun(db, c("Treatment", "EconState", "PLindisp",	"PLconnect",	"PLborn",	"PLproud",
                         "PLbetter",	"YearBorn",	"Origin",	"Sex",	"Education",	"EmplStatus",
                         "Income",	"PolInt",	"LeftRight",	"Voted"))

###########################################################################

# PREPARE DEPENDENT VARIABLE 1 (SumPop & PopBinary) and add to main dataset

conjoint <- read_excel("conjoint_05042018.xls", sheet = 1)

Populists <- conjoint %>%
  select(ID, Attribute1, Selected) %>% 
  mutate(Populist = Attribute1==3 & Selected == 1 | Attribute1==4 & Selected == 1)

Populists$Attribute1 <- NULL
Populists$Selected <- NULL

Populists$Populist <- as.numeric(Populists$Populist)

PopChoice <- Populists %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(SumPop = sum(Populist)) %>% 
  tidyr::complete(ID, fill = list(SumPop = 0))  ## https://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in & https://stackoverflow.com/questions/25956178/proper-idiom-for-adding-zero-count-rows-in-tidyr-dplyr?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

PopChoice <- PopChoice %>% 
  dplyr::group_by(ID, SumPop) %>% 
  dplyr::mutate(PopBinary = SumPop>2)

PopChoice$PopBinary <- as.numeric(PopChoice$PopBinary)

# JOIN THE DATASETS ANSWERS + CONJOINT FOR THE DEPENDENT VARIABLE
FULL <- dplyr::left_join(db, PopChoice, by = "ID")

###########################################################################

# PREPARE DEPENDENT VARIABLE 2 (Econ-Wonk) and add to main dataset

EconWonk <- conjoint %>%
  select(ID, Attribute1, Selected) %>% 
  mutate(EconWonk = Attribute1==1 & Selected == 1 | Attribute1==2 & Selected == 1)

EconWonk$Attribute1 <- NULL
EconWonk$Selected <- NULL

EconWonk$EconWonk <- as.numeric(EconWonk$EconWonk)

EconChoice <- EconWonk %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(SumEconWonk = sum(EconWonk)) %>% 
  tidyr::complete(ID, fill = list(SumEconWonk = 0))  ## https://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in & https://stackoverflow.com/questions/25956178/proper-idiom-for-adding-zero-count-rows-in-tidyr-dplyr?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

EconChoice <- EconChoice %>% 
  dplyr::group_by(ID, SumEconWonk) %>% 
  dplyr::mutate(EconWonkBinary = SumEconWonk>2)

EconChoice$EconWonkBinary <- as.numeric(EconChoice$EconWonkBinary)

# JOIN THE DATASETS ANSWERS + CONJOINT FOR THE DEPENDENT VARIABLE
FULL <- dplyr::left_join(db, EconChoice, by = "ID")

FULL <- FULL[,c(1,2,3,33,32,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]

# SAVE DATASET FOR LATER ANALYSIS

openxlsx::write.xlsx(FULL, 'Thesis4.xlsx')

###########################################################################

# ENSURE CORRECT VARIABLE LEVELS

db <- FULL

db$Voted[db$Voted == "tak"] <- 1
db$Voted[db$Voted == "nie"] <- 0

levels(factor(db$Income))
db$Income[db$Income == "poniżej 12 000 zł"] <- 1
db$Income[db$Income == "12 000-23 999 zł"] <- 2
db$Income[db$Income == "24 000-35 999 zł"] <- 3
db$Income[db$Income == "36 000-47 999 zł"] <- 4
db$Income[db$Income == "48 000-59 999 zł"] <- 5
db$Income[db$Income == "60 000-71 999 zł"] <- 6
db$Income[db$Income == "72 000-95 999 zł"] <- 7
db$Income[db$Income == "96 000-119 999 zł"] <- 8
db$Income[db$Income == "powyżej 120 000 zł"] <- 9

levels(factor(db$EconState))
db$EconState[db$EconState == "bardzo zła"] <- "very bad"
db$EconState[db$EconState == "zła"] <- "bad"
db$EconState[db$EconState == "dobra"] <- "good"
db$EconState[db$EconState == "rewelacyjna"] <- "excellent"

levels(factor(db$Treatment))
db$Treatment[db$Treatment == "1"] <- "A"
db$Treatment[db$Treatment == "2"] <- "B"
db$Treatment[db$Treatment == "3"] <- "control"

levels(factor(db$Sex))
db$Sex[db$Sex == "kobieta"] <- 1
db$Sex[db$Sex == "mężczyzna"] <- 0

levels(factor(db$Education))
db$Education[db$Education == "szkoła podstawowa"] <- 1
db$Education[db$Education == "gimnazjum"] <- 2
db$Education[db$Education == "liceum / technikum / szkoła zawodowa"] <- 3
db$Education[db$Education == "szkoła policealna"] <- 4
db$Education[db$Education == "studia krótkiego cyklu"] <- 5
db$Education[db$Education == "studia I stopnia (licencjackie, inżynierskie)"] <- 6
db$Education[db$Education == "studia II stopnia (magisterskie)"] <- 7
db$Education[db$Education == "studia doktoranckie"] <- 8

levels(factor(db$EmplStatus))
db$EmplStatus[db$EmplStatus == "na emeryturze"] <- "on pension"
db$EmplStatus[db$EmplStatus == "niepracujący/a"] <- "unemployed"
db$EmplStatus[db$EmplStatus == "student(ka)"] <- "student"
db$EmplStatus[db$EmplStatus == "własna działalność prywatna (np. właścicel(ka) sklepu, firmy)"] <- "small business owner"
db$EmplStatus[db$EmplStatus == "zatrudniony/a na pełen etat (sektor prywatny)"] <- "employed full-time (private)"
db$EmplStatus[db$EmplStatus == "zatrudniony/a na pełen etat (sektor publiczny)"] <- "employed full-time (public)"
db$EmplStatus[db$EmplStatus == "zatrudniony/a na pół etatu (sektor prywatny)"] <- "employed part-time (private)"
db$EmplStatus[db$EmplStatus == "zatrudniony/a na pół etatu (sektor publiczny)"] <- "employed part-time (public)"

levels(factor(db$Party2015))
db$Party2015[db$Party2015 == "Partia Razem"] <- "Razem"
db$Party2015[db$Party2015 == "Polskie Stronnictwo Ludowe"] <- "PSL"
db$Party2015[db$Party2015 == "Zjednoczona Lewica (SLD+TR+PPS+UP+Zieloni)"] <- "Lewica"
db$Party2015[db$Party2015 == "KWW Grzegorza Brauna „Szczęść Boże!”"] <- "Grzegorz Braun"
db$Party2015[db$Party2015 == "Nowoczesna Ryszarda Petru"] <- "Nowoczesna"
db$Party2015[db$Party2015 == "Platforma Obywatelska"] <- "PO"
db$Party2015[db$Party2015 == "Prawo i Sprawiedliwość"] <- "PiS"
db$Party2015[db$Party2015 == "KWW Ruch Społeczny Rzeczypospolitej Polskiej"] <- "Ruch Społeczny RP"

db <- db %>% 
  mutate(NationalID = PLindisp + PLconnect + PLborn + PLproud + PLbetter)
db <- db %>% 
  mutate(Age = 2018 - YearBorn)

###########################################################################

# ADD INFO ABOUT UNEMPLOYMENT
province <- read_excel("Unemp.xlsx", sheet = 1)
unemp <- read_excel("Unemp.xlsx", sheet = 2)
db <- dplyr::left_join(db, province, by = "Origin")
db <- dplyr::left_join(db, unemp, by = "Province")

db <- db[,c(1,22,3,4,30,31,12,13,14,15,16,17,18,19,20,23,24,25,26,27,28,29,2,5,6,7,8,9,10,11,21)]

###########################################################################

# ENSURE ALL RESPONDENT CRITERIA HAVE BEEN MET (age >= 22 & completed the survey in 3-13 min)

Hmisc::describe(db$Seconds)
Hmisc::describe(db$Age)

db <- db %>% 
  filter(between(Seconds, 200, 800) & Age > 21) %>% 
  group_by(ID) %>% 
  arrange(Treatment)

Hmisc::describe(db$Treatment)

# Improve the tidiness of the data (added later, after doing intial analysis)

db$Voted <- as.numeric(db$Voted)
db$LeftRight <- as.numeric(db$LeftRight) # Shall I make "NA" into baseline category?
db$Sex <- as.numeric(db$Sex)
db$Education <- as.numeric(db$Education)
db$Income <- as.numeric(db$Income)
db$Treatment <- as.character(db$Treatment)

names(db)[names(db)=="LTUnemp2016/15"] <- "LTUnemp201615"
names(db)[names(db)=="Unemp2016/15"] <- "Unemp201615"
names(db)[names(db)=="YouthUnemp2016/15"] <- "YouthUnemp201615"

db <- db[,c(1,2,31,3,4,7,8,9,10,11,12,13,14,15,16,17,18,20,6,22,19,5,21,23,29,30,24,25,26,27,28)]

# SAVE DATASET FOR ANALYSIS

openxlsx::write.xlsx(db, 'Thesis2.xlsx', showNA=TRUE)
