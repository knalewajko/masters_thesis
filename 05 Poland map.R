library(sp)
library(rgdal)
library(maptools)
library(raster)

#download.file("http://www.gis-support.pl/downloads/wojewodztwa.zip", "wojewodztwa.zip")
#unzip("wojewodztwa.zip", exdir=".")
woj <- maptools::readShapePoly("wojewodztwa") 
woj@data <- woj@data[ , c(6,16)]
names(woj@data) <- c("nazwa", "powierzchnia")
woj@data$nazwa <- c("opolskie", "??wi??tokrzyskie", "kujawsko-pomorskie", "mazowieckie", "pomorskie", "??l??skie",
                           "warmi??sko-mazurskie", "zachodniopomorskie", "dolno??l??skie", "wielkopolskie", "????dzkie",
                           "podlaskie", "ma??opolskie", "lubuskie", "podkarpackie", "lubelskie")
poland.map.gg <- ggplot2::fortify(model = woj, data="nazwa")
predictions <- readxl::read_excel("predictions2.xlsx", sheet = 1)
predictions$id <- as.character(predictions$id)
poland.map.gg2 <- left_join(poland.map.gg, predictions, by = "id")
#head(poland.map.gg2, n=16)
ggplot2::ggplot()+
  geom_polygon(data = poland.map.gg2, 
               aes(long, lat, group = group.x,fill = fit), 
               colour = "black", lwd=0.1) +
  facet_wrap(~group.y)+
  ggtitle("Heat Map of Reactant Response to Foreign Endorsement by Polish Voievodships") +
  labs(x="", y="", fill = "Predicted probability of reactant response") +
  theme_bw() +
  theme(legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient(low = "white", high = "black")




sfMap = map = ggmap::get_map(location = "Poland", zoom = 6, maptype = "road")
ggmap::ggmap(sfMap, extent = "device")

ggplot2::ggplot()

ggmap::ggmap(sfMap, extent = "device")

ggplot2::ggplot()+
  geom_polygon(data = poland.map.gg2, 
               aes(long, lat, group = group.x,fill = fit), 
               colour = "black", lwd=0.1) +
  facet_wrap(~group.y)+
  ggtitle("Heat Map of Reactant Response to Foreign Endorsement by Polish Voievodships") +
  labs(x="", y="", fill = "Predicted probability of reactant response") +
  theme_bw() +
  theme(legend.position="bottom", 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient(low = "white", high = "black")





head(poland.map.gg2, n=2)







poland.map <- readOGR(dsn="wojewodztwa", "wojewodztwa")

woj <- maptools::readShapePoly("wojewodztwa") 
woj <- rgdal::readOGR("woj.shp") 
plot(woj)

woj <- rgdal::readOGR("/Users/knalewajko/Documents/HSoG/04 Viertes Semester/03 Thesis/DATA", 
                      dsn = "wojewodztwa",
                      layer = "wojewodztwa")

woj <- shapefile("woj.shp")


woj.shape = maptools::readShapePoly("woj.shp") #,IDvar="NAZ_WOJ"

rgdal::readOGR(dsn = "woj.shp")


poland.map <- rgdal::readOGR(dsn="wojewodztwa", "wojewodztwa") #argumentami s?? nazwa folderu oraz nazwa pliku (nie podajemy rozszerzenia)

class(poland.map)





xw = as.numeric(strsplit("14.71769 17.21543 20.19659 14.74992 16.21633 17.84389 22.32370 18.90744 20.35774 22.14644 15.37838 17.39268 18.71407 19.43922 19.98711 21.45352"," ")[[1]])
yw = as.numeric(strsplit("53.63893 54.23526 53.95697 52.37669 52.39657 53.20162 53.26125 51.68097 52.55559 51.37287 51.19397 50.72684 50.61751 50.00130 50.77654 50.08081"," ")[[1]])
nw = c("zachodnio-\npomorskie","pomorskie","warmi??sko-\nmazurskie","lubuskie","wielkopolskie","kujawsko-\npomorskie","podlaskie","????dzkie","mazowieckie","lubelskie","dolno??l??skie","opolskie","??l??skie","ma??opolskie","??wi??tokrzyskie","podkarpackie")
#
kolory <- c("#f4e3d7","#e9c6af","#d38d5f","#a05a2c")
ind <- c(3,2,2,2,1,3,1,3,1,3,1,4,2,3,2,3)
legtxt <- c("<14","15-24","25-49",">50")
par(mar=c(0,0,0,0))
plot(woj.shape, border="black",lwd=0.5, axes=T,col=kolory[ind])
text(xw,yw,nw,adj=0,cex=0.8)
legend(x=14, y=49.5, legend=legtxt, fill=kolory, bty="n",cex=0.7)



library(maps)
map("world","poland")

library(maptools) 
woj <- maptools::readShapePoly("WOJ.shp") 
plot(woj)



x.mid <- function(x1, x2, y1, y2, y.mid) {
  x1 + ((x2 ??? x1) / (y2 ??? y1)) * (y.mid ??? y1)
}
poland <- map(???world???,???poland???, fill=T, col=???#D4213D???)

               
               
mid <- mean(poland$range[3:4])
               upper <- poland$y > mid
               cut1 <- which.max(diff(upper))
               x.first <- x.mid(poland$x[cut1], poland$x[cut1 + 1],
                                poland$y[cut1], poland$y[cut1 + 1], mid)
               cut2 <- which.min(diff(upper))
               x.last <- x.mid(poland$x[cut2], poland$x[cut2 + 1],
                               poland$y[cut2], poland$y[cut2 + 1], mid)
               
               upperx <- c(x.first,poland$x[upper],x.last)
               uppery <- c(mid, poland$y[upper], mid)
               polygon(upperx, uppery, col=???white???)
               

########################

db$Treatment <- relevel(as.factor(db$Treatment), "control")
db$Party2015[is.na(db$Party2015)] <- "non-voter"
db$Party2015 <- relevel(as.factor(db$Party2015), "non-voter")

log3p <- glm(PopBinary ~ Treatment + Sex + Age + Education + Income + PolInt + NationalID + LeftRight + Province + Party2015, db, weights = weights_pop, family=binomial())

pred_log3p <- expand.grid(Treatment = c("A", "B", "control"),
                          Income = median(db$Income),
                          Education = median(db$Education),
                          Sex = 0,
                          PolInt = median(db$PolInt),
                          LeftRight = median(db$LeftRight),
                          #Voted = 0,
                          PiD = median(db$PiD, na.rm = TRUE),
                          Age = median(db$Age),
                          NationalID = median(db$NationalID),
                          Party2015 = "non-voter",
                          Province = "WP")

pred_outlog3p <- predict(object = log3p, 
                         newdata = pred_log3p,
                         # interval = 'prediction',
                         level=.9,
                         type = 'response',
                         se.fit = TRUE)
pred_outlog3p$upr = pred_outlog3p$fit + 1.96 * pred_outlog3p$se.fit
pred_outlog3p$lwr = pred_outlog3p$fit - 1.96 * pred_outlog3p$se.fit
pred_fulllog3p <- cbind(pred_log3p, pred_outlog3p)

pred_fulllog3p[,12:16] <- lapply(pred_fulllog3p[,12:16], function(x) ifelse(x>1, 1, x))
pred_fulllog3p[,12:16] <- lapply(pred_fulllog3p[,12:16], function(x) ifelse(x<0, 0, x))

ggplot(data = pred_fulllog3p,
       aes(x = Treatment, y = fit, ymin = lwr, ymax = upr)) +
  geom_point(data = pred_fulllog3p, 
             aes(x = Treatment, y = fit), 
             alpha = 0.5,
             inherit.aes = FALSE) +
  geom_errorbar(data = pred_fulllog3p, aes(x = Treatment, ymin = lwr, ymax = upr)) +
  labs(title ="Plot of predicted probabilities of Treatment effects",
       x = "Modelled on logit 6, for non-voters from PK voievodship; all other values at median", 
       y = "Predicted probability of voting for a populist party",
       caption = "Method: Logit with 90% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election.") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  theme_bw()

##########################

log1 <- glm(data=db, subset=(Voted==1), weights = weights_vot, family=binomial(), 
            PopBinary ~ TreatmentA + Sex + Age + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              TreatmentA:NationalID)
log1a <- logitmfx(formula = log1, data = db, atmean = FALSE, robust = TRUE)

# Marginal effects plots for significant interactions
emp <- unique(db$NationalID) # Define x axis

# Define model
coefficients <- log1a$mfxest[,1]
se <- log1a$mfxest[,2]
coefs <- NULL
ci_upper <- NULL
ci_lower <- NULL

# Populate model with coefs and CIs
for(j in 1:length(emp)) {
  
  coefs <- append(coefs, coefficients["TreatmentA"] + 
                    coefficients["TreatmentA:NationalID"]*emp[j])
  
  ci_upper <- append(ci_upper, coefs[j] + 1.96*sqrt(se["TreatmentA"] + 
                                                   se["TreatmentA:NationalID"]*emp[j]^2 +
                                                   2*emp[j]*se["TreatmentA:NationalID"]))
  
  ci_lower <- append(ci_lower, coefs[j] - 1.96*sqrt(se["TreatmentA"] + 
                                                   se["TreatmentA:NationalID"]*emp[j]^2 +
                                                   2*emp[j]*se["TreatmentA:NationalID"]))
  
}

# Combine to DF
MEa <- as.data.frame(cbind(coefs, ci_upper, ci_lower, emp))
MEa <- tibble::rownames_to_column(MEa)
MEa <- MEa %>% 
  mutate(Treatment = rowname)
MEa$rowname <- NULL


##################################################

log2 <- glm(data=db, subset=(Voted==1), weights = weights_vot, family=binomial(), 
            PopBinary ~ TreatmentB + Sex + Age + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              TreatmentB:NationalID)
log2b <- logitmfx(formula = log2, data = db, atmean = FALSE, robust = TRUE)

# Marginal effects plots for significant interactions
emp <- unique(db$NationalID) # Define x axis

# Define model
coefficients <- log2b$mfxest[,1]
se <- log2b$mfxest[,2]
coefs <- NULL
ci_upper <- NULL
ci_lower <- NULL

# Populate model with coefs and CIs
for(j in 1:length(emp)) {
  
  coefs <- append(coefs, coefficients["TreatmentB"] + 
                    coefficients["TreatmentB:NationalID"]*emp[j])
  
  ci_upper <- append(ci_upper, coefs[j] + 1.96*sqrt(se["TreatmentB"] + 
                                                      se["TreatmentB:NationalID"]*emp[j]^2 +
                                                      2*emp[j]*se["TreatmentB:NationalID"]))
  
  ci_lower <- append(ci_lower, coefs[j] - 1.96*sqrt(se["TreatmentB"] + 
                                                      se["TreatmentB:NationalID"]*emp[j]^2 +
                                                      2*emp[j]*se["TreatmentB:NationalID"]))
  
}

# Combine to DF
MEb <- as.data.frame(cbind(coefs, ci_upper, ci_lower, emp))
MEb <- tibble::rownames_to_column(MEb)
MEb <- MEb %>% 
  mutate(Treatment = rowname)
MEb$rowname <- NULL

##################################################

log3 <- glm(data=db, subset=(Voted==1), weights = weights_vot, family=binomial(), 
            PopBinary ~ TreatmentC + Sex + Age + Education + Income + PolInt + NationalID + LeftRight + PiD + Province + Party2015 +
              TreatmentC:NationalID)
log3c <- logitmfx(formula = log3, data = db, atmean = FALSE, robust = TRUE)

# Marginal effects plots for significant interactions
emp <- unique(db$NationalID) # Define x axis

# Define model
coefficients <- log3c$mfxest[,1]
se <- log3c$mfxest[,2]
coefs <- NULL
ci_upper <- NULL
ci_lower <- NULL

# Populate model with coefs and CIs
for(j in 1:length(emp)) {
  
  coefs <- append(coefs, coefficients["TreatmentC"] + 
                    coefficients["TreatmentC:NationalID"]*emp[j])
  
  ci_upper <- append(ci_upper, coefs[j] + 1.96*sqrt(se["TreatmentC"] + 
                                                      se["TreatmentC:NationalID"]*emp[j]^2 +
                                                      2*emp[j]*se["TreatmentC:NationalID"]))
  
  ci_lower <- append(ci_lower, coefs[j] - 1.96*sqrt(se["TreatmentC"] + 
                                                      se["TreatmentC:NationalID"]*emp[j]^2 +
                                                      2*emp[j]*se["TreatmentC:NationalID"]))
  
}

# Combine to DF
MEc <- as.data.frame(cbind(coefs, ci_upper, ci_lower, emp))
MEc <- tibble::rownames_to_column(MEc)
MEc <- MEc %>% 
  mutate(Treatment = rowname)
MEc$rowname <- NULL

#####################################################

# plot

df <- rbind(MEa, MEb, MEc)

ggplot(data = df, aes(x = emp, y = coefs, fill = Treatment)) +
  geom_line(data = df, aes(x = emp, y = coefs, group = Treatment)) +
  geom_point(data = df, aes(shape = Treatment)) +
  geom_ribbon(aes(x = emp, ymin=ci_lower, ymax=ci_upper, group = Treatment), alpha=0.3) + 
  geom_hline(yintercept=0) + 
  labs(title ="Marginal effects plot of interaction Treatment:NationalID",
       subtitle = "Modelled on results from Logit model (6)",
       x = "Degree of national identification", 
       y = "ME of voting for a populist party",
       caption = expression(atop("Method: Logit with 95% CI. Target population: Polish citizens eligible to vote as of 2015 parliamentary election."))) +
  #scale_fill_discrete(breaks=c("Treatment A","Treatment B","Control")) +
  #scale_color_discrete(breaks=c("Treatment A","Treatment B","Control")) +
  scale_fill_grey(start = 0, end = .6) +
  scale_color_grey(start = 0, end = .6) +
  theme_bw()
