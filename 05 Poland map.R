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
