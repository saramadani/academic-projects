##Tunisie Telecom base##
menages_selon_source_d_eau <- read.csv("/home/saara/Downloads/menages selon source d'eau.csv")
View(menages_selon_source_d_eau)
data=menages_selon_source_d_eau
attach(data)
Regions=as.character(Regions)
Nombre.menages=as.numeric(Nombre.menages)
Menages.loin..plus.que.1.km.de.la.plus.proche.source.d.eau.liee.au.SONEDE.ou.GR=as.numeric(Menages.loin..plus.que.1.km.de.la.plus.proche.source.d.eau.liee.au.SONEDE.ou.GR)                                                                 
codes_maps_tunisia=read.csv("/home/saara/Downloads/codes_maps_tunisia.csv", sep=";")
hasc=codes_maps_tunisia
View(hasc)
i=match(data$Regions,hasc$Gouvernorat)
View(i)
data$code=hasc$HASC_1[i]
##dataor=dataor[,-16]

##code leaflet##
library(leaflet)
library(sp)
library(raster)
tnMAP=getData(name = "GADM", country="TUN" , level = 1)
View(tnMAP)

i=match(tnMAP$HASC_1,data$code)
View(i)
tnMAP=cbind.Spatial(tnMAP,data[i,])
View(tnMAP@data)
tnMAP@data=tnMAP@data[,-14]
View(tnMAP@data)
names(tnMAP@data)

col=colorRampPalette(c( "#66ccff","#ff9900"))                                                                         

e=col(24) 


my_popup <- paste0("<strong>",tnMAP@data$Regions,"</strong>"," (",tnMAP@data$Nombre.menages,")")

MyPaletteColor <- colorBin("RdYlBu", domain=(-40):70,bins=11,na.color = "white")
tnMAP@data$Nombre.menages=as.numeric(tnMAP@data$Nombre.menages)

pal=colorNumeric(e,domain = tnMAP@data$Nombre.menages)
pal

leaflet(tnMAP)%>%addProviderTiles("CartoDB.Positron")%>% 
  addPolygons(stroke = FALSE , smoothFactor = 0.2 ,fillColor = ~ pal(tnMAP@data$Nombre.menages),
              color = "#BDBDC3", fillOpacity = 0.8 ,    popup = my_popup)%>%  
  addLegend(pal = pal , values = ~Nombre.menages, opacity = 1.5 , position = "bottomright" )

