
############################################
## Create and display polygons on a map
##
## 5 april 2018
##
## Xavier Prudent - xprudent@civilia.ca
############################################


source("commodites.R")
source("polygon_decoupageCivilia.R")


## Read OD survey
d0 <- readRDS(file="data/enquete_OD_2015/sag15pv1.rds")
setDT(d0)

## Motored trips no way back
d1 <- d0[mode1 %in% motor | mode2 %in% motor | mode3 %in% motor]
d1 <- d1[motif != 12]
d1 <- d1[,c("cledeplacement","xlondes","ylatdes","xlonori","ylatori","ghredep","facdep","motif_gr")]
d1 <- unique(d1) 

## Plot the splitting of the city
create.polygone()
map <- plot.decoupage.Civilia()

## OD plus map
map %>% addCircles(d1$xlondes,d1$ylatdes,color="orange")
map %>% addCircles(d1$xlonori,d1$ylatori,color="chartreuse")
map %>% addCircles(d1$xlonori,d1$ylatori,color="brown") %>%addCircles(d1$xlondes,d1$ylatdes,color="brown")

## Assign trips to zones
d1[,smori := NA ]
d1[,smdes := NA ]

for( i in 1:length(zone.list)){
  ## Dataframe and name for that zone
  df <- zone.list[[i]]
  cdf <- comment(df)
  ## Zones for O-D
  d1[,smdes := ifelse(point.in.polygon(ylatdes, xlondes, df$lat, df$lon, mode.checked=FALSE)==1, cdf, smdes)]
  d1[,smori := ifelse(point.in.polygon(ylatori, xlonori, df$lat, df$lon, mode.checked=FALSE)==1, cdf, smori)]
}

## Remove NA
d1 <- d1[!is.na(smori) & !is.na(smdes)]

## Crée la matrice OD
sm.n <<- length(zone.list)
odmat.cree.plot(data=d1,list.sm=zone.names,title="")

## Save as csv
d2 <- d1 %>% 
  group_by(smori,smdes) %>% 
  dplyr::summarise(freq=round(sum(facdep))) %>%
  as.data.frame()
write.csv(d2,file="out/enqueteOD_voyMotor_tousMotifsSaufRetour.csv",row.names = F,quote = F)
