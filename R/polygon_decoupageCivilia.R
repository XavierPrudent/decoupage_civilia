
####################################
plot.decoupage.Civilia <- function(){
  
  ## Carte satellite
  map.city <- leaflet() %>% 
    addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%
    setView(coord$lon, coord$lat, zoom = 11)
  
  ## Zones
  for( i in 1:length(zone.list)){
    zone.df <- zone.list[[i]]
    if( i == 1 ) which.map <- map.city else which.map <- map1
    map1 <- addPolygons(which.map, 
                        lng=zone.list[[i]]$lon,
                        lat=zone.list[[i]]$lat,
                        opacity=1,
                        label=comment(zone.list[[i]]),
                        group="Zones")
  }
  
  ## Points
  coins$i <- 1:nrow(coins)
  map1 <- map1 %>% addCircles(coins$lon,coins$lat,color="red",label=as.character(coins$i))
  return(map1)
}

####################################
plot.decoupage.Civilia.gtfs <- function(){
  
  ## Saguenay GTFS
  stops <- fread("/Users/lavieestuntoucan/Civilia/projets/Saguenay/data/gtfs-static/gtfs-2017/stops.txt")
  routes <- fread("/Users/lavieestuntoucan/Civilia/projets/Saguenay/data/gtfs-static/gtfs-2017/shapes.txt")

    ## Carte satellite
  map.city <- leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% 
    setView(coord$lon, coord$lat, zoom = 11)
  
  ## Zones
  for( i in 1:length(zone.list)){
    zone.df <- zone.list[[i]]
    if( i == 1 ) which.map <- map.city else which.map <- map1
    map1 <- addPolygons(which.map, 
                        lng=zone.list[[i]]$lon,
                        lat=zone.list[[i]]$lat,
                        opacity=1,
                        highlightOptions = highlightOptions(color = "grey", 
                                                            weight = 8,
                                                            bringToFront = TRUE),
                        label=comment(zone.list[[i]]),
                        group="Zones")
  }
  
  ## Points
  for( i in 1:nrow(coins)){
    map1 <- addPopups(map1,
                      coins[i,]$lon,
                      coins[i,]$lat,
                      as.factor(i),
                      options = popupOptions(minWidth = 10, closeOnClick = FALSE, closeButton = FALSE),
                      group="Points de construction")
  }
  
  ## Add the gtfs 2017
  ## Lines
  for( rte in unique(routes$shape_id) ){
    shape <- routes %>% filter(shape_id == rte)  %>% distinct()
    map1 <- addPolylines(map1,
                         shape$shape_pt_lon,
                         shape$shape_pt_lat,
                         color = "orange",
                         opacity=1,
                         group = "Lignes de bus"
    )
  }
  
  ## Stops
  map1 <- addCircles(map1,
                     stops$stop_lon,
                     stops$stop_lat,
                     radius=20,
                     group="Arrêts de bus",
                     label=stops$stop_name,
                     color="red",
                     highlightOptions = highlightOptions(weight = 20,
                                                         col="darkorange",
                                                         bringToFront = TRUE)
  )
  
  ## Layer control
  map1 <- addLayersControl(map1,
                           overlayGroups = c("Zones","Arrêts de bus","Lignes de bus","Points de construction"),
                           options = layersControlOptions(collapsed = FALSE)) %>% hideGroup(c("Points de construction","Arrêts de bus","Lignes de bus"))
  
  ## Print map
  map1
}

####################################
create.polygone <- function(){
  ## Coins des polygones
  n <- 63
  coins <<- data.frame(lat = rep(x=NA,n), lon = rep(x=NA,n))
  coins[1,] <<- c(48.43008935405231, -71.07193152957763)
  coins[2,] <<- c(48.42593172234239, -71.0720173602661)
  coins[3,] <<- c(48.424821064525354,-71.08060042911376)
  coins[4,] <<- c(48.41661853231392, -71.09553496890868)
  coins[5,] <<- c(48.421375022984016,-71.10733668857421)
  coins[6,] <<- c(48.424166049819746, -71.10673587375487)
  coins[7,] <<- c(48.425960200428804, -71.09669368320311)
  coins[8,] <<- c(48.434673745351155, -71.09553496890868)
  coins[9,] <<- c(48.43401885763197, -71.08789603763427)
  coins[10,] <<- c(48.42527672195053, -71.0660735850891)
  coins[11,] <<- c(48.42707795271598, -71.06296222263182)
  coins[12,] <<- c(48.43048089430781, -71.06283347659911)
  coins[13,] <<- c(48.42934185973257, -71.0407320743164)
  coins[14,] <<- c(48.42401653441017, -71.04223411136473)
  coins[15,] <<- c(48.4260100370417, -71.05476539188231)
  coins[16,] <<- c(48.430110710871396, -71.03382270389403)
  coins[17,] <<- c(48.43534997924627, -71.02420966678466)
  coins[18,] <<- c(48.43096497627119, -71.01339500003661)
  coins[19,] <<- c(48.4155860017496, -71.02412383609618)
  coins[20,] <<- c(48.416326577522184, -71.03073279910888)
  coins[21,] <<- c(48.42304823349079, -71.03124778323973)
  coins[22,] <<- c(48.41857672233808, -71.04442279392089)
  coins[23,] <<- c(48.41213933411564, -71.05103175693358)
  coins[24,] <<- c(48.417095625560435, -71.05545203739013)
  coins[25,] <<- c(48.420855248508126, -71.05987231784667)
  coins[26,] <<- c(48.42209415424879,-71.07300441318358)
  coins[27,] <<- c(48.41785042065619,-71.0775105243286)
  coins[28,] <<- c(48.41340693739168,-71.06738250308837)
  coins[29,] <<- c(48.41060503821868,-71.07035670089721)
  coins[30,] <<- c(48.40764233704221,-71.07295307922362)
  coins[31,] <<- c(48.40457974815759,-71.07376847076415)
  coins[32,] <<- c(48.4076565812108,-71.0928872566223)
  coins[33,] <<- c(48.404038434199634,-71.09709296035766)
  coins[34,] <<- c(48.40671645752666,-71.10327276992797)
  coins[35,] <<- c(48.41076171472542,-71.10104117202758)
  coins[36,] <<- c(48.3979126379663,-71.10361609268188)
  coins[37,] <<- c(48.393495908634875,-71.07619318771361)
  coins[38,] <<- c(48.39566157866855,-71.07426199722289)
  coins[39,] <<- c(48.407033293709155,-71.06202475764161)
  coins[40,] <<- c(48.40441226739853,-71.0521542284668)
  coins[41,] <<- c(48.40501055704052,-71.04545943476563)
  coins[42,] <<- c(48.41042333391143,-71.04327075220948)
  coins[43,] <<- c(48.4105942543238,-71.04764811732178)
  coins[44,] <<- c(48.4005731118742,-71.06755437409669)
  coins[45,] <<- c(48.39250908008252,-71.04511469990842)
  coins[46,] <<- c(48.39011529110962,-71.0536119380676)
  coins[47,] <<- c(48.39111271686701,-71.06041402012937)
  coins[48,] <<- c(48.39627052094977,-71.05603665501707)
  coins[49,] <<- c(48.432524,-71.051009)
  coins[50,] <<- c(48.425228, -71.049524)
  coins[51,] <<- c(48.422050, -71.051949)
  coins[52,] <<- c(48.422634, -71.054534)
  coins[53,] <<- c(48.422601, -71.056200)
  coins[54,] <<- c(48.423484, -71.057230)
  coins[55,] <<- c(48.451108, -71.001333)
  coins[56,] <<- c(48.249969, -70.053076)
  coins[57,] <<- c(48.645301, -69.770178)
  coins[58,] <<- c(48.904138, -71.357702)
  coins[59,] <<- c(48.589752, -71.662573)
  coins[60,] <<- c(48.458224, -71.272335)
  coins[61,] <<- c(48.454581, -71.177578)
  coins[62,] <<- c(47.751250, -70.506725)
  coins[63,] <<- c(48.001791, -71.858044)
  
  
  ## zone 16
  zone16 <<- c(1,2,3,4,5,6,7,8,9)
  zone16 <<- coins[zone16,]
  comment(zone16) <<- "zone16"
  
  ## zone 11
  zone11 <<- c(3,2,26,27,32,33,34,35,4)
  zone11 <<- coins[zone11,]
  comment(zone11) <<- "zone11"
  
  ## zone Term
  zoneTerm <<- c(1,12,11,10,2)
  zoneTerm <<- coins[zoneTerm,]
  comment(zoneTerm) <<- "zoneTerm"
  
  ## zone 14a
  zone14a <<- c(2,10,11,15,54,25,26)
  zone14a <<- coins[zone14a,]
  comment(zone14a) <<- "zone14a"
  
  ## zone 14b
  zone14b <<- c(26,25,28,27)
  zone14b <<- coins[zone14b,]
  comment(zone14b) <<- "zone14b"
  
  ## zone 14c
  zone14c <<- c(27,28,29,30,31,32)
  zone14c <<- coins[zone14c,]
  comment(zone14c) <<- "zone14c"
  
  ## zone 2a
  zone2a <<- c(32,31,38,37,36,33)
  zone2a <<- coins[zone2a,]
  comment(zone2a) <<- "zone2a"
  
  ## zone Hop
  zoneHop <<- c(12,49,13,14,50,15,11)
  zoneHop <<- coins[zoneHop,]
  comment(zoneHop) <<- "zoneHop"
  
  ## zone Uni
  zoneUni <<- c(25,54,53,52,51,50,14,22,23,24)
  zoneUni <<- coins[zoneUni,]
  comment(zoneUni) <<- "zoneUni"
  
  ## zone 5b
  zone5b <<- c(25,28,29,39,23,24)
  zone5b <<- coins[zone5b,]
  comment(zone5b) <<- "zone5b"
  
  ## zone 15b
  zone15b <<- c(29,39,44,38,31,30)
  zone15b <<- coins[zone15b,]
  comment(zone15b) <<- "zone15b"
  
  ## zone 12
  zone12 <<- c(13,16,17,18,19,20,21,14)
  zone12 <<- coins[zone12,]
  comment(zone12) <<- "zone12"
  
  ## zone 10
  zone10 <<- c(14,21,20,42,43,23,22)
  zone10 <<- coins[zone10,]
  comment(zone10) <<- "zone10"
  
  ## zone 2d
  zone2d <<- c(23,43,40,39)
  zone2d <<- coins[zone2d,]
  comment(zone2d) <<- "zone2d"
  
  ## zone 2c
  zone2c <<- c(44,39,40,48)
  zone2c <<- coins[zone2c,]
  comment(zone2c) <<- "zone2c"
  
  ## zone 2b
  zone2b <<- c(44,48,47,37,38)
  zone2b <<- coins[zone2b,]
  comment(zone2b) <<- "zone2b"
  
  ## zone 5a
  zone5a <<- c(40,41,42,43)
  zone5a <<- coins[zone5a,]
  comment(zone5a) <<- "zone5a"
  
  ## zone 15a
  zone15a <<- c(40,41,45,46,47,48)
  zone15a <<- coins[zone15a,]
  comment(zone15a) <<- "zone15a"
  
  ## zone cegep
  zoneCegep <<- c(15,50,51,52,53,54)
  zoneCegep <<- coins[zoneCegep,]
  comment(zoneCegep) <<- "zoneCegep"
  
  ## zone nord
  zoneNord <<- c(8,9,1,12,49,13,16,17,55,56,57,58,59,60,61)
  zoneNord <<- coins[zoneNord,]
  comment(zoneNord) <<- "zoneNord"
  
  ## zone sud
  zoneSud <<- c(59,60,61,8,7,6,5,4,35,34,33,36,37,47,46,45,41,42,20,19,18,17,55,56,62,63)
  zoneSud <<- coins[zoneSud,]
  comment(zoneSud) <<- "zoneSud"

    ############
  ## Loop sur les zones
  zone.list <<- list(zone16,zone11,
                     zoneTerm,zone14a,
                     zone14b,zone14c,
                     zone2a,zoneHop,
                     zoneUni,zone5b,
                     zoneCegep,zoneNord,
                     zoneSud,
                     zone15b,zone12,
                     zone10,zone2d,
                     zone2c,zone2b,
                     zone5a,zone15a)
  zone.names <<- sapply(zone.list,comment)
  
}