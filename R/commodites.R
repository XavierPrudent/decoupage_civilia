
#######################################
## Carte du Saguenay 
coord <<- data.frame(lon=-71.06849,lat=48.42805)

#######################################
## Carte des secteurs municipaux
sm <<- readOGR(dsn="/Users/lavieestuntoucan/Civilia/projets/Saguenay/data/enquete OD 2015/SM21_Sag_2015.TAB",verbose=FALSE)
sm <<- spTransform(sm, CRS("+proj=longlat +datum=WGS84"))
sm <<- fortify(sm)
sm$group <- as.numeric(sm$group)

#######################################
## Motorised transports
motor <<- c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12)

#######################################
## Colors
blue.sg <<- "#2B4591"
green.sg <<- "#49842A"
red.sg <<- "#B82432"
myPalette <<- colorRampPalette(brewer.pal(10, "Spectral"))

#######################################
## Plot une matrice
odmat.plot <- function(mat,title){
  min.val <- min(mat$freq,na.rm=T)
  max.val <- max(mat$freq,na.rm=T)
  med.val <- (min.val + max.val)/2
  
  #mat$freq <-round(mat$freq)
  
  g <- ggplot(data = mat, aes(x=smdes, y=smori, fill = freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = freq),size=2.5) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "orange", 
                         midpoint = med.val, limit = c(min.val,max.val), space = "Lab", 
                         name="# Dépl.") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 1,size = 8, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 1,size = 8, hjust = 1)) +
    coord_fixed() + 
    labs(x="Destination",y="Origine") +
    ggtitle(title)
  
  ## Turn into plotly
  ggplotly(g,width=750,height=750,tooltip = c("x","y","freq"))
  # scale_x_continuous(breaks=seq(1:sm.n) ) +
  #   scale_y_continuous(breaks=seq(1:sm.n) ) +
  #   
}

#######################################
## Cree et plot les matrices
odmat.cree.plot <- function(data, title, list.sm){
  ## Cree la matrice
  sm.mat <- data %>% 
    group_by(smori,smdes) %>% 
    dplyr::summarise(freq=round(sum(facdep))) %>%
    as.data.frame()
  #  sm.mat <- sm.mat %>% mutate(freq = ifelse(smori == smdes,NA,freq))
  
  ## Pour les déplacements manquants, mettre à zéro
  for( i in list.sm ){
    for( j in list.sm ){
      nij <- sm.mat %>% filter(smori==i & smdes==j) %>% nrow()
      if( nij  == 0 ){
        sm.ij <- data.frame(smori=i,smdes=j,freq=0)
        sm.mat <- rbind(sm.mat,sm.ij) 
      }
    }
  }
  
  ## Matrix complète
  odmat.plot(sm.mat, title)
}
#####
#######################################
## Plot une matrice
w.odmat.plot <- function(mat,title,leg,dg){
  min.val <- min(mat$w.val,na.rm=T)
  max.val <- max(mat$w.val,na.rm=T)
  med.val <- (min.val + max.val)/2
  
  ggplot(data = mat, aes(x=smdes, y=smori, fill = w.val)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(w.val,digits=dg)),size=2.5) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "orange", 
                         midpoint = med.val, limit = c(min.val,max.val), space = "Lab", 
                         name=leg) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 1,size = 8, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 1,size = 8, hjust = 1)) +
    coord_fixed() + 
    labs(x="Destination",y="Origine") +
    ggtitle(title)
}
  
#######################################
## Cree et plot les matrices avec moyennes ponderées
w.odmat.cree.plot <- function(sm.mat, title, leg, list.sm,dg){
  
  ## Pour les déplacements manquants, mettre à zéro
  for( i in list.sm ){
    for( j in list.sm ){
      nij <- sm.mat %>% filter(smori==i & smdes==j) %>% nrow()
      if( nij  == 0 ){
        sm.ij <- data.frame(smori=i,smdes=j,w.val=NA)
        sm.mat <- rbind(sm.mat,sm.ij) 
      }
    }
  }
  
  ## Matrix complète
  w.odmat.plot(sm.mat, title, leg,dg)
}


#######################################
## Plot un histo de facteurs
plot.hist.fact <- function(data, x.lab, x.var, w){
  ggplot(data,aes_string(x=x.var, weight = w)) + 
    geom_histogram(fill=blue.sg, stat = "count")  + 
    xlab(x.lab) +
    ylab("Nombre de personnes") 
}  

#######################################
## Plot un histo de numeriques
plot.hist.num <- function(data, x.lab, x.var, w, bins){
  ggplot(data,aes_string(x=x.var, weight = w)) + 
    geom_histogram(fill=blue.sg)  + 
    xlab(x.lab) +
    ylab("Nombre de personnes") +
    scale_x_continuous(breaks=bins)
}  

#######################################
## min med and max
get.min.med.max <- function(x){
  val <- c("","","")
  val[1] <- min(x,na.rm = T)
  val[2] <- max(x,na.rm = T)
  val[3] <- (as.numeric(val[1]) + as.numeric(val[2]))/2
  return(as.numeric(val))
}

#######################################
## Compute barycenters
get.bary <- function(data,type="ori",rad=2500,max_n.cl=3){
  
  # data = d1
  # type="ori"
  # rad=2500
  # max_n.cl = 1
  # 
  ## Quel type de points
  if( type == "ori" ){
    x <- "xlonori"
    y <- "ylatori"
  }
  if( type == "des" ){
    x <- "xlondes"
    y <- "ylatdes"
  }
  
  ## Chicoutimi
  x.min <<- min(c(data$xlondes,data$xlonori))
  x.max <<- max(c(data$xlondes,data$xlonori))
  y.min <<- min(c(data$ylatdes,data$ylatori))
  y.max <<- max(c(data$ylatdes,data$ylatori))
  
  ## One degree for the latitude of Saguenay (48.41) in meter
  lat2m <- 111198.26
  lon2m <- 74032.11
  rad.lat <- rad / lat2m
  rad.lon <- rad / lon2m
  
  ## Grid
  grid.lat <<- seq(min(data[,y]),max(data[,y])+rad.lat,by=rad.lat)
  grid.lon <<- seq(min(data[,x]),max(data[,x])+rad.lon,by=rad.lon)
  
  ## Info sur les barycentres
  data$cl.id  <- NA
  data$cl.lon <- NA
  data$cl.lat <- NA
  
  ## Calcul des barycentes
  set.seed(1)
  n.cl.tot <- 0
  for( i in 2:length(grid.lat)){
    for( j in 2:length(grid.lon)){
      
      ## Selectionne la région
      d2 <- data %>% filter(
        get(y,envir=as.environment(data)) >= grid.lat[i-1] & 
          get(y,envir=as.environment(data)) < grid.lat[i] & 
          get(x,envir=as.environment(data)) >= grid.lon[j-1] & 
          get(x,envir=as.environment(data)) < grid.lon[j])
      
      ## Nombre de points distincts
      n.distinct <- d2 %>% 
        select(one_of(c(x,y))) %>% 
        distinct(get(x,envir=as.environment(d2)),get(y,envir=as.environment(d2))) %>% 
        nrow()
      
      ## Garde les régions avec un minimum de points distintcs
      if( nrow(d2) == 0 | n.distinct == 0) next
      if( nrow(d2) > max_n.cl & n.distinct > max_n.cl ) n.cl <- max_n.cl else n.cl <- 1
      
      ## Algo de clustering
      km <- kmeans(cbind(d2[,y], d2[,x]), centers = n.cl, iter.max = 100 )
      
      ## Coord des clusters
      km.coord <- as.data.frame(km$centers)
      
      ## ID du cluster
      id <- n.cl.tot + 1
      
      ## Ajoute les info sur les clusters
      data[which(data$cledeplacement %in% d2$cledeplacement),"cl.id"] <- id
      data[which(data$cledeplacement %in% d2$cledeplacement),"cl.lat"] <- km.coord$V1
      data[which(data$cledeplacement %in% d2$cledeplacement),"cl.lon"] <- km.coord$V2
      
      ## Nombre de clusters
      n.cl.tot <- n.cl.tot + 1
    }
  }
  
  ## Output
  return(data)
}





######################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

####################################
## Create a map of the flux
create.map <- function(data, cond){
  
  ## Sélection sur la période
  if( cond == "24h") d2 <- data
  if( cond == "6-9") d2 <- filter(data, ghredep == 1) 
  if( cond == "9-15") d2 <- filter(data, ghredep == 2) 
  if( cond == "15-18") d2 <- filter(data, ghredep == 3) 
  if( cond == "18-24") d2 <- filter(data, ghredep == 4) 
  if( cond == "24-6") d2 <- filter(data, ghredep == 5) 
  
  d1.freq.top <- d2 %>% 
    group_by(cl.ori,cl.des) %>% 
    dplyr::summarise(freq=sum(facdep)) %>% 
    arrange(desc(freq)) %>% 
    head(n.voyages) %>%
    inner_join(d1.ori.cl,by="cl.ori") %>%
    inner_join(d1.des.cl,by="cl.des") 
  
  ## Clusters d'origine
  map1 <- addCircles(map.city,
                     d1.freq.top$cl.lon.ori,
                     d1.freq.top$cl.lat.ori,
                     stroke=FALSE,
                     color="black",
                     radius=40,
                     fillOpacity=1, 
                     label=paste0("Origine ",d1.freq.top$cl.ori),
                     group = "origine (bary)") 
  
  ## Clusters de destination
  map1 <- addCircles(map1,d1.freq.top$cl.lon.des,
                     d1.freq.top$cl.lat.des,
                     stroke=FALSE,
                     color="darkorange",
                     radius=40,
                     fillOpacity=1, 
                     label=paste0("Destination ",d1.freq.top$cl.des),
                     group = "destination (bary)") 
  
  ## Boucle sur les plus gros flux
  for(i in 1:nrow(d1.freq.top)){
    i.lat <- as.numeric(d1.freq.top[i, c("cl.lat.ori", "cl.lat.des")])
    i.lon <- as.numeric(d1.freq.top[i, c("cl.lon.ori", "cl.lon.des")])
    i.freq <- as.numeric(round(d1.freq.top[i,"freq"],digits=0))
    
    i.ori <- d1.freq.top[i,"cl.ori"]
    i.des <- d1.freq.top[i,"cl.des"]
    ## Couleur 2 tons, au dessus/dessous de la moyenne
    i.col <- ifelse(i.freq <= 50, "blue", 
                    ifelse(i.freq>50 & i.freq<=100,"green",
                           ifelse(i.freq>100 & i.freq<=150,"orange",
                                  ifelse(i.freq>150 & i.freq<=200,"red",
                                         ifelse(i.freq>200,"purple","black")))))
    map1 <- addPolylines(map1, lat = i.lat, 
                         lng = i.lon,
                         weight=5,
                         color = i.col,
                         highlightOptions = highlightOptions(color = "grey", weight = 8,bringToFront = TRUE),
                         label=paste0(i.ori," -> ",i.des," (",i.freq,")"),
                         group="Flux"
    )
  }
  return(map1)
}
