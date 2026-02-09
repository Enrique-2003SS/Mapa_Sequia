library(sf)
library(dplyr)
library(leaflet)
library(leafem)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras2)

#Cambiar crs
ChangeCRS=function(X){
  return(st_transform(X,crs=4326))
}

Lista_shapes=vector("list", length = 385)

#Tantos como carpetas en SHAPES SEQUIAS
Años=as.character(c(2007:2025))


k=1
for(Año in Años){
  Meses=list.dirs(path = paste0("SHAPES SEQUIAS/",Año,"/"),recursive = TRUE,full.names = TRUE)
  Meses = Meses[-1]
  for(Mes in Meses){
    secuenia=paste0(Mes,"/")
    Names=list.files(path = secuenia, pattern = ".shp$", full.names = T, recursive = T)
    Shapes=Names |> lapply(read_sf) |> lapply(ChangeCRS)
    for(i in 1:length(Shapes)){
      Shapes[[i]]$Archivo=rep(Names[[i]],nrow(Shapes[[i]]))
      Shapes[[i]]$Nivel_Sequia=vector(mode = "character", length = nrow(Shapes[[i]]))
      Shapes[[i]]$Nivel_Sequia[grepl("d0",tolower(Shapes[[i]]$Archivo))]="d0"
      Shapes[[i]]$Nivel_Sequia[grepl("d1",tolower(Shapes[[i]]$Archivo))]="d1"
      Shapes[[i]]$Nivel_Sequia[grepl("d2",tolower(Shapes[[i]]$Archivo))]="d2"
      Shapes[[i]]$Nivel_Sequia[grepl("d3",tolower(Shapes[[i]]$Archivo))]="d3"
      Shapes[[i]]$Nivel_Sequia[grepl("d4",tolower(Shapes[[i]]$Archivo))]="d4"
      Shapes[[i]]$Año=rep(Año,nrow(Shapes[[i]]))
      Shapes[[i]]$Mes=rep(substr(strsplit(basename(Mes),"_")[[1]][2],5,7),nrow(Shapes[[i]]))
      Shapes[[i]]$Dia=rep(strsplit(Mes,"_")[[1]][3],nrow(Shapes[[i]]))
    }
    Pre=(bind_rows(Shapes) |> dplyr::select(Nivel_Sequia,Año,Mes,Dia,geometry))
    Lista_shapes[[k]]=Pre[Pre$Nivel_Sequia!="",]
    k=k+1
  }
}

Data=do.call(plyr::rbind.fill,Lista_shapes)
Base=st_cast(st_as_sf(Data),to = "MULTIPOLYGON") |> st_make_valid()

################################################################################

#Ahora vamos a simplificarlo
Base=sf::read_sf("Sequias2007_2025.geojson")
sf::sf_use_s2(F)

#Hay que pasarla a un sistema en metros
Base_Metrica=st_transform(Base,3857)

#Ahora sí simplificamos
New=st_simplify(x = Base_Metrica,preserveTopology = T,dTolerance = 150)

#Regresamos
Base2=st_transform(New,4326)
st_write(Base2 , "Sequias_SimplificadoadT150.geojson", driver = "GeoJSON")

Base2$time=as.numeric(paste0(Base2$Año,Base2$Mes))
st_write(Base2 |> select(Nivel_Sequia,Año,Mes,time,Dia,geometry), "ParaPrueba_Mapa.geojson", driver = "GeoJSON")
