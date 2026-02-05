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
  #Año=2024
  Meses=list.dirs(path = paste0("SHAPES SEQUIAS/",Año,"/"),recursive = TRUE,full.names = TRUE)
  Meses = Meses[-1]
  # for(i in 1:length(Meses)){
  # Meses[i] = strsplit(Meses[i],"/")[[1]][3] 
  # }
  for(Mes in Meses){
    # Año="2007"
    # Mes="01"
    secuenia=paste0(Mes,"/")
    # Names=list.files(path = "SHAPES SEQUIAS/2007/mexseq_200701/", pattern = ".shp$", full.names = T, recursive = T)
    Names=list.files(path = secuenia, pattern = ".shp$", full.names = T, recursive = T)
    Shapes=Names |> lapply(read_sf) |> lapply(ChangeCRS)
    for(i in 1:length(Shapes)){
      Shapes[[i]]$Archivo=rep(Names[[i]],nrow(Shapes[[i]]))
      Shapes[[i]]$Colorear=vector(mode = "character", length = nrow(Shapes[[i]]))
      Shapes[[i]]$Colorear[grepl("d0",tolower(Shapes[[i]]$Archivo))]="d0"
      Shapes[[i]]$Colorear[grepl("d1",tolower(Shapes[[i]]$Archivo))]="d1"
      Shapes[[i]]$Colorear[grepl("d2",tolower(Shapes[[i]]$Archivo))]="d2"
      Shapes[[i]]$Colorear[grepl("d3",tolower(Shapes[[i]]$Archivo))]="d3"
      Shapes[[i]]$Colorear[grepl("d4",tolower(Shapes[[i]]$Archivo))]="d4"
      Shapes[[i]]$Año=rep(Año,nrow(Shapes[[i]]))
      Shapes[[i]]$Mes=rep(substr(strsplit(basename(Mes),"_")[[1]][2],5,7),nrow(Shapes[[i]]))
      Shapes[[i]]$Dia=rep(strsplit(Mes,"_")[[1]][3],nrow(Shapes[[i]]))
    }
    Pre=(bind_rows(Shapes) |> dplyr::select(Colorear,Año,Mes,Dia,geometry))
    Lista_shapes[[k]]=Pre[Pre$Colorear!="",]
    k=k+1
  }
}

Data=do.call(plyr::rbind.fill,Lista_shapes)
Base=st_cast(st_as_sf(Data),to = "MULTIPOLYGON")
Base = Base |> st_make_valid()
colnames(Base)[1]="Nivel_Sequia"
st_write(Base,"Sequias2007_2025.shp")

#No se porque tarda demasiado
st_write(Base, "Sequias2007_2025.geojson", driver = "GeoJSON")

#Aquí acaba el programa donde creamos el compilado
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


################################################################################
A=sf::read_sf("Sequias_Smply.geojson")


#Crear Mapa con linea temporal
Base$time=as.numeric(paste0(Base$Año,Base$Mes))

Prueba=Base[c(1:451),]
Prueba2=st_make_valid(Prueba)
leaflet() %>%
  addTiles() %>%
  addTimeslider(
    data = Prueba,
    options = timesliderOptions(
      position = "topright",
      timeAttribute = "time",
      range = TRUE
    )
  )





A=Lista_shapes[[380]]
B=Lista_shapes[[381]]
leaflet() |> addTiles() |> addPolygons(data=A,color=paleta_categorias(A$Colorear),popup = A$Colorear,group="2007_01"
)|> addPolygons(data=B,color=paleta_categorias(B$Colorear),popup = B$Colorear,group="2007_02"
) |> addLayersControl(baseGroups = c("2007_01","2007_02")) |>
  htmlwidgets::onRender("
    function(el, x) {

      // 1. Crear TimeDimension global
      var timeDimension = new L.TimeDimension({
        period: 'PT1M'
      });

      this.timeDimension = timeDimension;

      // 2. Crear reproductor
      var player = new L.TimeDimension.Player({
        transitionTime: 500,
        loop: true
      }, timeDimension);

      // 3. Control visual
      var timeControl = new L.Control.TimeDimension({
        player: player,
        timeDimension: timeDimension,
        position: 'bottomleft'
      });

      this.addControl(timeControl);
    }
  ")

#leaflet() |> addTiles() |> addPolygons(data=A |> dplyr::filter(st_geometry_type(geometry)=='MULTIPOLYGON')) |> addMarkers(data=A |> dplyr::filter(st_geometry_type(geometry)=='POINT'),)