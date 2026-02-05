library(sf)
library(dplyr)
library(leaflet)
library(leafem)
library(leaftime)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(htmlwidgets)
#setwd("../Enrique(Practicas)/P1_Sequías/Mapa_Sequia/")

#Cargamos el GeoJSON con las geometrías simplificadas
sf_data=read_sf("Sequias_SimplificadoadT150.geojson")

#Para crear la línea del tiempo necesitamos una fecha de inicio y una de final
#Para ello necesitamos que este en formato Date como Año-Mes-Día (más concretamente AAAA-MM-DD)

#El código a continuación permite crear la fecha con días, pues la primera parte de los registro viene
#solo de manera mensual, mientras que la siguiente viene por quincenas
#Se toma en cuenta los años bisiestos
# sf_data$end=character(nrow(sf_data))
# sf_data$end[!is.na(sf_data$Dia)]=paste0(sf_data$Año[!is.na(sf_data$Dia)],"-",sf_data$Mes[!is.na(sf_data$Dia)],"-",sf_data$Dia[!is.na(sf_data$Dia)])
# 
# m30=c("04","06","09","11")
# m31=c("01","03","05","07","08","10","12")
# bis=c("2008","2012","2016","2020","2024")
# 
# sf_data$end[is.na(sf_data$Dia) & sf_data$Mes %in% m30]=paste0(sf_data$Año[is.na(sf_data$Dia) & sf_data$Mes %in% m30],
#                                                                 "-",
#                                                                 sf_data$Mes[is.na(sf_data$Dia) & sf_data$Mes %in% m30],
#                                                                 "-30")
# sf_data$end[is.na(sf_data$Dia) & sf_data$Mes %in% m31]=paste0(sf_data$Año[is.na(sf_data$Dia) & sf_data$Mes %in% m31],
#                                                                 "-",
#                                                                 sf_data$Mes[is.na(sf_data$Dia) & sf_data$Mes %in% m31],
#                                                                 "-31")
# 
# sf_data$end[is.na(sf_data$Dia) & sf_data$Mes == "02" & !(sf_data$Año %in% bis)]=paste0(sf_data$Año[is.na(sf_data$Dia) & sf_data$Mes == "02" & !(sf_data$Año %in% bis)],
#                                                                                          "-",
#                                                                                          sf_data$Mes[is.na(sf_data$Dia) & sf_data$Mes == "02" & !(sf_data$Año %in% bis)],
#                                                                                          "-28")
# 
# sf_data$end[is.na(sf_data$Dia) & sf_data$Mes == "02" & sf_data$Año %in% bis]=paste0(sf_data$Año[is.na(sf_data$Dia) & sf_data$Mes == "02" & sf_data$Año %in% bis],
#                                                                                          "-",
#                                                                                          sf_data$Mes[is.na(sf_data$Dia) & sf_data$Mes == "02" & sf_data$Año %in% bis],
#                                                                                          "-29")
# 

############
sf_data$start=character(nrow(sf_data))
sf_data$start[is.na(sf_data$Dia)]=paste0(sf_data$Año[is.na(sf_data$Dia)],"-",sf_data$Mes[is.na(sf_data$Dia)],"-01")
sf_data$start[sf_data$Dia =="15" & !is.na(sf_data$Dia)]=paste0(sf_data$Año[sf_data$Dia =="15" & !is.na(sf_data$Dia)],
                                                               "-",
                                                               sf_data$Mes[sf_data$Dia =="15" & !is.na(sf_data$Dia)],
                                                               "-01")
sf_data$start[sf_data$Dia %in% as.character(c(28:31))]=paste0(sf_data$Año[sf_data$Dia %in% as.character(c(28:31))],
                                                              "-",
                                                              sf_data$Mes[sf_data$Dia %in% as.character(c(28:31))],
                                                              "-16")
############
# fechas=c(unique(sf_data$start),"2026-01-01")
# sf_data$end=character(nrow(sf_data))
# for(fecha in fechas){
#   sf_data$end[sf_data$start == fecha]=as.character(rep(fechas[which(fechas==fecha)+1],length(sf_data$end[sf_data$start == fecha])))
# }
sf_data$end=as.Date(sf_data$end)
sf_data$start=as.Date(sf_data$start)
sf_data=sf_data|>select(Nivel_Sequia,Año,Mes,Dia,start,end,geometry)

#Ahora creamos las categorías y la paleta de colores para la simbología de los mapas
categorias=c("(D0) Anormalmente Seco",
             "(D1) Sequía Moderada",
             "(D2) Sequía Severa",
             "(D3) Sequía Extrema",
             "(D4) Sequía Excepcional")
paleta_categorias=colorFactor(palette = c(rgb(255,255,0, maxColorValue = 255),
                                          rgb(255,211,127, maxColorValue = 255),
                                          rgb(230,152,0, maxColorValue = 255),
                                          rgb(230,0,0, maxColorValue = 255),
                                          rgb(115,0,0, maxColorValue = 255)),
                              domain =categorias,
                              alpha = T, na.color = NA)

#Ahora, aquí un problema; dada la cantidad de entradas, aparte de simplificarlo previamente
#habrá que partirlo en 4 periodos para poder subirlo a GitHub, de otra forma la línea del tiempo .html 
#pesaría más de 100MB, además de tardar mucho en cargar uno solo al momento de cargarlo en el index.html

#Partir por periodos 2007-2013,2014-2019,2020-2022 y 2023-2025
Auxiliar=sf_data
P1=c(2007:2013)
P2=c(2014:2019)
P3=c(2020:2022)
P4=c(2023:2025)
#La razón de que no tengan la misma cantidad de años es que en los últimos años hay más entradas,
#pues son quincenales

for(i in 2:2){
  sf_data=Auxiliar #Esto para tener constantemente todos los datos
  if(i==1){
    sf_data=sf_data[as.numeric(sf_data$Año) %in% P1,]
  }
  if(i==2){
    sf_data=sf_data[as.numeric(sf_data$Año) %in% P2,]
  }
  if(i==3){
    sf_data=sf_data[as.numeric(sf_data$Año) %in% P3,]
  }
  if(i==4){
    sf_data=sf_data[as.numeric(sf_data$Año) %in% P4,]
  }
  
  geojson = geojsonio::geojson_json(sf_data) #Para poderlos leer bien en la línea del tiempo
  #Creamos nuestro mapa como ya es usual
  Mapa_Sequias = leaflet(data = geojson) |>
    addTiles() |>
    setView(lng = -102.7482, lat = 24.48856, zoom = 5) |> #son las coordenadas aproximadas de la bbox del centro del primer mapa
    addLegend("topright", pal = paleta_categorias,values = categorias, title = "Intensidad de Sequías",opacity = 1
    ) |> #Aquí añadimos la linea del tiempo
    addTimeline(data = geojson,group = "timeline_layer", #Es una especie de "capa fantasma" para más fácil
                sliderOpts = sliderOptions(duration = 15000, showTicks = TRUE,formatOutput = htmlwidgets::JS("
                function(date) {
                    var d = new Date(date);
                    var year = d.getFullYear();
                    var month = ('0' + (d.getMonth() + 1)).slice(-2);
                    var day = ('0' + d.getDate()).slice(-2);
                    return year + '-' + month + '-' + day;
                }")),width = "65%",
                timelineOpts = timelineOptions(
                  style = htmlwidgets::JS(
                    "function(feature) {
          if (feature.properties.Nivel_Sequia === 'd4'){
            return { color: 'rgb(115,0,0)', weight: 3 };
          }else{
            if (feature.properties.Nivel_Sequia === 'd3'){
              return { color: 'rgb(230,0,0)', weight: 3 };
            }else{
              if (feature.properties.Nivel_Sequia === 'd2'){
                return { color: 'rgb(230,152,0)', weight: 3 };
              }else{
                if (feature.properties.Nivel_Sequia === 'd1'){
                  return { color: 'rgb(255,211,127)', weight: 3 };
                }else{
                  if (feature.properties.Nivel_Sequia === 'd0'){
                    return { color: 'rgb(255,255,0)', weight: 3 };
                  }
                }
              }
            }
          }
        }"
                  ) #Los colores son los que especifica en la base de datos
                )
    ) |> #Aqui viene lo feo
    onRender("function(el, x) {
      var map = this;
      var DateSearchControl = L.Control.extend({
        options: { position: 'bottomleft' },//Queda bien, no se sobreponen
        
        //Creamos la cajita para el buscador por fecha
        onAdd: function(map) {
          var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control leaflet-control-custom');
          container.style.backgroundColor = 'white';
          container.style.padding = '5px';
          var input = L.DomUtil.create('input', 'date-input-custom', container);
          input.type = 'text';
          input.placeholder = 'AAAA-MM-DD -> Enter';
          input.style.width = '170px';
          
          L.DomEvent.disableClickPropagation(container);
          L.DomEvent.on(input, 'keydown', function(e) {
             if (e.keyCode === 13) { // el keydown checa el enter, de código 13
                 //var fechaStr = input.value;
                 var fechaObj = new Date(input.value + 'T12:00:00'); //Esto es para evitar brincos por las zonas horarias 
                                                                     //y no afecta pues la hora no se va a mostrar en el slider
                 var fechaTime = fechaObj.getTime();
                 
                 //Para usar el formato correcto
                 if (isNaN(fechaTime)) {
                    alert('Use el formato: Año-Mes-Día para su búsqueda');
                    return;
                 }
                 
                 //Ya tenemos la fecha, ahora lo más feo todavía es encontrar el mapa con esa fecha y 
                 //actualizarlo junto a la barra del slider
                 //De otra forma se puede actualizar el mapa pero no la barra del slider, lo que 
                 //haría contraproducente el reproductor del slider, pues empezaría desde la última fecha
                 //en la barra y no en la que se acaba de buscar

                 // Para ello, buscamos el input de tipo 'range' dentro de los controles del mapa, donde
                 // el slider de tiempo suele ser el único input de rango
                 var sliderInput = document.querySelector('.leaflet-control input[type=\"range\"]');
                 

                     //Actualizamos el valor del slider con nuestra fechita
                     sliderInput.value = fechaTime;
                     
                     //Y esto es lo más técnico, pues necesitamos disparar dos eventos;
                     //uno para mover el mapa de la línea del tiempo
                     //y otro para confirmar que se movió el slider
                     //Que es la parte más fea que habíamos dicho
                     //Cargamos las balas de la magnum...
                     var eventInput = new Event('input', { bubbles: true });
                     var eventChange = new Event('change', { bubbles: true });
                     //y le disparamos a Nemesis (jeje)
                     sliderInput.dispatchEvent(eventInput);
                     sliderInput.dispatchEvent(eventChange);

             }
          });
          return container;
        }
      });
      
      map.addControl(new DateSearchControl());
    }
  ")|>
    addFullscreenControl()|>addLogo(img = "https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/Planeacion_sigeh.png",
                                    ,position = "bottomright",width = 250)
  Mapa_Sequias
  saveWidget(Mapa_Sequias, file = paste0("MapaSequias_Periodo_",sf_data$Año[1],"_",sf_data$Año[length(sf_data$Año)],".html"), selfcontained = T)
}





















Mapa_Sequias = leaflet(data = geojson) |>
  addTiles() |>
  setView(lng = -102.7482, lat = 24.48856, zoom = 5) |> #son las coordenadas aproximadas de la bbox del centro del primer mapa
  addLegend("topright", pal = paleta_categorias,values = categorias, title = "Intensidad de Sequías",opacity = 1
  ) |>
  addTimeline(data = geojson,group = "timeline_layer", #Es una especie de "capa fantasma" para más fácil
    sliderOpts = sliderOptions(duration = 15000, showTicks = TRUE,
      formatOutput = htmlwidgets::JS("
                function(date) {
                    var d = new Date(date);
                    var year = d.getFullYear();
                    var month = ('0' + (d.getMonth() + 1)).slice(-2);
                    var day = ('0' + d.getDate()).slice(-2);
                    return year + '-' + month + '-' + day;
                }")),width = "65%",
    timelineOpts = timelineOptions(
      style = htmlwidgets::JS(
        "function(feature) {
          if (feature.properties.Nivel_Sequia === 'd4'){
            return { color: 'rgb(115,0,0)', weight: 3 };
          }else{
            if (feature.properties.Nivel_Sequia === 'd3'){
              return { color: 'rgb(230,0,0)', weight: 3 };
            }else{
              if (feature.properties.Nivel_Sequia === 'd2'){
                return { color: 'rgb(230,152,0)', weight: 3 };
              }else{
                if (feature.properties.Nivel_Sequia === 'd1'){
                  return { color: 'rgb(255,211,127)', weight: 3 };
                }else{
                  if (feature.properties.Nivel_Sequia === 'd0'){
                    return { color: 'rgb(255,255,0)', weight: 3 };
                  }
                }
              }
            }
          }
        }"
      ) #Los colores son los que especifica en la base de datos
    )
  ) |> #Aqui viene lo feo
  onRender("function(el, x) {
      var map = this;
      var DateSearchControl = L.Control.extend({
        options: { position: 'bottomleft' },//Queda bien, no se sobreponen
        
        //Creamos la cajita para el buscador por fecha
        onAdd: function(map) {
          var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control leaflet-control-custom');
          container.style.backgroundColor = 'white';
          container.style.padding = '5px';

          var input = L.DomUtil.create('input', 'date-input-custom', container);
          input.type = 'text';
          input.placeholder = 'AAAA-MM-DD -> Enter';
          input.style.width = '170px';
          
          L.DomEvent.disableClickPropagation(container);
          L.DomEvent.on(input, 'keydown', function(e) {
             if (e.keyCode === 13) { // el keydown checa el enter, de código 13
                 //var fechaStr = input.value;
                 var fechaObj = new Date(input.value + 'T00:00:00'); //Esto es para evitar brincos por las zonas horarias 
                                                                     //y no afecta pues la hora no se va a mostrar en el slider
                 var fechaTime = fechaObj.getTime();
                 
                 //Para usar el formato correcto
                 if (isNaN(fechaTime)) {
                    alert('Use el formato: Año-Mes-Día para su búsqueda');
                    return;
                 }
                 
                 //Ya tenemos la fecha, ahora lo más feo todavía es encontrar el mapa con esa fecha y 
                 //actualizarlo junto a la barra del slider
                 //De otra forma se puede actualizar el mapa pero no la barra del slider, lo que 
                 //haría contraproducente el reproductor del slider, pues empezaría desde la última fecha
                 //en la barra y no en la que se acaba de buscar

                 // Para ello, buscamos el input de tipo 'range' dentro de los controles del mapa, donde
                 // el slider de tiempo suele ser el único input de rango
                 var sliderInput = document.querySelector('.leaflet-control input[type=\"range\"]');
                 
                 if (sliderInput) {
                     //Actualizamos el valor del slider con nuestra fechita
                     sliderInput.value = fechaTime;
                     
                     //Y esto es lo más técnico, pues necesitamos disparar dos eventos;
                     //uno para mover el mapa de la línea del tiempo
                     //y otro para confirmar que se movió el slider
                     //Que es la parte más fea que habíamos dicho
                     //Cargamos...
                     var eventInput = new Event('input', { bubbles: true });
                     var eventChange = new Event('change', { bubbles: true });
                     //y disparamos
                     sliderInput.dispatchEvent(eventInput);
                     sliderInput.dispatchEvent(eventChange);
                     
                     //console.log('¡Slider encontrado y movido vía DOM!');
                 } else {
                     // Si falla, intentamos mover la capa manualmente como respaldo
                     //console.log('Chanclas, no encontramos el slider');
                     //map.eachLayer(function(layer) {
                       // if (layer.setTime) {
                          // layer.setTime(fechaTime);
                        //}
                  //   });
                 }
             }
          });
          return container;
        }
      });
      
      map.addControl(new DateSearchControl());
    }
  ")|>
  addFullscreenControl() 
#Mapa_Sequias

saveWidget(Mapa_Sequias, file = "../Enrique(Practicas)/P1_Sequías/Mapa_Sequia/Try3.html", selfcontained = T)
