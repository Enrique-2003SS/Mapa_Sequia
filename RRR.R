library(sf)
Base=sf::read_sf("../Enrique(Practicas)/P1_Sequías/Mapa_Sequia/Sequias2007_2025.geojson")
sf::sf_use_s2(F)

#Hay que pasarla a un sistema en metros
Base_Metrica=st_transform(Base,3857)

#Ahora sí simplificamos
New=st_simplify(x = Base_Metrica,preserveTopology = T,dTolerance = 150)

#Regresamos
Base2=st_transform(New,4326)
st_write(Base2 , "../Enrique(Practicas)/P1_Sequías/Mapa_Sequia/Sequias_SimplificadoadT150.geojson", driver = "GeoJSON")
