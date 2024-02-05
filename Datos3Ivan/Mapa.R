#Mapa de delitos y "cámaras"


#Cargar paquetes, yo ya los tengo por lo tanto no requiere instalación, en caso contrario instalar.

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(readxl)


#Base de datos

#gasolineras <- read_excel("gasolineras.xlsx")

escuelas1$escu <- 1

investigaciones1$investi <- 1


# Construir el espacio del mapa
ui <- fluidPage(
  mainPanel( 
    leafletOutput(outputId = "mymap")
  )
)

# Construir función de lanzamiento.
server <- function(input, output, session) {
  
  
  #definir colores para estaciones y TArs
  pal <- colorNumeric(
    palette = c('red', 'black'),
    domain = investigaciones1$investi) 
  
  #crear el mapa
  output$mymap <- renderLeaflet({
    leaflet(investigaciones1) %>% 
      setView(lng = -99, lat = 16, zoom = 5)  %>% #México es la vista (1 es global, 15 es una cuadra)
      addTiles() %>% 
      addCircles(data = investigaciones1, lat = ~ latitud, lng = ~ longitud, weight = 1, radius = ~sqrt(investi)*10, color = ~pal(investi), fillOpacity = 0.2)
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = investigaciones1)
    proxy %>% clearMarkers()
    
  })
}
#lanzar la aplicación
shinyApp(ui, server)

#medir la distancias
library(geosphere)

disTAR <- distm(c(gasolineras$longitud, gasolineras$latitud), c(lon = -99.84708, lat = 16.84064), fun = distHaversine)