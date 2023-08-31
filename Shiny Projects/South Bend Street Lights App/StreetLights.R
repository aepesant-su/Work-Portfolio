#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(tigris)
library(revgeo)

#loading data
street_data <- read_csv("Street_Lights.csv")
#making spatial data
street_data.spatial <- street_data %>% #projecting the table as an sf and setting the coordinate system
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

#generate 
zips_sf <- zctas(cb = T, starts_with = "466", class = "sf") %>%
  select(zip = ZCTA5CE20, geometry)  %>% 
  st_set_crs(value = 4326)
zipPal <- colorFactor(palette = 'Set2', domain = zips_sf$zip)

#cleaning lumens column
street_data.spatial$Lumens <- gsub("[^0-9.]", "", street_data.spatial$Lumens)
street_data.spatial$Lumens <- ifelse(street_data.spatial$Lumens == "500000","50000",
                                     street_data.spatial$Lumens)

street_data.spatial$Lumens <- ifelse(street_data.spatial$Lumens == "",NA,
                                     street_data.spatial$Lumens)

street_data.spatial$Lumens <- ifelse(street_data.spatial$Lumens == "95000","9500",
                                     street_data.spatial$Lumens)

street_data.spatial$Lumens <- as.numeric(street_data.spatial$Lumens)

#making scale range for color pallete
scale_range <- c(500, 50000)
pal_lum <- colorNumeric("RdBu", domain = scale_range)

#creating popup
street_data.spatial$popup <- paste("Pole Type: ",street_data.spatial$Pole_Type,"<br>",
                             "Lumens: ",street_data.spatial$Lumens,"<br>")

idx <- sf::st_intersects(street_data.spatial, zips_sf)
idx[sapply(idx, function(x) length(x)==0L)] <- NA
street_data.spatial$zip <- zips_sf$zip[unlist(idx)]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Street Lights"),

        # Show a plot of the generated distribution
    mainPanel(leafletOutput(outputId = "mymap")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>%
        addPolygons(data = zips_sf, popup = ~zip, opacity = 1, fillOpacity = 0.5, weight = 1, color = ~zipPal(zip)) %>%
        addCircleMarkers(data = street_data.spatial, stroke = 0, fillOpacity = 1,
                         radius = 4, color = ~pal_lum(Lumens), popup = ~popup) %>%
        addLegend(pal = pal_lum, values = scale_range, position = "bottomright",
                  title = "Lumens")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
