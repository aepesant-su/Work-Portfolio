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
library(DT)
library(nngeo)
library(viridis) 

# Street Light Data

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
zipPal <- colorFactor(palette = c('red1','blue3','seagreen','brown','purple1','deeppink','darkorange','blueviolet',
                                  'coral','darkred','darkcyan',"aquamarine","salmon4"), 
                      domain = c("46628","46616","46619","46637","46617","46613","46618","46614","46601","46623",
                                 "46625","46615","46635"),
                      na.color = "black")

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

# Summary Table
idx <- sf::st_intersects(street_data.spatial, zips_sf)
idx[sapply(idx, function(x) length(x)==0L)] <- NA
street_data.spatial$zip <- zips_sf$zip[unlist(idx)]

#making summary table for street lights
dt_data <- street_data.spatial %>% group_by(zip) %>% 
  summarise(avg_lumens = round(mean(Lumens, na.rm = TRUE)), count = n()) %>% 
  na.omit() %>% arrange(-count) %>% select(zip, count, avg_lumens) %>%
  rename("Average Lumens" = "avg_lumens")

dt_data$geometry <- NULL

#making summary table for schools

schools <- st_read("School_Boundaries.shp", stringsAsFactors = FALSE) %>%
  st_set_crs(value = 4326)

idx1 <- sf::st_intersects(schools, zips_sf)
idx1[sapply(idx1, function(x) length(x)==0L)] <- NA
schools$zip <- zips_sf$zip[unlist(idx1)]

schools_dt <- schools %>% group_by(zip) %>%
  summarise(num_schools = n()) %>% rename("Number of Schools" = "num_schools")

schools_dt$geometry <- NULL

#making summary for code enforcement
enforcement <- read_csv("code_enforcement.csv")

enforcement_dt <- enforcement %>% group_by(Zip) %>%
  summarise(num_cases= n()) %>% rename("Number of Code Enforcement Cases" = "num_cases")

enforcement_dt$geometry <- NULL

#making summary for abandoned properties
properties <- st_read("Abandoned_Property_Parcels.shp")

properties_dt <- properties %>% group_by(Zip_Code) %>%
  summarise(num_abn_props= n()) %>% rename("Number of Abandoned Properties" = "num_abn_props")

properties_dt$geometry <- NULL

#cleaning dt tables to merge
library(tibble)
new <- c("46637")
schools_dt <- rows_insert(schools_dt, tibble(zip = new))

#combing schools and street lights
final_dt <- left_join(schools_dt, dt_data)

#adding zips do final and enforcement dts to combine
new1 <- c("46618","46623","46625")
new2 <- c(46635,46637)
final_dt <- rows_insert(final_dt, tibble(zip = new1))
enforcement_dt <- rows_insert(enforcement_dt, tibble(Zip = new2))
enforcement_dt$Zip <- as.character(enforcement_dt$Zip)
final_dt <- left_join(final_dt, enforcement_dt, by = c("zip" = "Zip"))

#combining properties to rest of data
properties_dt$Zip_Code <- as.character(properties_dt$Zip_Code)
final_dt <- left_join(final_dt, properties_dt, by = c("zip" = "Zip_Code"))

#renaming columns and arranging by schools 
final_dt <- final_dt %>% rename("Zip Code" = "zip", "Number of Street Lights" = "count") %>%
  arrange(-(final_dt$`Number of Schools`))

# Census Data
census <- st_read("2020_CensusData.shp") %>% st_set_crs(value = 4326)
census <- census %>% mutate(pop_und18 = A01001_2 + A01001_3 + A01001_4 + A01001_5)

#st_nn gives 1:1 joins by proximity "maxdist"
census_test <- st_join(census, zips_sf, join = st_nn)

#making census data into wanted columns
census_dt <- census_test %>% select(pop_und18, zip)
census_dt$geometry <- NULL

#grouping by zip code
census_zip_dt <- census_dt %>% group_by(zip) %>% summarise(num_und_18 = sum(pop_und18))
census_zip_dt <- census_zip_dt %>% rename("Population Under 18" = num_und_18, 
                                          "Zip Code" = zip)
#combining census data to full data
final_dt <- left_join(final_dt, census_zip_dt)


# Code Enforcement

names(enforcement)[names(enforcement) == 'X'] <- 'Lon'
names(enforcement)[names(enforcement) == 'Y'] <- 'Lat'
enforcement$Recent_Hearing_Date <- as.Date(enforcement$Recent_Hearing_Date)

statusPal <- colorFactor(palette = 'RdYlBu', domain = enforcement$Status)


# Schools

schoolPal <- colorFactor(palette = viridis(2), domain = schools$SchoolType)

schools_subset_dt <- schools
schools_subset_dt$geometry <- NULL


# Abandoned Properties

prop <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)

prop_data <- prop %>% st_set_geometry(NULL)

####Start of UI


# Define UI for application that tabs and creates summary of data

ui <- fluidPage(
  navbarPage("Safest Locations",
             tabPanel("Summary", 
                      titlePanel("Safest Locations"),
                      p("What are the best routes for children to walk home from school?"),
                      mainPanel(DT::dataTableOutput("table"))),
             
             tabPanel("Street Lights", 
                      titlePanel("Street Lights"),
                      sidebarPanel(width = 5,
                                   selectInput(inputId = "Z_st",
                                               label = "Choose Zip Code(s):",
                                               choices = unique(as.character(na.omit(street_data.spatial$zip))),
                                               selected = NULL,
                                               multiple = T
                                   ),
                                   plotOutput("lightCount")),
                      mainPanel(width = 7, leafletOutput(outputId = "streetLights"),
                                plotOutput("lumensBar"))
             ),
             
             tabPanel("Code Enforcement",
                      titlePanel("Code Enforcement"),
             
                      # Sidebar with a slider input for time range 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("DateRange",
                                      "Date Range:",
                                      min = as.Date("2007-01-01","%Y-%m-%d"),
                                      max = as.Date("2023-12-31","%Y-%m-%d"),
                                      value = c(as.Date("2007-01-01","%Y-%m-%d"), as.Date("2023-12-31","%Y-%m-%d")),
                                      timeFormat="%Y-%m-%d"),
                          plotOutput("enforcementHist")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          leafletOutput("enforcementMap"),
                          plotOutput("enforcementBar")
                        )
                      )
                    ),
             
             tabPanel("Schools", 
                      titlePanel("Schools"),
                      sidebarPanel(
                        selectInput(inputId = "SchoolZip",
                                    label = "Choose Zip Code(s):",
                                    choices = unique(as.character(na.omit(schools$zip))),
                                      #c(zips_sf %>% st_set_geometry(NULL)),
                                    selected = NULL,
                                    multiple = T
                        )
                      ),
                      mainPanel(
                        leafletOutput(outputId = "schools"), 
                        DT::dataTableOutput("schools_table")
                      )
             ),
             
             tabPanel("Abandoned Properties",
                      titlePanel("Abandoned Properties"),
                      sidebarPanel(
                        selectInput(inputId = "Z",
                                    label = "Choose a Zip Code:", 
                                    choices = c("Select Here",
                                                unique(as.character(prop_data$Zip_Code))))
                      ),
                       
                      mainPanel(leafletOutput(outputId = "prop"),
                                plotOutput("ABHist"))
             )
  )
  
)


# Define server logic required to draw a map and summary table
server <- function(input, output) {
  
  # Summary Table
  output$table <- DT::renderDataTable({
    DT::datatable(final_dt, options(pageLength = 13))
  })
  
  
  # Street Lights
  street.subset <- reactive({
    if(is_null(input$Z_st)) {
      street_data.spatial
    } 
    else {
      street_data.spatial[street_data.spatial$zip %in% input$Z_st,]
    }
  })
  
  output$streetLights <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "Basic") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Toner") %>%
      addPolygons(data = zips_sf, popup = ~zip, opacity = 1, fillOpacity = 0.1, weight = 1, color = ~zipPal(zip)) %>% 
      addCircleMarkers(data = street.subset(), color = ~pal_lum(Lumens), stroke = 0,
                       fillOpacity = 1, radius = 3, popup = ~popup) %>%
      addLegend(pal = pal_lum, values = scale_range, position = "bottomright",
                title = "Lumens") %>%
      setView(lng = mean(street_data$Lon), lat = mean(street_data$Lat), zoom = 11) %>%
      addLayersControl(
        baseGroups = c("Basic", "Toner"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(-86.252, 41.6764, zoom = 11)
  })
  
  output$lumensBar <- renderPlot({
    ggplot(dt_data, aes(x = reorder(zip, -`Average Lumens`),
                        y = `Average Lumens`)) + geom_bar(stat = "identity") +
      xlab("Zip Code") + ggtitle("Average Amount of Lumens per Zip Code") +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
            title=element_text(size=14,face="bold"), panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
  })
  
  output$lightCount <- renderPlot({
    ggplot(dt_data, aes(x = reorder(zip, -count), y = count)) +
      geom_bar(stat = "identity") + xlab("Zip Code") + ylab("Number of Street Lights") +
      ggtitle("Street Lights per Zip Code") +
      theme(axis.text=element_text(size=7), axis.title=element_text(size=8),
            title=element_text(size=12,face="bold"), panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
  })
  
  
  # Code Enforcement
  datSubset <- reactive ({
    enforcement[enforcement$Recent_Hearing_Date >= input$DateRange[1] & 
                enforcement$Recent_Hearing_Date <= input$DateRange[2],]
  })
  
  output$enforcementMap <- renderLeaflet({
    leaflet()  %>%
      addTiles(group = "Basic") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addPolygons(data = zips_sf, popup = ~zip, opacity = 1, fillOpacity = 0.1, weight = 1, color = ~zipPal(zip)) %>%
      addCircleMarkers(data = datSubset()[datSubset()$Status=='CLOSED',], popup = ~State_Parcel_ID, color = ~statusPal(Status), 
                       stroke = 0, fillOpacity = 1, radius = 4, group = "Closed") %>%
      addCircleMarkers(data = datSubset()[datSubset()$Status=='ACTIVE',], popup = ~State_Parcel_ID, color = ~statusPal(Status), 
                       stroke = 0, fillOpacity = 1, radius = 4, group = "Active") %>%
      addLegend("bottomright", pal = statusPal, values = datSubset()$Status[!is.na(datSubset()$Status)], title = "Status") %>%
      addLayersControl(
        baseGroups = c("Basic", "Toner"),
        overlayGroups= c("Active", "Closed"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(-86.252, 41.6764, zoom = 11)
  })
  
  output$enforcementHist <- renderPlot({
    hist(enforcement$Recent_Hearing_Date, breaks = 25, col = 'darkgray',
         xlab = 'Recent Hearing Date',
         main = 'Histogram of Recent Hearing Date')
  })
  
  output$enforcementBar <- renderPlot({
    df2 <- datSubset()[datSubset()$Last_Compliance_Type != "" & !is.na(datSubset()$Last_Compliance_Type),]
    ggplot(df2, aes(Last_Compliance_Type)) + 
      geom_bar() +
      ggtitle("Last Compliance Types") +
      xlab("Last Compliance Type") +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), title=element_text(size=20,face="bold"))
  })
  
  
  # Schools
  schoolSubset <- reactive ({
    if(is_null(input$SchoolZip)) {
      schools
    } 
    else {
      schools[schools$zip %in% input$SchoolZip,]
    }
  })
  
  zipSubset <- reactive ({
    if(is_null(input$SchoolZip)) {
      zips_sf
    } 
    else {
      zips_sf[zips_sf$zip %in% input$SchoolZip,]
    }
  })
  
  output$schools <- renderLeaflet({
    leaflet()  %>%
      addTiles(group = "Basic")  %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Toner") %>%
      addPolygons(data = zipSubset(), popup = ~zip, opacity = 1, fillOpacity = 0.1, weight = 1, color = ~zipPal(zip)) %>%
      addPolygons(data = schoolSubset()[schoolSubset()$SchoolType=='Private',], color = ~schoolPal(SchoolType), 
                  weight = 1, fillOpacity = 1, popup = ~School, group = "Private") %>%  
      addPolygons(data = schoolSubset()[schoolSubset()$SchoolType=='Public',], color = ~schoolPal(SchoolType), 
                  weight = 1, fillOpacity = 1, popup = ~School, group = "Public") %>%  
      addLegend("bottomright", pal = schoolPal, values = schoolSubset()$SchoolType, title = "School Types") %>%
      addLayersControl(
        baseGroups = c("Basic", "Toner"),
        overlayGroups= c("Private", "Public"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(-86.252, 41.6764, zoom = 11)
  })
  
  school_tableSubset <- reactive ({
    if(is_null(input$SchoolZip)) {
      schools_subset_dt %>% select(-c(OBJECTID))
    } 
    else {
      schools_subset_dt[schools_subset_dt$zip %in% input$SchoolZip,] %>% select(-c(OBJECTID))
    }
  })
  
  output$schools_table <- DT::renderDataTable({
    DT::datatable(school_tableSubset(), options(pageLength = 10))
  })
  
  
  # Abandoned Properties
  
  prop.subset<- reactive({
    x <- prop %>% filter(prop$Zip_Code == input$Z)
    return(x)
  })
  
  output$prop <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "Basic")  %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Toner") %>%
      addPolygons(data = zips_sf, popup = ~zip, opacity = 1, fillOpacity = 0.1, weight = 1, color = ~zipPal(zip)) %>%
      addPolygons(data = prop.subset(), color = ~zipPal(prop.subset())) %>%
      addLayersControl(
        baseGroups = c("Basic", "Toner"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(-86.252, 41.6764, zoom = 11)
  }) 
  
  output$ABHist <- renderPlot({
    barplot(table(prop$Zip_Code),
            xlab='Types of Abandoned Properties',
            main = 'Abandoned Property Types by Zip Code')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




