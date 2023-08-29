#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(baseballr)
library(tidyverse)
library(ggrepel)
library(RMySQL)

#conn <- dbConnect(MySQL(), dbname = "abdwr",
                  #user = "sal413", password = "Orange1!", host = 'sportdataviz.com')

#masterID <- dbGetQuery(conn, "SELECT * FROM aepesantMaster")
#august2018 <- dbGetQuery(conn, "SELECT * FROM aepesant")
august2018 <- read_csv("august2018.csv")
masterID <- read_csv("masterid.csv")

august2018 <- august2018 %>% group_by(batter,zone,type) %>%
    summarize(N = n(),
              right_edge = min(1.5, max(plate_x)),
              left_edge = max(-1.5, min(plate_x)),
              top_edge = min(5, quantile(plate_z, 0.95, na.rm = TRUE)),
              bottom_edge = max(0, quantile(plate_z, 0.05, na.rm = TRUE)),
              swing_pct = (N-(sum(description == "called_strike" | description == "ball" | description == "pitchout" | description == "intent_ball" | description == "hit_by_pitch" | description == "blocked_ball")))/N,
              plate_x = mean(plate_x),
              plate_z = mean(plate_z)) -> my_pitches

august2018 %>% group_by(batter) %>% dplyr::summarise() %>% inner_join(masterID, by = c("batter"="mlb_id")) %>% select(batter, mlb_name) %>% arrange((mlb_name)) -> possibleBatters

possibleBattersList <- possibleBatters$batter
names(possibleBattersList) <- possibleBatters$mlb_name

plate_width <- 17 + 2 * (9/pi)
k_zone_plot <- ggplot(NULL, aes(x = plate_x, y = plate_z)) +
    geom_rect(xmin = -(plate_width/2)/12,
              xmax = (plate_width/2)/12,
              ymin = 1.5,
              ymax = 3.6, color = "cornflowerblue", alpha = 0) +
    coord_equal() +
    scale_x_continuous("Horizontal location (ft.)",
                       limits = c(-2,2)) +
    scale_y_continuous("Vertical location (ft.)",
                       limits = c(0,5))

#d
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Batter Swing Chart for August 2018"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("types",
                               h3("Types of Outcomes"),
                               choices = list("S" = "S",
                                              "B" = "B",
                                              "X" = "X"),
                               selected = c("S","B","X")),
            selectInput("selectBatter", h3("select box"),
                        choices = possibleBattersList, selected = 605141)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "batterChart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    SelectedBatterReactive <- reactive({
        my_pitches %>% filter(batter == input$selectBatter)
    })
    
    output$batterChart <- renderPlot({
        SelectedBatterReactive() %>% filter(type %in% input$types) -> SelectedBatterResults
        
        k_zone_plot %+% SelectedBatterResults +
            geom_rect(aes(xmax = right_edge, xmin = left_edge,
                          ymax = top_edge, ymin = bottom_edge,
                          fill = swing_pct, alpha = swing_pct),
                      color = "lightgray") +
            geom_text_repel(size = 3, aes(label = round(swing_pct, 2),
                                          color = swing_pct < 0.5)) +
            scale_fill_gradient(low = "blue", high = "red") +
            scale_color_manual(values = c("white", "black")) +
            guides(color = FALSE, alpha = FALSE) +
            ggtitle("Swing Per Pitch during August 2018")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
