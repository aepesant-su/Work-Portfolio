#
#My second app
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("ACME Art Company Dashboard"),
    
    sidebarLayout(
        sidebarPanel(tags$style(".well {background-color:#528B8B;"),
                     plotOutput("yearlyReceipts"),
                     selectInput("store", "Select Store:",choices = c("None","Portland","Davenport",
                                                                      "Syracuse", "Dublin")),
                     selectInput("year","Select Year:", choices = c("All", "2012",
                                                                    "2013","2014","2015"))
                     ),
        mainPanel(
        plotOutput("storePaper"),
        #plotOutput("storeEmployee")
        )
    )
)


# Define server logic for dashboard
server <- function(input, output) {
    art <- read_csv("art.csv")
    
    watercolor.col <- "cadetblue1"
    drawing.col <- "antiquewhite"
    output$yearlyReceipts <- renderPlot({
        my.title <- "Number of receipts per year."
        barplot(table(art$year), main = my.title, border = "white", col = "chartreuse")
    })
    
    output$storePaper <- renderPlot({
        if (input$store != "None") {
            sub.index <- which(art$store == input$store)
            tmp.data <- art[sub.index,]
            
            if(input$year != "All") {
                sub.index.2<-which(tmp.data$year == as.numeric(input$year))
                tmp.data<-tmp.data[sub.index.2,]
            }
            par(mfrow = c(1,2))
            sales.by.paper <- tapply(tmp.data$total.sale,list(tmp.data$paper),sum)
            barplot(sales.by.paper, beside = T,
                    main = "Revenue by paper type",
                    col = c(drawing.col, watercolor.col), border = NA)
            sales.by.rep <- tapply(tmp.data$total.sale,list(tmp.data$rep),sum)
            pie(sales.by.rep, border = NA, col = terrain.colors(length(sales.by.rep)))
            }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
