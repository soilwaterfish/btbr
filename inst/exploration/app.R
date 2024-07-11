#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
df <- bt_baseline_2017 %>% st_drop_geometry() %>% mutate(scale_300 = as.numeric(scale(perennial_stream_length_w_in_300_of_roads)),
                                                         log_300 = log(perennial_stream_length_w_in_300_of_roads))
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("color",
                        "Color of Points:",
                        multiple = F,choices = c('', colnames(df)), selected = NA)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({

      fill_color <- if(is.numeric(df[[input$color]])){

                    scale_fill_gradientn(colors = rev(hcl.colors(11, 'Zissou1')))

                    } else {
                    scale_fill_manual(values = rev(hcl.colors(length(unique(df[[input$color]])), 'Zissou1')))
                    }
      df %>%
      ggplot(aes(sdelFS, road_density)) +
        geom_point(shape = 21, aes(fill = .data[[input$color]]),color = 'black', size = 3) +
        #scale_fill_manual(values = c('forestgreen', 'yellow', 'red')) +
        fill_color +
        custom_theme() +
        labs(y = 'FS Only Road Density',
             x = 'Sediment Delivery (FS Only)')


    })
}

# Run the application
shinyApp(ui = ui, server = server)
