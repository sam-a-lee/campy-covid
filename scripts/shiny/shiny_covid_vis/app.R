################ 
# General info #
################

# May 3, 2021
# Samantha Lee

# The purpose of this script is to generate a map of Covid-19 data that can be
# modified based on a scale bar that selects for time

###############################
# load the required libraries #
###############################

library(shiny) # for running shiny apps
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps


#######################################
# the user interface of the shiny app #
#######################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Location and size of SARS-CoV-2 clusters over time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("date",
                        "Select date:",
                        min = as.Date("2020-1-1","%Y-%m-%d"),
                        max = as.Date("2020-6-1","%Y-%m-%d"),
                        value=as.Date("2020-1-1"),
                        timeFormat="%Y-%m-%d")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mymap"),
        )
    )
)


#################################################################### 
# sever logic: the plots and function that the shiny app will call #
####################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        
        # radius of circles = number of cases in outbreak
        # colour of circles can indicate the number of strains in a given outbreak
        # or colour can represent the cluster
        pal <- colorFactor(palette(rainbow(length(unique(clusdata$`TP1 cluster`)))),
                           domain = unique(clusdata$`TP1 cluster`))
        

        # subset data according to date selected in slider
        sub_date <- clusdata %>% subset(tp1_date == input$date)
        
        # set region for map
        # for now, whole world
        region <- map(regions = ".", fill = F, plot = F)
        
        # map the data using leaflet
        leaflet(data=region) %>% 
            # set base zoom over mid asia/europe
            setView(lng = 60, lat = 50, zoom = 2) %>%
            # load tile for selected region
            addTiles() %>%
            # add circles that correspond to clusters at TP1
            addCircleMarkers(lng = sub_date$Longitude,
                             lat = sub_date$Latitude,
                             radius = log(sub_date$`TP1 cluster size`+1),
                             fillColor = ~pal(sub_date$`TP1 cluster`), 
                             stroke = F, fillOpacity = 0.3) 
            
            # add legend for relating colours to cluster
            # set this as static so all colours always shown
            # addLegend("bottomright", pal = pal, 
            #          values = ~sub_date$`TP1 cluster`,
            #          title = "ClusterW",
            #          #labFormat = labelFormat(prefix = "$"),
            #          opacity = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
