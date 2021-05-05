library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(leaflet.minicharts) # for better mini charts on map
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps



#Load pre-processed data with epi-cluster cohesion info.
clusdata <- readxl::read_xlsx(here("input_data", "2021-05-03_0.Europe_1st wave.9000_Merged_strain_results.xlsx"))

# remove columns that dont contain useful information
clusdata <-  clusdata[,-c(32:35)]



test <- clusdata %>% 
    group_by(`TP1 cluster`, Month, Year, Province) %>% 
    summarise( day = mean(Day),
               country = unique(Country),
               lat = mean(Latitude), 
               long = mean(Longitude),
               num_tp1_strains = sum(TP1==1),
               tp1_t0_ecc_temporal = mean(TP1_T0_ECC_0.0.1),
               tp1_t0_ecc_geo = mean(TP1_T0_ECC_0.1.0),
               avg_tp1_date = mean.Date(as.Date(`Average TP1 date`, format="%d-%m-%Y")),
               avg_tp1_lat = mean(`Average TP1 latitude`),
               avg_tp1_long = mean(`Average TP1 longitude`),
               avg_tp1_cluster_diam = mean(as.numeric(`TP1 geo average cluster distance (km)`)),
               avg_tp1_cluster_dur = mean(as.numeric(`TP1 temp average cluster distance (days)`)),
               num_tp2_strains = sum(TP1==0),
               tp2_t0_ecc_temp = mean(TP2_T0_ECC_0.0.1),
               tp2_t0_ecc_geo = mean(TP2_T0_ECC_0.1.0),
               avg_tp2_date = mean.Date(as.Date(`Average TP2 date`, format="%d-%m-%Y")),
               avg_tp2_lat = mean(`Average TP2 latitude`),
               avg_tp2_long = mean(`Average TP2 longitude`),
               avg_tp2_cluster_diam = mean(as.numeric(`TP2 geo average cluster distance (km)`)),
               avg_tp2_cluster_dur = mean(as.numeric(`TP2 temp average cluster distance (days)`)),
               avg_delta_ecc_temp = mean(delta_ECC_0.0.1),
               avg_delta_geo_geo = mean(delta_ECC_0.1.0),
               strains = paste0(Strain, collapse = ";")) 

test <- as.data.frame(test)

# To use date in a shiny slider 
# it is probably easiet to format yyyy/mm/dd 
# Combine year, month, and day columns
#test$tp1_date <- paste(test$Year, test$Month, test$Day, sep="-")


# make a  sum of all strain
test$num_all_strains <- test$num_tp1_strains + test$num_tp2_strains


# colour for pie charts
pal <- data.frame("colour" = rainbow(length(unique(test$`TP1 cluster`))),
                  "tp1_cluster" = unique(test$`TP1 cluster`))

#pal <- colorFactor(palette(rainbow(length(unique(clusdata$`TP1 cluster`)))),
#                   domain = unique(clusdata$`TP1 cluster`))

test$chart_colours <- apply(test, 1, function(x){
    return(pal[pal$tp1_cluster == x["TP1 cluster"], "colour"])
})

##########################################
# helper functions for running Shiny app #
##########################################

# discussed month-by-month granularity
# function to create a slider bar for month selection
monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}




ui <- fluidPage(
    
    # Application title
    titlePanel("Location and size of SARS-CoV-2 clusters over time"),
    
    # Side bar with a sliders for data selection
    sidebarLayout(
        
        # side bar
        sidebarPanel(
            
            # slide to select day
            #sliderInput("date",
            #            "Select date:",
            #            min = as.Date("2020-1-1","%Y-%m-%d"),
            #            max = as.Date("2020-6-1","%Y-%m-%d"),
            #            value=as.Date("2020-1-1"),
            #            timeFormat="%Y-%m-%d"),
            
            # slider to select month 
            sliderInput("slider", 
                        "Select date:", 
                        min = as.Date("2020-01-01"),
                        max =as.Date("2020-06-30"),
                        value=as.Date("2020-01-01"),
                        timeFormat="%b %Y"),
            textOutput("SliderText"),
            
            
            # drop down menu to select country 
            pickerInput("country",
                        "Select country:", 
                        choices = unique(test$country), 
                        options = list(`actions-box` = TRUE),
                        multiple = T),
            
            # drop down menu to select country 
            pickerInput("cluster",
                        "Select cluster:", 
                        choices = unique(test$`TP1 cluster`), 
                        options = list(`actions-box` = TRUE),
                        multiple = T)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        )
    )
)

server <- function(input, output, session) {
    
    
    ######################################
    # create a reactive slider for month #
    ######################################
    
    sliderMonth <- reactiveValues()
    
    observe({
        full.date <- as.POSIXct(input$slider, tz="GMT")
        sliderMonth$Month <- as.character(monthStart(full.date))
    })
    
    output$SliderText <- renderText({sliderMonth$Month})
    
    
 

    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet() %>% 
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            setView(lng = 60, lat = 50, zoom = 2)
    })
    

    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        data <- test %>% 
            subset(country %in% input$country) %>% 
            subset(`TP1 cluster` %in% input$cluster) %>% 
            subset(substr(as.character(test$avg_tp1_date),1,7) == substr(as.character(input$slider),1,7)) 
        
        leafletProxy("map") %>%
            clearShapes() %>%
            addCircles( 
                lng = data$long,
                lat = data$lat,
                radius = (log((data$num_tp1_strains + data$num_tp1_strains)*10)*10)/2,
                fillColor =data$chart_colours, fillOpacity = 0.7)
    })
    

}

shinyApp(ui, server)