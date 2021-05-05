################ 
# General info #
################

# May 5, 2021
# Samantha Lee

# The purpose of this script is to generate a map of Covid-19 data that can be
# modified based on a scale bar that selects for time

###############################
# load the required libraries #
###############################

library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(leaflet.minicharts) # for better mini charts on map
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps

#######################
# load and clean data #
#######################

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

#######################################
# the user interface of the shiny app #
#######################################

# Define UI for application that draws a histogram
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
            leafletOutput("mymap"),
        )
    )
)



#################################################################### 
# sever logic: the plots and function that the shiny app will call #
####################################################################


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ######################################
    # create a reactive slider for month #
    ######################################
    
    sliderMonth <- reactiveValues()
    observe({
        full.date <- as.POSIXct(input$slider, tz="GMT")
        sliderMonth$Month <- as.character(monthStart(full.date))
    })
    output$SliderText <- renderText({sliderMonth$Month})
    
    
    #########################################
    # plot a reactive map of covid clusters #
    #########################################
    
    output$mymap <- renderLeaflet({
        
        # radius of circles = number of cases cluster 
        # radius will grow during time period as more strains identified
        # colour of circles can be used to indicate ECC values using dat matrix
        # colours will likely be similar between clusters and TP1 and TP2
        # How to make more distinct?
        
        # subset data according to data selected in side bar
        # want dates less than or equal to selected date
        # only want most recent data (largest cumulative sum)
        # otherwise circles become too opaque
        clusdata_sub <- test %>% 
            subset(country %in% input$country) %>% 
            subset(`TP1 cluster` %in% input$cluster) %>% 
            subset(substr(as.character(test$avg_tp1_date),1,7) == substr(as.character(input$slider),1,7)) %>% 
            group_by(Province) %>% 
            top_n(1, num_all_strains)
        
        # set region for map
        # for now, whole world
        region <- map(regions = ".", fill = F, plot = F)
        
        # map the data using leaflet
        leaflet(data=region) %>% 
            
            # set base zoom over mid asia/europesubstr(as.character(test$avg_tp1_date),1,7)
            #setView(lng = 60, lat = 50, zoom = 2) %>%
            
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            
            # add circles that correspond to clusters at TP1
            addCircleMarkers(lng = clusdata_sub$long,
                             lat = clusdata_sub$lat,
                             radius = (log((clusdata_sub$num_tp1_strains + clusdata_sub$num_tp1_strains)*10)*10)/2,
                             fillColor = clusdata_sub$chart_colours,
                             stroke = F, fillOpacity = 0.7) 
            
 

            # layer pie chart on top of cluster to represent new strains
            # present at TP2
            #addMinicharts(lng = clusdata_sub$long,
            #              lat = clusdata_sub$lat,
            #              type = "pie",
            #              chartdata = as.matrix(clusdata_sub[, c("num_tp1_strains", 
            #                                                     "num_tp2_strains")]), 
            #              colorPalette = c("white", "darkgrey"), 
            #              width = log((clusdata_sub$num_tp1_strains + clusdata_sub$num_tp1_strains)*10)*10, 
            #              transitionTime = 0,
            #              opacity = c(0.4,0.8))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
