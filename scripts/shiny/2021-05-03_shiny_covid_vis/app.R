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

# To use date in a shiny slider 
# it is probably easiet to format yyyy/mm/dd 
# Combine year, month, and day columns
clusdata$tp1_date <- paste(clusdata$Year, clusdata$Month, clusdata$Day, sep="-")

# reorder data by province, month, and day
clusdata <- clusdata %>% group_by(Province) %>% arrange(Month, Day, .by_group = T)

# calculate a cumulative total of strains present
tmp_all <- aggregate(!is.na(TP1)~Province, data=clusdata, "cumsum") 

# cum sum for size
clusdata$cumsum_all <- unlist(tmp_all$`!is.na(TP1)`)

# calculate a cumulative total of strains present
tmp_tp1 <- aggregate(TP1~Province, data=clusdata, "cumsum") 

# cum sum for size
clusdata$cumsum_tp1 <- unlist(tmp_tp1$TP1)

# cumsum tp2
clusdata$cumsum_tp2 <- clusdata$cumsum_all - clusdata$cumsum_tp1

rm(tmp_all, tmp_tp1)


# hard code colour in for pie charts

# colour for pie charts
pal <- data.frame("colour" = rainbow(length(unique(clusdata$`TP1 cluster`))),
                  "tp1_cluster" = unique(clusdata$`TP1 cluster`))
                  
clusdata$chart_colours <- apply(clusdata, 1, function(x){
    return(pal[pal$tp1_cluster == x["TP1 cluster"], "colour"])
})


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
            
            # slide to select date 
            sliderInput("date",
                        "Select date:",
                        min = as.Date("2020-1-1","%Y-%m-%d"),
                        max = as.Date("2020-6-1","%Y-%m-%d"),
                        value=as.Date("2020-1-1"),
                        timeFormat="%Y-%m-%d"),

            # drop down menu to select country 
            pickerInput("country",
                        "Select country:", 
                        choices = unique(clusdata$Country), 
                        options = list(`actions-box` = TRUE),
                        multiple = T),
            
            # drop down menu to select country 
            pickerInput("cluster",
                        "Select cluster:", 
                        choices = unique(clusdata$`TP1 cluster`), 
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
            subset(Country %in% input$country) %>% 
            subset(`TP1 cluster` %in% input$cluster) %>% 
            subset(tp1_date <= input$date) %>% 
            group_by(Province) %>% 
            top_n(1, cumsum_all)
        
        # set region for map
        # for now, whole world
        region <- map(regions = ".", fill = F, plot = F)
        
        # map the data using leaflet
        leaflet(data=region) %>% 
            
            # set base zoom over mid asia/europe
            #setView(lng = 60, lat = 50, zoom = 2) %>%
            
            # load tile for selected region
            addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
            
            # add circles that correspond to clusters at TP1
             addCircleMarkers(lng = clusdata_sub$Longitude,
                                 lat = clusdata_sub$Latitude,
                                 radius = (log(clusdata_sub$cumsum_all*10)*10)/2,
                                 fillColor = clusdata_sub$chart_colours,
                                 stroke = F, fillOpacity = 0.6) %>%
            
            # layer pie chart on top of cluster to represent new strains
            # present at TP2
            addMinicharts(lng = clusdata_sub$Longitude,
                          lat = clusdata_sub$Latitude,
                          type = "pie",
                          chartdata = as.matrix(clusdata_sub[, c("cumsum_tp1", 
                                                             "cumsum_tp2")]), 
                          colorPalette = c("white", "black"), 
                          width = log(clusdata_sub$cumsum_all*10)*10, 
                          transitionTime = 0,
                          opacity = 0.1)
        
        # add legend for relating colours to cluster
        # set this as static so all colours always shown
         addLegend("bottomright", pal = pal, 
                  values = ~sub_date$`TP1 cluster`,
                  title = "ClusterW",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
