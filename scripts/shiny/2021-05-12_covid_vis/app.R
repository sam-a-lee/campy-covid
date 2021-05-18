################ 
# General info #
################

# May 12, 2021
# Samantha Lee

# The purpose of this script is to generate a map of epi-cluster cohesian data
# data plots data at time point 1 and time point 2 simulateanously 
# to show how the geographic centroid of clusters changes 

###############################
# load the required libraries #
###############################

library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(leaflet) # for map visualizations
library(maps) # for drawing maps/getting map data
library(RColorBrewer) # for prettier colours
library(reactable) # nested interable dables
library(shinyBS)



#######################
# load and clean data #
#######################

# input an input_data/data in the app when uploading
# load pre-processed data with epi-cluster cohesion info.
eccdata <- read_xlsx(here("input_data", "2021-05-03_0.Europe_1st wave.9000_Merged_strain_results.xlsx"))

# remove non-informative cols
eccdata <- eccdata[,-c(32:35)]

# rename columns
# remove spaces, all lowercase
colnames(eccdata) <- c("strain", "country", "province", "city", "latitude", 
                       "longitude", "day", "month", "year", "present_at_tp1", 
                       "tp1_cluster", "tp1_cluster_size_2", "tp1_t0_ecc_0.1.0", 
                       "tp1_t0_ecc_0.0.1", "present_at_tp2", "tp2_cluster", 
                       "tp2_cluster_size_2", "tp2_t0_ecc_0.1.0", 
                       "tp2_t0_ecc_0.0.1", "delta_ecc_0.1.0", "delta_ecc_0.0.1", 
                       "avg_tp1_date", "avg_tp1_temporal_dist_days", 
                       "avg_tp1_latitude", "avg_tp1_longitude", 
                       "avg_tp1_geo_dist_km", "avg_tp2_date", 
                       "avg_tp2_temporal_dist_days", "avg_tp2_latitude", 
                       "avg_tp2_longitude", "avg_tp2_geo_dist_km", 
                       "tp1_cluster_size", "tp2_cluster_size", 
                       "delta_cluster_size", "num_additional_tp1_strains_tp2", 
                       "num_novel_tp2_strains", "overall_cluster_growth_rate", "cluster_novel_growth_rate", 
                       "type")

# make column for strain date
eccdata$strain_date <- paste(eccdata$day, eccdata$month, eccdata$year, sep = "-")

# remove month, day, year
eccdata <- subset(eccdata,select = -c(month, day, year))

# initialize colorpal as null
colorpal <- NULL

###################################
# user interface of the shiny app #
###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("SARS-CoV-2 epi-cluster cohesion (ECC) data"),
    
    # Side bar with a sliders for data selection
    sidebarLayout(
        
        # left side bar panel
        sidebarPanel(
            
            ####################################
            # cluster selection drop down menu #
            ####################################
            
            selectizeInput("cluster",
                        "Select cluster(s):", 
                        choices = levels(as.factor(eccdata$tp1_cluster)), 
                        selected = "TP1_h0_c001",
                        options = list(
                          "max-options" = 8,
                          "max-options-text" = "Maximum 8 selected clusters!"),
                        multiple = T),
            
            
            ############################
            # strain filtering options #
            ############################
            
            checkboxInput("strain_filtering", "Show strain filtering options"),
            
            conditionalPanel(
              condition = "input.strain_filtering == true",
              
              selectizeInput("strain_country",
                             "Country:", 
                             choices = sort(unique(eccdata$country)), 
                             multiple = T),
              
              selectizeInput("strain_province",
                             "Province:", 
                             choices = sort(unique(eccdata$country)), 
                             multiple = T),

            ),
            
            
            #############################
            # cluster filtering options #
            #############################
            
            checkboxInput("cluster_filtering", "Show cluster filtering options"),
            
            conditionalPanel(
              condition = "input.cluster_filtering == true",
              
              selectizeInput("cluster_country",
                             "Country:", 
                             choices = sort(unique(eccdata$country)), 
                             multiple = T),
              
              selectizeInput("cluster_province",
                             "Province:", 
                             choices = sort(unique(eccdata$country)), 
                             multiple = T),
              
            ),
            
            ###########################
            # plot appearance options #
            ###########################
            
            checkboxInput("plot_aes", 
                          "Show plot appearance options"),
            
            conditionalPanel(
                
                # if show appearance is checked
                condition = "input.plot_aes == true",

                # select transparency of strains
                sliderInput("strain_transparency",
                            "Select transparency for strain plotting:",
                            min = 0,
                            max = 100,
                            step = 1,
                            value = 50),
                
                # select transparency of clusters
                sliderInput("centroid_transparency",
                            "Select transparency for cluster plotting:",
                            min = 0,
                            max = 100,
                            step = 1,
                            value = 40),
                
                checkboxInput("legend", "Show legend", TRUE)

            )

        ),
        
        
        #################################
        # main panel for plots and tabs #
        #################################
        
        mainPanel(
            
            # a table panel at the top
            # switch between map and the raw data that created it
            tabsetPanel(
                # table for map
                tabPanel("Map", leafletOutput("map"),reactableOutput("tp1_filtered_grouped_data")), 
                # tab for raw data
                tabPanel("Raw data", reactableOutput("raw_data")),
                # table panel for filtered data
                tabPanel("Filterd data", reactableOutput("tp1_filtered_data"))
                )
        )
    )
)



#################################################################### 
# sever logic: the plots and function that the shiny app will call #
####################################################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##############################################
  # update picker inputs in advanced filtering #
  ##############################################
  observeEvent(input$cluster, {
    
    updateSelectInput(session = session, 
                      inputId = "country",
                      choices = sort(unique(eccdata %>% subset(tp1_cluster %in% input$cluster) %>% .$country)))
  })
  
  
  
  
  ############################################
  # reactive subsetting of data for plotting #
  ############################################
  
  # data subset avg centroid info
  avg_centroid <- reactive({
    
    # a cluster must be selected
    req(input$cluster)
    
    eccdata %>% 
      # subset by cluster
      subset(tp1_cluster %in% input$cluster) %>% 
      distinct(avg_tp1_date, .keep_all = T) %>%
      group_by(present_at_tp1) %>% 
      subset(present_at_tp1==1)
    
  })
  
  # data subset for TP1 strains
  tp1_strains <- reactive({
    
    # a cluster must be selected
    req(input$cluster)
    
    # this aggregates by province
    eccdata %>% 
      subset(tp1_cluster %in% input$cluster) %>%
      subset(present_at_tp1==1) %>% 
      aggregate(.~province, data = ., unique) %>% 
      mutate(num_tp1_strains = lengths(strain)) 
  })
  
  # data subset for TP1 clusters
  tp1_centroid <- reactive({
    
    # a cluster must be selected
    req(input$cluster)
    
    # subset data for tp1 clusters
    eccdata %>% 
      # subset by cluster
      subset(tp1_cluster %in% input$cluster) %>%
      # select only unique tp1 info
      distinct(avg_tp1_date, .keep_all = T) %>%
      subset(present_at_tp1==1) 
  })
  
  # data subset for TP2 strains
  tp2_strains <- reactive({
    
    # a cluster must be selected
    req(input$cluster)
    
    eccdata %>% 
      subset(tp1_cluster %in% input$cluster) %>%
      subset(present_at_tp1==0) %>% 
      aggregate(.~province, data = ., unique) %>% 
      mutate(num_tp2_strains = lengths(strain)) 
    
  })
  
  # data subset for TP2 clusters
  tp2_centroid <- reactive({
    
    # a cluster must be selected
    req(input$cluster)
    
    # subset data for tp1 clusters
    eccdata %>% 
      # subset by cluster
      subset(tp1_cluster %in% input$cluster) %>%
      # subset unique tp2 strains
      distinct(tp2_cluster, .keep_all = T) %>%
      subset(present_at_tp2==1)
  })
  
  
  #################### 
  # base leaflet map #
  ####################
  
  output$map <- renderLeaflet({
    
    # set region for map - whole world
    region <- map(regions = ".", fill = F, plot = F)
    
    # leaflet
    leaflet(data=region) %>% 
      
      # load tile for selected region
      addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}") %>%
      
      # set base zoom over mid asia/europe
      setView(lng = 60, lat = 50, zoom = 2) %>%
      
      # control layer for toggling strains and centroids on/off
      # never changes so can be part of base map
      addLayersControl(overlayGroups = c("TP1 centroid", "TP2 centroid",
                                         "TP1 strains", "TP2 strains", "Average centroid"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  
  ####################################################
  # reactive plotting of clusters and strains on map #
  ####################################################
  
  observe({
    
    ######################################
    # custom "function to colour strains #
    ######################################
    # this could be placed into a function in a separate module
    # and called here 
    
    # if some inputs are old
    if(sum(as.character(input$cluster) %in% as.character(colorpal$tp1_cluster)) > 0) {
      
      # find out position of exisiting cluster(s) in colorpal
      #pal_pos <-  colorpal$tp1_cluster %in% input$cluster
      pal_pos <- colorpal[colorpal$tp1_cluster %in% input$cluster, "rownum"]
      
      # set other positions to NA
      #colorpal$tp1_cluster[!pal_pos] <- as.character(NA)
      colorpal[!colorpal$rownum %in% pal_pos, "tp1_cluster"] <- as.character(NA)
      
      # reorder according to rownum
      colorpal <- colorpal[order(colorpal$rownum),]
      
      # get NA positions
      #na_pos <- is.na(colorpal$tp1_cluster)
      na_pos <- colorpal[is.na(colorpal$tp1_cluster), "rownum"]
      
      colorpal$tp1_cluster[na_pos] <- 
        as.character(c(input$cluster[!input$cluster %in% colorpal$tp1_cluster],
                       as.character(rep(NA, length(na_pos) - length(input$cluster[!input$cluster %in% colorpal$tp1_cluster])))))
      
      colorpal <<- colorpal
      colorpal_nona <<- na.omit(colorpal)
      
      cat(file=stderr(), "modifying old colours", "\n")
    } 
    
    
    # if all inputs are new
    if(is.null(colorpal)) {
      
      # new pal 
      colorpal <- data.frame(tp1_cluster = as.character(rep(NA,9)), colour = brewer.pal(9, "Set1"), rownum = 1:9)
      
      # assign new tp1 clusters
      colorpal$tp1_cluster <- as.character(c(input$cluster, as.character(rep(NA, 9-length(input$cluster)))))
      
      colorpal <<- colorpal
      colorpal_nona <<- na.omit(colorpal)
      
      cat(file=stderr(), "all new colours!", "\n")
    }
    
    # if no colours selected
    if(length(input$cluster)==0){
      colorpal <- NULL
      colorpal <<- colorpal
    }
    
    
    ##########################
    # put circles on the map #
    ##########################
    
    # some clusters do not have tp1 info to filer on
    # if they are the only cluster selected, app will crash
    # how to handle this

    leafletProxy("map") %>%
      
      # clear old shapes
      clearMarkers() %>%
      
      # add circles that is the average of tp1 and tp2
      # dashed stroke to indicate average 
      addCircleMarkers(lat = (((avg_centroid()$avg_tp1_latitude) + (avg_centroid()$avg_tp2_latitude))/2),
                       lng = (((avg_centroid()$avg_tp1_longitude) + (avg_centroid()$avg_tp2_longitude))/2),
                       radius = ((log10(as.numeric(avg_centroid()$avg_tp1_geo_dist_km))*10) + (log10(as.numeric(avg_centroid()$avg_tp2_geo_dist_km))*10))/2,
                       fillColor = unname(unlist(sapply(avg_centroid()$tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                       fillOpacity = input$centroid_transparency/100,
                       stroke = T,
                       opacity = input$centroid_transparency/100,
                       weight = 1,
                       color = "black",
                       dashArray = "4",
                       group = "Average centroid") %>%
      
      #add circles that correspond to tp1 centroids
      addCircleMarkers(data = tp1_centroid(),
                       lat =  ~avg_tp1_latitude,
                       lng =  ~avg_tp1_longitude,
                       radius = ~log10(as.numeric(avg_tp1_geo_dist_km))*10,
                       fillColor =  ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                       fillOpacity = input$centroid_transparency/100,
                       stroke = T,
                       opacity = input$centroid_transparency/100,
                       weight = 1,
                       color = "white",
                       label = lapply(seq(nrow(tp1_centroid())), function(i) {
                         ~HTML(paste("<b>", "TP1 cluster:", tp1_cluster[i], "</b>", "<br/>",
                                     "Average  date:", avg_tp1_date[i], "<br/>",
                                     "Strains in cluster:", (tp1_cluster_size_2[i])-1, "<br/>",
                                     "Latitude:", avg_tp1_latitude[i], "<br/>",
                                     "Longitude", avg_tp1_longitude[i], sep=" ")) 
                       }),
                       labelOptions = labelOptions(noHide = F),
                       group = "TP1 centroid") %>%
      
      # add circles that correspond to tp1 strains
      addCircleMarkers(data = tp1_strains(),
                       lat = ~as.numeric(as.character(latitude)),
                       lng = ~as.numeric(as.character(longitude)),
                       radius = 5,
                       fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                       fillOpacity = input$strain_transparency/100,
                       stroke = T,
                       opacity = input$strain_transparency/100,
                       weight = 1,
                       color = "white",
                       label = lapply(seq(nrow(tp1_strains())), function(i) {
                         ~HTML(paste("Country:", country[i], "<br/>",
                                     "Province:",province[i],"<br/>",
                                     "Number of TP1 strains:", num_tp1_strains[i], "<br/>",
                                     "Part of TP1 cluster:", tp1_cluster[i], sep=" ")) 
                       }),
                       labelOptions = labelOptions(noHide = F),
                       group = "TP1 strains") %>%
      
      # add circles that correspond to tp2  cluster centroids
      addCircleMarkers(data = tp2_centroid(),
                       lat = ~as.numeric(avg_tp2_latitude),
                       lng = ~as.numeric(avg_tp2_longitude),
                       radius = ~log10(as.numeric(avg_tp2_geo_dist_km))*10,
                       fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                       fillOpacity = input$centroid_transparency/100,
                       stroke = T,
                       opacity = input$centroid_transparency/100,
                       weight = 1,
                       color = "black",
                       label = lapply(seq(nrow(tp2_centroid())), function(i) {
                         ~HTML(paste("<b>", "TP2 cluster:", tp2_cluster[i], "</b>", "<br/>",
                                     "Contains TP1 cluster:", tp1_cluster[i],"<br/>",
                                     "Average date:", avg_tp2_date[i], "<br/>",
                                     "Strains in cluster:", (tp2_cluster_size_2[i])-1, "<br/>",
                                     "Novel strains:", num_novel_tp2_strains[i], "<br/>",
                                     "Latitude:", avg_tp2_latitude[i], "<br/>",
                                     "Longitude", avg_tp2_longitude[i], sep=" ")) 
                       }),
                       labelOptions = labelOptions(noHide = F),
                       group = "TP2 centroid") %>%
      
      # add circles that correspond to tp2 strains
      addCircleMarkers(data = tp2_strains(),
                       lat = ~as.numeric(as.character(latitude)),
                       lng = ~as.numeric(as.character(longitude)),
                       radius = 5,
                       fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                       fillOpacity = input$strain_transparency/100,
                       stroke = T,
                       opacity = input$strain_transparency/100,
                       weight = 1,
                       color = "black",                             
                       label = lapply(seq(nrow(tp2_strains())), function(i) {
                         ~HTML(paste("Country:", country[i], "<br/>",
                                     "Province:",province[i], "<br/>",
                                     "Number of TP2 strains:", num_tp2_strains[i], "<br/>",
                                     "Part of TP2 cluster:", tp2_cluster[i],sep=" ")) 
                       }),
                       labelOptions = labelOptions(noHide = F),
                       group = "TP2 strains")
  })
  
  
  ###############################
  # add reactive legend to plot #
  ###############################
  
  observe({
    
    # require input to change to updat map
    req(input$cluster)
    
    # proxy for leaflet map
    proxy <- leafletProxy("map")
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% 
        addLegend(group = "legend", 
                  "bottomleft", 
                  colors = as.character(colorpal_nona$colour), 
                  labels = as.character(colorpal_nona$tp1_cluster),
                  opacity = 1) 
    }
    
  })
  
  
  ###########################################
  # data tables for cluster and strain info #
  ###########################################
  
  # displayed below map
  output$tp1_filtered_grouped_data <- renderReactable({
    reactable(data = eccdata %>% subset(tp1_cluster %in% input$cluster),
              groupBy = c("tp1_cluster", "country"),
              filterable = T,
              searchable = T,
              sortable = T,
              showSortIcon = T,
              showSortable = T,
              compact = TRUE, 
              selection = "multiple")
  })
  

  # displayed in new tab without grouping
  output$tp1_filtered_data <- renderReactable({
    reactable(data = eccdata %>% subset(tp1_cluster %in% input$cluster),  
              columns = list(
                tp1_cluster = colDef(name = "TP1 cluster")
              ),
              # no sorting or filtering for now
              filterable = TRUE,
              searchable = TRUE,
              # sorting 
              sortable = TRUE,
              showSortIcon = TRUE,
              showSortable = TRUE,
              # pagination paramters
              pagination = TRUE,
              defaultPageSize = 10,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100),
              paginationType = "numbers")
  })
  
  
  
  ###########################################
  # raw data table to be displayed in a tab #
  ###########################################
  
  output$raw_data <- renderReactable({
    reactable(data = eccdata,
              filterable = TRUE,
              searchable = TRUE,
              # sorting 
              sortable = TRUE,
              showSortIcon = TRUE,
              showSortable = TRUE,
              # pagination paramters
              pagination = TRUE,
              defaultPageSize = 10,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100),
              paginationType = "numbers")
  })
  
  
  
  
  #########################
  # requests for data vis #
  #########################
  
  # third table - a version of the raw data table that is being filtered 
  # in the same way as the cascading table. i.e. if you select some clusters, 
  # they get filtered on a table similar to the raw table
  
  # link search data to the data selected to view on the map?  
  # i.e. say I filter the raw table for strains from Spain 
  # and let's say that they belong to 10 clusters. 
  # Any way to have those 10 clusters displayed on the map?
  
  ####################################################
  # clickable map to pull up info on clicked cluster #
  ####################################################
  
  observeEvent(input$map_marker_click, { 
    p <- input$map_marker_click  # typo was on this line
    print(p)
    # p contains $id, $.nonce, $group, $lat, $lng
  })
  
  ####################################
  # filter map output based on table #
  ####################################
  
  
  #filtered_table <- reactive({
  #     req(input$mytable_rows_all)
  #     mc[input$mytable_rows_all, ]  
  # })
  # 
  # output$plot1 <- renderPlot({
  #     plot(filtered_table()$wt, filtered_table()$mpg, col = "red", lwd = 10)
  # })
  # 
  # output$test <- renderPrint({
  #     filtered_table()
  # })
  
  
  
  
}


#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)



