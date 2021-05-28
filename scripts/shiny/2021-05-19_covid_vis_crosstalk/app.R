################ 
# General info #
################

# May 19, 2021
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
colnames(eccdata) <- c("strain", "country", "province", "city", "tp1_strain_latitude", 
                       "tp1_strain_longitude", "day", "month", "year", "present_at_tp1", 
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


##############################################
# create tp1 and tp2 strain specific columns #
##############################################

# create additional latitude and longitude for tp2 strains
eccdata$tp2_strain_latitude <- eccdata$tp1_strain_latitude
eccdata$tp2_strain_longitude <- eccdata$tp1_strain_longitude

# na out information that does not pertain to tp1 or tp1 strains
eccdata_na <- apply(eccdata, 1, function(x){
  # create NAs in tp1 data for tp2 strains
  if(x["present_at_tp1"]==0){
    x[c("tp1_strain_latitude", "tp1_strain_longitude", "present_at_tp1", "avg_tp1_date", 
        "avg_tp1_temporal_dist_days", "avg_tp1_geo_dist_km",
        "tp1_t0_ecc_0.1.0", "tp1_t0_ecc_0.0.1", "tp1_cluster_size_2",
        "tp1_cluster_size", "avg_tp1_longitude", "avg_tp1_latitude", 
        "avg_tp2_latitude", "avg_tp2_longitude", "avg_tp2_date",
        "avg_tp2_temporal_dist_days", "avg_tp2_geo_dist_km",
        "tp2_t0_ecc_0.1.0", "tp2_t0_ecc_0.0.1", "tp2_cluster_size_2", 
        "tp2_cluster_size", "delta_ecc_0.1.0", "delta_ecc_0.0.1",  
        "delta_cluster_size", "num_additional_tp1_strains_tp2", 
        "num_novel_tp2_strains", "overall_cluster_growth_rate", 
        "cluster_novel_growth_rate")] <- NA
  } 
  # create na in tp2 strains for tp1 
  else if(x["present_at_tp1"]==1){
    x[c("tp2_strain_latitude", "tp2_strain_longitude", "avg_tp1_date", 
        "avg_tp1_temporal_dist_days", "avg_tp1_geo_dist_km",
        "avg_tp1_longitude", "avg_tp1_latitude", "tp1_cluster_size_2", 
        "tp1_cluster_size", "tp1_t0_ecc_0.1.0", "tp1_t0_ecc_0.0.1",
        "avg_tp2_latitude", "avg_tp2_longitude", "avg_tp2_date",
        "avg_tp2_temporal_dist_days", "avg_tp2_geo_dist_km",
        "tp2_t0_ecc_0.1.0", "tp2_t0_ecc_0.0.1", "tp2_cluster_size_2", 
        "tp2_cluster_size", "delta_ecc_0.1.0", "delta_ecc_0.0.1",  
        "delta_cluster_size", "num_additional_tp1_strains_tp2", 
        "num_novel_tp2_strains", "overall_cluster_growth_rate", 
        "cluster_novel_growth_rate")] <- NA
  } 
  # catch if neither of these conditions exist
  else {print("uh oh")}
  
  # return df
  return(x)
})

# transpose and convert to df
eccdata_nat <- as.data.frame(t(eccdata_na))




##############################################
# create tp1 and tp2 strain specific columns #
##############################################

####### subset out unique tp1 cluster info 
tp1ecc <- eccdata %>% 
  distinct(tp1_cluster, .keep_all = T) 

# na out information that does not pertain to tp1 clusters
tp1ecc_na <- apply(tp1ecc, 1, function(x){
  
  # null out info that is not important 
  x[c("country", "province", 
      "tp1_strain_latitude", "tp1_strain_longitude",
      "tp2_strain_latitude", "tp2_strain_longitude",
      "avg_tp2_latitude", "avg_tp2_longitude","avg_tp2_date",
      "avg_tp2_temporal_dist_days", "avg_tp2_geo_dist_km",
      "tp2_t0_ecc_0.1.0", "tp2_t0_ecc_0.0.1", "tp2_cluster_size_2", 
      "tp2_cluster_size")] <- NA
  
  # set strain equal to tp1 cluster name
  x["strain"]  <- x["tp1_cluster"]
  
  # return df
  return(x)
})


# transpose and convert to df
tp1ecc_nat <- as.data.frame(t(tp1ecc_na))


####### subset out unique tp2 cluster info 
tp2ecc <- eccdata %>% 
  distinct(tp2_cluster, .keep_all = T) 

# na out information that does not pertain to tp1 clusters
tp2ecc_na <- apply(tp2ecc, 1, function(x){
  
  if(x["present_at_tp1"]==1){
    # null out info that is not important 
    x[c("country", "province", 
        "tp1_strain_latitude", "tp1_strain_longitude",
        "tp2_strain_latitude", "tp2_strain_longitude",
        "avg_tp1_latitude", "avg_tp1_longitude", "avg_tp1_date",
        "avg_tp1_temporal_dist_days", "avg_tp1_geo_dist_km",
        "tp1_cluster_size_2", "tp1_cluster_size", 
        "tp1_t0_ecc_0.1.0", "tp1_t0_ecc_0.0.1")] <- NA
    
    # set strain equal to tp1 cluster name
    x["strain"]  <- x["tp2_cluster"]
    
  } else if(x["present_at_tp1"]==0){
    # null out info that is not important 
    x[c("country", "province", 
        "tp1_strain_latitude", "tp1_strain_longitude",
        "tp2_strain_latitude", "tp2_strain_longitude",
        "avg_tp1_latitude", "avg_tp1_longitude", "avg_tp1_date",
        "avg_tp1_temporal_dist_days", "avg_tp1_geo_dist_km", 
        "tp1_cluster_size_2", "tp1_cluster_size",
        "tp1_t0_ecc_0.1.0", "tp1_t0_ecc_0.0.1")] <- NA
    
    # set strain equal to tp1 cluster name
    x["strain"]  <- x["tp2_cluster"]
    
    
  } else {
    print("ugh oh")
  }
  

  
  # return df
  return(x)
})


# transpose and convert to df
tp2ecc_nat <- as.data.frame(t(tp2ecc_na))


###########################
# rbind new data together #
###########################

ecc_all <- rbind(eccdata_nat, tp1ecc_nat, tp2ecc_nat)


#############################
# remove uninformative data #
#############################

# make column for strain date instead of having three separate columns
ecc_all$strain_date <- paste(ecc_all$day, 
                            ecc_all$month, 
                            ecc_all$year, 
                            sep = "-")

# remove month day year and city
ecc_all <- subset(ecc_all, select = -c(month, day, year, city))


################################
# add jitter to lats and longs #
################################

ecc_all <- ecc_all %>%
  mutate(tp1_strain_latitude_jit = round(jitter(as.numeric(tp1_strain_latitude),10,1),digits=4)) %>%
  mutate(tp1_strain_longitude_jit = round(jitter(as.numeric(tp1_strain_longitude),10,1),digits=4)) %>%
  mutate(tp2_strain_latitude_jit = round(jitter(as.numeric(tp2_strain_latitude),10,1),digits=4)) %>%
  mutate(tp2_strain_longitude_jit = round(jitter(as.numeric(tp2_strain_longitude),10,1),digits=4))

# 12-13 decimal places in lats and longs
# too long, round to four for our purposes

#################################
# type convert selected columns #
#################################

# covert appropriate columns to numeric
ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:37, 39:42)] <- 
  sapply(ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:37, 39:42)], as.numeric)


# initialize colorpal as null
colorpal <- NULL

###################################
# user interface of the shiny app #
###################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive SARS-CoV-2 epi-cluster cohesion (ECC) data"),
  
  # data is divided among tables
  # the main tab includes a subsettable map
  tabsetPanel(
    tabPanel("Map",
             
             # map row at top
             fluidRow(
               column(3,
                      wellPanel(
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
                      
                        selectInput("centroid_radius",
                                    "Centroid radius representation:",
                                    choices = c("Avg geo dist",
                                                "Tot num strain"),
                                    selected = "Avg geo dist", 
                                    multiple = FALSE),
                        
                        checkboxInput("legend", "Show legend", TRUE))),
               
               column(9,leafletOutput("map"))
             ),
             
             # data at bottom
             fluidRow(
               
               column(3,
                      wellPanel(
                        #cluster selection
                        selectizeInput("cluster",
                                       "TP1 clusters:",
                                       choices = sort(unique(ecc_all$tp1_cluster)),
                                       options = list(maxItems = 8L),
                                       multiple = T),

                        # cluster selection
                        selectizeInput("country",
                                       "Country:",
                                       choices = sort(unique(ecc_all$country)),
                                       multiple = T),

                        # province selection
                        selectizeInput("province",
                                       "Province:",
                                       choices = sort(unique(ecc_all$province)),
                                       multiple = T),
                        
                        sliderInput("tp1_cluster_size",
                                    "TP1 cluster size:",
                                    min = 0,
                                    max = max(na.omit(ecc_all$tp1_cluster_size_2)),
                                    step = 5,
                                    value = c(0,max(na.omit(ecc_all$tp1_cluster_size_2)))),
                        
                        sliderInput("tp2_cluster_size",
                                    "TP2 cluster size:",
                                    min = 0,
                                    max = max(na.omit(ecc_all$tp2_cluster_size_2)),
                                    step = 5,
                                    value = c(0,max(na.omit(ecc_all$tp2_cluster_size_2)))),
                        
                        # tp1 geospatial ecc
                        sliderInput("tp1_ecc_0.1.0",
                                    "TP1 geospatial ECC:",
                                    min = 0,
                                    max = 1,
                                    step = 0.01,
                                    value = c(0,1)),
                        
                        # tp1 temporal ecc
                        sliderInput("tp1_ecc_0.0.1",
                                    "TP1 temporal ECC:",
                                    min = 0,
                                    max = 1,
                                    step = 0.01,
                                    value = c(0,1)),
                        
                        # tp2 geospatial ecc
                        sliderInput("tp2_ecc_0.1.0",
                                    "TP2 geospatial ECC:",
                                    min = 0,
                                    max = 1,
                                    step = 0.01,
                                    value = c(0,1)),
                        
                        # tp2 temporal ecc
                        sliderInput("tp2_ecc_0.0.1",
                                    "TP2 temporal ECC:",
                                    min = 0,
                                    max = 1,
                                    step = 0.01,
                                    value = c(0,1)),
                        
                        # delta geospatial ecc
                        sliderInput("delta_ecc_0.1.0",
                                    "Delta geospatial ECC:",
                                    min = -1,
                                    max = 1,
                                    step = 0.01,
                                    value = c(-1,1)),
                        
                        # delta temporal ecc
                        sliderInput("delta_ecc_0.0.1",
                                    "Delta temporal ECC:",
                                    min = -1,
                                    max = 1,
                                    step = 0.01,
                                    value = c(-1,1))
                      )),
               column(9,
                      reactableOutput("ecctable")
               )
               
             )
             
    ), 
    tabPanel("Raw data",reactableOutput("raw_data")), 
    tabPanel("Filtered data", reactableOutput("tp1_filtered_data"))
  )
  
)


####################
# server interface #
####################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  ###################################
  # rendering the UI based on input #
  ###################################
  
  # update number of available selections for cluster (max 8)
  observe({
    updateSelectizeInput(session, 
                         "cluster", 
                         selected = isolate(input$cluster), 
                         options = list(maxItems = 8L - length(input$clusters)))
  })
  
  
  ######################################
  # data filtering based on selections #
  ######################################
  
  # filter based
  ecc_all_filtered <- reactive({
    
    ecc_all %>% 
      # if selection is not null then filter on it
      {if (!is.null(input$cluster)) filter(., tp1_cluster %in% input$cluster) else .} %>% 
      {if (!is.null(input$country)) filter(., country %in% input$country) else .} %>%
      {if (!is.null(input$province)) filter(., province %in% input$province) else .}
    
    #{if (FALSE) select(., cyl) else .}
  })
  
  
  # unique tp1 clusters for table 
  ecc_all_filtered_tp1 <- reactive({
    
    isolate(ecc_all_filtered()) %>%
      filter(grepl('TP1|^0$', strain)) %>%
      {if (!is.null(input$cluster)) filter(., strain %in% input$cluster) else .} %>% 
      distinct(tp1_cluster, .keep_all = T) %>%
      select(tp1_cluster, avg_tp1_latitude, avg_tp1_longitude, avg_tp1_geo_dist_km, 
                 avg_tp1_temporal_dist_days, avg_tp2_latitude, avg_tp2_longitude, 
                 avg_tp2_geo_dist_km, avg_tp2_temporal_dist_days, tp1_t0_ecc_0.1.0, 
                 tp1_t0_ecc_0.0.1, tp2_t0_ecc_0.1.0, tp2_t0_ecc_0.0.1, delta_ecc_0.1.0,
                 delta_ecc_0.0.1, tp1_cluster_size_2, tp2_cluster_size_2, tp2_cluster,
                 delta_cluster_size, num_additional_tp1_strains_tp2, num_novel_tp2_strains, 
                 overall_cluster_growth_rate, cluster_novel_growth_rate) %>% 
      relocate(tp1_cluster, tp1_cluster_size_2, tp1_t0_ecc_0.1.0, tp1_t0_ecc_0.0.1,
               avg_tp1_geo_dist_km, avg_tp1_temporal_dist_days,
               avg_tp1_latitude, avg_tp1_longitude,
               tp2_cluster, tp2_cluster_size_2, tp2_t0_ecc_0.1.0, tp2_t0_ecc_0.0.1,
               avg_tp2_geo_dist_km, avg_tp2_temporal_dist_days,
               avg_tp2_latitude, avg_tp2_longitude,
               delta_cluster_size, delta_ecc_0.1.0, delta_ecc_0.0.1,
               num_additional_tp1_strains_tp2, num_novel_tp2_strains,
               overall_cluster_growth_rate, cluster_novel_growth_rate)

    
    # this gets unique tp2 strains
    # (ecc_all %>% filter(grepl('TP1|^0$|TP2', strain)) %>% distinct(strain, tp1_cluster, .keep_all = T)) %>% filter(grepl('TP2', strain))
    # make.names for unique naming 
  })
  
  ###################################################################
  # reactable that appears below map and updates on user selections #
  ###################################################################
  
  # create a table using the shared data 
  output$ecctable <- renderReactable({
    

    
    reactable(data = ecc_all_filtered_tp1(),
              details = function(index) {
                plant_data <-  ecc_all_filtered() %>% 
                  subset(tp1_cluster == ecc_all_filtered_tp1()$tp1_cluster[index]) %>%
                  select(country, province, present_at_tp1, present_at_tp2, strain,
                         tp1_strain_latitude, tp1_strain_longitude, 
                         tp2_strain_latitude, tp2_strain_longitude,
                         strain_date) %>% 
                  # filter to remove cluster info from strains 
                  relocate(country, province, strain, strain_date, present_at_tp1, 
                           present_at_tp2, tp1_strain_latitude, tp1_strain_longitude, 
                           tp2_strain_latitude, tp2_strain_longitude)
                htmltools::div(style = "padding: 16px",
                               reactable(plant_data, 
                                         outlined = TRUE,
                                         compact = TRUE,
                                         resizable = TRUE,
                                         sortable = T,
                                         highlight = T,
                                         showSortIcon = T,
                                         showSortable = T,
                                         pagination = F,
                                         height = 600, 
                                         columns = list(country = colDef(name = "Country"),
                                                        province = colDef(name = "Province"),
                                                        strain = colDef(name = "Strain"),
                                                        present_at_tp1 = colDef(name = "Present at TP1"),
                                                        present_at_tp2 = colDef(name = "Present at TP2"),
                                                        tp1_strain_latitude = colDef(name = "TP1 strain latitude"),
                                                        tp1_strain_longitude = colDef(name = "TP1 strain longitude"),
                                                        tp2_strain_latitude = colDef(name = "TP2 strain latitude"),
                                                        tp2_strain_longitude = colDef(name = "TP2 strain longitude"),
                                                        strain_date = colDef(name = "Date identified"))
                               ))
              },
              compact = TRUE,
              resizable = TRUE,
              sortable = T,
              highlight = T,
              showSortIcon = T,
              showSortable = T,
              pagination = TRUE,
              defaultPageSize = 10,
              showPageSizeOptions = T,
              pageSizeOptions = c(10, 25, 50, 100),
              paginationType = "numbers",
              showPageInfo = TRUE,
              minRows = 1,
              wrap = TRUE,
              height = 500,
              #selection = {if(!is.null(input$cluster)) "multiple" else NULL },  
              onClick = "select",
              # theme = reactableTheme(
              #   rowSelectedStyle = list(backgroundColor = "hsl(58, 100%, 76%)", boxShadow = "inset 2px 0 0 0 #ffa62d")
              # ),
              columns = list(tp1_cluster = colDef(name = "TP1 cluster"),
                             tp2_cluster = colDef(name = "TP2 cluster"),
                             avg_tp1_latitude = colDef(name = "TP1 cluster latitude"),
                             avg_tp1_longitude = colDef(name = "TP1 cluster longitude" ),
                             avg_tp1_geo_dist_km = colDef(name = "Average distance between TP1 strains (km)"),
                             avg_tp1_temporal_dist_days = colDef(name = "Average time between TP1 strains (days)"),
                             avg_tp2_latitude = colDef(name = "TP2 cluster latitude"),
                             avg_tp2_longitude = colDef(name = "TP2 cluster longitude"),
                             avg_tp2_geo_dist_km = colDef(name = "Average distance between TP2 strains (km)"),
                             avg_tp2_temporal_dist_days = colDef(name = "Average time between TP2 strains (days)"),
                             tp1_t0_ecc_0.1.0 = colDef(name = "TP1 cluster geospatial ECC"),
                             tp1_t0_ecc_0.0.1 = colDef(name = "TP1 cluster temporal ECC"),
                             tp2_t0_ecc_0.1.0 = colDef(name = "TP2 cluster geospatial ECC"),
                             tp2_t0_ecc_0.0.1 = colDef(name = "TP2 cluster temporal ECC"),
                             delta_ecc_0.1.0 = colDef(name = "Delta geospatial ECC"),
                             delta_ecc_0.0.1 = colDef(name = "Delta temporal ECC"),
                             tp1_cluster_size_2 = colDef(name = "TP1 cluster size"),
                             tp2_cluster_size_2 = colDef(name = "TP2 cluster size"),
                             delta_cluster_size = colDef(name = "Delta cluster size"),
                             num_additional_tp1_strains_tp2 = colDef(name = "Number of additional TP1 strains in TP2 cluster"),
                             num_novel_tp2_strains =  colDef(name = "Number of novel TP2 strains"),
                             overall_cluster_growth_rate = colDef(name = "Overall cluster growth rate"),
                             cluster_novel_growth_rate = colDef(name = "Novel cluster growth rate")
                             )
    )
  })
  
  
  ################################################
  # filtering map data based on table selections #
  ################################################
  
  selected <- reactive(getReactableState("ecctable", "selected"))
  
  observe({
    print(ecc_all_filtered() %>% slice(as.numeric(selected())))
    print(selected())
  })
  
  
  ############################################
  # table rendering based on map data clicks #
  ############################################
  
   observeEvent(input$map_marker_click, { 
    # get info from marker click
    p <- input$map_marker_click
    
    # get the corresponding rownum in table
    rownum1 <- intersect(which(ecc_all_filtered()$tp1_strain_latitude_jit == p$lat),
                        which(ecc_all_filtered()$tp1_strain_longitude_jit == p$lng))
    
    rownum2 <- intersect(which(ecc_all_filtered()$tp2_strain_latitude_jit == p$lat),
                        which(ecc_all_filtered()$tp2_strain_longitude_jit == p$lng))
    
    # get the corresponding rownum in table
    rownum3 <- intersect(which(ecc_all_filtered()$avg_tp1_latitude == p$lat),
                        which(ecc_all_filtered()$avg_tp1_longitude == p$lng))
    
    rownum4 <- intersect(which(ecc_all_filtered()$avg_tp2_latitude == p$lat),
                        which(ecc_all_filtered()$avg_tp2_longitude == p$lng))
    
    rownum <- c(rownum1, rownum2, rownum3, rownum4)
    
    # updateReactable("ecctable",
    #                 selected = rownum,
    #                 expanded = T)
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
                       options = layersControlOptions(collapsed = TRUE)) %>%
      
      hideGroup("Average centroid")
  })
  
  
  ##########################################
  # add circle markers to base leaflet map #
  ##########################################
  
  # list for any change in input
  toListen <- reactive({
    list(input$cluster, input$country, input$province, 
         input$strain_transparency, input$centroid_transparency,
         input$centroid_radius)
  })
  
  observeEvent(toListen(),{
    
    ################### color pal function start #################
    
    # this should be moved into a function eventually
    # if some inputs are old
    if(sum(as.character(input$cluster) %in% as.character(colorpal$tp1_cluster)) > 0) {
      
      # find out position of exisiting cluster(s) in colorpal
      pal_pos <- colorpal[colorpal$tp1_cluster %in% input$cluster, "rownum"]
      
      # set other positions to NA
      colorpal[!colorpal$rownum %in% pal_pos, "tp1_cluster"] <- as.character(NA)
      
      # reorder according to rownum
      colorpal <- colorpal[order(colorpal$rownum),]
      
      # get NA positions
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
    
    ################### color pal function end #################
    
    
    # leaflet map with markers
    proxy <- leafletProxy("map")
    
      # clear old shapes
      proxy %>% clearMarkers()
      
      req(input$cluster)
      
      # add tp1 centroids
      if(length(na.omit(ecc_all_filtered()$avg_tp1_latitude))>0){
        
        print("updating tp1 centroids")
        proxy %>% 
          addCircleMarkers(data = ecc_all_filtered(),
                           lat =  ~avg_tp1_latitude,
                           lng =  ~avg_tp1_longitude,
                           radius = {if(input$centroid_radius=="Avg geo dist") ~log10(as.numeric(avg_tp1_geo_dist_km))*10 else ~log10(as.numeric(tp1_cluster_size))*10},
                           fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                           fillOpacity = input$centroid_transparency/100,
                           stroke = T,
                           opacity = input$centroid_transparency/100,
                           weight = 1,
                           color = "white",
                           label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                             ~HTML(paste("<b>", tp1_cluster[i],"</b>", "<br/>",
                                         "Num of strains:",tp1_cluster_size_2[i]-1,"<br/>",
                                         "Temporal ECC:", tp1_t0_ecc_0.0.1[i],"<br/>",
                                         "Geo ECC:",tp1_t0_ecc_0.1.0[i],"<br/>",
                                         "Avg date:", avg_tp1_date[i],"<br/>", sep=" ")) 
                           }),
                           group = "TP1 centroid")
      }
      
      # add tp2 centroids
      if(length(na.omit(ecc_all_filtered()$avg_tp2_latitude))>0){
        
        print("updating tp2 centroids")
        proxy %>% 
          addCircleMarkers(data = ecc_all_filtered(),
                           lat = ~avg_tp2_latitude,
                           lng = ~avg_tp2_longitude,  
                           radius = {if(input$centroid_radius=="Avg geo dist") ~log10(as.numeric(avg_tp2_geo_dist_km))*10 else ~log10(as.numeric(tp2_cluster_size))*10},
                           fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                           fillOpacity = input$centroid_transparency/100,
                           stroke = T,
                           opacity = input$centroid_transparency/100,
                           weight = 1,
                           color = "black",
                           label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                             ~HTML(paste("<b>",  tp2_cluster[i], "</b>", "<br/>",
                                         "Num of strains:",tp2_cluster_size_2[i]-1,"<br/>",
                                         "Num novel strains:",num_novel_tp2_strains[i],"<br/>",
                                         "Temporal ECC:", tp2_t0_ecc_0.0.1[i],"<br/>",
                                         "Geo ECC:",tp2_t0_ecc_0.1.0[i],"<br/>",
                                         "Delta temporal ECC:", delta_ecc_0.0.1[i],"<br/>",
                                         "Delta geo ECC:",delta_ecc_0.1.0[i],"<br/>",
                                         "Avg date:", avg_tp2_date[i],"<br/>", sep=" ")) 
                           }),
                           group = "TP2 centroid")
      }
      
      # add tp1 strains
      if(length(na.omit(ecc_all_filtered()$tp1_strain_latitude))>0){
        
        print("updating tp1 strains")
        proxy %>% 
          addCircleMarkers(data = ecc_all_filtered(),
                           layerId = ~strain,
                           lat = ~as.numeric(as.character(tp1_strain_latitude_jit), 10),
                           lng = ~as.numeric(as.character(tp1_strain_longitude_jit), 10),
                           radius = 5,
                           fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                           fillOpacity = input$strain_transparency/100,
                           stroke = T,
                           opacity = input$strain_transparency/100,
                           weight = 1,
                           color = "white",
                           label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                             ~HTML(paste("Strain:", "<b>", strain[i], "</b>", "<br/>",
                                         "Country:", country[i], "<br/>",
                                         "Province:",province[i],"<br/>",
                                         "Date:",strain_date[i],"<br/>", sep=" ")) 
                           }),
                           group = "TP1 strains")
      }
      
      # add tp2 strains
      if(length(na.omit(ecc_all_filtered()$tp2_strain_latitude))>0){

        print("updating tp2 strains")
        proxy %>%
          addCircleMarkers(data = ecc_all_filtered(),
                           lat = ~as.numeric(as.character(tp2_strain_latitude_jit), 10),
                           lng = ~as.numeric(as.character(tp2_strain_longitude_jit), 10),
                           radius = 5,
                           fillColor = ~unname(unlist(sapply(tp1_cluster, function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector"))),
                           fillOpacity = input$strain_transparency/100,
                           stroke = T,
                           opacity = input$strain_transparency/100,
                           weight = 1,
                           color = "black",
                           label = lapply(seq(nrow(ecc_all_filtered())), function(i) {
                             ~HTML(paste("Strain:", "<b>", strain[i], "</b>", "<br/>",
                                         "Country:", country[i], "<br/>",
                                         "Province:",province[i],"<br/>",
                                         "Date:",strain_date[i],"<br/>", sep=" ")) 
                           }),
                           group = "TP2 strains")
      }
      
  }, ignoreNULL = F) # forces section to run when input is null
  
  ###############################
  # add reactive legend to plot #
  ###############################
  
  # list for any change in input
  toListenLegend <- reactive({
    list(input$cluster, input$legend)
  })
  
  
  observeEvent(toListenLegend(), {
    
    # print statement for debugging
    print("updating legend")
    
    # proxy for leaflet map
    proxy <- leafletProxy("map")
    
    # Remove any existing legend, and only if the legend is
    # enabled, and only when the legend is changed (this includes
    # when all selection are removed)
    proxy %>% clearControls()
    
    # require input cluster to not be null to add new selection to legent 
    req(input$cluster)
    
    # update legend
    if (input$legend) {
      proxy %>% 
        addLegend(group = "legend", 
                  "bottomleft", 
                  colors = as.character(colorpal_nona$colour), 
                  labels = as.character(colorpal_nona$tp1_cluster),
                  opacity = 1) 
    }
  }, ignoreNULL = F) # forces section to run when input is null
  
  
  ####################################################
  # raw data filtered table to be displayed in a tab #
  #####################################################
  
  output$raw_data <- renderReactable({
    reactable(data = eccdata,
              filterable = TRUE,
              searchable = TRUE,
              sortable = TRUE,
              showSortIcon = TRUE,
              showSortable = TRUE,
              pagination = TRUE,
              defaultPageSize = 10,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100),
              paginationType = "numbers")
  })
  
  
  ##############################################
  # filtered data to be displayed in a new tab #
  ##############################################
  
  # displayed in new tab without grouping
  output$tp1_filtered_data <- renderReactable({
    reactable(data = ecc_all_filtered(),  
              filterable = TRUE,
              searchable = TRUE,
              sortable = TRUE,
              showSortIcon = TRUE,
              showSortable = TRUE,
              pagination = TRUE,
              defaultPageSize = 10,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 25, 50, 100),
              paginationType = "numbers")
  })
  
  
  
}

#######################
# Run the application #
#######################
shinyApp(ui = ui, server = server)



#############
# bug notes #
#############

# need to add jitter in before plotting - otherwise gets confusing
# because points move each time plot is updated



##############
# scrap code #
##############




# reactable(data = ecc_all_filtered(),
#           compact = TRUE,
#           resizable = TRUE, 
#           sortable = T,
#           highlight = T,
#           showSortIcon = T,
#           showSortable = T,
#           pagination = TRUE,
#           defaultPageSize = 10,
#           showPageSizeOptions = T,
#           pageSizeOptions = c(10, 25, 50, 100),
#           paginationType = "numbers",
#           showPageInfo = TRUE,
#           minRows = 1,
#           wrap = TRUE,
#           height = 500, 
#           defaultSorted = "tp1_cluster", 
#           groupBy = c("tp1_cluster", "country"),
#           selection = {if(!is.null(input$cluster)) "multiple" else NULL },  
#           onClick = "select",
#           theme = reactableTheme(
#             rowSelectedStyle = list(backgroundColor = "hsl(58, 100%, 76%)", boxShadow = "inset 2px 0 0 0 #ffa62d")
#           ),
#           columns = list(tp1_cluster = colDef(name = "TP1 cluster"),
#                          country = colDef(name = "Country"),
#                          province = colDef(name = "Province"),
#                          strain = colDef(name = "Strain"),
#                          present_at_tp1 = colDef(name = "Present at TP1"),
#                          tp1_strain_latitude = colDef(name = "TP1 strain latitude"),
#                          tp1_strain_longitude = colDef(name = "TP1 strain longitude"),
#                          tp2_strain_latitude = colDef(name = "TP2 strain latitude"),
#                          tp2_strain_longitude = colDef(name = "TP2 strain longitude"),
#                          avg_tp1_latitude = colDef(name = "TP1 cluster latitude"),
#                          avg_tp1_longitude = colDef(name = "TP1 cluster longitude" ),
#                          avg_tp1_geo_dist_km = colDef(name = "Average distance between TP1 strains (km)"),
#                          avg_tp1_temporal_dist_days = colDef(name = "Average time between TP1 strains (days)"),
#                          avg_tp2_latitude = colDef(name = "TP2 cluster latitude"),
#                          avg_tp2_longitude = colDef(name = "TP2 cluster longitude"),
#                          avg_tp2_geo_dist_km = colDef(name = "Average distance between TP2 strains (km)"),
#                          avg_tp2_temporal_dist_days = colDef(name = "Average time between TP2 strains (days)"),
#                          tp1_t0_ecc_0.1.0 = colDef(name = "TP1 cluster geospatial ECC"),
#                          tp1_t0_ecc_0.0.1 = colDef(name = "TP1 cluster temporal ECC",),
#                          tp2_t0_ecc_0.1.0 = colDef(name = "TP2 cluster geospatial ECC"),
#                          tp2_t0_ecc_0.0.1 = colDef(name = "TP2 cluster temporal ECC"),
#                          delta_ecc_0.1.0 = colDef(name = "Delta geospatial ECC"),
#                          delta_ecc_0.0.1 = colDef(name = "Delta temporal ECC"),
#                          tp1_cluster_size_2 = colDef(name = "TP1 cluster size"),
#                          tp2_cluster_size_2 = colDef(name = "TP2 cluster size"),
#                          delta_cluster_size = colDef(name = "Delta cluster size"),
#                          num_additional_tp1_strains_tp2 = colDef(name = "Number of additional TP1 strains in TP2 cluster"),
#                          num_novel_tp2_strains =  colDef(name = "Number of novel TP2 strains"),
#                          overall_cluster_growth_rate = colDef(name = "Overall cluster growth rate"),    
#                          cluster_novel_growth_rate = colDef(name = "Novel cluster growth rate"),
#                          type = colDef(show=FALSE),
#                          tp1_strain_latitude_jit = colDef(show=FALSE), 
#                          tp1_strain_longitude_jit = colDef(show=FALSE),
#                          tp2_strain_latitude_jit = colDef(show=FALSE), 
#                          tp2_strain_longitude_jit = colDef(show=FALSE),
#                          tp1_cluster_size = colDef(show=FALSE),
#                          tp2_cluster_size = colDef(show=FALSE)
#                          ))





# # get tp1 cluster
# tp1clust <- ecc_all_filtered()$tp1_cluster[rownum]
# 
# 
# print(tp1clust)
# 
# # get the index of tp1_cluster in unique
# tp1clustpos <- which(unique(ecc_all_filtered()$tp1_cluster)==tp1clust)
# 
# print(tp1clustpos )

# # get page size
# pagesize <- getReactableState("ecctable", name = "pageSize", session = NULL)
# 
# # round down page
# page = floor(tp1clustpos/pagesize)
# 



# returns jittered lats and longs
# these should USUALLY be unique 
# as the jitter function adds in 
# RANDOM noise 


