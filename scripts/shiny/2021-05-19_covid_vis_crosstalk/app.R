
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
library(plotly) # for interactive ggplots

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
colnames(eccdata) <- c("strain", "country", "province", "city", "strain_latitude", 
                       "strain_longitude", "day", "month", "year", "present_at_tp1", 
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

###############################
# create columsn for plotting #
###############################

# problem with data:
# info is kind of wide and long which makes it difficult to plot 
# clusters and strains without modifying format
# instead of creating a super long data frame (which would not be ideal for 
# data sets with millions of observations)
# create additional latitude and longitude columns for 
# plotting that can be NA'd out as needed

# create additional latitude and longitude for tp1 and tp2 strains and clusters
eccdata$tp2_strain_latitude_plot <- eccdata$strain_latitude
eccdata$tp2_strain_longitude_plot <- eccdata$strain_longitude
eccdata$tp1_strain_latitude_plot <- eccdata$strain_latitude
eccdata$tp1_strain_longitude_plot <- eccdata$strain_longitude
eccdata$avg_tp1_latitude_plot <- eccdata$avg_tp1_latitude
eccdata$avg_tp2_latitude_plot <- eccdata$avg_tp2_latitude
eccdata$avg_tp1_longitude_plot <- eccdata$avg_tp1_longitude
eccdata$avg_tp2_longitude_plot <- eccdata$avg_tp2_longitude

# na out information that does not pertain to tp1 or tp1 strains
eccdata_na <- apply(eccdata, 1, function(x){
  # create NAs in tp1 data for tp2 strains
  if(x["present_at_tp1"]==0){
    x[c("tp1_strain_latitude_plot", "tp1_strain_longitude_plot",  
        "avg_tp1_longitude_plot", "avg_tp1_latitude_plot", 
        "avg_tp2_latitude_plot", "avg_tp2_longitude_plot") ] <- NA
  } 
  # create na in tp2 strains for tp1 
  else if(x["present_at_tp1"]==1){
    x[c("tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
        "avg_tp1_longitude_plot", "avg_tp1_latitude_plot", 
        "avg_tp2_latitude_plot", "avg_tp2_longitude_plot")] <- NA
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
      "tp1_strain_latitude_plot", "tp1_strain_longitude_plot",
      "tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
      "avg_tp2_latitude_plot", "avg_tp2_longitude_plot")] <- NA
  
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
    x[c("country", "province", 
        "tp1_strain_latitude_plot", "tp1_strain_longitude_plot",
        "tp2_strain_latitude_plot", "tp2_strain_longitude_plot",
        "avg_tp1_latitude_plot", "avg_tp1_longitude_plot")] <- NA
    
    # set strain equal to tp1 cluster name
    x["strain"]  <- x["tp2_cluster"]
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

ecc_all$strain_date <- gsub(" ", "", ecc_all$strain_date)

# remove month day year and city
ecc_all <- subset(ecc_all, select = -c(month, day, year, city))


################################
# add jitter to lats and longs #
################################

ecc_all <- ecc_all %>%
  mutate(tp1_strain_latitude_jit = round(jitter(as.numeric(tp1_strain_latitude_plot),10,1),digits=4)) %>%
  mutate(tp1_strain_longitude_jit = round(jitter(as.numeric(tp1_strain_longitude_plot),10,1),digits=4)) %>%
  mutate(tp2_strain_latitude_jit = round(jitter(as.numeric(tp2_strain_latitude_plot),10,1),digits=4)) %>%
  mutate(tp2_strain_longitude_jit = round(jitter(as.numeric(tp2_strain_longitude_plot),10,1),digits=4))

# 12-13 decimal places in lats and longs
# too long, round to four for our purposes

#################################
# type convert selected columns #
#################################

# covert appropriate columns to numeric
ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:43, 45:48)] <- 
  sapply(ecc_all[,c(4:6, 8:11, 13:17, 19:22, 24:43, 45:48)], as.numeric)


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
               
               column(4,leafletOutput("map")),
               column(4, plotlyOutput(outputId = "ridgeplot"))
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
               # cluster table next to controls
               column(9,
                      reactableOutput("clustertable")
               )
             ),
             
             # Strain table under cluster table
             fluidRow(
               column(3),
               column(9,
                      reactableOutput("straintable")
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
  
  
  ##################################################
  # initialize some reactive values for map clicks #
  ##################################################
  
  strain_click <- reactiveVal(NA)
  cluster_click <- reactiveVal(NA)
  
  
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
  
  # table of filtered data used for plotting
  # and creation of other dependent tables 
  ecc_all_filtered <- reactive({
    
    ecc_all %>% 
      # if selection is not null then filter on it
      {if (!is.null(input$cluster)) filter(., tp1_cluster %in% input$cluster) else .} %>% 
      {if (!is.null(input$country)) filter(., country %in% input$country) else .} %>%
      {if (!is.null(input$province)) filter(., province %in% input$province) else .} %>%
      filter(.,  tp1_t0_ecc_0.1.0 >= input$tp1_ecc_0.1.0[1] & tp1_t0_ecc_0.1.0 <= input$tp1_ecc_0.1.0[2])  %>%
      filter(.,  tp1_t0_ecc_0.0.1 >= input$tp1_ecc_0.0.1[1] & tp1_t0_ecc_0.0.1 <= input$tp1_ecc_0.0.1[2])  %>%
      filter(.,  tp2_t0_ecc_0.1.0 >= input$tp2_ecc_0.1.0[1] & tp2_t0_ecc_0.1.0 <= input$tp2_ecc_0.1.0[2])  %>%
      filter(.,  tp2_t0_ecc_0.0.1 >= input$tp2_ecc_0.0.1[1] & tp2_t0_ecc_0.0.1 <= input$tp2_ecc_0.0.1[2])  %>%
      filter(.,  delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2])  %>%
      filter(.,  delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2])  %>%
      filter(.,  tp1_cluster_size_2 >= input$tp1_cluster_size[1] & tp1_cluster_size_2 <= input$tp1_cluster_size[2])  %>%
      filter(.,  tp2_cluster_size_2 >= input$tp2_cluster_size[1] & tp2_cluster_size_2 <= input$tp2_cluster_size[2])
    #{if (FALSE) select(., cyl) else .}
  })
  
  # table of unique tp1 cluste rin 
  cluster_table <- reactive({
    
    ecc_all %>%
      # grab tp1 strains
      filter(grepl('TP1|^0$', strain)) %>%
      # filter according to clusters if specified
      {if (!is.null(input$cluster)) filter(., tp1_cluster %in% input$cluster) else .} %>%
      # filter according to other parameters
      filter(.,  tp1_t0_ecc_0.1.0 >= input$tp1_ecc_0.1.0[1] & tp1_t0_ecc_0.1.0 <= input$tp1_ecc_0.1.0[2])  %>%
      filter(.,  tp1_t0_ecc_0.0.1 >= input$tp1_ecc_0.0.1[1] & tp1_t0_ecc_0.0.1 <= input$tp1_ecc_0.0.1[2])  %>%
      filter(.,  tp2_t0_ecc_0.1.0 >= input$tp2_ecc_0.1.0[1] & tp2_t0_ecc_0.1.0 <= input$tp2_ecc_0.1.0[2])  %>%
      filter(.,  tp2_t0_ecc_0.0.1 >= input$tp2_ecc_0.0.1[1] & tp2_t0_ecc_0.0.1 <= input$tp2_ecc_0.0.1[2])  %>%
      filter(.,  delta_ecc_0.1.0 >= input$delta_ecc_0.1.0[1] & delta_ecc_0.1.0 <= input$delta_ecc_0.1.0[2])  %>%
      filter(.,  delta_ecc_0.0.1 >= input$delta_ecc_0.0.1[1] & delta_ecc_0.0.1 <= input$delta_ecc_0.0.1[2])  %>%
      filter(.,  tp1_cluster_size_2 >= input$tp1_cluster_size[1] & tp1_cluster_size_2 <= input$tp1_cluster_size[2])  %>%
      filter(.,  tp2_cluster_size_2 >= input$tp2_cluster_size[1] & tp2_cluster_size_2 <= input$tp2_cluster_size[2])  %>%
      # only keep 1 unique instance of each tp1 cluster
      distinct(tp1_cluster, .keep_all = T) %>%
      # select the columns to be included in the table
      select(tp1_cluster, avg_tp1_latitude, avg_tp1_longitude, avg_tp1_geo_dist_km, 
             avg_tp1_temporal_dist_days, avg_tp2_latitude, avg_tp2_longitude, 
             avg_tp2_geo_dist_km, avg_tp2_temporal_dist_days, tp1_t0_ecc_0.1.0, 
             tp1_t0_ecc_0.0.1, tp2_t0_ecc_0.1.0, tp2_t0_ecc_0.0.1, delta_ecc_0.1.0,
             delta_ecc_0.0.1, tp1_cluster_size_2, tp2_cluster_size_2, tp2_cluster,
             delta_cluster_size, num_additional_tp1_strains_tp2, num_novel_tp2_strains, 
             overall_cluster_growth_rate, cluster_novel_growth_rate, 
             avg_tp1_latitude_plot, avg_tp1_longitude_plot,
             avg_tp2_latitude_plot, avg_tp2_longitude_plot) %>% 
      # reorder the columsn for clarity
      relocate(tp1_cluster, tp1_cluster_size_2, tp1_t0_ecc_0.1.0, tp1_t0_ecc_0.0.1,
               avg_tp1_geo_dist_km, avg_tp1_temporal_dist_days,
               avg_tp1_latitude, avg_tp1_longitude,
               tp2_cluster, tp2_cluster_size_2, tp2_t0_ecc_0.1.0, tp2_t0_ecc_0.0.1,
               avg_tp2_geo_dist_km, avg_tp2_temporal_dist_days,
               avg_tp2_latitude, avg_tp2_longitude,
               delta_cluster_size, delta_ecc_0.1.0, delta_ecc_0.0.1,
               num_additional_tp1_strains_tp2, num_novel_tp2_strains,
               overall_cluster_growth_rate, cluster_novel_growth_rate,
               avg_tp1_latitude_plot, avg_tp1_longitude_plot,
               avg_tp2_latitude_plot, avg_tp2_longitude_plot)
  })
  
  # table of strain info 
  strain_table <- reactive({
    
    ecc_all_filtered() %>% 
      # subset according to cluster table
      #filter(tp1_cluster %in% cluster_table()$tp1_cluster) %>%
      # remove cluster specific info
      filter(!grepl('TP1|^0$|TP2', strain)) %>%
      # selection strains needed for table/subsetting
      select(country, province, present_at_tp1, present_at_tp2, strain,
             strain_latitude, strain_longitude, strain_date, tp1_cluster, 
             tp1_strain_latitude_jit, tp1_strain_longitude_jit,
             tp2_strain_latitude_jit, tp2_strain_longitude_jit) %>%
      # reorder for clarity in table 
      relocate(country, province, strain, strain_date, present_at_tp1, 
               present_at_tp2, strain_latitude, strain_longitude,
               tp1_strain_latitude_jit, tp1_strain_longitude_jit,
               tp2_strain_latitude_jit, tp2_strain_longitude_jit)
    
  })
  
  
  #################################################################
  # cluster and strain reactable tables that appear on first page #
  #################################################################
  
  # table for cluster info  
  output$clustertable <- renderReactable({
    
    reactable(data = cluster_table(),
              compact = TRUE,
              resizable = TRUE,
              sortable = TRUE,
              highlight = TRUE,
              showSortIcon = TRUE,
              showSortable = TRUE,
              height = 600, 
              minRows = 1,
              wrap = TRUE,
              selection = {if(!is.null(input$cluster)) "multiple" else NULL },  
              onClick = "select",
              theme = reactableTheme(rowSelectedStyle = list(boxShadow = "inset 4px 0 0 0 #ffa62d")), 
              rowStyle = {if(!is.na(cluster_click()[1]))
                function(index) {
                  if (index == cluster_click()) list(backgroundColor = "hsl(58, 100%, 76%)")
                }else NULL },
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
                             cluster_novel_growth_rate = colDef(name = "Novel cluster growth rate"),
                             avg_tp1_latitude_plot = colDef(show = FALSE), 
                             avg_tp1_longitude_plot = colDef(show = FALSE),
                             avg_tp2_latitude_plot = colDef(show = FALSE),
                             avg_tp2_longitude_plot = colDef(show = FALSE)
              )
    )
  })
  
  # table for strain info 
  output$straintable <- renderReactable({
    
    reactable(strain_table(), 
              outlined = TRUE,
              compact = TRUE,
              resizable = TRUE,
              sortable = TRUE,
              showSortIcon = TRUE,
              showSortable = TRUE,
              highlight = TRUE, 
              pagination = FALSE,
              wrap=TRUE, 
              height = 600, 
              # selections are only available once clusters are chosen 
              selection = {if(!is.null(input$cluster)) "multiple" else NULL },  
              onClick = "select",
              # add dark yellow tab to selected rows 
              theme = reactableTheme(rowSelectedStyle = list(boxShadow = "inset 4px 0 0 0 #ffa62d")), 
              # style rows according to map clicks
              rowStyle = {if(!is.na(strain_click()[1]))
                function(index) {
                  if (index %in% strain_click()) list(backgroundColor = "hsl(58, 100%, 76%)")
                }else NULL },
              columns = list(country = colDef(name = "Country"),
                             province = colDef(name = "Province"),
                             strain = colDef(name = "Strain"),
                             present_at_tp1 = colDef(name = "Present at TP1"),
                             present_at_tp2 = colDef(name = "Present at TP2"),
                             strain_latitude = colDef(name = "Latitude"),
                             strain_longitude = colDef(name = "Longitude"),
                             strain_date = colDef(name = "Date identified"),
                             tp1_cluster = colDef(show = FALSE),
                             tp1_strain_latitude_jit = colDef(show = FALSE), 
                             tp1_strain_longitude_jit = colDef(show = FALSE),
                             tp2_strain_latitude_jit = colDef(show = FALSE), 
                             tp2_strain_longitude_jit = colDef(show = FALSE))
    )
  })
  
  ################################################
  # filtering map data based on table selections #
  ################################################
  
  # at the moment data is not filtered according to selectiosn
  # just shows selectiosn 
  
  cluster_selection <- reactive(getReactableState("clustertable", "selected"))
  
  observe({
    print(cluster_table() %>% slice(as.numeric(cluster_selection())))
    print(cluster_selection())
  })
  
  
  strain_selection <- reactive(getReactableState("straintable", "selected"))
  
  observe({
    print(strain_table() %>% slice(as.numeric(strain_selection())))
    print(strain_selection())
  })
  
  
  ############################################
  # table rendering based on map data clicks #
  ############################################
  
  observeEvent(input$map_marker_click, {
    # get info from marker click
    p <- input$map_marker_click
    print(p)

    # if a tp1 strain is clicked 
    if(p$group[1] == "TP1 strains"){
      
      # info from a cluster 1 strain
      strain_click(intersect(which(strain_table()$tp1_strain_latitude_jit == p$lat),
                                 which(strain_table()$tp1_strain_longitude_jit == p$lng)))

      # corresponding tp1 cluster info
      cluster_click(which(cluster_table()$tp1_cluster == strain_table()[strain_click(), "tp1_cluster"]))

      updateReactable("straintable")
      updateReactable("clustertable")
    }
    
    # if a tp2 strain is clicked 
    if(p$group[1] == "TP2 strains"){
      
      # info from a cluster 2 strain
      strain_click(intersect(which(strain_table()$tp2_strain_latitude_jit == p$lat),
                             which(strain_table()$tp2_strain_longitude_jit == p$lng)))
      
      # corresponding cluster 1 info
      cluster_click(which(cluster_table()$tp1_cluster == strain_table()[strain_click(), "tp1_cluster"]))

      updateReactable("straintable")
      updateReactable("clustertable")
      
      
    }
    
    # if a tp1 cluster is clicked 
    if(p$group[1] == "TP1 centroid"){
      
      # info from a cluster 2 strain
      cluster_click(intersect(which(cluster_table()$avg_tp1_latitude_plot == p$lat),
                              which(cluster_table()$avg_tp1_longitude_plot == p$lng)))
      
      # corresponding strain info
      strain_click(which(strain_table()$tp1_cluster == cluster_table()[cluster_click(), "tp1_cluster"]))
      
      updateReactable("straintable")
      updateReactable("clustertable")
    }
    
    # if a tp2 cluster is clicked 
    if(p$group[1] == "TP2 centroid"){
      
      # info from a cluster 2 strain
      tp2_cluster_click <- intersect(which(ecc_all_filtered()$avg_tp2_latitude_plot == p$lat),
                                      which(ecc_all_filtered()$avg_tp2_longitude_plot == p$lng))
      
      cluster_click(which(cluster_table()$tp1_cluster == ecc_all_filtered()[tp2_cluster_click, "tp1_cluster"]))
      
      # corresponding strain info
      strain_click(which(strain_table()$tp1_cluster == cluster_table()[cluster_click(), "tp1_cluster"]))
      
      updateReactable("straintable")
      updateReactable("clustertable")
    }

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
    if(length(na.omit(ecc_all_filtered()$avg_tp1_latitude_plot))>0){
      
      proxy %>% 
        addCircleMarkers(data = ecc_all_filtered(),
                         lat =  ~as.numeric(avg_tp1_latitude_plot),
                         lng =  ~as.numeric(avg_tp1_longitude_plot),
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
                                       "Avg date:", as.Date(avg_tp1_date)[i],"<br/>", sep=" ")) 
                         }),
                         group = "TP1 centroid")
    }
    
    # add tp2 centroids
    if(length(na.omit(ecc_all_filtered()$avg_tp2_latitude_plot))>0){
      
      proxy %>% 
        addCircleMarkers(data = ecc_all_filtered(),
                         lat = ~as.numeric(avg_tp2_latitude_plot),
                         lng = ~as.numeric(avg_tp2_longitude_plot),  
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
                                       "Avg date:", as.Date(avg_tp2_date)[i],"<br/>", sep=" ")) 
                         }),
                         group = "TP2 centroid")
    }
    
    # add tp1 strains
    if(length(na.omit(ecc_all_filtered()$tp1_strain_latitude_jit))>0){
      
      proxy %>% 
        addCircleMarkers(data = ecc_all_filtered(),
                         layerId = ~strain,
                         lat = ~as.numeric(tp1_strain_latitude_jit),
                         lng = ~as.numeric(tp1_strain_longitude_jit),
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
                                       "Date:", strain_date[i],"<br/>", sep=" ")) 
                         }),
                         group = "TP1 strains")
    }
    
    # add tp2 strains
    if(length(na.omit(ecc_all_filtered()$tp2_strain_latitude_jit))>0){
      
      proxy %>%
        addCircleMarkers(data = ecc_all_filtered(),
                         lat = ~as.numeric(tp2_strain_latitude_jit),
                         lng = ~as.numeric(tp2_strain_longitude_jit),
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
                                       "Date:", strain_date[i],"<br/>", sep=" ")) 
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
  
  
  
  ###################
  # ridgeline plots #
  ###################
  
  
  output$ridgeplot <- renderPlotly({
    # dont run until at least one cluster selected 
    req(input$cluster)
    
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
    
    

    
    df <- as.data.frame(ecc_all_filtered() %>% 
                          mutate(time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y")))))
    
    p <- plot_ly(type = 'violin')
    
    i = 0
    for (i in 1:length(unique(df$tp1_cluster))) {
      # plotly violin plot
      p <- p %>%  add_trace(df,
                            y = df$tp1_cluster[df$present_at_tp1 == 0 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
                            x = df$time_diff[df$present_at_tp1 == 0 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
                            legendgroup = "TP2 clusters",
                            scalemode = "count",
                            side = 'positive',
                            orientation = "h",
                            color = I(unname(unlist(sapply(df$tp1_cluster[df$present_at_tp1 == 0 & df$tp1_cluster == unique(df$tp1_cluster)[i]], 
                                                           function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector")))),
                            meanline = list(
                              visible = T
                            ),
                            name =  unique(df$tp2_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]]),
                            points = "all",
                            line = list(color = "black",
                                        width = 1),
                            marker = list(opacity = 0.4,
                                          size = 10, 
                                          line = list(color = "black",
                                                      width = 1)))
      
      p <-  p %>% add_trace(df,
                            y = df$tp1_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
                            x = df$time_diff[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
                            legendgroup = "TP1 clusters", 
                            scalemode = "count",
                            side = 'positive',
                            orientation = "h",
                            color = I(unname(unlist(sapply(df$tp1_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]], 
                                                           function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector")))),
                            meanline = list(
                              visible = T
                            ),
                            line = list(width = 1),
                            name =  df$tp1_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
                            points="all",
                            marker = list(opacity = 0.4, 
                                          size = 10)) 
    }
    
    p <- p %>%
      
      
      layout(
        # y axis formatting
        yaxis = list(
          title = "TP1 cluster"
        ),
        # x axis formating 
        xaxis = list(
          title = "Elapsed time (days) since 01-01-2020"
        )
      )
    
    p
    
  })
  
  
  
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
# would be nice to have the user be able to choose if data is jittered or not
# and to rejitter points if some overlap too much



##############
# scrap code #
##############

# major plots: histograms, ridgeline plots, bubble plots
#
# test <- ecc_all %>% filter(!grepl('TP1|^0$|TP2', strain)) %>% count(tp1_cluster, strain_date)   
# 
# test <- ecc_all %>% filter(!grepl('TP1|^0$|TP2', strain)) %>% subset(tp1_cluster %in% c("TP1_h0_c001", "TP1_h0_c002"))   
# 
# 
# ggplot(test, aes(x=as.numeric(as.Date(ecc_all$strain_date)), y=as.character(tp1_cluster))) +
#   geom_density_ridges(stat="identity")


# getting time difference
# test$time_diff <- as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(test$strain_date, format = "%d-%m-%y") ))

#ggplot(test %>% subset(present_at_tp1==1), aes(y=tp1_cluster, x=time_diff)) + geom_density_ridges()+ theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank()) + labs(x="Elapsed time (days) since January 01, 2020", y = "TP1 cluster")


# 
# ggplot() +
#   geom_density_ridges(ecc_all_filtered() %>% 
#                         subset(present_at_tp1==0) %>%
#                         mutate(time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y")))),
#                       mapping=aes(y=tp1_cluster, x=time_diff,
#                                   fill=tp1_cluster), linetype="dashed", alpha=0.5,  scale=0.8) +
#   geom_density_ridges(ecc_all_filtered() %>%
#                         subset(present_at_tp1==1) %>%
#                         mutate(time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y")))),
#                       mapping=aes(y=tp1_cluster, x=time_diff,
#                                   fill=tp1_cluster), alpha=0.5, scale=0.8, inherit.aes = F) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank()) +
#   labs(x="Elapsed time (days) since January 01, 2020", y = "TP1 cluster")
# 
# 
# 
# test$time_diff <- numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(test$strain_date, format = "%d-%m-%y")))
# 
# test <- ecc_all %>% subset(tp1_cluster %in% c("TP1_h0_c001", "TP1_h0_c002", "TP1_h0_c003", "TP1_h0_c004", "TP1_h0_c005", "TP1_h0_c006"))
# 
# ggplot(test %>% 
#          subset(present_at_tp1==1) %>%
#          mutate(time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y")))),
#        aes(x=tp1_cluster, y=time_diff, group=tp1_cluster)) +
#   geom_violin(trim=FALSE, side = "positive") +
#   theme_bw()
#   
# 
# 
# 
# fig <- test %>% 
#   subset(present_at_tp1==1) %>%
#   mutate(time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y")))) %>%
#   plot_ly(type = 'violin') 



# df <- as.data.frame(test %>% 
#   mutate(time_diff = as.numeric(abs(as.Date("01-01-2020", format = "%d-%m-%y") - as.Date(strain_date, format = "%d-%m-%y")))))
# 
# p <- 
#   plot_ly(type = 'violin')
#   
# i = 0
# for (i in 1:length(unique(df$tp1_cluster))) {
# # plotly violin plot
# p <- p %>%  add_trace(df,
#                       y = df$tp1_cluster[df$present_at_tp1 == 0 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
#                       x = df$time_diff[df$present_at_tp1 == 0 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
#                       legendgroup = "TP2 clusters",
#                       scalemode = "count",
#                       side = 'positive',
#                       orientation = "h",
#                       color = I(unname(unlist(sapply(df$tp1_cluster[df$present_at_tp1 == 0 & df$tp1_cluster == unique(df$tp1_cluster)[i]], 
#                                                      function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector")))),
#                       meanline = list(
#                         visible = T
#                       ),
#                       name =  unique(df$tp2_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]]),
#                       points = "all",
#                       line = list(color = "black",
#                                   width = 1),
#                       marker = list(opacity = 0.4,
#                                     size = 10, 
#                                     line = list(color = "black",
#                                                 width = 1)))
# 
# p <-  p %>% add_trace(df,
#                       y = df$tp1_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
#                       x = df$time_diff[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
#                       legendgroup = "TP1 clusters", 
#                       scalemode = "count",
#                       side = 'positive',
#                       orientation = "h",
#                       color = I(unname(unlist(sapply(df$tp1_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]], 
#                                                      function(x) {colorpal %>% subset(tp1_cluster==x) %>% select(colour)}, simplify="vector")))),
#                       meanline = list(
#                         visible = T
#                       ),
#                       line = list(width = 1),
#                       name =  df$tp1_cluster[df$present_at_tp1 == 1 & df$tp1_cluster == unique(df$tp1_cluster)[i]],
#                       points="all",
#                       marker = list(opacity = 0.4, 
#                                     size = 10)) 
# }
# 
# p <- p %>%
#   
#   
#   layout(
#     # y axis formatting
#     yaxis = list(
#       title = "TP1 cluster"
#     ),
#     # x axis formating 
#     xaxis = list(
#       title = "Elapsed time (days) since 01-01-2020"
#     )
#   )
# 
# p
