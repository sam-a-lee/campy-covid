##################
# user interface #
##################

library(shinydashboard) # the shiny dashboard
library(readxl) # for reading xlsx files 
library(shiny) # for running shiny apps
library(shinyWidgets) # for better slider/dropdown options
library(here) # for clear dir calling
library(tidyverse) # tidy data/ kind of used
library(pals) # for large colour palettes (similar to brewer)
library(plotly) # for interactive ggplots
library(crosstalk) # for shared data
library(DT) # data table
library(htmltools) # for html input
library(lubridate) # for dates 
library(geosphere) # for directions between points 
library(shinydashboardPlus) # for collapsable boxes
library(data.table) # for melting
library(rvest) # for downloading cardinal table 

# Define UI for application that draws a histogram

header <- dashboardHeader(title = "ECC Vis")

# the dashboard side bar
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
              
              # Cluster visualization tab
              menuItem("Cluster movement",tabName = "clusterMove"),
              
              # Strain visualization tab
              menuItem("Strain identification", tabName = "strainId"),
              
              # Data exploration tab
              menuItem("Explore the data", tabName = "exploreData",
                       radioButtons("number", "Number of clusters",
                                    choices = list("5" = 5, "10" = 10,
                                                   "20" = 20, "All" = 99), selected = 10),
                       radioButtons("subsets", "Top 'n' data subsets",
                                    choices = list("Largest cluster size" = 1, "Largest delta geospatial ECC" = 2,
                                                   "Largest delta temporal ECC" = 3, "None" = 4), selected = 1),
                       radioButtons("region", "Regional granularity",
                                    choices = list("No faceting" = 1, "By country" = 2,
                                                   "By province" = 3), selected = 1)
                       # conditionalPanel(
                       #     condition = "input.region == 3",
                       #     selectizeInput("regionProvince", "View provinces in: ", unique(strains$country),
                       #                    selected = NULL, multiple = FALSE))
              ),
              
              # Data filters
              menuItem("Data filters", tabName = "dataFilters",
                       
                       # cluster filters
                       menuItem('Cluster filters', tabName = "clusterFilters",
                                #cluster filters
                                selectizeInput("tp1_cluster", "TP1 cluster", c(1,2,3,4,5),  selected = NULL, multiple = T),
                                selectizeInput("timepoint", "Timepoint", c(1,2), selected = NULL, multiple = T),
                                selectizeInput("type", "Type", c(1,2,3,4), selected = NULL, multiple = T),
                                sliderInput(inputId = "cluster_size_2", label = "Cluster size", min = 0, max = 10000, value = c(1, 10000)),
                                sliderInput(inputId = "avg_date", label = "Avg date", min = as.Date("2020-01-01"), max = as.Date("2025-01-01"), value = c(as.Date("2020-01-01"), as.Date("2025-01-01"))),
                                sliderInput(inputId = "ecc_0.0.1", label = "ecc_0.0.1", min = 0, max = 1, value = c(0, 1)),
                                sliderInput(inputId = "ecc_0.1.0", label = "ecc_0.1.0", min = 0, max = 1, value = c(0, 1)),
                                sliderInput(inputId = "avg_latitude", label = "Avg latitude", min = -90, max = 90, value = c(-90, 90)),
                                sliderInput(inputId = "avg_longitude", label = "Avg longitude", min = -180, max = 180, value = c(-180, 180)),
                                # delta filters
                                sliderInput(inputId = "delta_cluster_size", label = "Delta cluster size", min = -10000, max = 10000, value = c(-10000, 10000)),
                                sliderInput(inputId = "num_novel_tp2_strains", label = "Novel TP2 strains", min = 0, max = 10000, value = c(0, 10000)),
                                sliderInput(inputId = "cluster_novel_growth_rate", label = "Novel growth rate", min = 0, max = 100, value = c(0, 100)),
                                sliderInput(inputId = "overall_cluster_growth_rate", label = "Overall growth rate", min = 0, max = 100, value = c(0, 100)),
                                sliderInput(inputId = "delta_ecc_0.0.1", label = "delta ecc_0.0.1", min = -1, max = 1, value = c(-1,1)),
                                sliderInput(inputId = "delta_ecc_0.1.0", label = "delta ecc_0.1.0", min = -1, max = 1, value = c(-1,1))
                       ),
                       
                       #strain filters
                       menuItem('Strain filters', tabName = "strainFilters",
                                selectizeInput("country", "Country", c("China", "Italy", "France"),  selected = NULL, multiple = T),
                                selectizeInput("province", "Province", c("China", "Italy", "France"),  selected = NULL, multiple = T)
                       )
              ),
              
              # Cluster data tab
              menuItem("Cluster data", tabName = "clusterData"),
              
              # Strain data tab
              menuItem("Strain data", tabName = "strainData"),
              
              # filtered data used in visualizations in wide format
              menuItem("Filtered raw data", tabName = "filteredData"),
              
              # raw data used to generate visualizations
              menuItem("Raw data", tabName = "rawData"),
              
              # upload user data
              menuItem("Upload data", tabName = "userData", selected = TRUE))
)

# the dashboard body
body <- dashboardBody(
  
  # custom colors for BANs
  tags$style(".small-box { background-color: #1F78C8 !important; color: #FFFFFF !important; }"),
  
  # custom colors for skin
  tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #1F78C8;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #1762a3;
                              }

        /* the colored tab of sidebar tabs*/
      .skin-blue .sidebar-menu > li.active > a {
        border-left-color: #1F78C8;
                              }
                          
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #2183db;
                              }        
           
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #1F78C8;
                              }
                              '))),
  
  tabItems(
    tabItem(tabName = "clusterMove",
            # tab title
            h2("Cluster movement"),
            
            fluidRow(
              valueBoxOutput("clustercountbox", width =2),
              valueBoxOutput("cardinalbox", width =4),
              valueBoxOutput("speedbox", width =3),
              valueBoxOutput("spreadbox", width =3)
            ),
            
            
            # row 1 for map of clusters and cardinal movement
            fluidRow(
              box(width=12,
                  title = "", 
                  plotlyOutput("cluster_map"),
                  collapsible = TRUE)
            ),
            
            fluidRow(

                  box(width = 6,
                      solidHeader = T,
                      title = NULL,
                      plotlyOutput("cardinal_polar", width = "100%", height = "100%")),
                  box(width = 6,
                      title = NULL,
                      plotlyOutput("ecc_radar", width = "100%", height = "100%"))
            ),
            
            # row 2 for bubble plot and change vector plot 
            fluidRow(
              box(title = HTML("<b>", "ECC value plots", "</b>"),
                  width = 12,
                  #bubble plot
                  box(title = "Bubble plot", 
                      width = 6, 
                      solidHeader=TRUE,
                      plotlyOutput("bubbleplot", width = "100%", height = "100%")),
                  # change vector plot
                  box(title = "Change vector", 
                      width = 6,
                      solidHeader=TRUE,
                      plotlyOutput("change_vector", width = "100%", height = "100%")),
                  collapsible = TRUE
              )
            ),
            
            # row 3 for histograms
            fluidRow(
              box(title = HTML("<b>", "Geospatial and temporal ECC indices", "</b>"),
                  width = 12,
                  # geo ecc histogram by time point
                  box(title = "Geospatial ECC histogram", 
                      width = 6, 
                      solidHeader=TRUE,
                      plotlyOutput("geo_histogram")),
                  # temp ecc histogram by time point
                  box(title = "Temporal ECC histogram", 
                      width = 6, 
                      solidHeader=TRUE,
                      plotlyOutput("temp_histogram")),
                  collapsible = TRUE
              )
            ), 
            
            # row 4 for delta histograms
            fluidRow(
              box(title = HTML("<b>", "Delta geospatial and temporal ECC indices", "</b>"),
                  width = 12,
                  # delta geo ecc histogram 
                  box(title = "Delta geospatial ECC", 
                      width = 6, 
                      solidHeader=TRUE,
                      plotlyOutput("delta_geo_histogram")),
                  # delta temp ecc histogram 
                  box(title = "Delta temporal ECC ", 
                      width = 6, 
                      solidHeader=TRUE,
                      plotlyOutput("delta_temp_histogram")),
                  collapsible = TRUE
              )
            ),
            
            # fluid row 5
            fluidRow(
              # single vs multi strain cluster histogram 
              box(title = HTML("<b>", "Number of single and multistrain clusters by date",  "</b>"),
                  width = 12,
                  box(width = 6,
                      title = "Time point 1",
                      solidHeader = TRUE, 
                      plotlyOutput("singlemultclust_tp1", width = "100%", height = "100%")),
                  box(width = 6,
                      solidHeader = TRUE, 
                      title = "Time point 2",
                      plotlyOutput("singlemultclust_tp2", width = "100%", height = "100%")),
                  collapsible = TRUE)
            )
    ),
    
    
    # strain visualizations 
    tabItem(tabName = "strainId",
            h2("Strain identification"),
            
            fluidRow(
              valueBoxOutput("straincountbox", width =2)
            ),
            
            # row 1 for map
            fluidRow(
              # map
              box(solidHeader = TRUE, 
                  width = 12, 
                  plotlyOutput("strain_map", width = "100%", height = "100%"),
                  collapsible = TRUE)
            ),
            
            fluidRow( 
              # strain ridgeline plot (denisty by date)
              box(title = HTML("Daily number of strains identified"),
                  width = 12, 
                  plotlyOutput("ridgeplot", width = "100%", height = "100%"),
                  collapsible = TRUE)
            ),
            
            fluidRow(

                  # geo ecc histogram by time point
                  box(title =NULL, 
                      width = 12, 
                      solidHeader = TRUE, 
                      plotlyOutput("singlemultclust", width = "100%", height = "100%" ))
                  
   
            ), 
            
            # row 2 for delta strain ecc data 
            fluidRow(
              box(title = HTML("<b>", "Delta geospatial and temporal ECC indices", "</b>"),
                  width = 12,
                  # geo ecc histogram by time point
                  box(title = "Delta geospatial ECC histogram", 
                      width = 6, 
                      solidHeader = TRUE, 
                      plotlyOutput("strain_delta_geo_histogram")),
                  # temp ecc histogram by time point
                  box(title = "Delta temporal ECC histogram", 
                      width = 6, 
                      solidHeader = TRUE, 
                      plotlyOutput("strain_delta_temp_histogram")),
                  collapsible = TRUE
              )
            ), 
            

            
            # fluid row 5
            fluidRow(
              # single vs multi strain histogram 
              box(title = HTML("<b>", "Number of strains single part of single and multi strain clusters",  "</b>"),
                  width = 12,
                  box(width = 6,
                      title = "Time point 1",
                      solidHeader = TRUE, 
                      plotlyOutput("straintypes_tp1", width = "100%", height = "100%")),
                  box(width = 6,
                      solidHeader = TRUE, 
                      title = "Time point 2",
                      plotlyOutput("straintypes_tp2", width = "100%", height = "100%")),
                  collapsible = TRUE)
            ),
            
            # fluid row 6
            fluidRow(
              #strain histogram 
              box(title = HTML("<b>", "Number of new strains identified per month",  "</b>"),
                  width = 12,
                  box(width = 6,
                      title = "Time point 1",
                      solidHeader = TRUE, 
                      plotlyOutput("strain_histogram_tp1", width = "100%", height = "100%")),
                  box(width = 6,
                      solidHeader = TRUE, 
                      title = "Time point 2",
                      plotlyOutput("strain_histogram_tp2", width = "100%", height = "100%")),
                  collapsible = TRUE)
            ),
            
            # row 7
            fluidRow(
              # cumulative strain identification by time point 
              box(title = HTML("<b>", "Cumulative strain identification by cluster ",  "</b>"),
                  width = 12, 
                  box(width = 6, 
                      solidHeader = TRUE,
                      title = "Time point 1",
                      plotlyOutput("cum_strains_tp1")),
                  box(width = 6, 
                      solidHeader = TRUE, 
                      title = "Time point 2",
                      plotlyOutput("cum_strains_tp2")),
                  collapsible = TRUE)
            )
    ),
    
    # tab for filtered data
    tabItem(tabName = "filteredData",
            h2("Filtered data"),
            fluidRow(
              box(width = 12, 
                  title = "Filtered raw data",
                  dataTableOutput("filtered_data"))
            )
    ),
    
    # table for raw data
    tabItem(tabName = "rawData",
            h2("Raw data"),
            fluidRow(
              box(width = 12,
                  title = "Unfiltered raw data",
                  dataTableOutput("raw_data"))
            )
    ),
    
    # table for cluster data
    tabItem(tabName = "clusterData",
            h2("Cluster data"),
            fluidRow(
              box(width = 12,
                  title = "Filtered cluster data used for plotting", 
                  dataTableOutput("clusters_dt"))
            )
    ),
    
    # table for strain data
    tabItem(tabName = "strainData",
            h2("Strain data"),
            fluidRow(
              box(width = 12,
                  title = "Filtered strain data used for plotting", 
                  dataTableOutput("strains_dt"))
            )
    ),
    
    # user input data
    tabItem(tabName = "userData",
            
            fluidRow(
              box(width = 12,
                  title = "Select file for upload",
                  # prompt to upload file 
                  fileInput("userFile", "Choose file",
                            multiple = TRUE,
                            accept = c("text/csv/xlsx",
                                       "text/comma-separated-values,text/plain",
                                       ".csv", ".txt", ".xlsx")),
                  # check box for header
                  checkboxInput("header", "Header", TRUE),
                  
                  # check box for separator 
                  radioButtons("separator", "Separator",
                               inline = TRUE,
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ","),
                  # check box for quotes, if applicable
                  radioButtons("quote", "Quote",
                               inline = TRUE,
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = "")
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)
