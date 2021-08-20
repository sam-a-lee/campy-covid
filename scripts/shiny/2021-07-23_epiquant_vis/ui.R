##################
# user interface #
##################

# load libraries required for UI
library(shinydashboard)
library(plotly)
library(shinyalert)

# this is the title that appears over the side bar
header <- dashboardHeader(title = "EpiQuant SARS-CoVis")

# the dashboard side bar
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
              
              # ECC indices tab
              menuItem("ECC indices", tabName = "eccIndices"),
              
              # cluster map tab
              menuItem("Cluster map", tabName = "maps"),
              
              # Data exploration tab - preset subsets of data
              menuItem("Explore the data", tabName = "exploreData",
                       radioButtons("number", "Number of clusters",
                                    choices = list("5" = 5, "10" = 10,
                                                   "20" = 20, "All" = 99), selected = 10),
                       radioButtons("subsets", "Top 'n' data subsets",
                                    choices = list("Largest cluster size" = 1, "Largest delta geospatial ECC" = 2,
                                                   "Largest delta temporal ECC" = 3, "None" = 4), selected = 1),
                       radioButtons("region", "Regional granularity",
                                    choices = list("No faceting" = 1, "By country" = 2,
                                                   "By province" = 3), selected = 1),
                       conditionalPanel(
                         condition = "input.region == 3",
                         # selectize input is updated once data is loaded
                         selectizeInput("regionProvince", "View provinces in: ", c("China", "France", "Italy"),
                                        selected = NULL, multiple = FALSE))
              ),
              
              # Data filters
              # filters are updated on server side once data loaded to reflect values present in data set
              menuItem("Data filters", tabName = "dataFilters",
                       
                       # cluster filters
                       menuItem('Cluster filters', tabName = "clusterFilters",
                                
                                # cluster filters
                                selectizeInput("tp1.cluster", "TP1 cluster", c(1,2,3,4,5),  selected = NULL, multiple = T),
                                selectizeInput("timepoint", "Timepoint", c(1,2), selected = NULL, multiple = T),
                                selectizeInput("type", "Type", c(1,2,3,4), selected = NULL, multiple = T),
                                sliderInput(inputId = "cluster.size.1.2", label = "Cluster size", min = 0, max = 10000, value = c(1, 10000)),
                                sliderInput(inputId = "average.date", label = "Average date", min = as.Date("2020-01-01"), max = as.Date("2025-01-01"), value = c(as.Date("2020-01-01"), as.Date("2025-01-01"))),
                                sliderInput(inputId = "ecc.0.0.1", label = "Temporal ECC", min = 0, max = 1, value = c(0, 1)),
                                sliderInput(inputId = "ecc.0.1.0", label = "Geospatial ECC", min = 0, max = 1, value = c(0, 1)),
                                sliderInput(inputId = "average.latitude", label = "Average latitude", min = -90, max = 90, value = c(-90, 90)),
                                sliderInput(inputId = "average.longitude", label = "Average longitude", min = -180, max = 180, value = c(-180, 180)),
                                
                                # delta cluster filters
                                sliderInput(inputId = "actual.cluster.size.tp2.size.tp1.size", label = "Delta cluster size", min = -10000, max = 10000, value = c(-10000, 10000)),
                                sliderInput(inputId = "number.of.novels.in.the.tp2.match", label = "Number of new strains", min = 0, max = 10000, value = c(0, 10000)),
                                sliderInput(inputId = "novel.growth.tp2.size.tp2.size.number.of.novels", label = "Novel growth rate", min = 0, max = 100, value = c(0, 100)),
                                sliderInput(inputId = "actual.growth.rate.tp2.size.tp1.size.tp1.size", label = "Overall growth rate", min = 0, max = 100, value = c(0, 100)),
                                sliderInput(inputId = "delta.ecc.0.0.1", label = "Delta temporal ECC", min = -1, max = 1, value = c(-1,1)),
                                sliderInput(inputId = "delta.ecc.0.1.0", label = "Delta geosptial ECC", min = -1, max = 1, value = c(-1,1))
                       ),
                       
                       # strain filters
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
              menuItem("Upload data", tabName = "userData", selected = TRUE)
  )
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
  
  # tab items specifies the plots/data that will appear in each tab
  tabItems(
    
    # strain visualizations 
    tabItem(tabName = "eccIndices",
            
            # BAN header and BANs
            # first pabel (row) across top
            h2('At the most recent time point there are: ', style = "color: #1F78C8; font-size: 38px; font-weight: bold;"),
            fluidRow(
              # BAN for actively growing clusters
              valueBoxOutput("activegrowth", width =4),
              # BAN for num of active spreading clusters
              valueBoxOutput("activespread", width =4),
              # BAN for num of clusters with significant local transmission
              valueBoxOutput("sigtransmission", width =4)
            ),
            
            # geospatial ECC panel 
            # second panel (row)
            fluidRow(
              tabBox(
                title = "Geospatial and temporal epicluster cohesion indices",
                width =12,
                side = "right",
                # open on bubble plot
                selected = "ECC bubble plots",
                # interpretation of bubble and histogram plots  
                tabPanel("Interpretation", 
                         HTML('<p>The <strong>geospatial epicluster cohesion index&nbsp;</strong>representes the relative mean distance (km) between strains and the geographical centre of the cluster. A larger geospatial epicluster cohesion index represents a greater mean distance between strains and the cluster centre.</p>
<p>The <strong>temporal epicluster cohesion index&nbsp;</strong>representes the relative time (days) between identification of new strains that are part of a given cluster. A larger temporal epicluster cohesion index represents a greater amount of time between the identification of one strain to the next.&nbsp;</p>
<p>As the geospatial and temporal epicluster cohesion indices are relative to the input data, the interpretation of these values will vary between inputted data sets. i.e. A geospatial epicluster cohesion index of 0.5 may represent a distance of 75 km in one data set, but 1032 km in another data set.&nbsp;</p>
<p>The <strong>bubble plots&nbsp;</strong>show the geospatial and temporal epicluster indices of each individual cluster. The <strong>histograms</strong> allow examination of how the geospatial and temporal epicluster indices of clusters are shifting as a whole.</p>')
                ),
                # ECC histogram visualizations
                tabPanel("ECC histograms",
                         plotlyOutput("ecc_histograms", height = "100%")
                ),
                # ECC bubble plot visualizations
                tabPanel("ECC bubble plots",
                         plotlyOutput("bubble", height = "100%")
                )
              )
            ),
            
            # panel for cluster transmission indices
            # third panel (row)
            fluidRow(
              tabBox(
                title = "Cluster transmission",
                width =12,
                side = "right",
                # open on radar plot
                selected = "Radar plot",
                # interpretation of plots
                tabPanel("Interpretation", 
                         HTML("<p><strong>Cluster transmission</strong> is determined by examining the<em>&nbsp;<strong>change in geospatial and temporal epicluster cohesions indices</strong></em>, and translating these differences into cluster spread and transmission, respectively. Specifically, an increase in the geospatial index indicates that the cluster is becoming more disperse (i.e. spreading geographically), whereas a decrease in the geosptial index indicates that cluster spread is becoming more isolated (i.e. strains have been identified in fewer areas than before). An increase in the temporal index indicates the length of time between identification of each new strain is increasing, and therefore the transmission of the cluster is slowing. Conversely, a decrease in the temporal index indicates that new strains are being identified more rapidly and suggests transmission is increasing.&nbsp;</p>
<p>We can view the changes in geospatial and temporal indices by plotting them against one another and drawing vectors, which we call &quot;<strong>change vectors</strong>&quot;.&nbsp;</p>
<p>To ease the intepreation of change vectors, we convert them to an angle and plot them on a 16-rose compass that describes how the spread and transmission are changing. We summarize this information using a <strong>radar plot</strong>, which describes the number of clusters displaying a given change in spread and transmission.&nbsp;</p>
<p><br></p>")
                ),
                # change vector visualization
                tabPanel("Change vectors",
                         plotlyOutput("changevector", height = "100%")
                ),
                # human-readable cluster transmission in radar plot
                tabPanel("Radar plot",
                         plotlyOutput("radar", height = "100%")
                )
                
              )
            ),
            
            # panel for cluster growth metrics
            # fourth panel (row)
            fluidRow(
              tabBox(
                title = "Growth",
                width =12,
                side = "right",
                # default is to show cluster growth metrics
                selected = "Cluster growth",
                # interpretation of plot meanings
                tabPanel("Interpretation", 
                         HTML("<p>Growth visualizations display changes in the number of strains identified by cluster or by date.&nbsp;</p>
<p><strong><u>Cluster growth metrics:</u></strong> determine the overall and novel growth rate of clusters. The <strong>overall growth rate</strong> is determined by looking at <em>all</em> strains (previously identified strains and novel strains) that have joined a given cluster from one time point to the next. The <strong>novel</strong><strong>&nbsp;growth rate</strong> is determined by examining the number of <em>novel</em> strains (not previously identified) that have a joined a cluster from one time point to the next.</p>
<p><strong><u>Strains by cluster:</u></strong> includes visualizations of the <strong>number of novel strains</strong> identified by date for each cluster, as well as the <strong>cumulative number of strains&nbsp;</strong>that are identified in each cluster.&nbsp;</p>
<p><strong><u>Strains by date:</u></strong> visualizes the overall number of <strong>new strains identified by date</strong>. Data are not separated by cluster. Data can be binned by day, month, or year.</p>
<p><strong><u>Single vs multi strain:</u></strong> displays the number of novel strains identified by date that are part of multi strain cluster or single strain clusters.</p>
<p><br></p>
<p>&nbsp;&nbsp;</p>")
                ),
                # single vs multi strain cluster visualizations
                tabPanel("Single vs multi strain clusters",
                         plotlyOutput("singlevsmulti", height = "100%")
                ),
                # total num of novel strains identified by date
                tabPanel("Strains by date",
                         plotlyOutput("newstrainsbydate", height = "100%")
                ),
                # novel strains identified and cumulative strains identified by date per cluster
                tabPanel("Strains by cluster",
                         plotlyOutput("strainsbycluster", height = "100%")
                ),
                # cluster growth metrics
                tabPanel("Cluster growth",
                         plotlyOutput("cluster_growth", height = "100%")
                )
              )
            ),
    ),
    
    # cluster map and cardinal direction
    tabItem(tabName = "maps",
            
            # only one panel (row)
            fluidRow(
              tabBox(
                title = "Cluster map",
                width =12,
                side = "right",
                # default is to open on map
                selected = "Map",
                # interpretation of map and cardinal direction
                tabPanel("Interpretation", 
                         HTML("<p>The <strong>map</strong> displays each cluster, plotted at the average latitude and longitude of all strains included in that cluster. The area of the cluster is proportional to the number of strains included in that cluster.</p>
<p>The<strong>&nbsp;polar plot</strong> displays the <em><strong>cardinal direction of cluster movement</strong></em> from one time point to the next. The length which the bars extend are proportional to the number of clusters displaying movement in a given direction.&nbsp;</p>")
                ),
                # cardinal direction of cluster movement
                tabPanel("Cluster movement",
                         plotlyOutput("cardinal_movement", height = "100%")
                ),
                # cluster map - plotted by centroid lat and lng
                tabPanel("Map",
                         plotlyOutput("cluster_map", height = "100%")
                )
              )
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
                            multiple = FALSE, # only one file at a time
                            accept = c(".csv", ".txt", ".xlsx", ".tsv")),
                  useShinyalert(),  # Set up shinyalert
                  
                  # check box for header; default is true
                  checkboxInput("header", "Header", TRUE),
                  
                  # check box for separator; default is comma
                  radioButtons("separator", "Separator",
                               inline = TRUE,
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ","),
                  
                  # check box for quotes, if applicable; default is none
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

# output ui
ui <- dashboardPage(header, sidebar, body)