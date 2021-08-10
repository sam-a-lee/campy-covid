##########
# server #
##########

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
#library(shinyjqui) # for resizable boxes 


############# 
# functions #
#############

angle <- function(x,y) { 
    z <- x + 1i * y
    res <- 90 - Arg(z) / pi * 180
    res %% 360
}

page <- read_html('http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm')
directions.raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions.raw %>% 
    set_names(~tolower(sub(' Direction', '', .x))) %>% 
    slice(-1) %>% 
    separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

# create a new column and assign a "speed" and a "spread"
directions$ecc.speed <- c("Slower", "Slower", "Slower", "Slower",
                          "No change", "Faster", "Faster", "Faster",
                          "Faster", "Faster", "Faster", "Faster", 
                          "No change", "Slower", "Slower", "Slower")

directions$ecc.spread <- c("No change", "Isolated", "Isolated", "Isolated",
                           "Isolated", "Isolated", "Isolated", "Isolated",
                           "No change", "Dispersed", "Dispersed", "Dispersed", 
                           "Dispersed", "Dispersed", "Dispersed", "Dispersed")

directions$ecc.comb <- c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                         "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                         "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                         "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed")

# add midpoints
directions$degree.mid<- c(0,22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5,225, 247.5, 270, 292.5, 315, 337.5)



server <- function(input, output, session) { 
    
    
    ############################
    # set max file upload size #
    ############################
    
    # default size too small
    # set max file size to 1 gb
    options(shiny.maxRequestSize=1048576000)
    
    #######################################
    # get user inputted data to visualize #
    #######################################
    
    # create reactive values
    vals <- reactiveValues(data.raw = NULL, data_proc = NULL, clusters = NULL, strains = NULL)
    
    # read data frame into reactive values
    observeEvent(input$userFile, {
        ext <- tools::file_ext(input$userFile$datapath)
        
        # csv files 
        if (ext == "csv") {
            vals$data.raw <- read.csv(file = input$userFile$datapath, 
                                      header = input$header)
        } 
        
        # tsv files
        if (ext == "tsv") {
            vals$data.raw <- fread(file = input$userFile$datapath, 
                                   header = input$header) 
        }
        
        # excel files
        if (ext == "xlsx") {
            vals$data.raw <- readxl::read_xlsx(file = input$userFile$datapath, 
                                               header = input$header)
        }
        
        # text files
        if (ext == "txt") {
            vals$data.raw <- read.delim(file = input$userFile$datapath, 
                                        header = input$header)
        }
        
        
    }, ignoreNULL = T)
    
    #####################
    # process user data #
    #####################
    
    observeEvent(vals$data.raw, {
        
        print("new data!")
        
        # get data 
        eccdata <- vals$data.raw
        
        #################
        # clean eccdata #
        #################
        
        # rename columns, remove spaces, all lowercase
        # remove redundant periods and those at end of name
        print("naming nicely...")
        colnames(eccdata) <- make.names(colnames(eccdata), unique=TRUE, allow_ = F) %>%
            tolower() %>%
            gsub("([[:punct:]])\\1+", "\\1", .) %>%
            gsub("[.]$", "", .)
        
        # remove 0 for now as its causing problems
        # eccdata <- eccdata %>% subset(tp1.cluster != 0 )
        
        print("coverting classes...")
        # convert columns to appropriate types
        # as.is = T specifies character stays as character
        # type.convert is stable and used in read.table
        eccdata <- lapply(eccdata, type.convert, as.is = T)
        eccdata <- bind_rows(eccdata)
        
        ##################################
        # calculate cluster transmission #
        ##################################
        
        eccdata <- eccdata %>% 
            mutate(delta.ecc.angle = angle(delta.ecc.0.1.0, delta.ecc.0.0.1))
        
        # use new column to assign ecc values speed, spread, and combined
        eccdata <- eccdata %>%
            mutate(delta.ecc.direction = cut(delta.ecc.angle,
                                             breaks = c(0, directions$degree_max, 360), 
                                             labels = c(directions$ecc.comb, 'F')),
                   delta.ecc.speed = cut(delta.ecc.angle, 
                                         breaks = c(0, directions$degree_max, 360), 
                                         labels = c(directions$ecc.speed, 'F')),
                   delta.ecc.spread = cut(delta.ecc.angle,
                                          breaks = c(0, directions$degree_max, 360), 
                                          labels = c(directions$ecc.spread, 'F')))
        
        # convert to factor
        eccdata$delta.ecc.direction <- as.factor(eccdata$delta.ecc.direction)
        
        # set levels 
        eccdata$delta.ecc.direction <-  factor(eccdata$delta.ecc.direction, 
                                               levels = c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                                                          "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                                                          "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                                                          "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed"))
        
        #############################
        # calculate cluster bearing #
        #############################
        
        # get bearing (direction of change)
        eccdata$delta.bearing <- bearing(as.matrix(eccdata[,c("average.tp1.longitude", "average.tp1.latitude")]),
                                         as.matrix(eccdata[,c("average.tp2.longitude", "average.tp2.latitude")]))
        
        # convert bearing to compass direction
        # centre on directions 
        eccdata$delta.cardinal <- sapply(eccdata$delta.bearing, function(x){
            
            if(x <= 11.25 & x >= 0) {return("N")}
            if(x > 11.25 & x <= 33.75) {return("NNE")}
            if(x > 33.75 & x <= 56.25) {return("NE")}
            if(x > 56.25 & x <= 78.75) {return("ENE")}
            if(x > 78.75 & x <= 101.25) {return("E")}
            if(x > 101.25 & x <= 123.75) {return("ESE")}
            if(x > 123.75 & x <= 146.25) {return("SE")}
            if(x > 146.25 & x <= 168.75) {return("SSE")}
            if(x > 168.5 & x <= 180) {return("S")}
            if(x < -168.5 & x >= -180) {return("S")}
            if(x < -146.25 & x >= -168.5) {return("SSW")}
            if(x < -123.75 & x >= -146.25) {return("SW")}
            if(x < -101.25 & x >= -123.75) {return("WSW")}
            if(x < -78.75 & x >= -101.25) {return("W")}
            if(x < -56.25 & x >= -78.75) {return("WNW")}
            if(x < -33.75 & x >= -56.25) {return("NW")}
            if(x < -11.25 & x >= -33.75) {return("NNW")}
            if(x < 0 & x >= -11.25) {return("N")}
        })  
        
        
        # convert to factor
        eccdata$delta.cardinal <- as.factor(eccdata$delta.cardinal)
        
        # set levels
        levels(eccdata$delta.cardinal) <-  c("N", "NNE", "NE", "ENE",
                                             "E", "ESE", "SE", "SSE",
                                             "S", "SSW", "SW", "WSW", 
                                             "W", "WNW", "NW", "NNW")
        
        # set levels 
        eccdata$delta.cardinal.long <- plyr::revalue(eccdata$delta.cardinal, 
                                                     c("N" = "North", "NNE" = "North/Northeast", "NE" = "Northeast", "ENE" = "East/Northeast",
                                                       "E"= "East", "ESE" = "East/Southeast", "SE" = "Southeast", "SSE" = "South/Southeast", 
                                                       "S" = "South", "SSW" = "South/Southwest", "SW" = "Southwest", "WSW" = "West/Southwest", 
                                                       "W" = "West", "WNW" = "West/Northwest", "NW" = "Northwest", "NNW" = "North/Northwest"))
        
        # write processed data to reactive vals 
        # will show to user in table later
        vals$data_proc <- eccdata
        
        ##################################
        # pull out cluster specific info #
        ##################################
        
        print("pulling cluster data...")
        
        # summarize data by cluster 
        clusters <- eccdata %>% 
            group_by(tp1.cluster) %>%
            # this removes strains that appear in tp2
            # but should not affect overall values
            subset(!is.na(tp1.cluster)) %>% 
            summarise(across(where(is.character), ~unique(.x)[1], .names = "{.col}"),
                      across(where(is.factor), ~unique(.x)[1], .names = "{.col}"),
                      across(where(is.integer), ~mean(.x), .names = "{.col}"),
                      across(where(is.numeric), ~mean(.x), .names = "{.col}"))
        
        # select only complete cases
        # clusters_cc <- na.omit(clusters)
        
        # now reshape the data 
        # grep generalizable to many data points
        clusters_long <- data.table::melt(setDT(clusters), 
                                          measure.vars=list(colnames(clusters)[grep("longitude", colnames(clusters))], 
                                                            colnames(clusters)[grep("latitude", colnames(clusters))],
                                                            colnames(clusters)[grep("t0.ecc.0.0.1", colnames(clusters))],
                                                            colnames(clusters)[grep("t0.ecc.0.1.0", colnames(clusters))],
                                                            colnames(clusters)[grep("date", colnames(clusters))],
                                                            colnames(clusters)[grep("cluster.size.1.2", colnames(clusters))],
                                                            colnames(clusters)[grep("temp.average", colnames(clusters))],
                                                            colnames(clusters)[grep("geo.average", colnames(clusters))],
                                                            colnames(clusters)[grep("cluster.size.1$", colnames(clusters))]),
                                          variable.name='timepoint',
                                          value.name=c('average.longitude', 'average.latitude', 
                                                       "ecc.0.0.1", "ecc.0.1.0",
                                                       "average.date", "cluster.size.1.2", 
                                                       "temp.average.cluster.distance.days", "geo.average.cluster.distance.km",
                                                       "cluster.size.1"))
        
        # write to reactive vals df 
        # to be displayed in table later
        vals$clusters <- clusters_long
        
        print("formatting strain data...")
        
        #################################
        # pull out strain specific info #
        #################################
        
        # create new df for starin data
        strains <- eccdata 
        
        # convert to long form
        strains.long <- data.table::melt(setDT(strains),
                                         measure.vars=list(colnames(strains)[grep("t0.ecc.0.0.1", colnames(strains))],
                                                           colnames(strains)[grep("t0.ecc.0.1.0", colnames(strains))],
                                                           colnames(strains)[grep("cluster.size.1.2", colnames(strains))]),
                                         variable.name='timepoint', value.name=c("ecc.0.0.1", "ecc.0.1.0",
                                                                                 "cluster.size.1.2"))

        # make column for strain date instead of having three separate columns
        strains.long$strain.date <- paste(strains.long$day, strains.long$month, strains.long$year, sep = "-")
        
        # categorize as single vs multistrain
        # >2 because counts are +1 of actual value 
        strains.long <- strains.long %>% 
            mutate(single.mult = ifelse(cluster.size.1.2>2,"Multi strain clusters","Single strain clusters"))

        # write out to reactive vals object
        vals$strains.long <- strains.long
        
        ############################################## 
        # update selectize inputs based on user data #
        ##############################################
        
        print("updating selectize inputs...")
        
        # update cluster sliders
        updateSelectInput(inputId = "tp1.cluster", label = "Cluster", choices = unique(vals$clusters$tp1.cluster),  selected = NULL)
        updateSelectInput(inputId = "timepoint", label = "Time point", choices = unique(vals$clusters$timepoint), selected = NULL)
        updateSelectInput(inputId = "type", label = "Type", choices = unique(vals$clusters$type),  selected = NULL)
        updateSliderInput(inputId = "cluster.size.1.2", label = "Cluster size", min = min(vals$clusters$cluster.size.1.2, na.rm = T), max =  max(vals$clusters$cluster.size.1.2, na.rm = T), value = c(min(vals$clusters$cluster.size.1.2, na.rm = T),  max(vals$clusters$cluster.size.1.2, na.rm = T)))
        updateSliderInput(inputId = "average.date", label = "Cluster date", min = min(vals$clusters$average.date, na.rm = T), max = max(vals$clusters$average.date, na.rm = T), value = c(min(vals$clusters$average.date, na.rm = T), max(vals$clusters$average.date, na.rm = T)))
        updateSliderInput(inputId = "ecc.0.0.1", label = "Temporal ECC", min = min(vals$clusters$ecc.0.0.1, na.rm = T), max = max(vals$clusters$ecc.0.0.1, na.rm = T), value = c(min(vals$clusters$ecc.0.0.1, na.rm = T), max(vals$clusters$ecc.0.0.1, na.rm = T)))
        updateSliderInput(inputId = "ecc.0.1.0", label = "Geospatial ECC", min = min(vals$clusters$ecc.0.1.0, na.rm = T), max = max(vals$clusters$ecc.0.1.0, na.rm = T), value = c(min(vals$clusters$ecc.0.1.0, na.rm = T), max(vals$clusters$ecc.0.1.0, na.rm = T)))
        updateSliderInput(inputId = "average.latitude", label = "Average latitude", min = min(vals$clusters$average.latitude, na.rm = T), max = max(vals$clusters$average.latitude, na.rm = T), value = c(min(vals$clusters$average.latitude, na.rm = T), max(vals$clusters$average.latitude, na.rm = T)))
        updateSliderInput(inputId = "average.longitude", label = "Average longitude", min = min(vals$clusters$average.longitude, na.rm = T), max = max(vals$clusters$average.longitude, na.rm = T), value = c(min(vals$clusters$average.longitude, na.rm = T), max(vals$clusters$average.longitude, na.rm = T)))
        # update cluster delta sliders 
        sliderInput(inputId = "actual.cluster.size.tp2.size.tp1.size", label = "Delta cluster size", min = min(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T), max = max(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T), value = c(min(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T), max(vals$clusters$actual.cluster.size.tp2.size.tp1.size, na.rm = T)))
        sliderInput(inputId = "number.of.novels.in.the.tp2.match", label = "Number of new strains", min = min(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T), max = max(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T), value = c(min(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T), max(vals$clusters$number.of.novels.in.the.tp2.match, na.rm = T)))
        sliderInput(inputId = "actual.growth.rate.tp2.size.tp1.size.tp1.size", label = "Overall growth rate", min = min(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T), max = max(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T), value = c(min(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T), max(vals$clusters$actual.growth.rate.tp2.size.tp1.size.tp1.size, na.rm = T)))
        sliderInput(inputId = "novel.growth.tp2.size.tp2.size.number.of.novels", label = "Novel growth rate", min = min(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T), max = max(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T), value = c(min(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T), max(vals$clusters$novel.growth.tp2.size.tp2.size.number.of.novels, na.rm = T)))
        sliderInput(inputId = "delta.ecc.0.0.1", label = "Delta temporal ECC", min = min(vals$clusters$delta.ecc.0.0.1, na.rm = T), max = max(vals$clusters$delta.ecc.0.0.1, na.rm = T), value = c(min(vals$clusters$delta.ecc.0.0.1, na.rm = T), max(vals$clusters$delta.ecc.0.0.1, na.rm = T)))
        sliderInput(inputId = "delta.ecc.0.1.0", label = "Delta geospatial ECC", min = min(vals$clusters$delta.ecc.0.1.0, na.rm = T), max = max(vals$clusters$delta.ecc.0.1.0, na.rm = T), value = c(min(vals$clusters$delta.ecc.0.1.0, na.rm = T), max(vals$clusters$delta.ecc.0.1.0, na.rm = T)))
        #update stain filters
        updateSelectInput(inputId = "country", label = "Country", choices = unique(vals$strains.long$country),  selected = NULL)
        updateSelectInput(inputId = "province", label = "Province", choices = unique(vals$strains.long$province),  selected = NULL)
        # regional facet inputs
        updateSelectizeInput(inputId = "regionProvince", label = "View provinces in: ", choices = unique(vals$strains.long$country), selected = NULL)
    })
    
    
    ###########################
    # reactive data filtering #
    ###########################
    
    clusters_r <- reactive({
        
        print("creating reactive cluster data...")
        req(vals$clusters)
        data <- vals$clusters 
        
        # get top n
        topn <- {if (input$number != "all") as.numeric(input$number) else nrow(data)}
        
        filtered <- data %>%
            {if (!is.null(input$tp1.cluster)) filter(., tp1.cluster %in% input$tp1.cluster)  else . } %>%
            {if (!is.null(input$timepoint)) filter(., timepoint %in% input$timepoint)  else . } %>%
            {if (!is.null(input$type)) filter(., type %in% input$type)  else . } %>%
            # filter(cluster.size.1.2 >= input$cluster.size.1.2[1]) %>% filter(cluster.size.1.2 <= input$cluster.size.1.2[2]) %>%
            # filter(average.date >= input$average.date[1]) %>% filter(average.date <= input$average.date[2]) %>%
            # filter(ecc.0.0.1 >= input$ecc.0.0.1[1]) %>% filter(ecc.0.0.1 <= input$ecc.0.0.1[2]) %>%
            # filter(ecc.0.1.0 >= input$ecc.0.1.0[1]) %>% filter(ecc.0.1.0 <= input$ecc.0.1.0[2]) %>%
            # filter(average.latitude >= input$average.latitude[1]) %>%  filter(average.latitude <= input$average.latitude[2]) %>%
            # filter(average.longitude >= input$average.longitude[1]) %>% filter(average.longitude <= input$average.longitude[2])  %>%
            # delta filters
            # filter(actual.cluster.size.tp2.size.tp1.size >= input$actual.cluster.size.tp2.size.tp1.size[1] & actual.cluster.size.tp2.size.tp1.size <= input$actual.cluster.size.tp2.size.tp1.size[2]) %>%
            # filter(number.of.novels.in.the.tp2.match >= input$number.of.novels.in.the.tp2.match[1] & number.of.novels.in.the.tp2.match <= input$number.of.novels.in.the.tp2.match[2]) %>%
            # filter(actual.growth.rate.tp2.size.tp1.size.tp1.size >= input$actual.growth.rate.tp2.size.tp1.size.tp1.size[1] & actual.growth.rate.tp2.size.tp1.size.tp1.size <= input$actual.growth.rate.tp2.size.tp1.size.tp1.size[2]) %>%
            # filter(novel.growth.tp2.size.tp2.size.number.of.novels >= input$novel.growth.tp2.size.tp2.size.number.of.novels[1] & novel.growth.tp2.size.tp2.size.number.of.novels <= input$novel.growth.tp2.size.tp2.size.number.of.novels[2]) %>%
            # filter(delta.ecc.0.0.1 >= input$delta.ecc.0.0.1[1] & delta.ecc.0.0.1 <= input$delta.ecc.0.0.1[2]) %>%
            # filter(delta.ecc.0.1.0 >= input$delta.ecc.0.1.0[1] & delta.ecc.0.1.0 <= input$delta.ecc.0.1.0[2])%>%
            
            # data subsetting 
            {if (input$subsets==1) arrange(., desc(abs(cluster.size.1.2))) else . }  %>%
            {if (input$subsets==2) arrange(., desc(abs(delta.ecc.0.0.1))) else . }  %>%
            {if (input$subsets==3) arrange(., desc(abs(delta.ecc.0.1.0))) else . } 
        
        # grab n rows according to input
        if (input$number != 99){
            topn <- as.numeric(input$number)
            unique.clust <- head(unique(filtered$tp1.cluster),topn)
            return(filtered %>% subset(tp1.cluster %in% unique.clust))
        } else{  
            return(filtered)
        }
    })
    
    
    # cluster shared data frame 
    clusters_sh <- SharedData$new(clusters_r, group = "clusters")
    
    
    # all strain filtering 
    strains_r <- reactive({
        print("creating reactive strain data...")
        req(vals$strains.long,clusters_sh)
        
        vals$strains.long %>%
            {if (!is.null(input$country)) filter(., country %in% input$country)  else . } %>%
            {if (!is.null(input$province)) filter(., province %in% input$province)  else . } %>%
            # filter by shared cluster selections
            filter(tp1.cluster %in% (clusters_sh$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% pull(tp1.cluster)))
    })
    
    strains_r_group <- reactive({
        
        ungrouped <- strains_r()
        if (input$region == 2) {
            ungrouped %>%
                group_by(country, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.0.1 = mean(ecc.0.0.1),
                          ecc.0.1.0 = mean(ecc.0.1.0),
                          ecc.comb = unique(as.character(ecc_direction_delta))) %>%
                rowid_to_column(., var = "rowid") %>%
                group_by(country)
            
        } else if (input$region == 3){
            ungrouped %>%
                subset(country %in% input$regionProvince) %>% 
                group_by(province, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.0.1 = mean(ecc.0.0.1),
                          ecc.0.1.0 = mean(ecc.0.1.0),
                          ecc.comb = unique(as.character(ecc_direction_delta))) %>%
                rowid_to_column(., var = "rowid") %>%
                group_by(province)
        } else {
            ungrouped
        }
        
        
    })
    
    # strain shared data
    strains_sh <- SharedData$new(strains_r, group = "strains")
    strains_sh_gr <- SharedData$new(strains_r_group,  group = "strains_grouped")
    
    #################################
    # data frames displayed in tabs #
    #################################
    
    # clusters
    output$clusters_dt <- renderDataTable({
        datatable(clusters_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  #extension = 'Scroller', 
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    }, server=F)
    
    # strains
    output$strains_dt <- renderDataTable({
        datatable(data = strains_sh,
                  extensions = c('Select', 'Buttons','Scroller'),
                  options = list(
                      select = list(style = 'os', items = 'row'),
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('selectRows', 'selectAll', 'selectNone', 'csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none',
        )
    }, server=F)
    
    # filtered data table
    output$filtered_data <- renderDataTable({
        datatable(vals$data_proc,
                  extensions = c('Buttons','Scroller'),
                  options = list(
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    })
    
    # raw data table
    output$raw_data <- renderDataTable({
        datatable(vals$data.raw,
                  extensions = c('Buttons','Scroller'),
                  options = list(
                      dom = 'Blfrtip',
                      rowId = 0,
                      buttons = list('csv', 'excel'),
                      deferRender = TRUE,
                      scrollY = 500,
                      scrollX = 600,
                      scroller = TRUE),
                  selection = 'none')
    })
    
    
    ################################
    # cluster movement value boxes #
    ################################
    
    output$activegrowth <- renderValueBox({
        valueBox(
            subtitle = "clusters actively growing",
            value =    clusters_r() %>% 
                subset(as.numeric(timepoint) == max(as.numeric(timepoint))) %>%
                subset(actual.growth.rate.tp2.size.tp1.size.tp1.size >= 3) %>% 
                nrow()
        )
    })
    
    output$activespread <- renderValueBox({
        valueBox(
            subtitle = "clusters actively spreading",
            value = clusters_r() %>% 
                subset(as.numeric(timepoint) == max(as.numeric(timepoint))) %>%
                subset(delta.ecc.spread == "Dispersed") %>% 
                nrow()
        )
    })
    
    output$sigtransmission <- renderValueBox({
        valueBox(
            subtitle = "clusters with local transmission",
            value = clusters_r() %>% 
                slice(which(clusters_r()$ecc.spread=="Isolated")) %>%                 
                subset(as.numeric(timepoint) == max(as.numeric(timepoint))) %>%
                subset(actual.growth.rate.tp2.size.tp1.size.tp1.size >= 3) %>%
                nrow()
        )
    })
    
    
    # radio plot for ecc explanation of cluster spread
    output$ecc_radar <- renderPlotly({
        
        # #time point to time point
        # plot_ly(type="scatterpolar",
        #         data = clusters_long %>%
        #             mutate(ecc.comb = ecc_direction) %>%
        #             group_by(timepoint) %>%
        #             count(ecc.comb, .drop=FALSE) %>%
        #             full_join(directions, by = c('ecc.comb')),
        #         r = ~n,
        #         theta = ~degree.mid,
        #         frame = ~timepoint,
        #         showlegend = F,
        #         connectgaps = T,
        #         color =  I("#1F78C8"),
        #         fill = 'toself') %>%
        #     layout(showlegend = F,
        #            margin = list(
        #                l = 100, r = 100),
        #            polar = list(
        #                radialaxis = list(
        #                    range =c(0,max(clusters_long %>%
        #                                       mutate(ecc.comb = ecc_direction) %>%
        #                                       group_by(timepoint) %>%
        #                                       count(ecc.comb, .drop=FALSE) %>%
        #                                       full_join(directions, by = c('ecc.comb')) %>% 
        #                                       pull(n)))),
        #                angularaxis = list(
        #                    rotation = 90,
        #                    direction = 'clockwise',
        #                    tickmode = 'array',
        #                    tickvals = c(0, 45,  90, 135, 180,
        #                                 225, 270, 315),
        #                    ticktext = c("Slower spread", 
        #                                 HTML(paste("Slower spread,", "<br>", "more concentrated")), 
        #                                 HTML(paste("More ", "<br>", "concentrated", sep="")), 
        #                                 HTML(paste("Faster spread,", "<br>", "more concentrated")),
        #                                 "Faster spread",   
        #                                 HTML(paste("Faster spread,", "<br>", "more disperse")),
        #                                 HTML(paste("More ", "<br>", "disperse", sep="")),
        #                                 HTML(paste("Slower spread,", "<br>", "more disperse"))))))
        
        
        # change to change
        plot_ly(type="scatterpolar",
                data = clusters_sh$data(withSelection = TRUE) %>%
                    mutate(selfact = selection_factor(.),
                           ecc.comb = delta.ecc.direction) %>%
                    group_by(selfact) %>%
                    distinct(tp1.cluster, .keep_all = T) %>%
                    count(ecc.comb, .drop=FALSE) %>%
                    full_join(directions, by = c('ecc.comb')),
                r = ~n,
                theta = ~degree.mid,
                color = ~selfact,
                colors = c("#1F78C8", "red"),
                fill = 'toself') %>%
            layout(showlegend = F,
                   margin = list(l = 100, r = 100),
                   polar = list(
                       angularaxis = list(
                           rotation = 90,
                           direction = 'clockwise',
                           tickmode = 'array',
                           tickvals = c(0, 45,  90, 135, 180,
                                        225, 270, 315),
                           ticktext = c("Slower spread",
                                        HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                        HTML(paste("More ", "<br>", "concentrated", sep="")),
                                        HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                        "Faster spread",
                                        HTML(paste("Faster spread,", "<br>", "more disperse")),
                                        HTML(paste("More ", "<br>", "disperse", sep="")),
                                        HTML(paste("Slower spread,", "<br>", "more disperse"))))))
    })
    
    output$bubble <- renderPlotly({
        
        # no faceting
        if(input$region == 1) {
            a <- plot_ly() %>%
                add_trace(data = clusters_sh, 
                          type = "scatter",
                          mode = "markers", 
                          x = ~ecc.0.1.0,
                          y = 1,
                          frame = ~timepoint, 
                          size = ~I(cluster.size.1.2),
                          color = ~I("#898a8c"), 
                          hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                 'Geospatial ECC:', ecc.0.1.0,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          marker = list(sizemode = "area", opacity = 0.5)) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.3,
                                          #font = list(size = 14),
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            b <- plot_ly() %>%
                add_trace(data = clusters_sh, 
                          type = "scatter",
                          mode = "markers", 
                          x = ~ecc.0.0.1,
                          y = 1,
                          size = ~I(cluster.size.1.2),
                          color = ~I("#898a8c"), 
                          frame = ~timepoint, 
                          hovertemplate = ~paste('<b>', tp1.cluster,'</b>','<br>',
                                                 'Temporal ECC:', ecc.0.0.1,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE,
                          marker = list(sizemode = "area", opacity = 0.5)) %>%
                layout(xaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0, 1.05)),
                       yaxis = list(showticklabels = F),
                       annotations = list(text= "Temporal epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.3,
                                          showarrow = FALSE)) %>%
                highlight(color = "#1F78C8")
            
            c <- plot_ly() %>%
                add_trace(data = clusters_sh,
                          type = "scatter",
                          mode = "markers", 
                          x = ~ecc.0.1.0,
                          y = ~ecc.0.0.1,
                          frame = ~timepoint,  
                          size = ~I(cluster.size.1.2),
                          color = ~I("#898a8c"), 
                          hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                 'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                 'Temporal ECC:', ecc.0.0.1,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE, 
                          marker = list(sizemode = "area", opacity = 0.5)) %>%
                layout(xaxis = list(title = "Geospatial epicluster cohesion index",
                                    range = c(0,1.05)),
                       yaxis = list(title = "Temporal epicluster cohesion index",
                                    range = c(0,1.05)),
                       annotations = list(list(text= "Geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.15,
                                               #font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.5,
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            # Link Animated views
            subplot(c, subplot(a, b, nrows = 2, margin = 0.075), nrows = 1, margin = 0.025)
            
        } else if (input$region == 2 ){
            
            geo <- strains_sh$data(withSelection=T) %>%
                group_by(country, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.1.0 = mean(ecc.0.1.0)) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, # group and summarize here?
                                           # catch statement for less than 3 groups?
                                           type = "scatter",
                                           mode = "markers",
                                           x = ~ecc.0.1.0,
                                           y = 1,
                                           color = I("#898a8c"),
                                           frame = ~timepoint,
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", opacity = 0.5),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(range = c(-0.05,1.05), title = "", zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")
                ) %>%
                subplot(nrows = 3,   shareX = T) %>%
                layout(annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE))
            
            temp <-  strains_sh$data(withSelection=T)  %>%
                group_by(country, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.0.1 = mean(ecc.0.0.1)) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.0.1,
                                           y = 1,
                                           color = I("#898a8c"),
                                           frame = ~timepoint, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", opacity = 0.5),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(range = c(-0.05,1.05), title = "", zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")
                ) %>% 
                subplot(nrows = 3, shareX = T) %>%
                layout(annotations = list(text= "Temporal epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE))
            
            both <- strains_sh$data(withSelection=T)  %>%
                group_by(country, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.0.1 = mean(ecc.0.0.1),
                          ecc.0.1.0 = mean(ecc.0.1.0)) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.0.1,
                                           y = ~ecc.0.1.0,
                                           color = I("#898a8c"),
                                           frame = ~timepoint, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", opacity = 0.5),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(range = c(-0.05,1.05), title = ""),
                                  yaxis = list(range = c(-0.05,1.05)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.95,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")
                ) %>% 
                subplot(nrows = 3, shareX = T) %>%
                layout(annotations = list( list(text= "Geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.075,
                                                font = list(size = 14),
                                                showarrow = FALSE),
                                           list(text= "Temporal epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "center",
                                                xanchor = "center",
                                                align = "center",
                                                x = -0.1,
                                                y = 0.3,
                                                textangle = -90,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            # Link Animated views
            subplot(both, subplot(geo, temp, nrows = 2, margin = 0.075), nrows = 1, margin = 0.02)
            
        } else if (input$region == 3) {
            
            geo <- strains_sh$data(withSelection=T)  %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.1.0 = mean(ecc.0.1.0)) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.1.0,
                                           y = 1,
                                           color = I("#898a8c"),
                                           frame = ~timepoint, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", opacity = 0.5),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(range = c(-0.05,1.05), title = "", zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")
                ) %>% 
                subplot(nrows = 3,   shareX = T) %>%
                layout(annotations = list(text= "Geospatial epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE))
            
            temp <- strains_sh$data(withSelection=T)  %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.0.1 = mean(ecc.0.0.1)) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.0.1,
                                           y = 1,
                                           color = I("#898a8c"),
                                           frame = ~timepoint, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", opacity = 0.5),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(range = c(-0.05,1.05), title = "", zeroline = F),
                                  yaxis = list(showticklabels = F),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.9,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")
                ) %>% 
                subplot(nrows = 3, shareX = T) %>%
                layout(annotations = list(text= "Temporal epicluster cohesion index",
                                          xref = "paper",
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "center",
                                          x = 0.5,
                                          y = -0.175,
                                          font = list(size = 14),
                                          showarrow = FALSE))
            
            both <- strains_sh$data(withSelection=T)  %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, timepoint) %>%
                summarize(n = n(),
                          ecc.0.0.1 = mean(ecc.0.0.1),
                          ecc.0.1.0 = mean(ecc.0.1.0)) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d,                          
                                           type = "scatter",
                                           mode = "markers", 
                                           x = ~ecc.0.0.1,
                                           y = ~ecc.0.1.0,
                                           color = I("#898a8c"),
                                           frame = ~timepoint, 
                                           hovertemplate = ~paste('<b>', tp1.cluster,'</b>', '<br>',
                                                                  'Geospatial ECC:', ecc.0.1.0, '<br>',
                                                                  'Temporal ECC:', ecc.0.0.1,
                                                                  '<extra></extra>',
                                                                  sep = " "),                                           size = ~I(n),
                                           marker = list(sizemode = "area", opacity = 0.5),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(range = c(-0.05,1.05), title = ""),
                                  yaxis = list(range = c(-0.05,1.05)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 0.95,
                                                     showarrow = FALSE)) %>%
                           highlight(color = "#1F78C8")
                ) %>% 
                subplot(nrows = 3, shareX = T) %>%
                layout(annotations = list( list(text= "Geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.075,
                                                font = list(size = 14),
                                                showarrow = FALSE),
                                           list(text= "Temporal epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "center",
                                                xanchor = "center",
                                                align = "center",
                                                x = -0.1,
                                                y = 0.3,
                                                textangle = -90,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            # Link Animated views
            subplot(both, subplot(geo, temp, nrows = 2, margin = 0.075), nrows = 1, margin = 0.02)
        }
        
    })
    
    output$ecc_histograms <- renderPlotly({
        
        if(input$region == 1){
            # no faceting 
            geo <- plot_ly() %>%
                add_trace(data = strains_sh,
                          type = "histogram",
                          x = ~ecc.0.1.0,
                          frame = ~timepoint,
                          color = I("#898a8c"),
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE) %>%
                layout(xaxis = list(range = c(0,1),
                                    title = "Geospatial epicluster cohesion index"),
                       yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_))))),
                       annotations = list(list(text= "Geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.3,
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.3,
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            delta_geo <- plot_ly() %>%
                add_trace(data = strains_sh,
                          type = "histogram",
                          x = ~delta.ecc.0.1.0,
                          frame = ~timepoint,
                          color = I("#898a8c"),
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE) %>%
                layout(xaxis = list(range = c(-1,1),
                                    title = "Delta geospatial epicluster cohesion index"),
                       yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_))))),
                       annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.3,
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.3,
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            
            temp <- plot_ly() %>%
                add_trace(data = strains_sh,
                          type = "histogram",
                          x = ~ecc.0.0.1,
                          frame = ~timepoint,
                          color = I("#898a8c"), 
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE) %>%
                layout(xaxis = list(range = c(0,1),
                                    title = "Temporal epicluster cohesion index"),
                       yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_))))),
                       annotations = list(list(text= "Temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.3,
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.3,
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            delta_temp <- plot_ly() %>%
                add_trace(data = strains_sh,
                          type = "histogram",
                          x = ~delta.ecc.0.0.1,
                          frame = ~timepoint,
                          color = I("#898a8c"), 
                          xbins = list(size = 0.01),
                          hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                 'Count:', '%{y}' ,
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = FALSE) %>%
                layout(xaxis = list(range = c(-1,1),
                                    title = "Delta temporal epicluster cohesion index"),
                       yaxis = list(range = c(0, nrow(strains_sh$data(withSelection = T) %>%
                                                          filter(selected_ | is.na(selected_))))),
                       annotations = list(list(text= "Delta temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "bottom",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.3,
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "centre",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.075,
                                               y = 0.3,
                                               textangle = -90,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            subplot(subplot(geo, temp, nrows=1,  margin = 0.025), subplot(delta_geo, delta_temp, nrows=1, margin = 0.025), nrows=2, margin = 0.075) 
            
        } else if (input$region == 2){
            
            geo <- strains_sh$data(withSelection=T)  %>% 
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(0,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     group_by(country, timepoint) %>%
                                                                     count(cut_width(ecc.0.1.0,width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            temp <- strains_sh$data(withSelection=T) %>% 
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(0,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     group_by(country, timepoint) %>%
                                                                     count(cut_width(ecc.0.0.1, width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Temporal epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            delta_geo <- strains_sh$data(withSelection=T) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(-1,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     group_by(country, timepoint) %>%
                                                                     count(cut_width(delta.ecc.0.1.0, width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            delta_temp <- strains_sh$data(withSelection=T) %>% 
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(-1,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     group_by(country, timepoint) %>%
                                                                     count(cut_width(delta.ecc.0.0.1, width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(country),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            subplot(subplot(geo, temp, nrows=1, margin = 0.05), 
                    subplot(delta_geo, delta_temp, nrows=1, margin = 0.05), 
                    nrows=2, margin = 0.1)  
            
        } else if (input$region == 3) {
            
            # by province
            geo <- strains_sh$data(withSelection=T)  %>% 
                subset(country %in% input$regionProvince) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(0,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     subset(country %in% input$regionProvince) %>%
                                                                     group_by(province, timepoint) %>%
                                                                     count(cut_width(ecc.0.1.0,width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            temp <- strains_sh$data(withSelection=T) %>% 
                subset(country %in% input$regionProvince) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(0,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     subset(country %in% input$regionProvince) %>%
                                                                     group_by(province, timepoint) %>%
                                                                     count(cut_width(ecc.0.0.1, width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Temporal epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            delta_geo <- strains_sh$data(withSelection=T) %>%
                subset(country %in% input$regionProvince) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.1.0,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(-1,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     subset(country %in% input$regionProvince) %>%
                                                                     group_by(province, timepoint) %>%
                                                                     count(cut_width(delta.ecc.0.1.0, width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            delta_temp <- strains_sh$data(withSelection=T) %>% 
                subset(country %in% input$regionProvince) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           x = ~delta.ecc.0.0.1,
                                           color = I("#898a8c"),
                                           xbins = list(size = 0.01),
                                           showlegend = FALSE,
                                           hovertemplate = ~paste('Bin range:', '%{x}', '<br>',
                                                                  'Count:', '%{y}' ,
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           frame = ~timepoint) %>%
                           layout(xaxis = list(range = c(-1,1), title = ""),
                                  yaxis = list(range = c(0, max( strains_sh$data(withSelection=T) %>%
                                                                     subset(country %in% input$regionProvince) %>%
                                                                     group_by(province, timepoint) %>%
                                                                     count(cut_width(delta.ecc.0.0.1, width=0.01)) %>%
                                                                     pull(n))*1.1)),
                                  annotations = list(text= ~unique(province),
                                                     xref = "paper",
                                                     yref = "paper",
                                                     yanchor = "bottom",
                                                     xanchor = "center",
                                                     align = "center",
                                                     x = 0.5,
                                                     y = 1,
                                                     showarrow = FALSE))
                ) %>%
                subplot(nrows = 3, margin = 0.05, shareY=T) %>%
                layout( annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                                xref = "paper",
                                                yref = "paper",
                                                yanchor = "bottom",
                                                xanchor = "center",
                                                align = "center",
                                                x = 0.5,
                                                y = -0.15,
                                                font = list(size = 14),
                                                showarrow = FALSE)))
            
            subplot(subplot(geo, temp, nrows=1, margin = 0.05), 
                    subplot(delta_geo, delta_temp, nrows=1, margin = 0.05), 
                    nrows=2, margin = 0.1)  
            
        }
    })
    
    output$radar <- renderPlotly({
        
        if (input$region ==1) {
            
            clusters_sh$data(withSelection = TRUE) %>%
                filter(selected_ | is.na(selected_)) %>%
                mutate(ecc.comb = delta.ecc.direction) %>%
                group_by(timepoint) %>%
                count(ecc.comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc.comb')) %>% 
                subset(n>0) %>%
                plot_ly(type="scatterpolar",
                        r = ~n,
                        theta = ~degree.mid,
                        color  = I("#898a8c"),
                        #color = I(ifelse(sum(clusters_sh$data(withSelection = TRUE) %>% pull(selected_))>0, "#1F78C8", "#898a8c")),
                        frame = ~timepoint, 
                        hovertemplate = ~paste('<b>', ecc.comb, '</b>', '<br>',
                                               'Count:', '%{r}', 
                                               '<extra></extra>',
                                               sep = " "),
                        fill = 'toself') %>%
                layout(showlegend = F,
                       margin = list(l = 100, r = 100),
                       polar = list(
                           angularaxis = list(
                               rotation = 90,
                               direction = 'clockwise',
                               tickmode = 'array',
                               tickvals = c(0, 45,  90, 135, 180,
                                            225, 270, 315),
                               ticktext = c("Slower spread",
                                            HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                            HTML(paste("More ", "<br>", "concentrated", sep="")),
                                            HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                            "Faster spread",
                                            HTML(paste("Faster spread,", "<br>", "more disperse")),
                                            HTML(paste("More ", "<br>", "disperse", sep="")),
                                            HTML(paste("Slower spread,", "<br>", "more disperse"))))))
        } else if (input$region == 2) {
            
            # by country 
            strains_sh_gr$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(country, timepoint) %>%
                count(ecc.comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc.comb')) %>%
                na.omit() %>%
                subset(n>0) %>%
                arrange(cardinal) %>%
                plot_ly(type="scatterpolar",
                        frame = ~timepoint,
                        r = ~n,
                        split = ~country,
                        theta = ~degree.mid,
                        name = ~country,
                        hovertemplate = ~paste('<b>', ecc.comb, '</b>', '<br>',
                                               'Count:', '%{r}', 
                                               '<extra></extra>',
                                               sep = " "),
                        color  = I("#898a8c"),
                        fill = "tonext") %>%
                layout(margin = list(l = 100, r = 100),
                       polar = list(
                           angularaxis = list(
                               rotation = 90,
                               direction = 'clockwise',
                               tickmode = 'array',
                               tickvals = c(0, 45,  90, 135, 180,
                                            225, 270, 315),
                               ticktext = c("Slower spread",
                                            HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                            HTML(paste("More ", "<br>", "concentrated", sep="")),
                                            HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                            "Faster spread",
                                            HTML(paste("Faster spread,", "<br>", "more disperse")),
                                            HTML(paste("More ", "<br>", "disperse", sep="")),
                                            HTML(paste("Slower spread,", "<br>", "more disperse"))))))
            
        } else if (input$region == 3){
            # by province 
            strains_sh_gr$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(province, timepoint) %>%
                count(ecc.comb, .drop=FALSE) %>%
                full_join(directions, by = c('ecc.comb')) %>%
                na.omit() %>%
                arrange(cardinal) %>%
                plot_ly(type="scatterpolar",
                        frame = ~timepoint,
                        r = ~n,
                        split = ~province,
                        theta = ~degree.mid,
                        name = ~province,
                        hovertemplate = ~paste('<b>', ecc.comb, '</b>', '<br>',
                                               'Count:', '%{r}', 
                                               '<extra></extra>',
                                               sep = " "),
                        color  = I("#898a8c"),
                        fill = "tonext") %>%
                layout(margin = list(l = 100, r = 100),
                       polar = list(
                           angularaxis = list(
                               rotation = 90,
                               direction = 'clockwise',
                               tickmode = 'array',
                               tickvals = c(0, 45,  90, 135, 180,
                                            225, 270, 315),
                               ticktext = c("Slower spread",
                                            HTML(paste("Slower spread,", "<br>", "more concentrated")),
                                            HTML(paste("More ", "<br>", "concentrated", sep="")),
                                            HTML(paste("Faster spread,", "<br>", "more concentrated")),
                                            "Faster spread",
                                            HTML(paste("Faster spread,", "<br>", "more disperse")),
                                            HTML(paste("More ", "<br>", "disperse", sep="")),
                                            HTML(paste("Slower spread,", "<br>", "more disperse"))))))
        }
    })
    
    
    output$changevector <- renderPlotly({
        
        # no faceting 
        if (input$region == 1) {
            
            # opacity settings
            op <- clusters_sh$data(withSelection = T)
            op$opacity <- ifelse(op$selected_ | is.na(op$selected_), 0.6, 0.6*0.3)
            op$colour <- ifelse(op$selected_ , "#1F78C8","#898a8c")
            
            plot_ly(type = "scatter", mode="markers") %>% 
                #add segment to connect each point to origin
                add_trace(data = clusters_sh,
                          x = ~delta.ecc.0.1.0, # geographical
                          y = ~delta.ecc.0.0.1, # temporal 
                          split = ~tp1.cluster,
                          legendgroup = ~tp1.cluster,
                          opacity = I(op$opacity),
                          frame = ~timepoint,
                          color = I(op$colour),
                          showlegend = F,
                          opacity = 1,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Delta geospatial ECC:', '%{x}', '<br>',
                                                 'Delta temporal ECC:', '%{y}', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          size=I(0)) %>%
                # set range and overlay arrow annotations
                layout(xaxis = list(range = c(-1, 1.5), 
                                    title = "Delta geospatial ECC value"),
                       yaxis = list(range = c(-1, 1.5),
                                    title = "Delta temporal ECC value"),
                       annotations = list(ax = 0, ay = 0,
                                          axref='x', ayref='y',
                                          x = ~delta.ecc.0.1.0, # geospatial
                                          y = ~delta.ecc.0.0.1, # temporal
                                          opacity = I(op$opacity),
                                          #name = ~paste(tp1.cluster, timepoint, sep="_"),
                                          arrowcolor = I(op$colour),
                                          xref='x', yref='y', text = ""))
            
            # facet by country  
        } else if (input$region == 2) {
            
            strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                distinct(country, tp1.cluster, .keep_all = T) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter", 
                                           mode='markers',
                                           x = ~delta.ecc.0.1.0, # geographical
                                           y = ~delta.ecc.0.0.1, # temporal 
                                           name = ~tp1.cluster,
                                           #legendgroup = ~tp1.cluster,
                                           #opacity = I(op$opacity),
                                           #frame = ~timepoint,
                                           color = I("#898a8c"),
                                           showlegend = F,
                                           opacity = 1,
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Delta geospatial ECC:', '%{x}', '<br>',
                                                                  'Delta temporal ECC:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           size=I(1)) %>%
                           layout(xaxis = list(range = c(-1, 1),
                                               title = ""),
                                  yaxis = list(range = c(-1, 1),
                                               title = "")) %>%
                           add_annotations(text= ~unique(country),
                                           xref = "paper",
                                           yref = "paper",
                                           yanchor = "center",
                                           xanchor = "center",
                                           align = "center",
                                           x = 0.5,
                                           y = 1.1,
                                           showarrow = FALSE)) %>%
                subplot(nrows = 3, shareX = T, shareY = T) %>%
                layout(annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Delta temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            # facet by province
        } else if (input$region == 3) {
            
            strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>% 
                distinct(province, tp1.cluster, .keep_all = T) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "scatter", 
                                           mode='markers',
                                           x = ~delta.ecc.0.1.0, # geographical
                                           y = ~delta.ecc.0.0.1, # temporal 
                                           #legendgroup = ~tp1.cluster,
                                           #opacity = I(op$opacity),
                                           #frame = ~timepoint,
                                           color = I("#898a8c"),
                                           showlegend = F,
                                           opacity = 1,
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Delta geospatial ECC:', '%{x}', '<br>',
                                                                  'Delta temporal ECC:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           size=I(1)) %>%
                           layout(xaxis = list(range = c(-1, 1),
                                               title = ""),
                                  yaxis = list(range = c(-1, 1),
                                               title = ""),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>%
                subplot(nrows = 3, shareX = T, shareY = T) %>%
                layout(annotations = list(list(text= "Delta geospatial epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Delta temporal epicluster cohesion index",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
        }
    })
    
    
    output$cluster_growth <- renderPlotly({
        
        if (input$region == 1) {
            
            a <- plot_ly(data = clusters_sh) %>%
                add_trace(type = "scatter",
                          mode = "markers",
                          x = ~timepoint,
                          y = ~actual.growth.rate.tp2.size.tp1.size.tp1.size,
                          color = I("#898a8c"),
                          showlegend = F,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Overall growth rate:', '%{y}', '<br>', 
                                                 'Time point:', '%{x}', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          frame=~timepoint) %>%
                layout(yaxis = list(rangemode = "tozero", 
                                    title = "Overall growth rate"),
                       xaxis = list(title = "Time point"),
                       annotations = list(list(text= "Overall cluster growth rate",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               textangle = -90,
                                               x = -0.05,
                                               y = 0.5,
                                               showarrow = FALSE),
                                          list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               showarrow = FALSE)))%>%
                highlight(color = "#1F78C8")
            
            b <- plot_ly(data = clusters_sh) %>%
                add_trace(type = "scatter",
                          mode = "markers",
                          x = ~timepoint,
                          y = ~novel.growth.tp2.size.tp2.size.number.of.novels,
                          color = I("#898a8c"),
                          showlegend = F,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Novel growth rate:', '%{y}', '<br>', 
                                                 'Time point:', '%{x}', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          frame=~timepoint) %>%
                layout(yaxis = list(rangemode = "tozero",
                                    title = "Novel growth rate"),
                       xaxis = list(title = "Time point"),
                       annotations = list(list(text= "Novel cluster growth rate",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               textangle = -90,
                                               x = -0.05,
                                               y = 0.5,
                                               showarrow = FALSE),
                                          list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               showarrow = FALSE))) %>%
                highlight(color = "#1F78C8")
            
            subplot(a, b, nrows = 1, margin = 0.025)
            
        } else if (input$region == 2) {
            
            bycountry <- 
                strains_sh$data(withSelection=T) %>%
                filter(selected_ | is.na(selected_)) %>%
                mutate(country = as.factor(country),
                       timepoint = as.factor(timepoint),
                       tp1 = as.factor(tp1)) %>%
                group_by(country, tp1.cluster, timepoint, tp1) %>%
                count() %>%
                ungroup() %>%
                complete(expand(.,timepoint, tp1, country, tp1.cluster), fill = list(n = NA)) 
            
            growth = data.frame(expand(bycountry, country, tp1.cluster))
            
            for(i in seq(length(levels(bycountry$timepoint))-1)){
                
                tp2 <- bycountry %>% 
                    group_by(country) %>%
                    subset(timepoint == i+1) %>%
                    group_by(country, tp1.cluster) %>%
                    summarize(n=sum(na.omit(n)))
                
                tp2novels <- bycountry %>% 
                    group_by(country) %>%
                    subset(timepoint == i+1) %>%
                    group_by(country, tp1.cluster, tp1) %>%
                    summarize(n=sum(n)) %>%
                    subset(tp1==0)
                
                tp1 <- bycountry %>% 
                    group_by(country) %>%
                    subset(timepoint == i) %>%
                    subset(tp1==1) %>%
                    group_by(country, tp1.cluster) %>%
                    summarize(n=sum(na.omit(n)))
                
                # from vasena 
                #actual_growth_rate = ((tp2_cl_size - tp1_cl_size) / tp1_cl_size) %>% round(., digits = 3), 
                #new_growth = (tp2_cl_size / (tp2_cl_size - num_novs)) %>% round(., digits = 3))
                
                overall_name <- paste("overall", i, sep="_")
                novel_name <- paste("novel", i, sep="_")
                
                growth[[overall_name]] <- (tp2$n - tp1$n)/tp1$n
                growth[[novel_name]] <- tp2$n/(tp2$n -tp2novels$n)
            }
            
            # for more time points, use this method
            long_df <- as.data.frame(data.table::melt(setDT(growth),
                                                      id.vars = c("country", "tp1.cluster"),
                                                      measure = patterns(overall = "^overall_*",
                                                                         novel = "^novel_*"),
                                                      variable.name='timepoint'))
            
            overall_max <- max(na.omit(long_df$overall)[!is.infinite(na.omit(long_df$overall))])
            novel_max <- max(na.omit(long_df$novel)[!is.infinite(na.omit(long_df$novel))])
            
            overall <- long_df %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           y = ~overall,
                                           x = ~timepoint, 
                                           frame = ~timepoint,
                                           hovertext = ~tp1.cluster,
                                           color = I("#898a8c"),
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Ovwerall growth rate:', '%{y}', '<br>', 
                                                                  'Time point:', '%{x}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(title = ""), 
                                  yaxis = list(title = "",
                                               range = c(0,overall_max)),
                                  annotations = list(list(text= ~unique(country),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>%
                subplot(nrows = 3,  margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Overall cluster growth",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            novel <- long_df %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           y = ~novel,
                                           x = ~timepoint, 
                                           frame = ~timepoint,
                                           hovertext = ~tp1.cluster,
                                           color = I("#898a8c"),
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Novel growth rate:', '%{y}', '<br>', 
                                                                  'Time point:', '%{x}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(title = ""), 
                                  yaxis = list(title = "",
                                               range = c(0,novel_max)),
                                  annotations = list(list(text= ~unique(country),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>%
                subplot(nrows = 3, shareX = T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Novel cluster growth",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            subplot(overall, novel, margin = 0.025)
            
        } else if (input$region == 3) {
            
            byprovince <- 
                strains_sh$data(withSelection=T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                mutate(province = as.factor(province),
                       timepoint = as.factor(timepoint),
                       tp1 = as.factor(tp1)) %>%
                group_by(province, tp1.cluster, timepoint, tp1) %>%
                count() %>%
                ungroup() %>%
                complete(expand(.,timepoint, tp1, province, tp1.cluster), fill = list(n = NA)) 
            
            growth = data.frame(expand(byprovince, province, tp1.cluster))
            
            for(i in seq(length(levels(byprovince$timepoint))-1)){
                
                tp2 <- byprovince %>% 
                    group_by(province) %>%
                    subset(timepoint == i+1) %>%
                    group_by(province, tp1.cluster) %>%
                    summarize(n=sum(na.omit(n)))
                
                tp2novels <- byprovince %>% 
                    group_by(province) %>%
                    subset(timepoint == i+1) %>%
                    group_by(province, tp1.cluster, tp1) %>%
                    summarize(n=sum(n)) %>%
                    subset(tp1==0)
                
                tp1 <- byprovince %>% 
                    group_by(province) %>%
                    subset(timepoint == i) %>%
                    subset(tp1==1) %>%
                    group_by(province, tp1.cluster) %>%
                    summarize(n=sum(na.omit(n)))
                
                # from vasena
                #actual_growth_rate = ((tp2_cl_size - tp1_cl_size) / tp1_cl_size) %>% round(., digits = 3), 
                #new_growth = (tp2_cl_size / (tp2_cl_size - num_novs)) %>% round(., digits = 3))
                
                overall_name <- paste("overall", i, sep="_")
                novel_name <- paste("novel", i, sep="_")
                
                growth[[overall_name]] <- (tp2$n - tp1$n)/tp1$n
                growth[[novel_name]] <- tp2$n/(tp2$n -tp2novels$n)
            }
            
            # for more time points, use this method
            long_df <- as.data.frame(data.table::melt(setDT(growth),
                                                      id.vars = c("province", "tp1.cluster"),
                                                      measure = patterns(overall = "^overall_*",
                                                                         novel = "^novel_*"),
                                                      variable.name='timepoint'))
            
            overall_max <- max(na.omit(long_df$overall)[!is.infinite(na.omit(long_df$overall))])
            novel_max <- max(na.omit(long_df$novel)[!is.infinite(na.omit(long_df$novel))])
            
            overall <- long_df %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           y = ~overall,
                                           x = ~timepoint, 
                                           frame = ~timepoint,
                                           hovertext = ~tp1.cluster,
                                           color = I("#898a8c"),
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Overall growth rate:', '%{y}', '<br>', 
                                                                  'Time point:', '%{x}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(title = ""), 
                                  yaxis = list(title = "",
                                               range = c(0,overall_max)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>%
                subplot(nrows = 3, shareX = T,  margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Overall cluster growth",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            novel <- long_df %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d,
                                           type = "scatter",
                                           y = ~novel,
                                           x = ~timepoint, 
                                           frame = ~timepoint,
                                           hovertext = ~tp1.cluster,
                                           color = I("#898a8c"),
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Novel growth rate:', '%{y}', '<br>', 
                                                                  'Time point:', '%{x}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = FALSE) %>%
                           layout(xaxis = list(title = ""), 
                                  yaxis = list(title = "",
                                               range = c(0, novel_max)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>%
                subplot(nrows = 3, shareX = T,  margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.1,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Novel cluster growth",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            subplot(overall, novel, margin = 0.025)
        }
    }) 
    
    output$strainsbycluster <- renderPlotly({
        
        # no faceting 
        if (input$region == 1) {
            
            # tally counts by cluster and strain date 
            counts <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1.cluster, strain.date) %>%
                tally()
            
            # mountain plot
            mountain <- plot_ly(data = counts) %>%
                add_trace(type = "scatter",
                          mode = "line",
                          color = I("#898a8c"),
                          x = ~strain.date,
                          y = ~n, 
                          name = ~tp1.cluster, 
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Date:', '%{x}', '<br>', 
                                                 'Count:', '%{y}', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = F) %>%
                layout(yaxis = list(rangemode = "tozero"),
                       xaxis = list(tickvals = as.list(strains_sh$data() %>%
                                                            group_by(tp1) %>%
                                                            filter(strain.date == min(strain.date)) %>%
                                                            distinct(strain.date) %>%
                                                            pull(strain.date) %>%
                                                            sort()),
                                    ticktext = as.list( as.character(seq(length(strains_sh$data() %>% 
                                                                                    group_by(tp1) %>% 
                                                                                    filter(strain.date == min(strain.date)) %>% 
                                                                                    distinct(strain.date) %>% 
                                                                                    pull(strain.date) %>% 
                                                                                    order())))),
                                    tickmode = "array")) %>%
                add_annotations(text= "Timepoint",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.1,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                add_annotations(text= "Number of novel strains identified",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.1,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                highlight(color = "#1F78C8")
            
            # need to aggregate data for cumsum 
            cumsum <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(tp1.cluster, strain.date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # cumsum plot 
            cumplot <- plot_ly(data = cumsum) %>%
                add_trace(type = "scatter",
                          mode = "line",
                          color = I("#898a8c"),
                          x = ~strain.date,
                          y = ~cumsum, 
                          name = ~tp1.cluster,
                          hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                 'Date:', '%{x}', '<br>', 
                                                 'Count:', '%{y}', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          showlegend = F) %>%
                layout(yaxis = list(rangemode = "tozero"),
                       xaxis = list(tickvals = as.list(strains_sh$data() %>%
                                                            group_by(tp1) %>%
                                                            filter(strain.date == min(strain.date)) %>%
                                                            distinct(strain.date) %>%
                                                            pull(strain.date) %>%
                                                            sort()),
                                    ticktext = as.list( as.character(seq(length(strains_sh$data() %>% 
                                                                                    group_by(tp1) %>% 
                                                                                    filter(strain.date == min(strain.date)) %>% 
                                                                                    distinct(strain.date) %>% 
                                                                                    pull(strain.date) %>% 
                                                                                    order())))),
                                    tickmode = "array")) %>%
                add_annotations(text= "Timepoint",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = 0.5,
                                y = -0.1,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                add_annotations(text= "Cumulative number of strains identified",
                                xref = "paper",
                                yref = "paper",
                                yanchor = "center",
                                xanchor = "center",
                                align = "center",
                                x = -0.1,
                                y = 0.5,
                                textangle = -90,
                                font = list(size = 14),
                                showarrow = FALSE) %>%
                highlight(color = "#1F78C8")
            
            # subplot mountain and cumsum together 
            subplot(mountain, cumplot, nrows = 1, margin = 0.05)
            
            # facet by country     
        } else if (input$region == 2) {
            
            # tally counts by country, cluster, and date
            counts <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(country, tp1.cluster, strain.date) %>%
                tally()
            
            # mountain plot 
            mountain <- counts %>% 
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "scatter",
                                           mode = "line",
                                           color = I("#898a8c"),
                                           x = ~strain.date,
                                           y = ~n, 
                                           name = ~tp1.cluster, 
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>', 
                                                                  'Count:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F) %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.long %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.long %>% 
                                                                                               group_by(tp1) %>% 
                                                                                               filter(strain.date == min(strain.date)) %>% 
                                                                                               distinct(strain.date) %>% 
                                                                                               pull(strain.date) %>% 
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(counts$n))*1.1)),
                                  annotations = list(list(text= ~unique(country),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = 3, shareX = T, shareY=T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Date",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            # aggregate data by country, cluster, and strain date 
            # take cumsum of counts 
            cumsum <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(country, tp1.cluster, strain.date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # cumsum plot 
            cumplot <- cumsum %>% 
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "scatter",
                                           mode = "line",
                                           color = I("#898a8c"),
                                           x = ~strain.date,
                                           y = ~cumsum, 
                                           name = ~tp1.cluster, 
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>', 
                                                                  'Count:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F) %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.long %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.long %>% 
                                                                                               group_by(tp1) %>% 
                                                                                               filter(strain.date == min(strain.date)) %>% 
                                                                                               distinct(strain.date) %>% 
                                                                                               pull(strain.date) %>% 
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(cumsum$cumsum))*1.1)),
                                  annotations = list(list(text= ~unique(country),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = 3, shareX = T, shareY=T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Cumulative count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            # subplot mountain and cumsum together 
            subplot(mountain, cumplot, nrows = 1, margin = 0.025)
            
            # facet by province    
        } else if (input$region == 3) {
            
            # tally counts by province, cluster, and date
            counts <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, strain.date) %>%
                tally()
            
            # mountain plot 
            mountain <- counts %>% 
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "scatter",
                                           mode = "line",
                                           color = I("#898a8c"),
                                           x = ~strain.date,
                                           y = ~n, 
                                           name = ~tp1.cluster, 
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>', 
                                                                  'Count:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F) %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.long %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.long %>% 
                                                                                               group_by(tp1) %>% 
                                                                                               filter(strain.date == min(strain.date)) %>% 
                                                                                               distinct(strain.date) %>% 
                                                                                               pull(strain.date) %>% 
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(counts$n))*1.1)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = 3, shareX = T, shareY=T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Date",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            # aggregate data by province, cluster, and strain date 
            # take cumsum of counts 
            cumsum <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, tp1.cluster, strain.date) %>% 
                tally(!is.na(strain)) %>% 
                mutate(cumsum = cumsum(n))
            
            # cumsum plot 
            cumplot <- cumsum %>% 
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "scatter",
                                           mode = "line",
                                           color = I("#898a8c"),
                                           x = ~strain.date,
                                           y = ~cumsum, 
                                           name = ~tp1.cluster, 
                                           hovertemplate = ~paste('<b>', tp1.cluster, '</b>', '<br>',
                                                                  'Date:', '%{x}', '<br>', 
                                                                  'Count:', '%{y}', 
                                                                  '<extra></extra>',
                                                                  sep = " "),
                                           showlegend = F) %>%
                           layout(xaxis = list(title = "",
                                               tickvals = as.list( strains.long %>%
                                                                       group_by(tp1) %>%
                                                                       filter(strain.date == min(strain.date)) %>%
                                                                       distinct(strain.date) %>%
                                                                       pull(strain.date) %>%
                                                                       sort()),
                                               ticktext = as.list( as.character(seq(length(strains.long %>% 
                                                                                               group_by(tp1) %>% 
                                                                                               filter(strain.date == min(strain.date)) %>% 
                                                                                               distinct(strain.date) %>% 
                                                                                               pull(strain.date) %>% 
                                                                                               order())))),
                                               tickmode = "array"),
                                  yaxis = list(title = "",
                                               range = c(0, max(na.omit(cumsum$cumsum))*1.1)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE))) %>%
                           highlight(color = "#1F78C8")) %>% 
                subplot(nrows = 3, shareX = T, shareY=T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Cumulative count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            # subplot mountain and cumsum together 
            subplot(mountain, cumplot, nrows = 1, margin = 0.025)
        }
    })
    
    output$newstrainsbydate <- renderPlotly({
        
        # no faceting
        if (input$region == 1) {
            plot_ly(type = "histogram",
                    data = strains_sh,
                    histfunc = "count",
                    x = ~strain.date,
                    color = I("#898a8c"),                
                    xbins = list(size = 86400000.0),
                    hovertemplate = ~paste('Count: %{y}', '<br>',
                                           'Date range: %{x}', '<extra></extra>', sep=" ")) %>%
                layout(barmode = "stack",
                       xaxis = list(title = "Time point",
                                    tickvals = as.list(strains_sh$data() %>%
                                                            group_by(tp1) %>%
                                                            filter(strain.date == min(strain.date)) %>%
                                                            distinct(strain.date) %>%
                                                            pull(strain.date) %>%
                                                            sort()),
                                    ticktext = as.list( as.character(seq(length(strains_sh$data() %>% 
                                                                                    group_by(tp1) %>% 
                                                                                    filter(strain.date == min(strain.date)) %>% 
                                                                                    distinct(strain.date) %>% 
                                                                                    pull(strain.date) %>% 
                                                                                    order())))),
                                    tickmode = "array"),
                       yaxis = list(title = "Number of new strains identified"),
                       updatemenus = list( list(
                           active = -1,
                           x= -0.1,
                           type = 'buttons',
                           buttons = list(
                               list(
                                   label = "By day",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 86400000.0)))),
                               list(
                                   label = "By week",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = 604800000.0)))),
                               list(
                                   label = "By month",
                                   method = "restyle",
                                   args = list(list(xbins = list(size = "M1"))))
                           )))) %>%
                highlight(color = "#1F78C8")
            
            # facet by country
        } else if (input$region == 2) {
            
            # calc maximum count and use to set yaxis max
            counts <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(country, strain.date) %>% 
                tally() 
            # cumsum plot 
            
            strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           histfunc = "count",
                                           x = ~strain.date,
                                           color = I("#898a8c"),                
                                           xbins = list(size = 86400000.0),
                                           showlegend = F,
                                           hovertemplate = ~paste('Count: %{y}', '<br>',
                                                                  'Date range: %{x}', 
                                                                  '<extra></extra>', 
                                                                  sep=" ")) %>%
                           layout(xaxis = list(title = "", 
                                               range = c(min(counts$strain.date), 
                                                         max(counts$strain.date))),
                                  yaxis = list(title = "", range = c(0, max(counts$n)*1.1)),
                                  annotations = list(list(text= ~unique(country),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>% 
                subplot(nrows = 3, shareX = T, shareY = T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
            
            # facet by province
        } else if (input$region == 3) {
            
            # calc maximum count and use to set yaxis max
            counts <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, strain.date) %>% 
                tally() 
            
            # cumsum plot 
            strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           histfunc = "count",
                                           x = ~strain.date,
                                           color = I("#898a8c"),                
                                           xbins = list(size = 86400000.0),
                                           showlegend = F,
                                           hovertemplate = ~paste('Count: %{y}', '<br>',
                                                                  'Date range: %{x}', 
                                                                  '<extra></extra>', 
                                                                  sep=" ")) %>%
                           layout(xaxis = list(title = "", 
                                               range = c(min(counts$strain.date), 
                                                         max(counts$strain.date))),
                                  yaxis = list(title = "", 
                                               range = c(0, max(counts$n)*1.1)),
                                  annotations = list(list(text= ~unique(province),
                                                          xref = "paper",
                                                          yref = "paper",
                                                          yanchor = "center",
                                                          xanchor = "center",
                                                          align = "center",
                                                          x = 0.5,
                                                          y = 1.1,
                                                          showarrow = FALSE)))) %>% 
                subplot(nrows = 3, shareX = T, shareY = T, margin = 0.025) %>%
                layout(annotations = list(list(text= "Time point",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = 0.5,
                                               y = -0.22,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               x = -0.1,
                                               y = 0.5,
                                               textangle = -90,
                                               font = list(size = 14),
                                               showarrow = FALSE)))
        }
    })
    
    
    output$singlevsmulti <- renderPlotly({
        
        # no faceting 
        if (input$region == 1) {
            
            # multi strain count
            multiplot <- plot_ly(type = "histogram") %>%
                add_trace(data = strains_sh$data(withSelection = T) %>%
                              filter(selected_ | is.na(selected_)) %>%
                              subset(single.mult == "Multi strain clusters"),
                          histfunc = "count",
                          x = ~strain.date,
                          color = I("#898a8c"),    
                          showlegend = F, 
                          hovertemplate = ~paste('Count:', '%{y}', '<br>',
                                                 'Date:', '%{x}', '<br>', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          xbins = list(size = 86400000.0)) %>%
                layout(xaxis = list(title = "Time point",
                                    tickvals = as.list( strains_sh$data() %>%
                                                            group_by(tp1) %>%
                                                            filter(strain.date == min(strain.date)) %>%
                                                            distinct(strain.date) %>%
                                                            pull(strain.date) %>%
                                                            sort()),
                                    ticktext = as.list( as.character(seq(length(strains_sh$data() %>% 
                                                                                    group_by(tp1) %>% 
                                                                                    filter(strain.date == min(strain.date)) %>% 
                                                                                    distinct(strain.date) %>% 
                                                                                    pull(strain.date) %>% 
                                                                                    order())))),
                                    tickmode = "array"),
                       annotations = list(list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               textangle = -90,
                                               x = -0.05,
                                               y = 0.3,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "New strains identified as part of a multi-strain cluster",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "left",
                                               align = "left",
                                               font = list(size = 14),
                                               x = 0,
                                               y = 1,
                                               showarrow = FALSE)))
            
            # single strain counts
            singleplot <- plot_ly(type = "histogram") %>%
                add_trace(data = strains_sh$data(withSelection = T) %>%
                              filter(selected_ | is.na(selected_)) %>%
                              subset(single.mult == "Single strain clusters"),
                          histfunc = "count",
                          x = ~strain.date,
                          color = I("#898a8c"),     
                          showlegend = F,
                          showlegend = F, 
                          hovertemplate = ~paste('Count:', '%{y}', '<br>',
                                                 'Date:', '%{x}', '<br>', 
                                                 '<extra></extra>',
                                                 sep = " "),
                          xbins = list(size = 86400000.0)) %>%
                layout(xaxis = list(title = "Date"),
                       yaxis = list(range = c(0, max(strains_sh$data(withSelection = T) %>%
                                                         filter(selected_ | is.na(selected_)) %>%
                                                         subset(single.mult == "Multi strain clusters") %>%
                                                         group_by(strain.date) %>%
                                                         count() %>%
                                                         pull(n)))),
                       annotations = list(list(text= "Count",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "center",
                                               align = "center",
                                               textangle = -90,
                                               x = -0.05,
                                               y = 0.5,
                                               font = list(size = 14),
                                               showarrow = FALSE),
                                          list(text= "New strains identified as part of single-strain cluster",
                                               xref = "paper",
                                               yref = "paper",
                                               yanchor = "center",
                                               xanchor = "left",
                                               align = "left",
                                               font = list(size = 14),
                                               x = 0,
                                               y = 1,
                                               showarrow = FALSE)))
            
            subplot(singleplot, multiplot, nrows = 2, shareX = T, margin = 0.075)
            
            # facet by country
        } else if (input$region == 2) {
            
            multi <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                group_by(country, single.mult, strain.date) %>%
                tally()
            
            ifelse(length(unique(multi$single.mult))>1, 
                   (colors = c("#898a8c", "#373738")), 
                   (colors = c("#898a8c")))
            
            strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                split(.$country) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           histfunc = "count",
                                           x = ~strain.date,
                                           color = ~single.mult, 
                                           colors = colors, 
                                           xbins = list(size = 86400000.0),
                                           legendgroup = ~single.mult, 
                                           showlegend = ifelse(which(names(strains_sh$data(withSelection = T) %>%
                                                                               filter(selected_ | is.na(selected_)) %>%
                                                                               split(.$country)) == unique(d$country))==1, TRUE, FALSE), 
                                           hovertemplate = ~paste('Count: %{y}', '<br>',
                                                                  'Date range: %{x}', 
                                                                  '<extra></extra>', 
                                                                  sep=" ")) %>%
                           layout( barmode = 'overlay',
                                   xaxis = list(title = "",
                                                tickvals = as.list( strains.long %>%
                                                                        group_by(tp1) %>%
                                                                        filter(strain.date == min(strain.date)) %>%
                                                                        distinct(strain.date) %>%
                                                                        pull(strain.date) %>%
                                                                        sort()),
                                                ticktext = as.list( as.character(seq(length(strains.long %>% 
                                                                                                group_by(tp1) %>% 
                                                                                                filter(strain.date == min(strain.date)) %>% 
                                                                                                distinct(strain.date) %>% 
                                                                                                pull(strain.date) %>% 
                                                                                                order())))),
                                                tickmode = "array"),
                                   yaxis = list(range = c(0, max(multi$n))),
                                   annotations = list(list(text= ~unique(country),
                                                           xref = "paper",
                                                           yref = "paper",
                                                           yanchor = "center",
                                                           xanchor = "center",
                                                           align = "center",
                                                           x = 0.5,
                                                           y = 1.1,
                                                           showarrow = FALSE)))) %>% 
                subplot(nrows = 3, shareX = T, shareY = T)
            
            # single <- strains_sh$data(withSelection = T) %>%
            #     filter(selected_ | is.na(selected_)) %>%
            #     subset(single.mult == "Single strain clusters") %>%
            #     group_by(country, strain.date) %>%
            #     tally()
            # 
            # # single strain 
            # singleplot <- strains_sh$data() %>%
            #     subset(single.mult == "Single strain clusters") %>%
            
            # strains.long %>% 
            #     split(.$country) %>%
            #     lapply(function(d) plot_ly(d,
            #                                type = "histogram",
            #                                histfunc = "count",
            #                                x = ~strain.date,
            #                                color = I("#898a8c"),
            #                                xbins = list(size = 86400000.0),
            #                                showlegend = F,
            #                                hovertemplate = ~paste('Count: %{y}', '<br>',
            #                                                       'Date range: %{x}',
            #                                                       '<extra></extra>',
            #                                                       sep=" ")) %>%
            #                layout(xaxis = list(range = c(min(strains.long$strain.date),
            #                                              max(strains.long$strain.date))),
            #                       yaxis = list(range = c(0, max(multi$n))),
            #                       annotations = list(list(text= ~unique(country),
            #                                               xref = "paper",
            #                                               yref = "paper",
            #                                               yanchor = "center",
            #                                               xanchor = "center",
            #                                               align = "center",
            #                                               x = 0.5,
            #                                               y = 1.1,
            #                                               showarrow = FALSE)))) %>%
            #     subplot(nrows = 3, shareX = T, shareY = T)
            # 
            # subplot(multiplot, singleplot, margin = 0.05)
            
            # facet by province 
        } else if (input$region == 3) {
            
            multi <- strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                group_by(province, single.mult, strain.date) %>%
                tally()
            
            ifelse(length(unique(multi$single.mult))>1, 
                   (colors = c("#898a8c", "#373738")), 
                   (colors = c("#898a8c")))
            
            strains_sh$data(withSelection = T) %>%
                filter(selected_ | is.na(selected_)) %>%
                subset(country %in% input$regionProvince) %>%
                split(.$province) %>%
                lapply(function(d) plot_ly(d, 
                                           type = "histogram",
                                           histfunc = "count",
                                           x = ~strain.date,
                                           color = ~single.mult, 
                                           colors = colors, 
                                           xbins = list(size = 86400000.0),
                                           legendgroup = ~single.mult, 
                                           showlegend = ifelse(which(names(strains_sh$data(withSelection = T) %>%
                                                                               filter(selected_ | is.na(selected_)) %>%
                                                                               subset(country %in% input$regionProvince) %>%
                                                                               split(.$province)) == unique(d$province))==1, TRUE, FALSE), 
                                           hovertemplate = ~paste('Count: %{y}', '<br>',
                                                                  'Date range: %{x}', 
                                                                  '<extra></extra>', 
                                                                  sep=" ")) %>%
                           layout( barmode = 'overlay',
                                   xaxis = list(title = "",
                                                tickvals = as.list( strains.long %>%
                                                                        group_by(tp1) %>%
                                                                        filter(strain.date == min(strain.date)) %>%
                                                                        distinct(strain.date) %>%
                                                                        pull(strain.date) %>%
                                                                        sort()),
                                                ticktext = as.list( as.character(seq(length(strains.long %>% 
                                                                                                group_by(tp1) %>% 
                                                                                                filter(strain.date == min(strain.date)) %>% 
                                                                                                distinct(strain.date) %>% 
                                                                                                pull(strain.date) %>% 
                                                                                                order())))),
                                                tickmode = "array"),
                                   yaxis = list(range = c(0, max(multi$n))),
                                   annotations = list(list(text= ~unique(province),
                                                           xref = "paper",
                                                           yref = "paper",
                                                           yanchor = "center",
                                                           xanchor = "center",
                                                           align = "center",
                                                           x = 0.5,
                                                           y = 1.1,
                                                           showarrow = FALSE)))) %>% 
                subplot(nrows = 3, shareX = T, shareY = T)
        }
    })
    
    # mapbox token 
    mapboxToken <- "pk.eyJ1Ijoic2FtLWEtbGVlIiwiYSI6ImNrb2s0bXVpbzFhMGkybm5zeHI1dHR1aTgifQ.1K8o7OaSOWo_y5UdVH348w"    # You need your own token
    Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
    
    output$cluster_map <- renderPlotly({
        plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(data = clusters_sh,
                        name = "Clusters",
                        x = ~average.longitude,
                        y = ~average.latitude,
                        frame = ~timepoint,
                        color= I("#646566"),
                        size = ~log10(cluster.size.1.2)*10,
                        opacity = 0.6,
                        hovertemplate = ~paste(tp1.cluster, '<br>',
                                               'Avg distance between strains (km):', avg_geo_dist,
                                               '<extra></extra>',
                                               sep = " ")) %>%
            config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN")) %>%
            layout(mapbox = list(zoom = 1,
                                 style = 'light',
                                 center = list(lon = 50, lat = 40))) %>%
            highlight(color = I("#1F78C8")) %>%            
            animation_opts(1000, redraw = T, transition = 100) %>% 
            animation_button(visible = T) %>%
            animation_slider(active = 0)
    })
    
}






# P <- plot_ly(data = clusters_long)
# for(k in 1:nrow(clusters_long)) {
#     dfk <- clusters_long[k,c("delta.ecc.0.1.0", "delta.ecc.0.0.1", "tp1.cluster", "timepoint")]
#     P <- add_trace(P,
#                    y=c(0,~delta.ecc.0.0.1),
#                    x=c(0,~delta.ecc.0.1.0),
#                    frame =~timepoint,
#                    data=dfk,
#                    hovertemplate = ~tp1.cluster,
#                    type="scatter",
#                    mode="lines+markers",
#                    showlegend = F,
#                    color = ~I("#898a8c"))
# }
# 
# P <- P %>%  
#     layout(xaxis = list(range = c(-1,1)),
#            yaxis = list(range = c(-1,1))) %>%
#     highlight(color = "#1F78C8")
# P


# 
# plot_ly() %>%
#     add_trace(data = clusters_long, 
#               type = "scatter",
#               mode = "markers", 
#               x = ~ecc.0.1.0,
#               frame = ~timepoint, 
#               size = ~I(cluster.size.1.2),
#               color = ~I("#898a8c"), 
#               hovertemplate = ~tp1.cluster,
#               showlegend = FALSE,
#               marker = list(sizemode = "area", opacity = 0.5)) %>%
#     layout(xaxis = list(title = "Geospatial epicluster cohesion index",
#                         range = c(0, 1.05)),
#            yaxis = list(showticklabels = F),
#            annotations = list(text= "Geospatial epicluster cohesion index",
#                               xref = "paper",
#                               yref = "paper",
#                               yanchor = "bottom",
#                               xanchor = "center",
#                               align = "center",
#                               x = 0.5,
#                               y = -0.3,
#                               #font = list(size = 14),
#                               showarrow = FALSE)) %>%
#     highlight(color = "#1F78C8")
# 
# 
# 
# strains.long %>%
#     group_by(country, tp1.cluster, timepoint) %>%
#     summarize(n = n(),
#               index = mean(ecc.0.0.1)) %>%
#     group_by(country) %>%
# do(p=plot_ly(.) %>%
#        add_trace(x = ~index,
#                  type = "scatter",
#                  mode = "markers",
#                  frame = ~timepoint,
#                  size = ~I(n),
#                  color = I("#1F78C8"),
#                  showlegend = FALSE) %>%
#        layout(xaxis = list(range = c(0,1), title = ""),
#               yaxis = list(showticklabels = F, title = ""),
#               annotations = list(text= ~unique(country),
#                                  xref = "paper",
#                                  yref = "paper",
#                                  yanchor = "bottom",
#                                  xanchor = "center",
#                                  align = "center",
#                                  x = 0.5,
#                                  y = 1,
#                                  showarrow = FALSE))) %>%
# subplot(nrows = 3, shareX = TRUE, shareY = TRUE) %>%
# layout( annotations = list(list(text= "Temporal epicluster cohesion index",
#                                 xref = "paper",
#                                 yref = "paper",
#                                 yanchor = "bottom",
#                                 xanchor = "center",
#                                 align = "center",
#                                 x = 0.5,
#                                 y = -0.1,
#                                 showarrow = FALSE)))
