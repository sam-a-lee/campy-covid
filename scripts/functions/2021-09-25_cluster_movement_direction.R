# Title: Cluster movement function
# Author: Sam and Guangzhi
# Date: 2021-09-25

# Purpose: this script calculates the cardinal angle and cluster movement
# descriptor between clusters

# read in table describing cardinal direction and degrees for a 16-rose compass 
page <- read_html('http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm')
directions.raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

# clean up table 
directions <- directions.raw %>% 
  set_names(~tolower(sub(' Direction', '', .x))) %>% 
  slice(-1) %>% 
  separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

# create a new column in direction and assign a cluster transmission "speed" 
directions$ecc.speed <- c("Slower", "Slower", "Slower", "Slower",
                          "No change", "Faster", "Faster", "Faster",
                          "Faster", "Faster", "Faster", "Faster", 
                          "No change", "Slower", "Slower", "Slower")

# create a new column in direction and assign a cluster transmission "spread"
directions$ecc.spread <- c("No change", "Isolated", "Isolated", "Isolated",
                           "Isolated", "Isolated", "Isolated", "Isolated",
                           "No change", "Dispersed", "Dispersed", "Dispersed", 
                           "Dispersed", "Dispersed", "Dispersed", "Dispersed")

# create a new column in table that creates a combined description of cluster transmission speed and spread
directions$ecc.comb <- c("Slower", "Slower/Slower, isolated", "Slower, Isolated", "Isolated/Slower, isolated",
                         "Isolated", "Isolated/Faster, isolated", "Faster, Isolated", "Faster/Faster, isolated",
                         "Faster", "Faster/Faster, dispersed", "Faster, dispersed", "Dispersed/Faster, dispersed", 
                         "Dispersed", "Dispersed/Slower, dispersed", "Slower, dispersed", "Slower/Slower, dispersed")

# create a new column for the degree mid point between two cardinal directions 
directions$degree.mid<- c(0,22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180, 202.5,225, 247.5, 270, 292.5, 315, 337.5)

