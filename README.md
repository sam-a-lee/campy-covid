# EpiQuant CoVis 


### About the app

This repo contains script for an R Shiny app designed to visualize output from a [modified version of EpiQuant](https://github.com/vjayaman/Metrics-CGM-ECC) applied to publicly available SARS-CoV-2 data obtained from [GISAID](https://www.gisaid.org/). 


### Usage
 
To use the app, download the most recent version of the R shiny app from the scripts folder (as indicated by date). This will include a server.R file and a ui.R file. These should be located in same folder after download, and there should be no other server.R or ui.R files in that folder.

Example output data generated by the most recent version of EpiQuant is located in the "input_data" folder. This data can be used for exploration of the app. 

To run the app, open the ui.R or server.R file in RStudio, then click the "Run App" button.

<<<<<<< HEAD
The app should open on a page prompting the user to input a file. Generally, users will only be working with .csv files as this is the format that is output from EpiQuant. However, the app current accepts a variety of formats (.csv, .tsv, .txt, and .xlsx) for testing purposes. The app assumes the inputted file has a header and is comma separated, however, there are options to specify other formats. The user can go back and load at a new file at any time without restarting the app. 
=======
The app should open on a page prompting the user to input a file. Generally, users will only be working with .csv files as this is the format that is output from EpiQuant. However, the app current accepts a variety of formats (.csv, .tsv, .txt, and .xlsx) for testing purposes. The app assumes the inputted file has a header and is comma separated, however, there are options to specify other formats. 
>>>>>>> e35ed6402efe5c768a6a1879de8b9398ef4b0dbc

The app expects several columns to be present in the data and will throw a "Column Error" if these columns are missing or if the wrong file delimiter was chosen. Expected columns are based on the most recent version of EpiQuant.

Once data are loaded, the user may begin exploring the data using the sidebar menu to visualize pre-defined subsets, or using filters to create their own subsets of data. 

The app is designed to highlight epicluster cohesion metrics developed by [Vasena](https://github.com/vjayaman) and the Taboada lab. These metrics are displayed first in each visualization panel. Related visualizations can be viewed by selecting different tabs in each visualization panel. Each panel also includes a tab to help the user interpret what the visualization is displaying. 

The app also includes tables for cluster-related data, strain-related data, as well as the raw data inputted by the user and a filtered version of this table based on data subsets and filtered. 

<<<<<<< HEAD

### Brushing 

Users can click on visualizations or table rows to highlight all information related to a given cluster, strain, etc. Multiple selections can be made using the appropriate key for your operating system (e.g. hold command and click for Mac). Deselection is accomplished by double clicking. *Brushing works best when data are not faceted by region and is currently under development.* 

=======
### Brushing 


### Expected columns
>>>>>>> e35ed6402efe5c768a6a1879de8b9398ef4b0dbc
