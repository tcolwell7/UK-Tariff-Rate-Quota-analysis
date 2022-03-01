## trqCharts
#'
#' Script to output charts from trqChartFunctions. 
#'
#'
#'

# Set up:
rm(list = ls())

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

# load in functions and data:
source("trqChartFunctions.R")

# TRQ region plot -----------------------------------------------------------------
##  1. plot intro ---------------------------

#' TRQ plot creates a grouped bar chart
#' For the user selected region (i.e Africa, Eastern Europe etc)
#' 
#' The x axis is broken down into selected country or grouping (i.e. EEA, Central America etc.)
#' This is plotted against each country / grouping fill rate 
#' The x axis is grouped for UK (2021) vs EU (2020)
#'
#' Where a cross is denoted this indicates a UK quota exists but has a 0% fill rate. 
#' 
#' The default plot creates a chart for the regions individual groupings where data exists. 
#' Users can filter for their select country/grouping with individual country selections. 

### 1.1 function inputs ----------------------

#'  Function inputs: (case sensitive)
#'  
#' [.region] = region selection: 
#' selections: [Africa, Asia Pacific Eastern Europe, The Americas, Wider Europe & the middle east]
#' 
#' [quotaUnit] = TRQ quota unit of measurement to plot:
#' selections: [Kilograms, Litres, Square metre (m2), Number of items, Litre pure (100%) alcohol, Number of pairs]
#'  
#' [country] = Country plot flag. This must be flagged if users want to select individual countries
#' 
#' Note: [Any character vector will initiate a custom country plot.]
#'  
#'  
#' [countrySelection] = individual country selection input:
#' 
#' Note:
#' [Input must be a valid country name]
#' [If inputting multiple country - must be within a character vector c()]
#' [i.e. countrySelection = c("South Africa", "Tunisia)]
#' 
#' [grouping] = Grouping plot flag. This must be flagged if users want to select cusotm groupings for chart. 
#' 
#' Note:
#' [Input must be a valid grouping name]
#' [Groupings can be indvidual country names i.e. Tunisia, Egypt etc.]
#' [But this filter captures EEA / Central America / Andean countries etc.]
#'  
#' [groupSelection] = individual country/grouping selection
#'  
#' [Input must be valid country and or grouping name]
#' [If inputting multiple groupings - must be within a character vector c()]
#' [i.e. countrySelection = c("Mexico", "Andean Countries, "Central America)]
#' 
#' 
#'  


#### 1.2 examples ---------------------------

# Africa region plot with country selection. Plus specified country plot/

trqPlot(.region = "Africa",
        country= "Y")

trqPlot(.region = "Africa",
        country= "Y",
        countrySelection = c("Morocco","Tunisia"))
#
# South Africa TRQ plot for Litres (wine quotas)

trqPlot(.region = "Africa",
        quotaUnit = "Litres", 
        country= "Y",
        countrySelection = "South Africa")
#
#' Americas plot with default grouping option. 
#' The Americas covers Andean countries and Central Americas
#' By grouping this reduces the overlap between countries on the x-axis
#' And makes the plot smaller 
trqPlot(.region = "The Americas")
trqPlot(.region = "The Americas", country = "Y")


# TRQ filled quota plot ----------------------------------------
## i.  plot intro -----------------------

#' UK quota data provides dates when the last quota allocations occurred. 
#' If a quota is fully filled (i.e. no more volume is available)
#' The last allocations date can then determine
#' How many days it took until the quota filled. 
#' This duration can be used as a proxy of how "in demand" a quota is. 
#' If a quota is filled in quickly it indicates it is in demand
#' If a quota is filled within the last few days of a quota period
#' This indicates less demand then if the quota filled up in a few days. 
#'
#' The plot creates a stacked bar plot for the selected regions countries 
#' Where filled quotas exist. 
#' 
#' The average quota length in days and 
#' Number of days until the quotas are filled 
#' For each country is calculated
#'
#' This is then stacked into one bar and plotted against the number of days on the x axis. 
#'
#' The smaller the red bar - the quicker the quotas has been filled. 
#' The smaller the proportion of red bar compared to the total bar plot
#' Comparisons can be made between countries and how quickly they fill their quotas. 
#' 
#' 

### ii. function inputs ------------------------------

#' Function inputs (case sensitive).
#' 
#' [.region] = User selected region input
#' selections: [Africa, Asia Pacific Eastern Europe, The Americas, Wider Europe & the middle east]
#'
#' [.trqDsc] = TRQ sector / description flag. 
#' 
#' Any character vector can be present to initiate a sector plot. 
#' If flagged the sector plot will chart
#' all countries within the selected region
#' for all available sectors and filled quotas
#' 
#' 
#' [.all_countries] = flag to plot all countries together 
#' 
#' Any character vector can be present to plot all countries together 
#' Note:
#' 
#' .all_countries can't be plotted if the .region is selected too. 
#' 
#' 

#### iii. examples -----------------------------------


trqFilledPlot("Africa", .trqDsc = "Y") # Africa plot broken down by sectors

trqFilledPlot("Eastern Europe", .trqDsc = "Y") # Eastern Europe plot broken down by sectors

trqFilledPlot("Africa") # Africa plot - country level only. 

trqFilledPlot("Africa",.all_countries = "Y") # function breaks. Needs one input or the other, not both. 


# TRQ Sector plot ----------------------------------------

## i. plot intro ------------------------
#'
#' This plot plots a stacked bar chart broken down by
#' The regions sector breakdown
#' i.e. All the TRQ sectors (Fruits, Dairy, Textiles etc.) which are available to
#' Any country within the selected region. 
#'
#' The stacked bar chart highlights:
#' Total allocation volume 
#' Total usage 
#' Total allocation fill rate (Total usage / Total allocation volume)
#' (Total allocation fill rate = "utilization rate" or "fill rate")
#'
#' The fill rate is plotted within each bar as a percentage text value
#'
#' The plot is broken down by TRQ sector categorization. 
#' i.e. What allocation and usage and fill rate is applicable
#'      against each TRQ sector available per region. 
#'      
#' Users select their region and can select
#' or remove specific sectors they wish to plot
#'      
#'

### ii. function inputs ---------------------

#'  Function inputs (case sensitive).
#'
#' [.region] = region selection: 
#' selections: [Africa, Asia Pacific Eastern Europe, The Americas, Wider Europe & the middle east]
#' 
#' [quotaUnit] = TRQ quota unit of measurement to plot:
#' selections: [Kilograms, Litres, Square metre (m2), Number of items, Litre pure (100%) alcohol, Number of pairs]
#'  
#' [sectorSelect] = user input for manual sector selection
#' # Note: see source data for full sector listing name
#' 
#' [sectorRemove] = user input for manual removal of sector from plot 
#' 
#' Note for both sector inputs:
#' # If any sector is misspelled the function will run
#' # A warning message will be left highlighting which sector has been mispelled. 
#'
#' 


#### iii. plot examples -----------------

#' Africa region sector TRQ breakdown
trqSectorRegionPlot(.region = "Africa") 

#' misspelling of Litres - run to generate error message
trqSectorRegionPlot(.region = "Africa", quotaUnit = "Ltres") 

#' Africa region plot with Fruits and vegetables removed
trqSectorRegionPlot(.region = "Africa", sectorRemove = "Fruits and vegetables") 

#' The Americas regional sector plot
trqSectorRegionPlot(.region = "The Americas")

#' The Americas plot for selected sectors
trqSectorRegionPlot(.region = "The Americas", 
                    sectorSelect = c("Preperations. food", 
                                     "Sugars", 
                                     "Fish")
                    )

#' Example of when both sector inputs are run. 
#' The default region plot is returned with a warning message in console. 
trqSectorRegionPlot(.region = "Africa", sectorSelect = c("Fruits and vegetables", "Sugars"), sectorRemove = "Cerals")



# TRQ Sector country plot ----------------------------------

## i. plot intro ------------------------------------------

#' The country sector plot is a facet wrap of the region sector plot
#' The stacked bar chart format remains
#' The facet uses the TRQ sector to split out the country plot. 
#' This is designed so the plot is more readable and aesthetic. 
#' 
#' The purpose of this chart is to enable a further dive into the data
#' Breaking down the sector data by specific countries within the region
#' As some countries have a a large impact on sector utilisaiton rates
#' While some countries and agreements predominantly focus on specific sectors
#' For example North African countries have important
#' Fruit and vegetable quotas which are highly in demand
#'
#' Users are recommended to select their specific sectors before running the function
#' The default is set up to produce a facet wrap of all sectors and countries
#' This can be un-readable
#' Until an edit has been updated to ensure sectors are selected
#' Please select sectors for a more usable plot
#'
#'
#'

### ii. function inputs ----------------------------------

#' Function inputs (case sensitive):
#' 
#' [chartType] = define chart type whether to plot country or grouping (i.e. EEA/Central America)
#' This input selects and filters the data before create the visual
#' 
#' [.region] = region selection: 
#' selections: [Africa, Asia Pacific Eastern Europe, The Americas, Wider Europe & the middle east]
#' 
#' [quotaUnit] = TRQ quota unit of measurement to plot:
#' selections: [Kilograms, Litres, Square metre (m2), Number of items, Litre pure (100%) alcohol, Number of pairs]
#'  
#' [countrySelect] = individual country selection input
#' 
#' [countryRemove] = individual country input to remove from plot.
#' This input is encouraged to be used when removing one country form a plot
#' which is more efficient than type all countries out individually
#' 
#' Note:
#' [Input must be a valid country name]
#' [If inputting multiple country - must be within a character vector c()]
#' [i.e. countrySelection = c("South Africa", "Tunisia)]
#' 
#' [groupSelect] = individual grouping selection input (i.e. EEA/Central America)
#' # Individual countries will be plot too. For example Mexico with Central America. 
#' 
#' [groupRemove] = individual grouping input to remove from plot
#' This input is encouraged to be used when removing one country form a plot
#' which is more efficient than type all countries out individually
#'   
#' [sectorSelect] = user input for manual sector selection
#' # Note: see source data for full sector listing name
#' 
#' [sectorRemove] = user input for manual removal of sector from plot 
#' 
#' Note for both sector inputs:
#' # If any sector is misspelled the function will run
#' # A warning message will be left highlighting which sector has been misspelled.
#'
#' 


#### iii. examples -------------------

#' Africa user selected sector and country facet plot

trqSectorCountryPlot(chartType = "country",
                     .region = "Africa",
                     countrySelect = c("Morocco","Egypt"),
                     sectorSelect = c("Fruits and vegetables","Cereals"))

#' Africa user selected sector and grouping facet plot
trqSectorCountryPlot(chartType = "grouping",
                    .region = "Africa",
                    groupSelect = c("Morocco","Egypt", "South Africa"),
                    sectorSelect = c("Fruits and vegetables","Cereals")) 


#' The Americas - default error if no sector selection. 
#' This is as there are too many sectors to neatly plot. 
trqSectorCountryPlot(chartType = "grouping",
                     .region = "The Americas")
 
#' Eastern Europe country plot for specific sectors measured in Litres. 
#' Note Fruits and vegetables ans Cereals not plotted as they aren't measured in Litres. 
trqSectorCountryPlot(chartType = "country",
                     .region = "Eastern Europe",
                     sectorSelect = c("Fruits and vegetables","Cereals","Beverages & spirits"),
                     quotaUnit = "Litres")

                  
#' Americas grouping facet plot for selected TRQ sectors. 
#' Textiles is not valid name so warning message is generated
#' And plot still is produced
#' 
trqSectorCountryPlot(chartType = "grouping",
                     .region = "The Americas",
                     sectorSelect = c("Sugars", "Dairy, eggs, honey", "Textiles"),
                     groupSelect = c("Mexico", "Andean countries", "Central America"))


