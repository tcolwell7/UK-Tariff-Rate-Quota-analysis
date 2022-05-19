# UK-Tariff-Rate-Quota-analysis

### Comparative analysis and visualisations of UK Tariff Rate Quotas following the first full year of EU-exit using R. 

### Description

Initial analysis following publication of two public UK TRQ data sources compared against public EU quota data comparing TRQ fill rates to the year leading up to EU-exit. 

### Objectives

Produce Tariff Rate Quota data and utilisation rates across UK partner country groupings. The data is to broken down into groupings, defined by stakeholder interest depending on the geographical location of countries which the UK has Free TRade Agreements with. Any output is presented across each region. A secondary purpose of this work is to automate repetitive processes to improve efficiency working across a multi-disciplinary team. 

Multiple reports are to be generated using the data produced. Effort has been made to provide the means to multiple people to work on the data and prdocue outputs in a RAP way. 

User centred functions have been created for the purpose of analytical outputs to be presented to non-analysts using MS applications. They are designed so users can tailer their charts depnding on what story the data is highlgihting. R users with little to intermeidate expereince are able to help produce visual outputs. 

### Outputs 

There are four main output excel files covering different TRQ metrics. These are the outputs created from the main trq_data compilation script. The Web-scrpae output can be found in the web-scrape folder. Outputs and chartFunctions were shared across multiple analytical teams to be compiled and analysed in reports. These reports were to be shared across the orgnisation. 

### Analysis is split into:

1. Data cleaning
2. Data aggregations and outputs
3. TRQ visualisaiton functions
4. TRQ test script for user functions
5. Shiny App to disseminate compiled data
6. tbc. Markdown 


**Scripts:**

1. **trq_data10.R** - TRQ data compilation script. Data cleaning, wrangling and aggregations to create data outputs to be used for further dissmeniation and analysis. 
2. **trq_chartFunctions.R** - user-centred R visualisation functions. Functions created for users to create charts in R with little to no R knowledge. Automatic testing built within functions. 
3. **trqCharts.R** - example script for users. Function descriptions and exmaples provided for users utilising functions. 
4. **uk_trqs_web_scrape.R** - webscrape of public UK license quota data. Output is used in trq data compilation. 
5. **app.R** - shiny App devleoped to disseminate the UK TRQ data across multiple metrics. 
6. **uk_trq_qa.ipynb** juypter notebook for quality assurance of the trq_data compilation. Alternative method to compile data as a check against the R script. 


