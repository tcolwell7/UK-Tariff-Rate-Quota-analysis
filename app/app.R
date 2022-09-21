## shiny app for UK TRQ data
#'
#'
#' TO Do:
#' Update notes model box for dashboard guidance
#' update TRQ tab button text
#' update country comparison button text
#' Add EU data sources ro data sources button
#' 
#' 


library(tidyverse)
library(janitor)
library(stringr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(kableExtra)

source("serverFunctions.R", local = TRUE)
source("data_source_summary_table.R", local = TRUE)


# Define header ---------------------------------

header <-
  dashboardHeader(
    title="UK Tariff Rate Quotas",
    titleWidth = 350,
    
    tags$li(actionButton('dashboardGuidance', 'Dashboard Guidance'),
            class= 'dropdown',
            style = "padding-right:2px;
                     padding-top:15px; 
                     padding-bottom:15px;
                     overflow: hidden;
                            ",
            title = "Click here to read guidance on this dashboard"),
    tags$li(actionButton('sources', 'Data Sources'),
            class= 'dropdown',
            style = "padding-right:2px;
                     padding-top:15px; 
                     padding-bottom:15px;
                     overflow: hidden;
                            ",
            title = "Click here to read guidance on the sources used in this dashboard")
    
    )

# Define sidebar ----------------------------------------

sidebar <-
  dashboardSidebar(
    width=350,
      sidebarMenu(
         
        menuItem(
          "Global summary", 
          tabName="globalSum", 
          icon=icon("globe")
          ),
        
        menuItem(
          "Tariff Rate Quotas (TRQs)", 
          tabName="trqs", 
          icon=icon("creative-commons-nc")
          ),
        
        menuItem(
          "TRQ data", 
          tabName="trq_data", 
          icon=icon("th")
        ),
        
        br(),
        
        menuItem("Select input",
                 
          radioButtons(
            inputId = "data_selection",
            label="View charts",
            choices = c("by Region","by Trade Agreement", "by Partner Country")
            ),
          
          selectInput(
            inputId = "partner_selection", 
            label = "Select input",
            choices = NULL,
            multiple = FALSE
         )
         
    ), # end menu Items select input
    
    br(),
    
    menuItem("Select TRQ inputs",
             
             div(
               style = "font-size:15px;",
               selectInput(
                 inputId = "sector_list",
                 label = div(style = "font-size:15px ; color: #666","Select sector:"),
                 choices = sectorList,
                 selected = "All"
               )
             ),
             
             div(
               style = "font-size:15px;",
               selectInput(
                 inputId = "quota_unit_list",
                 label = div(style = "font-size:15px ; color: #666","Select Quota unit:"),
                 choices = quotaUnitList,
                 selected = "Kilograms"
               )
             ),
             
           br()
             
    )
    
    
 ) # end sideBarMenu
 
)# dashboardSidebar 


# Define UI ------------------------------------------------


ui <-
  dashboardPage(
    header,
    sidebar,
    dashboardBody(
      
   
# UI: css styles -------------------------------------------
      tags$head( 
        tags$style(
          HTML("
          
        
         /* logo */
         .skin-blue .main-header .logo {
           background-color: #a7031c;
         }
                    
          /* navbar (rest of the header) */          
               .skin-blue .main-header .navbar {
                           background-color: #cf102d;
        }
                           
               
          /* side bar font size */
               .main-sidebar { font-size: 15px; }
               
            
          /* partner selection input formatting */     
               #partner_selection + div>.selectize-dropdown{
                            width: 300px !important;
                            color: #3c8dbc; 
                 font-size: 15px
               }
                                                                                                           }'
          #quota_unit_list + div> .selectize-input, .selectize-input input {
            font-size: 20px;
            color:#3c8dbc}    
          
          
          #quota_unit_list + div>.selectize-dropdown{
                        width: 300px !important;
                        color: #3c8dbc;
                        font-size: 15px
          }
          
            #sector_list + div>.selectize-dropdown{
                        width: 300px !important;
                        color: #3c8dbc;
                        font-size: 15px
          }
                
        /* colour of danger boxes */
        .skin-blue .content-wrapper .box.box-solid.box-danger>.box-header {
                              background-color: #cf102d;
        }                       
                                       
          
        /* slider colour */
        
       .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
       background: #cf102d;
       border-bottom: #999;
       border-top: #999
       }
          
       .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #666 }      
               
               
    "                             
         )) 
      ),


      tabItems(

# UI: tab - global summary -----------------------------------------  
       tabItem(
         tabName = "globalSum",
         
          #  box(
              h3(strong("Welcome to the UK Tariff Rate Quota dashboard.")),
              h4("You will find summarise and interactive UK TRQ data for all countries in which the UK has agreend trade agreements with."),
              h5("Please navigate through each tab and chart utilisaing the help buttons populated throughout the dashboard."),
              h5("Before continuing please refer to the dashbaord guidance and data sources at the top right hand corner."),
           #   width = 12,
            #  status = "danger",
             # solidHeader = TRUE
        # ), 
        
           fluidRow(box(height = 1, width = 12)), # box for aesthetics
                    
           fluidRow(
            
            column(
              width = 3,
                actionButton(
                 'summaryGuidance', 
                 'Global summary guidance',
                 width = '100%',
                 style = "font-size:16px;
                           color: #666",
                title = "Click here to read summary for this tab"
                 )
               )
             ),
            br(),
        
            fluidRow(
           
             box(
               title = "Global summary of quota allocations",
               width = 12,
               status = "danger",
               solidHeader = TRUE,
               checkboxGroupInput(
                 inputId = "mapButton",
                 label=NULL,
                 choices=c("by total quota utilisation:")
               ),
               leafletOutput(outputId = "summary_map")
            )
            
          ), # end fluidRow
         
         fluidRow(
           
           box(
             title = "Country level data",
             width = 12,
             status = "danger",
             solidHeader = TRUE,
             DT::dataTableOutput("country_table")
           )
         )
         
      ), # end globalSum tabName

# UI: tab - trqs -------------------------------------------------------

       tabItem(
         tabName= "trqs",
         
         tabsetPanel(
           type = "tabs",
          tabPanel(
            div(
              style = "font-size:17px;",
              "UK quota summary"
              ), br(),
            
            
            fluidRow(
              column(
                width = 2,

                  actionButton(
                  'tabGuidance', 
                  'TRQs tab guidance',
                  width = '100%',
                  style = "font-size:16px;
                           color: #666",
                  title = "Click here to read summary for this tab"
                  ),
                br(),
                downloadButton(
                  "download_ukTRQguidance", 
                  "Download tab guidance",
                  class="butt",
                  icon=icon("feed"),
                  tags$head(
                    tags$style(".butt{color: black ;} .butt{font-style: italic;} .butt{font-size: 18px;}" 
                               )
                    )
                  )
                )
            ),
            
            br(),
      
            fluidRow(
              box(
                title = "UK (2021) vs EU (2020) fill rate plot",
                width = 6,
                status = "danger",
                solidHeader = TRUE,
                plotlyOutput("trq_plot")
              ),
              
              box(
                title = "UK quota allocation and usage plot",
                width = 6,
                status = "danger",
                solidHeader = TRUE,
                plotlyOutput("trq_plot2")
              )
           ),
          
           fluidRow(
             box(
               checkboxGroupInput(
                 inputId = "sectorButton",
                 label=NULL,
                 choices=c("EU/UK utilisation rate comparison")
                 ),
               title = textOutput("trq_sector_plot_title"),
               width = 12,
               status = "danger",
               solidHeader = TRUE,
               plotlyOutput("trq_sector_plot")
            )
          )

         
       
         ), # end UK quota summary tabPanel

### tabSet: country comparison --------------------------

          tabPanel(
            div(
              style = "font-size:17px;",
              "Country comparison"
              ),
            
            br(),
            
            fluidRow(
              column(
                width = 3,
                
                actionButton(
                  'ccGuidance', 
                  'Country Comparison tab guidance',
                  width = '100%',
                  style = "font-size:16px;
                           color: #666",
                  title = "Click here to read summary for this tab"
                )
              )
            ), br(),
            
            fluidRow(
              box(
                width = 12,
                status = "danger",
                solidHeader = TRUE,
                title = "Top partner country UK quota utilisaiton rates (2021)",

                  sliderInput(
                    inputId = "quota_min_volume",
                    label = 
                      div(
                       style = "font-size:16px; ",
                       "Select minimum quota allocation (units):"
                      ),
                    value=c(0,100000),
                    min = 0,
                    max = 100000,
                    width = "380px",
                    step = 10000
                  ),
                
                plotlyOutput("trq_country_plot")
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "danger",
                solidHeader = TRUE,
                title = "Country quota utilisation rates change year on year",
                plotlyOutput("trq_top_country_plot")
             )
           )
         ) # end country comparison tab
       
        ) # end tabsetPanel
       
      ), # end trqs

# UI: tab - trq data ---------------------------------------------------

     tabItem(
       tabName = "trq_data",
       
       fluidRow(
       column(
         width = 3,
         actionButton(
          'trq_data_button', 
          'TRQ data tab guidance',
          width = '100%',
          style = "font-size:16px",
          title = "Click here to read summary for this tab"
         )
        )
       ), 
       
       br(),
       
       fluidRow(
       
          box(
            title = textOutput("trq_table_text"),
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            br(),
            DT::dataTableOutput("trq_table")
          ),
          
          br(),
          
          
            box(
              width = 12,
              status="danger",
              downloadButton(
                "download_trqs",
                "Download all UK TRQ data",
                class="butt",
                tags$head(
                  tags$style(
                    ".butt{color: black ;} .butt{font-style: italic;} .butt{font-size: 18px;}" )
                  )
                )
            )
          ) # end fluidRow
          
          
          
       ) # end tabItem trqData
      
   ) # end tabItems
   
 ) # end dashboardBody

) # end dashboardPage



server <- function(input, output, session) {

# general: button selection ----------------------------------------
  #"by Region","by Trade Agreement", "by Partner Country")
  observe({
    # this updates the drop down menu for partner based on which radio button is selected
    # x is the value of the radio button
    x <- input$data_selection
    
    if (x == "by Partner Country") {  # set the label and choices for partner countries
      
      updateSelectInput(
        session, 
        "partner_selection",
        label = paste("Select partner country:"),
        choices = countryList
      )
      
    } else if (x == "by Trade Agreement") { # set the label and choices for trade agreements
      
      updateSelectInput(
        session, 
        "partner_selection",
        label = paste("Select UK agreement:"),
        choices = groupingList
       )
   
    } else if(x == "by Region"){
      
      updateSelectInput(
        session, 
        "partner_selection",
        label = paste("Select region:"),
        choices = regionList
        )
      }
    }
  ) # end observe
  
  # general: action button texts ----------------------------------------
  
  
  observeEvent(input$dashboardGuidance, {
    showModal(modalDialog(
      title = "How to use this dashboard",
      HTML(' <p>This&nbsp;dashboard has been developed by the Data Science team&nbsp;in the&nbsp;Analysis Group&nbsp;to&nbsp;provide&nbsp;interactive&nbsp;visualisations&nbsp;on data&nbsp;sources relating to covid-19 and trade.&nbsp;Please note that this tool is under active development&nbsp;and&nbsp;we are&nbsp;working to deliver bug fixes and general improvements. It is&nbsp;for&nbsp;<strong>internal use only.</strong></p>
      <p>Information on data sources can be found at the &ldquo;Sources Guidance&rdquo;&nbsp;button.&nbsp;</p>
          ')
    )
    )
  }
  )
  
  observeEvent(input$tabGuidance, {
    showModal(modalDialog(
      title = "How to use this tab",
      HTML(' <p><h4>The Tariff Rate Quota – UK quota summary tab is an interactive page where users are required to select their Region, trade agreement or country of choice. </p></h4><br>
      <p><h5>Selections can be made using the side bar. Users should select their geographical input using the "Select input" dropdown. Users can select their desired sector and quota of measurment using the "TRQ inputs" dropdown</p></h5><br>
      <p><h4>This page is broken into three parts:</p></h4>
      <li>Overall quota utilisaiton</li>
     <li>Second list item</li>
     <li>Third list item</li>
          ')
       )
     )
    }
  )

  
  observeEvent(input$sources, {
    showModal(modalDialog(
      title = "Guidance on sources",
      "This dashboard uses the following data sources.",
      sources_summary_kable() %>% HTML(),
      easyClose = TRUE,
      size = "l"
    )
    )
  })
  
  
  
  
  observeEvent(input$trq_data_button, {
    showModal(modalDialog(
      title = "TRQ data tab guidance:",
      HTML(' 
      <p><h4>Custom data tables are available for UK and EU28 data for years 2021 and 2020 respectively.</p></h4><br>
      <p><h4>Users are able to select and download custom data tables generated by the selected region and sector. Custom excel downloads are available using the excel button above the data table. </p></h4><br>
      <p><h5>Full data table outputs can be downloaded in excel using the download button at the bottom of the page.</p></h5><br>
      <p><h5><strong>Please note:</strong> through the select inputs by agreement or quota origin is not available. Quota origin can be selected within the data table.</p></h5>
    
          ')
    )
    )
  }
  )

  
  # TRQ tab guidance doc
  output$download_ukTRQguidance<- downloadHandler(
    filename = function() {
      "TRQ tab guidance.docx"
    },
    
    content = function(file) {
      src <- normalizePath(c('www/trqGuidance.Rmd', "www/template.docx"))
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, c('trqGuidance.Rmd', "template.docx"), overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('trqGuidance.Rmd',
                    envir = new.env(parent = globalenv()))
      
      file.rename(out, file)
    }
  )
  
  
  
observeEvent(input$summaryGuidance, {
    showModal(modalDialog(
      title = "How to navigate global summary tab",
      HTML(' <p><h4>The Tariff Rate Quota – UK quota summary tab is an interactive page where users are required to select their Region, trade agreement or country of choice. </p></h4><br>
      <p><h5>Selections can be made using the side bar. Users should select their geographical input using the "Select input" dropdown. Users can select their desired sector and quota of measurment using the "TRQ inputs" dropdown</p></h5><br>
      
     
          ')
       )
      )
    }
)


observeEvent(input$ccGuidance, {
  showModal(modalDialog(
    title = "Country comparison tab guidance:",
    HTML(' 
      <p><h4>Custom data tables are available for UK and EU28 data for years 2021 and 2020 respectively.</p></h4><br>
      <p><h4>Users are able to select and download custom data tables generated by the selected region and sector. Custom excel downloads are available using the excel button above the data table. </p></h4><br>
      <p><h5>Full data table outputs can be downloaded in excel using the download button at the bottom of the page.</p></h5><br>
      <p><h5><strong>Please note:</strong> through the select inputs by agreement or quota origin is not available. Quota origin can be selected within the data table.</p></h5>
    
          ')
  )
  )
}
)
  
  
# general: quota_unit_list dynamic --------------------------------------
  
  #' update quota unit drop down depending on sector selected
  #' Create dynamic array
  #' then input array into updateSelectInput function:
  
  quotaList_update <- reactive({
    
    sectorSelection <- input$sector_list
    
    if(sectorSelection != "All"){
      
    quota_array <- sectors_region %>% 
          filter(trq_dsc == sectorSelection) %>% 
          select(quota_unit_final) %>%
          distinct() %>%
          pull()
    
    } else{
    
      quota_array <- quotaUnitList}
    
    return(quota_array)
    
  })
  
  
  observe({
    updateSelectInput(session, "quota_unit_list", choices = quotaList_update())
  })
  
  
# global summary  -----------------------------------------------------
  
  
output$summary_map <- renderLeaflet(

    expr = {
     summaryMap(switch = input$mapButton)
    }
  ) #renderLeaflet  
  
  
  
output$country_table <- DT::renderDT({

    countryDataTable()

  })
  
# trqs: trq plots ---------------------------------------------------------
  
  
  output$trq_plot <- renderPlotly({
    
    #' logic: 
    #' if select input is not by region - 
    #' a new regional filter is required to
    #' input into .region 
    #' .grouping is then used for partner_selection input
    #' 
     
    
    if(input$data_selection == "by Region"){
    
    t <- 
      trqPlot(
      .region = input$partner_selection,
      quotaUnit = input$quota_unit_list,
      sectorSelect = input$sector_list
      )
                             
    t
    
    } else{

    #' filter for new region based on partner selection 
    #' if select input is By Agreement
    
    .reg <- trqs_grouping %>%
      filter(grouping == input$partner_selection) %>%
      select(region) %>%
      distinct() %>% pull()

    t <-
      trqPlot(
        .region = .reg,
        quotaUnit = input$quota_unit_list,
        sectorSelect = input$sector_list,
        .grouping = input$partner_selection
      )
     }
  
  })
  
  
  output$trq_plot2 <- renderPlotly({
    
    t <- 
      trqAllocationPlot(
        .region = input$partner_selection,
        quotaUnit = input$quota_unit_list,
        sectorSelect = input$sector_list
      )
    
    t
    
  })

# trqs: trq sector plots -----------------------------------------
  
  
  output$trq_sector_plot <- renderPlotly({
    
   
    t <- trqSectorPlot(
     
      chartType = input$data_selection,
      .region = input$partner_selection,
      quotaUnit = input$quota_unit_list,
      sectorSelect = input$sector_list,
      euSwitch = input$sectorButton
    )
   
   
 })  
  
  output$trq_sector_plot_title <- renderText({
    
    tx <- paste0("Sector quota allocation breakdown: ", input$partner_selection)
    
  })
  
  
  

  
  
  
# trqs: trq country plot --------------------------------------------------
  
  

  output$trq_country_plot <- renderPlotly({
    
    
    t <- trqTopCountryPlot(
      quotaUnit = input$quota_unit_list,
      sectorSelect = input$sector_list,
      quotaMin = input$quota_min_volume[2]
      )
    
    t
    
    
  })
  
  
  output$trq_top_country_plot <- renderPlotly({
    
    t <- trqTopChangePlot(
      quotaUnit = input$quota_unit_list,
      sectorSelect = input$sector_list
    )
    
    t
    
  })
  
  
# trq data -------------------------------------------------------
  
  
  output$trq_table_text <- renderText({

    if(input$sector_list == "All"){
      
    txt <- paste0("TRQ data table for ",input$partner_selection, " across All sectors")
    } else{
      text <- paste0("TRQ data table for ",input$partner_selection, " across ", input$sector_list, " sectors.")
    }

  })

  output$trq_table <- DT::renderDT({   
    
    shiny::validate(need(input$data_selection == "by Region", message = "Selection by region only."))
    
    trqDataTable(
      .region = input$partner_selection,
      .sector = input$sector_list
    )
      
    
  })
  
  
  
  # UK xl spreadsheet
  
  output$download_trqs <- downloadHandler(
    
    filename = function() {
      
      uk_trq_date <- Sys.Date()
      
      paste0("UK Tariff Rate Quotas","_", uk_trq_date, ".xlsx")
      
    },
    content = function(file) {
      
      uk_trq_date <- Sys.Date()
      
      wb <- trqUKxl() 
     
      saveWorkbook(wb, file)
      
    }
  )
  
  
} # end server bracket


shinyApp(ui = ui, server = server)

