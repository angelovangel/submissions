library(shiny)
library(bslib)
library(dplyr)
library(reactable)
library(bsicons)
library(lubridate)

source('global.R')

df25 <- readRDS('data/df25.rds') %>%
  select(-c('RollNumber', 'LabName', 'AccountCode'))

cards <- list(
  # card(
  #   full_screen = TRUE,
  #   card_header("Submissions overview"),
  #   DTOutput("submissions1")
  # ),
  card(
    full_screen = TRUE,
    card_header("Submissions table"),
    reactableOutput("submissions1")
  )
)

sidebar <- sidebar(
  open = F,
  #dateRangeInput('date', 'Date filter', min = "2024-01-01", max = today(), start = "2025-01-01", end = today(), autoclose = T),
  #actionButton('callapi', 'Update data', icon = icon('play')),
  tags$a('Last update:',
    verbatimTextOutput('lastcall')),
  tags$a('Last entry:',
    verbatimTextOutput('lastentry'))
)
  
ui <- page_sidebar(
  window_title = "Infinity submissions",
  title = tags$div(
    fluidRow(
    dateRangeInput('date', label = '', min = "2024-01-01", max = today(), start = today() - months(1), end = today(), autoclose = T),
    selectizeInput('template', '', choices = service_types, selected = service_types[1], width = '350px'),
    #selectizeInput('submission', '', choices = unique(df$SampleSubmissionId), selected = NULL)
    )
  ),
  sidebar = sidebar,
  reactableOutput("submissions1")
  #!!!cards
)

server <- function(input, output, session) {
  # api calls
  
  submissions <- reactive({
    df25 %>% 
      #mutate_if(is.timepoint, format, format = '%Y-%m-%d %H:%M') %>%
      #mutate(Status = as.factor(Status)) %>%
      dplyr::filter(Created >= input$date[1] & Created <= input$date[2]) %>%
      dplyr::filter(TemplateName == input$template)
      #dplyr::filter(SampleSubmissionId == input$submission)
  })
  
  # observe({
  #   updateSelectizeInput(session, 'submission', choices = submissions()$SampleSubmissionId)
  #   updateSelectizeInput(session, 'template', choices = submissions()$TemplateName)
  # })
  # 
  
  ########
  
  output$lastcall <- renderText({
    Sys.time() %>% format.POSIXct()
  })
  
  output$lastentry <- renderText({
    df25$Created %>% max(na.rm = T) %>% format.POSIXct()
  })
  
  
  #output$submissions1 <- renderDataTable(dfsummary)
  cols <- c('Created', 'SamplesReceived', 'Complete', 'Billed', 'LastUpdated')
  col_defs <- lapply(cols, function(x) {
    x = colDef(format = colFormat(date = T, locales = 'en-GB'))  
  }
  #Created = colDef(format = colFormat(datetime = T, locales = 'en-GB'))
  )
  names(col_defs) <- cols
  
  output$submissions1 <- renderReactable(
    
    reactable(
      submissions() %>% select(-c('StatusUpdates', 'TemplateName')), 
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 20, 50, 150),
      defaultPageSize = 50,
      defaultSorted = list('SampleSubmissionId' = 'desc'),
      #searchable = T, 
      rownames = F, compact = T,
      filterable = T,
      rowStyle = function(index) {
        if(submissions()[index, "Status"] == 'Billed' | submissions()[index, "Status"] == 'Complete & Ready to be billed') {
          list(color = "#1a9641")
        } else if (submissions()[index, "Status"] == 'Approval Process (Cancel)') {
          list(color = '#d7191c')
        } else if (submissions()[index, "Status"] == 'Approved by User') {
          list(color = '#f46d43')
        }
      },
      columns = col_defs,
      details = function(index) {
        status_data <- 
          submissions()[index, ]$StatusUpdates[[1]] %>% as_tibble() %>%
          mutate(
            StartDate = as.POSIXct(StartDate, format = '%m/%d/%Y %I:%M:%S %p'),
            EndDate = as.POSIXct(EndDate, format = '%m/%d/%Y %I:%M:%S %p')
            ) %>% 
          select(Status = Name, Start = StartDate, End = EndDate)
        htmltools::div(style = "padding: 1rem",
                       reactable(
                         status_data, outlined = TRUE, rownames = FALSE, compact = TRUE,
                         columns = list(
                           Start = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
                           End = colDef(format = colFormat(datetime = T, locales = 'en-GB'))
                         )
                         )
        )
      }
    ) 
  )
  
}

shinyApp(ui, server)