library(shiny)
library(bslib)
library(dplyr)
library(reactable)
library(bsicons)
library(apexcharter)

source('global.R')

df25 <- readRDS('data/df25.rds') %>%
  select(-c('RollNumber', 'LabName', 'AccountCode'))

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    class='btn btn-default form-control', style = "width: 18%; padding: .375rem .375rem;",
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

sidebar <- sidebar(
  open = F,
  #dateRangeInput('date', 'Date filter', min = "2024-01-01", max = today(), start = "2025-01-01", end = today(), autoclose = T),
  #actionButton('callapi', 'Update data', icon = icon('play')),
  tags$a('Last update:',
    verbatimTextOutput('lastcall')),
  tags$a('Last entry:',
    verbatimTextOutput('lastentry'))
)
  
ui <- page_navbar(
  includeCSS('www/custom.css'),
  title = '',
  #nav_panel(title = "Submissions table",
  tags$div(
    style = "padding: 0px 0px 0px 25px; margin: 0;", # Adjust padding, margin, and width as needed
    fluidRow(
      dateRangeInput('date', label = '', min = "2024-01-01", max = today(), start = today() - months(1), end = today(), autoclose = T),
      selectizeInput('template', '', choices = service_types, selected = service_types[1], width = '350px'),
      selectizeInput('time_units', '', choices = c('Turnaround in hours' = 'hours','Turnaround in days' = 'days'), selected = 'hours'),
      selectizeInput('subtract', '', choices = c("Subtract weekends" = TRUE, "Do not subtract weekends" = FALSE)),
      #csvDownloadButton(id = "submissions_table", filename = 'submissions-data.csv')
    )
  ),
  
  nav_panel(title = "Submissions table",
      #tags$div(
      reactableOutput("submissions_table")
      #)
  ),
  nav_panel(title = "Turnaround time",
    card(
      height = '200px',
      apexchartOutput('apex_tot')
    ),
    card(
      reactableOutput('submissions_selected')
    )
  )
)

server <- function(input, output, session) {
  
  ######## DATA
  submissions1 <- reactive({
    df25 %>% 
      #mutate_if(is.timepoint, format, format = '%Y-%m-%d %H:%M') %>%
      #mutate(Status = as.factor(Status)) %>%
      dplyr::filter(Created >= input$date[1] & Created <= input$date[2]) %>%
      dplyr::filter(TemplateName == input$template)
      
  })
  
  submissions2 <- reactive({
    submissions1() %>%
      group_by(Created, Billed) %>%
      mutate(tot = time_length(Billed - Created, unit = input$time_units)) %>%
      mutate(Weekend = is_weekend(Created, Billed)) %>%
      mutate(tot = ifelse(Weekend & input$subtract == "TRUE", tot - ifelse(input$time_units == 'hours', 48, 2), tot))
  })
  
   ######## DATA
  
  output$lastcall <- renderText({
    paste0("Last update: ", Sys.time() %>% format.POSIXct())
  })
  
  output$lastentry <- renderText({
    df25$Created %>% max(na.rm = T) %>% format.POSIXct()
  })
  
  
  ######## TABLE
  
  #output$submissions1 <- renderDataTable(dfsummary)
  cols <- c('Created', 'SamplesReceived', 'Billed', 'LastUpdated')
  col_defs <- lapply(cols, function(x) {
    x = colDef(format = colFormat(date = T, locales = 'en-GB'), filterable = F, na = "-", minWidth = 80)  
  }
  #Created = colDef(format = colFormat(datetime = T, locales = 'en-GB'))
  )
  names(col_defs) <- cols
  id_coldef <- list(SampleSubmissionId = colDef(name = "SubmissionID", style = list(fontWeight = 'bold', fontSize = '1em')))
  nsamples_colref <- list(NumberOfSamples = colDef(name = '# samples', filterable = F, minWidth = 70))
  tot_coldef <- reactive({
    list(tot = colDef(name = ifelse(input$subtract == "TRUE", 'TOT (WE subtr)', 'TOT'), 
                      filterable = F, na = "-",
                      format = colFormat(digits = 1)))
  })
  status_coldef <- list(Status = colDef(minWidth = 200))
  weekend_coldef <- list(Weekend = colDef(sortable = F, minWidth = 70, na = "-"))
  
  output$submissions_table <- renderReactable(
    
    reactable(
      submissions2() %>% select(-c('StatusUpdates', 'TemplateName')), 
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 20, 50, 150),
      defaultPageSize = 50,
      defaultSorted = list('SampleSubmissionId' = 'desc'), 
      class = 'my-tbl', 
      outlined = F, 
      rownames = F, compact = T, wrap = F,
      filterable = T,
      rowStyle = function(index) {
        if(submissions2()[index, "Status"] == 'Billed' | submissions2()[index, "Status"] == 'Complete & Ready to be billed') {
          list(color = "#1a9641")
        } else if (submissions2()[index, "Status"] == 'Approval Process (Cancel)') {
          list(color = '#d7191c')
        } else if (submissions2()[index, "Status"] == 'Approved by User') {
          list(color = '#f46d43')
        }
      },
      columns = c(col_defs, tot_coldef(), id_coldef, nsamples_colref, status_coldef, weekend_coldef),
      onClick = 'expand',
      details = function(index) {
        status_data <- 
          submissions2()[index, ]$StatusUpdates[[1]] %>% as_tibble() %>%
          mutate(
            StartDate = as.POSIXct(StartDate, format = '%m/%d/%Y %I:%M:%S %p'),
            EndDate = as.POSIXct(EndDate, format = '%m/%d/%Y %I:%M:%S %p'),
            TOT = time_length(EndDate - StartDate, unit = input$time_units)
            ) %>% 
          select(Status = Name, Start = StartDate, End = EndDate, TOT)
        htmltools::div(style = "padding: 0.5rem",
                       reactable(
                         status_data, outlined = T, rownames = F, compact = T, borderless = T, highlight = T,
                         class = 'my-tbl', width = '100%',
                         columns = list(
                           Start = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
                           End = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
                           TOT = colDef(
                             name = paste0("Turnaround (", input$time_units, ")"), 
                             format = colFormat(digits = 1),
                             )
                         )
                         )
        )
      }
    ) 
  )
  ######## TABLE
  
  
  ######## APEX
  output$apex_tot <- renderApexchart({
    apex(data = mtcars, type = 'bar', mapping = aes(x = cyl))
  })
  ######## APEX
  
}

shinyApp(ui, server)