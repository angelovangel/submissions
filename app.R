library(shiny)
library(bslib)
library(dplyr)
library(reactable)
library(bsicons)
library(apexcharter)
library(RcppRoll)
library(fs)

source('global.R')

df25 <- readRDS('data/df25.rds') %>%
  select(-c('RollNumber', 'LabName', 'AccountCode')) %>%
  # hidden column to track if it is finalized
  mutate(Finished = Status %in% finished_status)

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$div(
    class='nav-item', 
    #style = "width: 18%; padding: .375rem .375rem;",
    label,
    # tagList(
    #   #icon("download"), 
    #   label),
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
  
  nav_panel(
    title = "Submissions table",
    tags$div(
      style = "padding: 0px 0px 0px 25px; margin: 0;",
      # Adjust padding, margin, and width as needed
      fluidRow(
        dateRangeInput(
          'date',
          label = '',
          min = "2024-01-01",
          max = today(),
          start = today() - months(1),
          end = today(),
          autoclose = T
        ),
        selectInput(
          'template',
          '',
          choices = service_types,
          selected = service_types[1],
          width = '330px'
        ),
        selectInput(
          'time_units',
          '',
          choices = c(
            'Turnaround in hours' = 'hours',
            'Turnaround in days' = 'days'
          ),
          selected = 'hours',
          width = '210px'
        ),
        selectInput(
          'subtract',
          '',
          choices = c(
            "Subtract weekends" = TRUE,
            "Do not subtract weekends" = FALSE
          ),
          width = '210px'
        ),
        bslib::tooltip(
          selectInput(
            'tat_start',
            '',
            choices = c('Created', 'SamplesReceived'),
            selected = 'SamplesReceived',
            width = '190px'
          ),
          'Use as start of TAT calculation'
        ),
        bslib::tooltip(
          selectInput(
            'tat_end',
            '',
            choices = c('Billed', 'DataReleased'),
            selected = 'DataReleased',
            width = '190px'
          ),
          'Use as end of TAT calculation'
        )
      )
    ),
    
    reactableOutput("submissions_table"),
    #csvDownloadButton(id = "submissions_table", filename = 'submissions-data.csv')
  ), 
  nav_panel(title = "Turnaround time",
            
            layout_column_wrap(
              #width = 1/3,
              max_height = '80px',
              #tags$div(),
              sliderInput(
                'time_apex_data', 'Select time period for turnaround time calculation', 
                timeFormat = "%F", step = 24*60*60,
                value = c(today() - dmonths(3), today()),
                #value = as.POSIXlt.Date(c(today() - months(3), today())), 
                min = min(df25$Created, na.rm = T), 
                max = max(df25$Billed, na.rm = T)),
              #tags$div()
            ),
            # tat plots only in days
            layout_column_wrap(
              max_height = '80px',
              sliderInput('tat_sanger', 'TAT target Sanger', min = 1, max = 15, value = 2, post = " days"),
              sliderInput('tat_plasmid', 'TAT target plasmid', min = 1, max = 30, value = 7, post = " days"),
              sliderInput('tat_tgs', 'TAT target TGS', min = 1, max = 60, value = 21, post = " days")
            ),
            layout_column_wrap(
              max_height = '150px',
              uiOutput('vb1'),
              uiOutput('vb2'),
              uiOutput('vb3')
            ),
            layout_column_wrap(
              max_height = '300px',
              plotlyOutput('roll1'),
              plotlyOutput('roll2'),
              plotlyOutput('roll3')
            )
  ),
  nav_panel(title = csvDownloadButton(id = "submissions_table", filename = 'submissions-data.csv')),
  nav_panel(
    title = 'App info',
    layout_column_wrap(
      verbatimTextOutput('lastcall'),
      verbatimTextOutput('lastentry'),
      verbatimTextOutput('total_entries')
    )
  )
)

server <- function(input, output, session) {

  ######## DATA
  submissions1 <- reactive({
    df25 %>% 
      #mutate_if(is.timepoint, format, format = '%Y-%m-%d %H:%M') %>%
      #mutate(Status = as.factor(Status)) %>%
      dplyr::filter(Created >= input$date[1] & Created <= input$date[2])
      
  })
  
  submissions2 <- reactive({
    submissions1() %>%
      dplyr::filter(TemplateName == input$template) %>%
      rowwise() %>%
      mutate(tat = time_length((!! rlang::sym(input$tat_end)) - (!! rlang::sym(input$tat_start)), unit = input$time_units)
      ) %>% 
      # if sample is received but not yet finalized, display elapsed time
      mutate(tat = ifelse(
        #!is.na( !! rlang::sym(input$tat_start) ) & is.na( !! rlang::sym(input$tat_end) ), 
        !Finished,
        time_length(now() - !! rlang::sym(input$tat_start), unit = input$time_units), 
        tat)) %>%
      mutate(Weekend = mapply(is_weekend, !! rlang::sym(input$tat_start), !! rlang::sym(input$tat_end))
      ) %>%
      mutate(tat = ifelse(isTRUE(Weekend) & input$subtract == "TRUE", tat - ifelse(input$time_units == 'hours', 48, 2), tat))
  })
  
  # here create datasets for every service type for plotting, also for showing selected records below the apex plots
  valuebox_data <- reactive({
    df25 %>%
    #submissions1() %>%
      filter(Created >= input$time_apex_data[1] & Created <= input$time_apex_data[2]) %>%
      filter(Finished) %>%
      rowwise() %>%
      mutate(
        tatstart = as.POSIXct(ifelse(TemplateName == service_types[1], SamplesReceived, Created)),
        tatend = as.POSIXct(ifelse(TemplateName == service_types[1], DataReleased, Billed)),
        tat = time_length(tatend - tatstart, unit = 'days')
      ) %>%
      mutate(
        Weekend = mapply(is_weekend, tatstart, tatend)
      ) %>%
      mutate(tat = ifelse(isTRUE(Weekend), tat - 2, tat))
  })
  
  
  
   ######## DATA
  
  output$lastcall <- renderText({
    lastupdate <- fs::file_info('data/df25.rds')
    paste0("Last update: ", lastupdate$modification_time %>% format.POSIXct())
  })
  
  output$lastentry <- renderText({
    lastentry <- df25$Created %>% max(na.rm = T) %>% format.POSIXct()
    paste0("Last entry: ", lastentry)
  })
  
  output$total_entries <- renderText({
    paste0("Total entries: ", nrow(df25))
  })
  
  # adjust TAT automatically based on service type
  observeEvent(input$template, {
    updateSelectInput('tat_start', session = session, selected = ifelse(input$template == service_types[1], 'SamplesReceived', 'Created'))
    updateSelectInput('tat_end', session = session, selected = ifelse(input$template == service_types[1], 'DataReleased', 'Billed'))
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
  nsamples_colref <- list(NumberOfSamples = colDef(name = '# samples', filterable = F, minWidth = 60))
  tat_coldef <- reactive({
    list(tat = colDef(align = 'left', minWidth = 100,
                      name = ifelse(input$subtract == "TRUE", 'TAT (WE subtr)', 'TAT'), 
                      filterable = F, na = "-",
                      format = colFormat(digits = 1),
                      style = function(value) {
                        tathours <- ifelse(input$template == service_types[1], 48, 168)
                        tatdays <- ifelse(input$template == service_types[1], 2, 7)
                        if (is.na(value)) {
                          color <- '#f46d43'
                        } else if (input$time_units == 'hours' && value > tathours) {
                          color <- '#f46d43'
                        } else if (input$time_units == 'days' && value > tatdays) {
                          color <- '#f46d43'
                        } else {
                          color <- '#12692d'
                        }
                        list(
                          color = color
                          #fontFamily = 'Roboto Mono'
                          )
                      },
                      cell = function(value, index){
                        # 2 days for Sanger, 7 days for all else
                        tathours <- ifelse(input$template == service_types[1], 48, 168)
                        tatdays <- ifelse(input$template == service_types[1], 2, 7)
                        percent <- ifelse(input$time_units == 'hours', value / tathours * 100, value / tatdays * 100)
                        width <-  paste0(percent, "%") 
                        fill <- ifelse(percent < 100, "#d1ead9", "#ead9d1")
                        isfinished <- submissions2()[index, ]$Finished
                        mylabel <- ifelse(
                          isTRUE(isfinished),
                          formatC(value, digits = 1, format = 'f'),
                          paste0('In progress: ', formatC(value, digits = 1, format = 'f'))
                        )
                        bar_chart(label = mylabel, width = width, fill = fill, drawbar = isTRUE(isfinished))
                      })
                      #)
         )
  })
  status_coldef <- list(Status = colDef(minWidth = 150))
  weekend_coldef <- list(Weekend = colDef(sortable = F, minWidth = 70, na = "-", show = F))
  datarel_coldef <- list(DataReleased = colDef(show = F))
  finished_coldef <- list(Finished = colDef(show = F))
  
  output$submissions_table <- renderReactable(
    
    reactable(
      submissions2() %>% select(-c('StatusUpdates', 'TemplateName')), 
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 20, 50, 150),
      defaultPageSize = 150,
      defaultSorted = list('SampleSubmissionId' = 'desc'), 
      class = 'my-tbl', 
      outlined = F, 
      rownames = F, compact = T, wrap = F,
      filterable = T,
      rowStyle = function(index) {
        if(submissions2()[index, "Status"] == 'Billed' | submissions2()[index, "Status"] == 'Complete & Ready to be billed') {
          list(color = "#12692d")
        } else if (submissions2()[index, "Status"] == 'Approval Process (Cancel)') {
          list(color = '#197bd7')
        } else { #(submissions2()[index, "Status"] == 'Approved by User') {
          list(color = '#ff6500')
        }
      },
      columns = c(col_defs, tat_coldef(), id_coldef, nsamples_colref, status_coldef, weekend_coldef, datarel_coldef, finished_coldef),
      onClick = 'expand',
      details = function(index) {
        status_data <- 
          submissions2()[index, ]$StatusUpdates[[1]] %>% as_tibble() %>%
          mutate(
            StartDate = as.POSIXct(StartDate, format = '%m/%d/%Y %I:%M:%S %p'),
            EndDate = as.POSIXct(EndDate, format = '%m/%d/%Y %I:%M:%S %p'),
            process_length = time_length(EndDate - StartDate, unit = input$time_units)
            ) %>% 
          select(Process = Name, Start = StartDate, End = EndDate, process_length)
        htmltools::div(style = "padding: 0.5rem",
                       reactable(
                         status_data, outlined = T, rownames = F, compact = T, borderless = T, highlight = T,
                         class = 'my-tbl', width = '100%',
                         rowStyle = function(index) {
                           if (str_detect(status_data[index, 'Process'], 'InProgress.*')) {
                              list(fontWeight = "bold")
                            } else {
                              list(color = "#566573")
                           }
                         },
                         columns = list(
                           Start = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
                           End = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
                           process_length = colDef(
                             name = paste0("Process length (", input$time_units, ")"), 
                             format = colFormat(digits = 1)
                             )
                         )
                         )
        )
      }
    ) 
  )
  
  
  ######## Value boxes
  
  output$vb1 <- renderUI({
    make_vb(data = valuebox_data(), filter = service_types[1], cutoff = input$tat_sanger, totaldays = 15)
  })
  output$vb2 <- renderUI({
   make_vb(data = valuebox_data(), filter = service_types[2], cutoff = input$tat_plasmid, totaldays = 30)
  })
  output$vb3 <- renderUI({
    make_vb(data = valuebox_data(), filter = service_types[3], cutoff = input$tat_tgs, totaldays = 60)
  })
  
  ######## Rolling average plots
  output$roll1 <- renderPlotly({
      make_rollm(data = valuebox_data(), filter = service_types[1], n = 7)
  })
  output$roll2 <- renderPlotly({
    make_rollm(data = valuebox_data(), filter = service_types[2], n = 14)
  })
  output$roll3 <- renderPlotly({
    make_rollm(data = valuebox_data(), filter = service_types[3], n = 14)
  })
  
}

shinyApp(ui, server)