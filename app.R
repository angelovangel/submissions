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
  mutate(Finished = Status %in% finished_status) %>%
  as.data.table()

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
      class = "my-header",
      style = "padding: 10px 10px 0px 10px; margin: 0; display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-end;",
      dateRangeInput(
        'date',
        label = '',
        min = "2024-01-01",
        max = today(),
        start = today() - lubridate::days(30),
        end = today(),
        autoclose = T
      ),
      selectInput(
        'template',
        '',
        choices = service_types,
        selected = service_types[1],
        width = '200px'
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
    ),
    reactableOutput("submissions_table"),
    #csvDownloadButton(id = "submissions_table", filename = 'submissions-data.csv')
  ), 
  nav_panel(title = "Turnaround time",
            
            layout_column_wrap(
              #width = 1/3,
              max_height = '120px',
              gap = "30px",
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
              max_height = '120px',
              gap = "30px",
              style = "margin-top: 30px;",
              sliderInput('tat_sanger', 'TAT target Sanger', min = 1, max = 15, value = 2, post = " days"),
              sliderInput('tat_plasmid', 'TAT target plasmid', min = 1, max = 30, value = 7, post = " days"),
              sliderInput('tat_tgs', 'TAT target TGS', min = 1, max = 60, value = 21, post = " days")
            ),
            layout_column_wrap(
              #width = 1/3,
              fill = FALSE,
              gap = "30px",
              style = "margin-top: 30px;",
              uiOutput('vb1'),
              uiOutput('vb2'),
              uiOutput('vb3')
            ),
            layout_column_wrap(
              max_height = '400px',
              gap = "30px",
              style = "margin-top: 30px;",
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
    df25[Created >= input$date[1] & Created <= input$date[2]]
  })
  
  submissions2 <- reactive({
    req(input$tat_start, input$tat_end, input$time_units, input$subtract)
    
    start_col <- input$tat_start
    end_col <- input$tat_end
    unit <- input$time_units
    sub_we <- input$subtract == "TRUE"
    
    # Filter by template using data.table
    subs <- submissions1()[TemplateName == input$template]
    
    if (nrow(subs) == 0) return(subs)
    
    # Vectorized TAT calculation
    starts <- subs[[start_col]]
    ends <- subs[[end_col]]
    
    # Handle In Progress: use now() if not finished
    eff_ends <- ends
    in_progress <- !subs$Finished
    if (any(in_progress)) {
      eff_ends[in_progress] <- lubridate::now()
    }
    
    subs$tat <- lubridate::time_length(eff_ends - starts, unit = unit)
    
    # Vectorized Weekend check (now using the vectorized is_weekend from global.R)
    subs$Weekend <- is_weekend(starts, eff_ends)
    
    # Weekend subtraction logic
    if (sub_we) {
      val_to_sub <- if (unit == 'hours') 48 else 2
      # Only subtract if it's actually a weekend
      idx_sub <- !is.na(subs$Weekend) & subs$Weekend
      subs$tat[idx_sub] <- subs$tat[idx_sub] - val_to_sub
    }
    
    subs
  })
  
  # here create datasets for every service type for plotting, also for showing selected records below the apex plots
  valuebox_data <- reactive({
    req(input$time_apex_data)
    
    # Filter using data.table
    df <- df25[Created >= input$time_apex_data[1] & Created <= input$time_apex_data[2] & Finished == TRUE]
    
    if (nrow(df) == 0) return(df)
    
    # Vectorized logic
    is_sanger <- df$TemplateName == service_types[1]
    
    # Use copy to avoid modifying original data.table by reference if needed, 
    # but here we are creating a new df from the filter above.
    
    tatstart <- df$Created
    # Replace for Sanger
    idx_sanger <- which(is_sanger)
    if (length(idx_sanger) > 0) {
      tatstart[idx_sanger] <- df$SamplesReceived[idx_sanger]
    }
    
    tatend <- df$Billed
    if (length(idx_sanger) > 0) {
      tatend[idx_sanger] <- df$DataReleased[idx_sanger]
    }
    
    # Ensure POSIXct class is maintained (sometimes indexing can be tricky)
    tatstart <- as.POSIXct(tatstart)
    tatend <- as.POSIXct(tatend)
    
    df[, tat := lubridate::time_length(tatend - tatstart, unit = 'days')]
    df[, Weekend := is_weekend(tatstart, tatend)]
    
    # Weekend subtraction
    df[(!is.na(Weekend) & Weekend), tat := tat - 2]
    
    df
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
    freezeReactiveValue(input, 'tat_start')
    freezeReactiveValue(input, 'tat_end')
    updateSelectInput('tat_start', session = session, selected = ifelse(input$template == service_types[1], 'SamplesReceived', 'Created'))
    updateSelectInput('tat_end', session = session, selected = ifelse(input$template == service_types[1], 'DataReleased', 'Billed'))
  }, ignoreInit = TRUE)
  
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
  # Function to generate TAT column definition
  get_tat_coldef <- function(template, time_units, subtract, finished_vec) {
    tathours <- ifelse(template == service_types[1], 48, 168)
    tatdays <- ifelse(template == service_types[1], 2, 7)

    colDef(
      align = 'left', minWidth = 100,
      name = ifelse(subtract == "TRUE", 'TAT (WE subtr)', 'TAT'), 
      filterable = F, na = "-",
      format = colFormat(digits = 1),
      style = function(value) {
        if (is.na(value)) {
          color <- '#f46d43'
        } else if (time_units == 'hours' && value > tathours) {
          color <- '#f46d43'
        } else if (time_units == 'days' && value > tatdays) {
          color <- '#f46d43'
        } else {
          color <- '#12692d'
        }
        list(color = color)
      },
      cell = function(value, index) {
        percent <- ifelse(time_units == 'hours', value / tathours * 100, value / tatdays * 100)
        width <- paste0(pmin(pmax(percent, 0), 100), "%") 
        fill <- ifelse(percent < 100, "#d1ead9", "#ead9d1")
        isfinished <- finished_vec[index]
        
        mylabel <- if (isTRUE(isfinished)) {
          formatC(value, digits = 1, format = 'f')
        } else {
          paste0('In progress: ', formatC(value, digits = 1, format = 'f'))
        }
        bar_chart(label = mylabel, width = width, fill = fill, drawbar = isTRUE(isfinished))
      }
    )
  }
  status_coldef <- list(Status = colDef(minWidth = 150))
  weekend_coldef <- list(Weekend = colDef(sortable = F, minWidth = 70, na = "-", show = F))
  datarel_coldef <- list(DataReleased = colDef(show = F))
  finished_coldef <- list(Finished = colDef(show = F))
  
  output$submissions_table <- renderReactable({
    
    subs <- submissions2()
    status_vec <- subs$Status
    status_updates_list <- subs$StatusUpdates
    time_units <- input$time_units
    
    reactable(
      subs %>% select(-c('StatusUpdates', 'TemplateName')), 
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 20, 50, 150),
      defaultPageSize = 150,
      defaultSorted = list('SampleSubmissionId' = 'desc'), 
      class = 'my-tbl', 
      outlined = F, 
      rownames = F, compact = T, wrap = F,
      filterable = T,
      rowStyle = function(index) {
        stat <- status_vec[index]
        if(stat == 'Billed' | stat == 'Complete & Ready to be billed') {
          list(color = "#12692d")
        } else if (stat == 'Approval Process (Cancel)') {
          list(color = '#197bd7')
        } else { #(stat == 'Approved by User') {
          list(color = '#ff6500')
        }
      },
      columns = c(
        col_defs, 
        list(tat = get_tat_coldef(input$template, input$time_units, input$subtract, subs$Finished)),
        id_coldef, nsamples_colref, status_coldef, weekend_coldef, datarel_coldef, finished_coldef
      ),
      onClick = 'expand',
      details = function(index) {
        status_data <- 
          status_updates_list[[index]] %>% as_tibble() %>%
          mutate(
            StartDate = as.POSIXct(StartDate, format = '%m/%d/%Y %I:%M:%S %p'),
            EndDate = as.POSIXct(EndDate, format = '%m/%d/%Y %I:%M:%S %p'),
            process_length = time_length(EndDate - StartDate, unit = time_units)
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
                             name = paste0("Process length (", time_units, ")"), 
                             format = colFormat(digits = 1)
                             )
                         )
                         )
        )
      }
    ) 
  })
  
  
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