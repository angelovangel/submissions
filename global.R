# Infinity API
library(httr)
library(jsonlite)
library(stringr)
library(tidyr)
library(dplyr)
library(textclean)
library(lubridate)
library(plotly)
library(data.table)
#library(tibblify) # devtools::install_github("mgirlich/tibblify")

#path_const <- "kaust_test/WS/IdeaElanService.svc/GetAllSampleSubmissionDetails/"
service_types <- c(
  "Sanger" = "Service Request - Sanger Sequencing", 
  "Plasmid ONT" = "Service Request - Plasmid_Amplicon", 
  "TGS" = "Service Request - TGS Sequencing", 
  "Fragment analysis" = "Service Request - Fragment Analysis", 
  "External" = "Service Request - 2025 External Prepaid without Agreement_nonSLA TGS Sequencing - BCL TGS",
  "Manpower"= "Samples, Supplies & Manpower")

finished_status <- c(
  "Billed", "Complete & Ready to be billed", "Approval Process (Cancel)"
)


myapi <- function(startDate, endDate, timeout = 10, token = usertoken) { # i.e. "-1,-1,2022-09-18,2022-09-19"
  ua <- user_agent("bcl/angelangelov")
  
  myfacility <- ";CL%20BCL%20Capillary%20and%20Third%20Generation%20Sequencing;"
  #myfacility <- '-1'
  myid <- '-1'
  #myid <- '15892'
  
  path <- str_c(myid, myfacility, startDate, endDate, sep = ",")
  url <- str_c("https://secure20.ideaelan.com/kaust/WS/IdeaElanService.svc/GetAllSampleSubmissionDetails/", path)
  print(url)
  
  resp <- GET(url, add_headers('Usertoken' = token), ua, timeout(timeout))
  stop_for_status(resp)
  
  if (http_type(resp) != "text/plain") {
    stop("API did not return text/plain", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "Infinity API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url), 
      call. = FALSE
    )
  }
  parsed <- jsonlite::fromJSON(content(resp))
  print(paste0(nrow(parsed$SampleSubmissionDetails$SampleSubmission), " entries received."))
  parsed
}

make_table <- function(parsed) {
  if (length(parsed$SampleSubmissionDetails$SampleSubmission) < 1) {
    print('No entries in reponse')
    return()
  }
  
  df <- tibble(
    #Agent = parsed$SampleSubmissionDetails$SampleSubmission$Agent,
    #CreatedDate = as.POSIXct( parsed$SampleSubmissionDetails$SampleSubmission$CreatedDate, format = '%m/%d/%Y %I:%M:%S %p'),
    RollNumber = parsed$SampleSubmissionDetails$SampleSubmission$RollNumber,
    SampleSubmissionId = parsed$SampleSubmissionDetails$SampleSubmission$SampleSubmissionId,
    NumberOfSamples = parsed$SampleSubmissionDetails$SampleSubmission$NumberOfSamples,
    User = parsed$SampleSubmissionDetails$SampleSubmission$User,
    PI = parsed$SampleSubmissionDetails$SampleSubmission$PIName,
    AccountCode = parsed$SampleSubmissionDetails$SampleSubmission$AccountCode,
    LabName = parsed$SampleSubmissionDetails$SampleSubmission$LabName,
    #SampleSubmissionId = parsed$SampleSubmissionDetails$SampleSubmission$SampleSubmissionId,
    Status = parsed$SampleSubmissionDetails$SampleSubmission$Status,
    TemplateName = parsed$SampleSubmissionDetails$SampleSubmission$TemplateName,
    #TotalFee = parsed$SampleSubmissionDetails$SampleSubmission$TotalFee,
    
    #Initiated = lapply(parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status, function(x) {x$StartDate[x$Name == 'Initiated']}),
    Created = as.POSIXct(parsed$SampleSubmissionDetails$SampleSubmission$CreatedDate, format = '%m/%d/%Y %I:%M:%S %p'),
    # visible column used for Sanger only to start tat, take first
    SamplesReceived = lapply(
      parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status, function(x) {x$StartDate[x$Name == 'InProgress(Samples received)'][1]}
      ),
    # hidden column used for Sanger only to end tat
    DataReleased = lapply(
      parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status, function(x) {x$StartDate[x$Name == 'InProgress(Data released)'][1]}
      ),
    #Complete = lapply(parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status, function(x) {x$StartDate[x$Name == 'Complete & Ready to be billed']}),
    Billed = lapply(
      parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status, 
      function(x) {x$StartDate[x$Name == 'Complete & Ready to be billed' | x$Name == 'Billed'][1]}), # !!! end date here?
    LastUpdated = as.POSIXct(parsed$SampleSubmissionDetails$SampleSubmission$LastUpdated, format = '%m/%d/%Y %I:%M:%S %p'),
    StatusUpdates = c(parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status)
    )
    
  #statuses <- parsed$SampleSubmissionDetails$SampleSubmission$Statuses$Status
    
  df %>%
    mutate(
      TemplateName = textclean::mgsub(TemplateName, pattern = paste0(service_types, ".*"), replacement = service_types, fixed = F),
      #Initiated = as.POSIXct(as.character(Initiated), format = '%m/%d/%Y %I:%M:%S %p'),
      Billed = as.POSIXct(as.character(Billed), format = '%m/%d/%Y %I:%M:%S %p'),
      SamplesReceived = as.POSIXct(as.character(SamplesReceived), format = '%m/%d/%Y %I:%M:%S %p'),
      DataReleased = as.POSIXct(as.character(DataReleased), format = '%m/%d/%Y %I:%M:%S %p')
      #Complete = as.POSIXct(as.character(Complete), format = '%m/%d/%Y %I:%M:%S %p')) 
    )
}

# determine if there is a weekend in a date interval
is_weekend <- function(x, y) {
  # Vectorized calculation to match any(format(seq.POSIXt(x, y, by = 'day'), '%u') %in% 5:6)
  # Elements in sequence are x + i*24*3600 for i in 0, 1, ..., floor(difftime(y, x, units='days'))
  
  days_diff <- as.numeric(difftime(y, x, units = "days"))
  
  # Number of full 24h increments
  k <- floor(days_diff)
  
  wday_start <- as.numeric(format(x, "%u"))
  wday_end <- as.numeric(format(y, "%u")) # not strictly needed for the seq logic but useful
  
  # If k >= 6, we definitely cover at least one Friday (5) or Saturday (6)
  has_we_long <- k >= 6
  
  # For k < 6, we check if the range of wdays [wday_start, wday_start + k] (mod 7) hits 5 or 6
  # Same week (no wrap around modulo 7)
  same_week <- (wday_start + k) <= 7
  has_5_same <- same_week & (wday_start <= 5 & (wday_start + k) >= 5)
  has_6_same <- same_week & (wday_start <= 6 & (wday_start + k) >= 6)
  
  # Cross week boundary (wrap around)
  cross_week <- !same_week
  # If it wraps, it covers wday_start..7 AND 1..(wday_start + k - 7)
  wday_end_wrapped <- wday_start + k - 7
  has_5_cross <- cross_week & (wday_start <= 5 | wday_end_wrapped >= 5)
  has_6_cross <- cross_week & (wday_start <= 6 | wday_end_wrapped >= 6)
  
  has_we <- has_we_long | has_5_same | has_6_same | has_5_cross | has_6_cross
  
  res <- ifelse(is.na(x) | is.na(y) | (x > y), NA, has_we)
  return(res)
}

# barcharts for tat
bar_chart <- function(label, width = "100%", height = "16px", fill = "#d1ead9", drawbar = TRUE) {
  bar <- div(
    style = list(background = fill, 
                 width = ifelse(drawbar, width, '0%'), 
                 height = height)
    )
  chart <- div(style = list(flexGrow = 1, marginLeft = "2px", marginTop = "4px"), bar)
  div(style = list(display = "flex", alignItems = "right"), chart, label)
}

# days is how many days back to update rel to today 
update_data <- function(olddf, days = 7, timeout = 300, token = usertoken) {
  if (nrow(olddf) < 1) {
    return()
  }
  #startDate <- olddf$Initiated %>% max(na.rm = T) %>% as.Date()
  startDate <- lubridate::today() - days(days)
  endDate <- lubridate::today() + days(1) #Infinity response does not include today
  newdata <- myapi(startDate, endDate, timeout = timeout, token = token)
  newdf <- make_table(newdata)
  if (nrow(newdf) > 0) {
    print(paste0(nrow(dplyr::setdiff(newdf, olddf)), " rows updated"))
    olddf %>% dplyr::rows_upsert(newdf, by = 'SampleSubmissionId')
  } else {
    olddf
  }
}

make_vb <- function(data, filter, cutoff, totaldays) {
  friendly_title <- names(service_types)[service_types == filter]
  if (length(friendly_title) == 0) friendly_title <- filter
  
  # data.table filtering
  df <- data[TemplateName == filter & !is.na(tat)]
  total <- nrow(df)
  
  if (total == 0) {
    return(value_box(title = friendly_title, value = "No Data", theme = "primary"))
  }
  
  submissions_pass <- sum(df$tat <= cutoff, na.rm = TRUE)
  value <- (submissions_pass / total) * 100
  value_fmt <- paste0(formatC(value, format = "f", digits = 1), "%")
  
  spark_tats <- seq(1, totaldays, by = 0.5)
  # Vectorized ECDF calculation
  spark_percents <- ecdf(df$tat)(spark_tats) * 100
  sparkdata <- data.frame(tat = spark_tats, percent = spark_percents)
  
  sparkline <- plot_ly(sparkdata) %>%
    layout(
      xaxis = list(visible = T, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>% 
    add_segments(
      x = cutoff, xend = cutoff, y = 0, yend = value, 
      color = I("grey"), 
      line = list(dash = "dot"), 
      showlegend = F) %>%
    add_lines(
      x = ~tat, y = ~percent,
      color = I("white"), span = I(3),
      fill = 'tozeroy', alpha = 0.3
    )
    
  value_box(
    title = friendly_title,
    value = value_fmt,
    showcase = sparkline,
    p('of submissions are with TAT < ', cutoff, ' days', style = "color: lightgrey; font-size: 0.9em; margin: 0;"),
    p('Total: ', total, style="text-align:right; color: lightgrey; font-size: 0.9em; margin: 0;"),
    p('Below cutoff: ', submissions_pass, style="text-align:right; color:lightgrey; font-size: 0.9em; margin: 0;"),
    full_screen = T,
    theme = 'primary'
  )
}

make_rollm <- function(data, filter, n) {
  # data.table filtering
  df <- data[TemplateName == filter & !is.na(tat)]
  
  if (nrow(df) == 0) return(NULL)
  
  # data.table rolling mean
  df[, rollmean := RcppRoll::roll_meanl(tat, n = n, na.rm = T)]
  
  plot_ly(df) %>% 
    layout(
      xaxis = list(visible = T, showgrid = F, title = ""),
      yaxis = list(visible = T, showgrid = T, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0)
    ) %>%
    config(displayModeBar = F) %>%
    add_lines(x= ~Created, y = ~rollmean, fill = 'tozeroy', alpha = 0.3)
}

  
