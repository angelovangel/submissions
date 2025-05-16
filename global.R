# Infinity API
library(httr)
library(jsonlite)
library(stringr)
library(tidyr)
library(dplyr)
library(textclean)
library(lubridate)
#library(tibblify) # devtools::install_github("mgirlich/tibblify")

#path_const <- "kaust_test/WS/IdeaElanService.svc/GetAllSampleSubmissionDetails/"
service_types <- c(
  "Service Request - Sanger Sequencing", 
  "Service Request - Plasmid_Amplicon", 
  "Service Request - TGS Sequencing", 
  "Service Request - Fragment Analysis", 
  "Samples, Supplies & Manpower")

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
  if(is.na(x) || is.na(y)) {
    return(NA)
  } else if(x > y) { #otherwise seq will error for time
    return(NA)
  }
  any(format(seq.POSIXt(x, y, by = 'day'), '%u') %in% 5:6) # %u is day of week as numeric
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
update_data <- function(olddf, days = 7, timeout = 60, token = usertoken) {
  if (nrow(olddf) < 1) {
    return()
  }
  #startDate <- olddf$Initiated %>% max(na.rm = T) %>% as.Date()
  startDate <- today() - days(days)
  endDate <- today()
  newdata <- myapi(startDate, endDate, timeout = timeout, token = token)
  newdf <- make_table(newdata)
  if (nrow(newdf) > 0) {
    print(paste0(nrow(dplyr::setdiff(newdf, olddf)), " rows updated"))
    olddf %>% dplyr::rows_upsert(newdf, by = 'SampleSubmissionId')
  } else {
    olddf
  }
}
