#
# Script to manage the updating and distribution of the legacy swap report
#

source("\\\\istdba/BuildArchive/ShinyApps/EnvironmentScript/EnvironmentScript_Latest/LOCAL/Artifactory/environment.R")
# set parameters -----------------------------------------------------
baseDir <- "C:/Users/328576/source/repos/EmergingMarketMonitor" #

# boe checkpoint
checkpoint_date <- "2021-03-03"
message(paste0("checkpoint_date: ", checkpoint_date))
boeCheckpoint(checkpoint_date, scanForPackages = F)

library(dplyr)
library(DT)
library(futile.logger)
library(janitor)
library(readr)
library(rmarkdown)
library(taskscheduleR)

clean_tmpfiles_mod <- function() {
  message("Calling clean_tmpfiles_mod()")
}

assignInNamespace("clean_tmpfiles", clean_tmpfiles_mod, ns = "rmarkdown")

FIRVr_str <- "library(FIRVr, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages/_R4.0')"
eval(parse(text=FIRVr_str))
RDCOM_str <- "library(RDCOMClient, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages/_R4.0')"
eval(parse(text=RDCOM_str))

flog.info("--------------- Started ---------------")

if(as.numeric(R.Version()$major) < 4) flog.error("R version needs to be 4 or greater to  run market monitor!")

#
# Set the pandoc path for users without pandoc in their path
#

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

#
# Determine the directory in which the script resides
#

script_directory <- function() {
  
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  
  if ( any(grepl("--interactive", cmdArgs)) ) {
    return(normalizePath(dirname(sys.frames()[[1]]$ofile)))
  } else{
    needle = "--file="
    match = grep(needle, cmdArgs)
    return(normalizePath(dirname(sub(needle, "", cmdArgs[match]))))
  }
  
}


#
# Set the working directory
#

file_name <- "EmergingMarketMonitor"

rmarkdown::render(
  input = file.path(baseDir, "report/EmergingMarketMonitor.Rmd"),
  output_file = file.path(baseDir, paste0(file_name, ".html"))
)

# Copy the files to the archive and to the dashboard
flog.info("Copying the report ...")

file.copy(
  file.path(baseDir, paste0(file_name, ".html")),
  paste0(
    "N:/Offdata/RM/_Dashboard/Dashboard_1.1/RMDashboard/www/reports/",
    file_name,
    ".html"
  ),
  overwrite = TRUE
)

run_date <- Sys.Date()
static <- readr::read_csv("user,email
                           NA,Markets-ChinaandEmergingMarketsMITeam@bankofengland.co.uk
                           328576,joshua.allen@bankofengland.co.uk",
                           col_types = "dc")

# Read to check which jobs should currently be running
current_user <- Sys.info()[["user"]]

flog.debug("Running for location %s", location)

current_day <- as.character(lubridate::wday(run_date, label = TRUE))
Outlook <- RDCOMClient::COMCreate("Outlook.Application")
Email = Outlook$CreateItem(0)
Email[["to"]] = paste(static$email, collapse = "; ")
Email[["subject"]] = paste0("Emerging Markets Monitor ", format(run_date, "%d-%b %Y"))
Email[["body"]] = stringr::str_c(
  paste0("Please find the latest Emerging Market Monitor attached, the data is valid up until cob ", format(run_date - 3, "%d-%b %Y"), " \n \n ", 
  "if you have any questions please contact: \n \n ", 
  "Joshua.Allen@bankofengland.co.uk or \n ", 
  "Rafael.Kinston@bankofengland.co.uk \n \n \n ", 
  "Note: this is an automated email."
))

Email[["attachments"]]$Add(file.path(baseDir, paste0(file_name, ".html")))

Email$Send()
flog.info("Email sent")


flog.info("Finished")




# taskscheduler_create(
#   taskname = paste0("EmergingMarketMonitor"),
#   rscript = normalizePath(paste0(baseDir, "/run_report.R")),
#   schedule = "Weekly" %>% toupper(),
#   starttime = "09:30",
#   days = strsplit("Fri", split = "\\|") %>% unlist() %>% toupper(),
#   Rexe = "C:/PROGRA~1/R/R-4.0.5/bin/x64/Rscript.exe"
# )
