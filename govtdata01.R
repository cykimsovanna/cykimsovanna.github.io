## Scraping Government data
## Website: GovInfo (https://www.govinfo.gov/app/search/)
## Prerequisite: Download from website the list of files to be downloaded
## Designed for background job

# Start with a clean plate and lean loading to save memory
 
gc(reset=T)

# install.packages(c("purrr", "magrittr")
library(purrr)
library(magrittr) # Alternatively, load tidyverse

## Set path for reading the listing and home directory
## For Windows, use "c:\\directory\\subdirectory\\"
## For Mac, "/Users/YOURNAME/path/"

# setwd("yourpath")
library(rjson)
library(jsonlite)
library(data.table)
library(readr)
library(dplyr)

## CSV method
govfiles= read.csv(file="https://github.com/datageneration/datamethods/raw/refs/heads/master/webdata/govinfo-search-results-2024-10-13T07_10_42.csv", skip=2)

## JSON method
### rjson
gf_list <- rjson::fromJSON(file ="https://github.com/datageneration/datamethods/raw/refs/heads/master/webdata/govinfo-search-results-2024-10-13T07_18_29.json")
govfile2=dplyr::bind_rows(gf_list$resultSet)

### jsonlite
gf_list1 = jsonlite::read_json("https://github.com/datageneration/datamethods/raw/refs/heads/master/webdata/govinfo-search-results-2024-10-13T07_18_29.json")

### Extract the list
govfiles3 <- gf_list1$resultSet

### One more step
govfiles3 <- gf_list1$resultSet |> dplyr::bind_rows()


# Preparing for bulk download of government documents
govfiles$id = govfiles$packageId
pdf_govfiles_url = govfiles$pdfLink
pdf_govfiles_id <- govfiles$id

# Directory to save the pdf's
# save_dir <- "yourpath"

# Function to download pdfs
download_govfiles_pdf <- function(url, id) {
  tryCatch({
    destfile <- paste0(save_dir, "govfiles_", id, ".pdf")
    download.file(url, destfile = destfile, mode = "wb") # Binary files
    Sys.sleep(runif(1, 1, 3))  # Important: random sleep between 1 and 3 seconds to avoid suspicion of "hacking" the server
    return(paste("Successfully downloaded:", url))
  },
  error = function(e) {
    return(paste("Failed to download:", url))
  })
}


# Download files, potentially in parallel for speed
# Simple timer, can use package like tictoc
# 

## Try downloading one document
start.time <- Sys.time()
message("Starting downloads")
results <- 1:1 %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Try all five
start.time <- Sys.time()
message("Starting downloads")
results <- 1:length(pdf_govfiles_url) %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Print results
print(results)


## Exercise: Try downloading 118th Congress Congressional Hearings in Committee on Foreign Affairs?
