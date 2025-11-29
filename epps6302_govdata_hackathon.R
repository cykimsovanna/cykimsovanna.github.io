# Setting Up
library(httr2)
library(jsonlite)
library(fs)
library(pdftools)
library(tm)

API_KEY <- Sys.getenv("GOVINFO_API_KEY") ## change to your own API if not set
OUTDIR <- ## set your desired location
dir_create(OUTDIR)

# Defining Parameters
need <- 100 ## how many documents you want downloaded
ids <- character()
mark <- "*"

# Collecting Package IDs
repeat {
  body <- list(
    query='("South China Sea") AND (collection:CHRG OR collection:CPRT)',
    resultLevel="package", pageSize=1000, offsetMark=mark,
    sorts=list(list(field="publishdate", sortOrder="DESC"))
  )
  r <- request("https://api.govinfo.gov/search") |>
    req_url_query(api_key=API_KEY) |>
    req_body_json(body) |> req_method("POST") |> req_perform()
  j <- fromJSON(resp_body_string(r), flatten=TRUE)
  if (is.null(j$results) || !length(j$results)) break
  ids <- unique(c(ids, j$results$packageId))
  mark <- j$offsetMark; if (is.null(mark) || !nzchar(mark) || length(ids) >= need*3) break
}

# Downloading PDFs
saved <- 0; pad <- nchar(as.character(need))
for (pid in ids) {
  if (saved >= need) break
  url <- paste0("https://api.govinfo.gov/packages/", pid, "/pdf?api_key=", API_KEY)
  resp <- try(request(url) |> req_perform(), silent=TRUE)
  if (inherits(resp, "try-error") || resp_status(resp)!=200) next
  writeBin(resp_body_raw(resp), path(OUTDIR, paste0(pid, ".pdf")))
  saved <- saved + 1
  cat(sprintf("[%0*d/%d] %s\n", pad, saved, need, pid))
}
cat("Done. Saved", saved, "PDF(s) to:\n", OUTDIR, "\n")

# Creating Corpus
pdf_files <- dir(OUTDIR, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdf_files)) {
  texts <- lapply(pdf_files, pdf_text)
  texts <- sapply(texts, paste, collapse="\n")
  names(texts) <- basename(pdf_files)
  corpus <- Corpus(VectorSource(texts))
  cat("Corpus created with", length(corpus), "documents.\n")
} else {
  cat("No PDFs found in OUTDIR; corpus was not created.\n")
}