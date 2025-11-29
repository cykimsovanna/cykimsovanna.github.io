<<<<<<< HEAD
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(rvest)
library(readxl)
library(purrr)
library(stringr)

## NAICS52 (2016-2024)
approval_data <- read_csv("Employer Information.csv") %>%
  janitor::clean_names()

approval_summary <- approval_data %>%
  group_by(fiscal_year) %>%
  summarise(
    total_initial_approvals = sum(initial_approval, na.rm = TRUE),
    total_continuing_approvals = sum(continuing_approval, na.rm = TRUE),
    total_initial_denials = sum(initial_denial, na.rm = TRUE),
    total_continuing_denials = sum(continuing_denial, na.rm = TRUE),
    total_approvals = total_initial_approvals + total_continuing_approvals,
    total_denials = total_initial_denials + total_continuing_denials
  ) %>%
  arrange(fiscal_year)

print(approval_summary, width = Inf)
# View(approval_summary)

## H1B DOL 2016-24
finance_keywords <- c(
  "financ", "investor", "investing", "accounting", "accountant",  
  "actuary", "treasury", "risk", "compliance", "quantitative", 
  "portfolio", "equity", "asset", "securities", "hedge", "audit",
  "valuation", "credit", "bank", "underwriter", "tax"
)

pattern <- paste(finance_keywords, collapse = "|")

### NAICS52 (2016)
dol_2016 <- "dol_h1b/H-1B_Disclosure_Data_FY16.xlsx"

df_2016 <- read_excel(dol_2016, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2016)
View(df_2016)

### NAICS52 (2017)
dol_2017 <- "dol_h1b/H-1B_Disclosure_Data_FY17.xlsx"

df_2017 <- read_excel(dol_2017, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2017)
View(df_2017)

### NAICS52 (2018)
dol_2018 <- "dol_h1b/H-1B_Disclosure_Data_FY2018_EOY.xlsx"

df_2018 <- read_excel(dol_2018, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2018)
View(df_2018)

### NAICS52 (2019)
dol_2019 <- "dol_h1b/H-1B_Disclosure_Data_FY2019.xlsx"

df_2019 <- read_excel(dol_2019, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2019)
View(df_2019)

### NAICS52 (2020)
dol_2020_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q1.xlsx"
dol_2020_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q2.xlsx"
dol_2020_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q3.xlsx"
dol_2020_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q4.xlsx"

df_2020 <- list(dol_2020_q1, dol_2020_q2, dol_2020_q3, dol_2020_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2020)
View(df_2020)

### NAICS52 (2021)
dol_2021_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q1.xlsx"
dol_2021_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q2.xlsx"
dol_2021_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q3.xlsx"
dol_2021_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q4.xlsx"

df_2021 <- list(dol_2021_q1, dol_2021_q2, dol_2021_q3, dol_2021_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2021)
View(df_2021)

### NAICS52 (2022)
dol_2022_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q1.xlsx"
dol_2022_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q2.xlsx"
dol_2022_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q3.xlsx"
dol_2022_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q4.xlsx"

df_2022 <- list(dol_2022_q1, dol_2022_q2, dol_2022_q3, dol_2022_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2022)
View(df_2022)

### NAICS52 (2023)
dol_2023_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q1.xlsx"
dol_2023_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q2.xlsx"
dol_2023_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q3.xlsx"
dol_2023_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q4.xlsx"

df_2023 <- list(dol_2023_q1, dol_2023_q2, dol_2023_q3, dol_2023_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2023)
View(df_2023)

### NAICS52 (2024)
dol_2024_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q1.xlsx"
dol_2024_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q2.xlsx"
dol_2024_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q3.xlsx"
dol_2024_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q4.xlsx"

df_2024 <- list(dol_2024_q1, dol_2024_q2, dol_2024_q3, dol_2024_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2024)
View(df_2024)

### Saving files
saveRDS(df_2016, "dol_h1b_2016.rds")
saveRDS(df_2017, "dol_h1b_2017.rds")
saveRDS(df_2018, "dol_h1b_2018.rds")
saveRDS(df_2019, "dol_h1b_2019.rds")
saveRDS(df_2020, "dol_h1b_2020.rds")
saveRDS(df_2021, "dol_h1b_2021.rds")
saveRDS(df_2022, "dol_h1b_2022.rds")
saveRDS(df_2023, "dol_h1b_2023.rds")
saveRDS(df_2024, "dol_h1b_2024.rds")
=======
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(rvest)
library(readxl)
library(purrr)
library(stringr)

## NAICS52 (2016-2024)
approval_data <- read_csv("Employer Information.csv") %>%
  janitor::clean_names()

approval_summary <- approval_data %>%
  group_by(fiscal_year) %>%
  summarise(
    total_initial_approvals = sum(initial_approval, na.rm = TRUE),
    total_continuing_approvals = sum(continuing_approval, na.rm = TRUE),
    total_initial_denials = sum(initial_denial, na.rm = TRUE),
    total_continuing_denials = sum(continuing_denial, na.rm = TRUE),
    total_approvals = total_initial_approvals + total_continuing_approvals,
    total_denials = total_initial_denials + total_continuing_denials
  ) %>%
  arrange(fiscal_year)

print(approval_summary, width = Inf)
# View(approval_summary)

## H1B DOL 2016-24
finance_keywords <- c(
  "financ", "investor", "investing", "accounting", "accountant",  
  "actuary", "treasury", "risk", "compliance", "quantitative", 
  "portfolio", "equity", "asset", "securities", "hedge", "audit",
  "valuation", "credit", "bank", "underwriter", "tax"
)

pattern <- paste(finance_keywords, collapse = "|")

### NAICS52 (2016)
dol_2016 <- "dol_h1b/H-1B_Disclosure_Data_FY16.xlsx"

df_2016 <- read_excel(dol_2016, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2016)
View(df_2016)

### NAICS52 (2017)
dol_2017 <- "dol_h1b/H-1B_Disclosure_Data_FY17.xlsx"

df_2017 <- read_excel(dol_2017, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2017)
View(df_2017)

### NAICS52 (2018)
dol_2018 <- "dol_h1b/H-1B_Disclosure_Data_FY2018_EOY.xlsx"

df_2018 <- read_excel(dol_2018, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2018)
View(df_2018)

### NAICS52 (2019)
dol_2019 <- "dol_h1b/H-1B_Disclosure_Data_FY2019.xlsx"

df_2019 <- read_excel(dol_2019, guess_max = 10000) %>%
  clean_names() %>%
  select(case_status, job_title) %>%
  mutate(case_status = tolower(case_status), job_title = toupper(job_title)) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2019)
View(df_2019)

### NAICS52 (2020)
dol_2020_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q1.xlsx"
dol_2020_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q2.xlsx"
dol_2020_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q3.xlsx"
dol_2020_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2020_Q4.xlsx"

df_2020 <- list(dol_2020_q1, dol_2020_q2, dol_2020_q3, dol_2020_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2020)
View(df_2020)

### NAICS52 (2021)
dol_2021_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q1.xlsx"
dol_2021_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q2.xlsx"
dol_2021_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q3.xlsx"
dol_2021_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2021_Q4.xlsx"

df_2021 <- list(dol_2021_q1, dol_2021_q2, dol_2021_q3, dol_2021_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2021)
View(df_2021)

### NAICS52 (2022)
dol_2022_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q1.xlsx"
dol_2022_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q2.xlsx"
dol_2022_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q3.xlsx"
dol_2022_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2022_Q4.xlsx"

df_2022 <- list(dol_2022_q1, dol_2022_q2, dol_2022_q3, dol_2022_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2022)
View(df_2022)

### NAICS52 (2023)
dol_2023_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q1.xlsx"
dol_2023_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q2.xlsx"
dol_2023_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q3.xlsx"
dol_2023_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2023_Q4.xlsx"

df_2023 <- list(dol_2023_q1, dol_2023_q2, dol_2023_q3, dol_2023_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2023)
View(df_2023)

### NAICS52 (2024)
dol_2024_q1 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q1.xlsx"
dol_2024_q2 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q2.xlsx"
dol_2024_q3 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q3.xlsx"
dol_2024_q4 <- "dol_h1b/LCA_Disclosure_Data_FY2024_Q4.xlsx"

df_2024 <- list(dol_2024_q1, dol_2024_q2, dol_2024_q3, dol_2024_q4) %>%
  lapply(function(file) read_excel(file, guess_max = 10000)) %>%
  lapply(clean_names) %>%
  lapply(function(x) select(x, case_status, job_title)) %>%
  bind_rows() %>%
  mutate(
    case_status = tolower(case_status),
    case_status = str_replace(case_status, "^certified\\s*-\\s*withdrawn$", "certified-withdrawn"),
    job_title = toupper(job_title)
  ) %>%
  filter(
    case_status %in% c("certified", "certified-withdrawn", "denied", "withdrawn"),
    !is.na(job_title),
    grepl(pattern, job_title, ignore.case = TRUE)
  )

head(df_2024)
View(df_2024)

### Saving files
saveRDS(df_2016, "dol_h1b_2016.rds")
saveRDS(df_2017, "dol_h1b_2017.rds")
saveRDS(df_2018, "dol_h1b_2018.rds")
saveRDS(df_2019, "dol_h1b_2019.rds")
saveRDS(df_2020, "dol_h1b_2020.rds")
saveRDS(df_2021, "dol_h1b_2021.rds")
saveRDS(df_2022, "dol_h1b_2022.rds")
saveRDS(df_2023, "dol_h1b_2023.rds")
saveRDS(df_2024, "dol_h1b_2024.rds")
>>>>>>> 8f79c9b00b2cfed878881fa5eb14d308d8627c32
