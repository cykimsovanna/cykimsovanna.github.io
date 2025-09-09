rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggwordcloud)
library(patchwork)
library(wordcloud)
library(RColorBrewer)
library(readxl)
library(readr)
library(tidyr)

years <- 2016:2024
job_titles_by_year <- list()

for (y in years) {
  df <- readRDS(paste0("dol_", y, "_tc.rds"))
  job_titles_by_year[[as.character(y)]] <- df$clean_title
}


## Wordcloud for each cluster
cluster_titles <- read_csv("cluster_top_titles.csv")
custom_stopwords <- c("and", "i", "ii", "iii", "=")

h1b_cluster_wordcloud <- function(titles_text, cluster_num) {
  text_blob <- titles_text %>%
    tolower() %>%
    str_replace_all("[^a-z ]", " ") %>%
    str_squish()
  
  words_df <- tibble(text = text_blob) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% custom_stopwords) %>%
    count(word, sort = TRUE) %>%
    filter(nchar(word) > 2)
  
  wordcloud(
    words = words_df$word,
    freq = words_df$n,
    max.words = 150,
    min.freq = 2,
    scale = c(8, 2),
    random.order = FALSE,
    rot.per = 0.2,
    colors = brewer.pal(8, "Dark2")
  )
  
  title(main = paste("Cluster", cluster_num), cex.main = 3)
}

png("h1b_cluster_wordcloud_panel.png", width = 2400, height = 1800)
par(mfrow = c(3, 3), mar = c(0.2, 0.2, 1.2, 0.2), oma = c(0, 0, 0, 0))

for (clust in cluster_titles$cluster) {
  h1b_cluster_wordcloud(cluster_titles$top_titles[cluster_titles$cluster == clust], clust)
}

dev.off()

## Bar Charts
h1b_all_years <- read.csv("dol_all_tc_with_cluster_status.csv")

cluster_labels <- c(
  "0" = "Banking & Credit Services",
  "1" = "Financial Systems & Support Services",
  "2" = "Risk Management",
  "3" = "Tax, Audit & Accounting",
  "4" = "Financial & Business Analytics",
  "5" = "Quantitative & Research Analytics",
  "6" = "Portfolio & Asset Management",
  "7" = "Compliance & Regulation"
)

case_status_colors <- c(
  "certified" = "#1a9850",
  "certified-withdrawn" = "#BBA900",
  "denied" = "#d73027",
  "withdrawn" = "#4575b4"
)

h1b_all_years <- h1b_all_years %>%
  mutate(
    cluster_label = cluster_labels[as.character(cluster)],
    case_status = tolower(case_status)
  )

for (clust in sort(unique(h1b_all_years$cluster))) {
  
  # Summarize data by year and case_status for this cluster
  cluster_data <- h1b_all_years %>%
    filter(cluster == clust) %>%
    group_by(year, case_status) %>%
    summarise(count = n(), .groups = "drop")
  
  cluster_title <- cluster_labels[as.character(clust)]
  
  cluster_plot <- ggplot(cluster_data, aes(x = factor(year), y = count, fill = case_status)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = paste0("Cluster ", clust, ": ", cluster_title),
      x = "Year",
      y = "Number of Cases",
      fill = "Case Status"
    ) +
    theme_light(base_size = 12) +
    scale_fill_manual(values = case_status_colors) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold")
    )
  
  ggsave(
    filename = paste0("cluster_", clust, "_bar_chart.png"),
    plot = cluster_plot,
    width = 10, height = 6, dpi = 300
  )
}

### Statistics
cluster_summaries <- list()
summary_table <- h1b_all_years %>%
  group_by(cluster, year) %>%
  summarise(total_jobs = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = year,
    values_from = total_jobs,
    values_fill = 0
  ) %>%
  arrange(cluster)

View(summary_table)

status_summaries <- h1b_all_years %>%
  group_by(year, case_status) %>%
  summarise(total_jobs = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = case_status,
    values_from = total_jobs,
    values_fill = 0
  ) %>%
  arrange(year)

View(status_summaries)

### USCIS Approval Number
uscis_approval <- read_csv("Employer Information.csv")
uscis_approval_yearly <- uscis_approval %>%
  rename(year = `Fiscal Year`) %>%
  group_by(year) %>%
  summarise(
    `Initial Approvals` = sum(`Initial Approval`, na.rm = TRUE),
    `Initial Denials` = sum(`Initial Denial`, na.rm = TRUE),
    `Continuing Approvals` = sum(`Continuing Approval`, na.rm = TRUE),
    `Continuing Denials` = sum(`Continuing Denial`, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "case_type",
    values_to = "count"
  ) %>%
  mutate(
    case_type = factor(
      case_type,
      levels = c("Continuing Approvals", "Initial Approvals", "Continuing Denials", "Initial Denials")
    )
  )

ggplot(uscis_approval_yearly, aes(x = year, y = count, color = case_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "H1B Visa Approvals for Finance & Insurance Industry According to USCIS (2016–2024)",
    x = "Year",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  scale_color_manual(
    values = c(
      "Continuing Approvals" = "#2ca02c",
      "Initial Approvals" = "#61d621",
      "Continuing Denials" = "#bf0606",
      "Initial Denials" = "#dc5656"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )

print(uscis_approval_yearly, n = Inf)
