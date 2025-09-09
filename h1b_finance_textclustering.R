<<<<<<< HEAD
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(reticulate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

use_condaenv("bert_env", required = TRUE)

transformers <- import("sentence_transformers")
np <- import("numpy")
sk <- import("sklearn.cluster")
skm <- import("sklearn.metrics")
pd <- import("pandas")
sktext <- import("sklearn.feature_extraction.text")

model <- transformers$SentenceTransformer('all-MiniLM-L6-v2')

## Text Clustering
years <- 2016:2024
all_titles <- list()
all_datasets <- list()
all_cluster_summaries <- data.frame()


### Loading & cleaning job titles for each year
useless_words <- paste0(
  "\\b(",
  paste(c(
    "SR", "SENIOR",
    "JR", "JUNIOR",
    "VP", "VICE PRESIDENT",
    "SVP", "AVP",
    "DIR", "DIRECTOR",
    "MGR", "MANAGER",
    "LEAD", "HEAD",
    "ASSOC", "ASSOCIATE",
    "MD", "CEO", "CFO", "COO"
  ), collapse = "|"),
  ")\\b"
)

for (y in years) {
  df <- readRDS(paste0("dol_h1b_", y, ".rds")) %>%
    mutate(
      clean_title = job_title %>%
        toupper() %>%
        str_remove_all("KBGFJG[0-9-]*") %>%
        str_remove_all("[0-9]+") %>%
        str_replace_all("[[:punct:]]", "") %>%
        str_replace_all(useless_words, " ") %>%
        str_squish()
    )
  
  assign(paste0("dol_", y, "_tc"), df)
  all_datasets[[as.character(y)]] <- df
  all_titles[[as.character(y)]] <- unique(df$clean_title)
  
  saveRDS(df, file = paste0("dol_", y, "_tc.rds"))
}

### Combining all titles
dol_h1b_combined_titles <- unique(unlist(all_titles))
dol_h1b_embeddings <- model$encode(dol_h1b_combined_titles)

saveRDS(dol_h1b_combined_titles, file = "dol_h1b_combined_titles.rds")
saveRDS(dol_h1b_embeddings, file = "dol_h1b_embeddings.rds")

dol_h1b_combined_titles <- readRDS("dol_h1b_combined_titles.rds")
dol_h1b_embeddings <- readRDS("dol_h1b_embeddings.rds")

for (y in years) {
  df <- readRDS(paste0("dol_", y, "_tc.rds"))
  all_datasets[[as.character(y)]] <- df
}

### Finding optimal K
set.seed(123)
sample_indices <- sample(1:nrow(dol_h1b_embeddings), 1000)
embedding_subset <- dol_h1b_embeddings[sample_indices, ]

k_values <- 5:25
sil_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  message("Trying k = ", k)
  
  km_temp <- sk$KMeans(n_clusters = as.integer(k), random_state = 42L)
  labels <- km_temp$fit_predict(embedding_subset)
  
  sil_scores[i] <- skm$silhouette_score(embedding_subset, labels)
}

plot(k_values, sil_scores, type = "b", pch = 19, col = "steelblue",
     main = "Silhouette Score vs. Number of Clusters",
     xlab = "Number of Clusters (k)", ylab = "Silhouette Score")

best_k <- k_values[which.max(sil_scores)]
cat("Best k based on silhouette score:", best_k, "\n")

### BERT & KMeans clustering
k <- 8
km <- sk$KMeans(n_clusters = as.integer(k), random_state = 42L)
clusters <- km$fit_predict(dol_h1b_embeddings)

# Mapping clusters
cluster_df <- data.frame(clean_title = dol_h1b_combined_titles, cluster = as.integer(clusters))

# Assigning clusters back to each year
dol_all_tc <- data.frame()

for (y in years) {
  dol_tc <- all_datasets[[as.character(y)]] %>%
    left_join(cluster_df, by = "clean_title")
  
  assign(paste0("dol_", y, "_tc"), dol_tc)
  
  dol_summary <- dol_tc %>%
    group_by(cluster) %>%
    summarise(
      count = n(),
      sample_titles = paste(head(unique(clean_title), 5), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(year = y)
  
  assign(paste0("dol_", y, "_tc_summary"), dol_summary)
  dol_all_tc <- bind_rows(dol_all_tc, dol_summary)
}

View(dol_all_tc)
View(dol_2016_tc)
View(dol_2017_tc)
View(dol_2018_tc)
View(dol_2019_tc)
View(dol_2020_tc)
View(dol_2021_tc)
View(dol_2022_tc)
View(dol_2023_tc)
View(dol_2024_tc)

### Clusters with case status
title_cluster_map <- data.frame(
  clean_title = dol_h1b_combined_titles,
  cluster = clusters
)

all_datasets <- list()

for (y in years) {
  df <- readRDS(paste0("dol_", y, "_tc.rds")) %>%
    mutate(year = y) %>%
    left_join(title_cluster_map, by = "clean_title")
  
  all_datasets[[as.character(y)]] <- df
}

dol_all_tc <- bind_rows(all_datasets)
write_csv(dol_all_tc, "dol_all_tc_with_cluster_status.csv")

cluster_plot_df <- dol_all_tc %>%
  count(cluster, clean_title, sort = TRUE) %>%
  rename(title = clean_title)

cluster_top_titles <- cluster_plot_df %>%
  count(cluster, title, sort = TRUE) %>%
  group_by(cluster) %>%
  slice_max(n, n = 50) %>%
  summarise(
    top_titles = paste(title, collapse = ", "),
    .groups = "drop"
  )

View(cluster_top_titles)
write.csv(cluster_top_titles, "cluster_top_titles.csv", row.names = FALSE)
write.csv(dol_all_tc, "dol_all_tc.csv", row.names = FALSE)
write.csv(cluster_top_titles, "cluster_top_titles.csv", row.names = FALSE)
=======
rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(reticulate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

use_condaenv("bert_env", required = TRUE)

transformers <- import("sentence_transformers")
np <- import("numpy")
sk <- import("sklearn.cluster")
skm <- import("sklearn.metrics")
pd <- import("pandas")
sktext <- import("sklearn.feature_extraction.text")

model <- transformers$SentenceTransformer('all-MiniLM-L6-v2')

## Text Clustering
years <- 2016:2024
all_titles <- list()
all_datasets <- list()
all_cluster_summaries <- data.frame()


### Loading & cleaning job titles for each year
useless_words <- paste0(
  "\\b(",
  paste(c(
    "SR", "SENIOR",
    "JR", "JUNIOR",
    "VP", "VICE PRESIDENT",
    "SVP", "AVP",
    "DIR", "DIRECTOR",
    "MGR", "MANAGER",
    "LEAD", "HEAD",
    "ASSOC", "ASSOCIATE",
    "MD", "CEO", "CFO", "COO"
  ), collapse = "|"),
  ")\\b"
)

for (y in years) {
  df <- readRDS(paste0("dol_h1b_", y, ".rds")) %>%
    mutate(
      clean_title = job_title %>%
        toupper() %>%
        str_remove_all("KBGFJG[0-9-]*") %>%
        str_remove_all("[0-9]+") %>%
        str_replace_all("[[:punct:]]", "") %>%
        str_replace_all(useless_words, " ") %>%
        str_squish()
    )
  
  assign(paste0("dol_", y, "_tc"), df)
  all_datasets[[as.character(y)]] <- df
  all_titles[[as.character(y)]] <- unique(df$clean_title)
  
  saveRDS(df, file = paste0("dol_", y, "_tc.rds"))
}

### Combining all titles
dol_h1b_combined_titles <- unique(unlist(all_titles))
dol_h1b_embeddings <- model$encode(dol_h1b_combined_titles)

saveRDS(dol_h1b_combined_titles, file = "dol_h1b_combined_titles.rds")
saveRDS(dol_h1b_embeddings, file = "dol_h1b_embeddings.rds")

dol_h1b_combined_titles <- readRDS("dol_h1b_combined_titles.rds")
dol_h1b_embeddings <- readRDS("dol_h1b_embeddings.rds")

for (y in years) {
  df <- readRDS(paste0("dol_", y, "_tc.rds"))
  all_datasets[[as.character(y)]] <- df
}

### Finding optimal K
set.seed(123)
sample_indices <- sample(1:nrow(dol_h1b_embeddings), 1000)
embedding_subset <- dol_h1b_embeddings[sample_indices, ]

k_values <- 5:25
sil_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  message("Trying k = ", k)
  
  km_temp <- sk$KMeans(n_clusters = as.integer(k), random_state = 42L)
  labels <- km_temp$fit_predict(embedding_subset)
  
  sil_scores[i] <- skm$silhouette_score(embedding_subset, labels)
}

plot(k_values, sil_scores, type = "b", pch = 19, col = "steelblue",
     main = "Silhouette Score vs. Number of Clusters",
     xlab = "Number of Clusters (k)", ylab = "Silhouette Score")

best_k <- k_values[which.max(sil_scores)]
cat("Best k based on silhouette score:", best_k, "\n")

### BERT & KMeans clustering
k <- 8
km <- sk$KMeans(n_clusters = as.integer(k), random_state = 42L)
clusters <- km$fit_predict(dol_h1b_embeddings)

# Mapping clusters
cluster_df <- data.frame(clean_title = dol_h1b_combined_titles, cluster = as.integer(clusters))

# Assigning clusters back to each year
dol_all_tc <- data.frame()

for (y in years) {
  dol_tc <- all_datasets[[as.character(y)]] %>%
    left_join(cluster_df, by = "clean_title")
  
  assign(paste0("dol_", y, "_tc"), dol_tc)
  
  dol_summary <- dol_tc %>%
    group_by(cluster) %>%
    summarise(
      count = n(),
      sample_titles = paste(head(unique(clean_title), 5), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(year = y)
  
  assign(paste0("dol_", y, "_tc_summary"), dol_summary)
  dol_all_tc <- bind_rows(dol_all_tc, dol_summary)
}

View(dol_all_tc)
View(dol_2016_tc)
View(dol_2017_tc)
View(dol_2018_tc)
View(dol_2019_tc)
View(dol_2020_tc)
View(dol_2021_tc)
View(dol_2022_tc)
View(dol_2023_tc)
View(dol_2024_tc)

### Clusters with case status
title_cluster_map <- data.frame(
  clean_title = dol_h1b_combined_titles,
  cluster = clusters
)

all_datasets <- list()

for (y in years) {
  df <- readRDS(paste0("dol_", y, "_tc.rds")) %>%
    mutate(year = y) %>%
    left_join(title_cluster_map, by = "clean_title")
  
  all_datasets[[as.character(y)]] <- df
}

dol_all_tc <- bind_rows(all_datasets)
write_csv(dol_all_tc, "dol_all_tc_with_cluster_status.csv")

cluster_plot_df <- dol_all_tc %>%
  count(cluster, clean_title, sort = TRUE) %>%
  rename(title = clean_title)

cluster_top_titles <- cluster_plot_df %>%
  count(cluster, title, sort = TRUE) %>%
  group_by(cluster) %>%
  slice_max(n, n = 50) %>%
  summarise(
    top_titles = paste(title, collapse = ", "),
    .groups = "drop"
  )

View(cluster_top_titles)
write.csv(cluster_top_titles, "cluster_top_titles.csv", row.names = FALSE)
write.csv(dol_all_tc, "dol_all_tc.csv", row.names = FALSE)
write.csv(cluster_top_titles, "cluster_top_titles.csv", row.names = FALSE)
>>>>>>> 8f79c9b00b2cfed878881fa5eb14d308d8627c32
