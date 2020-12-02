## ---
## title: 2 tag_verbs_w_topics
## author: Gabriel Ong, Paddy Tobias
## email: gabriel.ong@srcentre.com.au, paddy.tobias@srcentre.com.au
## created: 2020-11-19
## overview: |
##     Tag the original verbatim responses with their dominant topic
## ---
## 
library(glue)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)

moddir <- 'data/processed/models/' # Directory where models are stored


# Load ----
verb <- readRDS(glue('data/processed/imdb_data.RDS'))
dirs <- list.dirs(moddir, recursive = FALSE) 

# Functions ----
read_model <- function(dirs, type) {
  # Type = file we're interested in
  # 'lda' or 'summary'

  out <- list()
  for (model in dirs) {
    dat <- readRDS(glue('{model}/{type}.RDS'))
    group_var <- str_replace(model, '.*group_var-(.*)$', '\\1')
    dat$group_var <- group_var
    out[[group_var]] <- dat
  }
  
  return(out)
  
}

## Load topic model and topic summary data to merge ----
models <- list()

# Load topic models
models <- read_model(dirs, 'lda')

# Load topic labels
topic_sum <- read_model(dirs, 'summary') %>%
  bind_rows()


# Assign topics back to verbatim ----
assignTopics <- function(model_, topic_sum) {
  
  # Get Theta, the topic proportions
  theta <- as.data.frame(model_$theta)
  theta <- tibble::rownames_to_column(theta, "docid")
  
  # Get topic labels
  labels <- topic_sum %>%
    filter(group_var == model_$group_var) %>%
    select(topic, summary, group_var, top_terms, coherence, prevalence) %>%
    mutate(topic = glue("t_{topic}"))
  
  # Get the dominant topic for each document
  main_topics <- theta %>%
    pivot_longer(-docid, names_to = "topic") %>%
    left_join(labels) %>%
    group_by(docid) %>%
    mutate(value = round(value, 2)) %>%
    arrange(desc(value)) %>%
    filter(row_number()==1) %>%
    mutate(reviewID = as.numeric(str_match(docid, '[0-9]*')))
  
  return(main_topics)
  
}

allmods <- lapply(models, assignTopics, topic_sum) %>%
  bind_rows()

# Spaces removed during the model fitting
# Convert " " to "-" 
verb$group_var <- str_replace_all(verb$group_var, " ", "-") 
  

# Select relevant variables
verb %<>%
  select(reviewID, group_var, reviewText, reviewerName,
         overall, helpfulness) 

# Join verbatims
main_topics <- allmods %>%
  left_join(verb, by = c('reviewID', 'group_var')) %>%
  ungroup()

# Save ----
saveRDS(main_topics, glue('Dashboard/data/movie/db_imdb_data.RDS'))

#---
#EOF
#---
