## ---
## title: 1 topic_model
## author: Gabriel Ong, Paddy Tobias
## email: gabriel.ong@srcentre.com.au, paddy.tobias@srcentre.com.au
## created: 2020-11-17
## overview: |
##     Load DFM and fit topic models
## ---

library(quanteda)
library(textmineR)
library(openxlsx)
library(glue)
library(tidyr)
library(dplyr)
library(stringr)
source('1.1 topic_functions.R')

# Define k, the number of topics
k <- 5 

# Load Data ----
dfm <- readRDS(glue('data/processed/imdb_dfm.RDS'))

# Run topic models ----
filtvars <- list()
filtvars$group_var <- unique(docvars(dfm)[['group_var']]) # Get all group_var in dfm

filts <- do.call("crossing", filtvars) %>% # Apply crossing across all filtvars
  mutate(filt = str_glue(filter_join(filtvars))) %>%  # Create mutation expressions depending on filter vars
  pull(filt) %>% 
  rlang::parse_exprs()

## Run topic models ====

# Save models
print(k)
purrr::walk(filts, ~run_lda(dfm, .x, k, 'data/processed/models/')) 

# Save DFM in model folder
purrr::walk(filts, ~save_dfm(dfm, .x, 'data/processed/models/'))

#---
#EOF
#---
