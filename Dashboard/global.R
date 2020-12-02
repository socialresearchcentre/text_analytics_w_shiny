## ---
## title: global.R - Example dashboard syntax
## author: Gabriel Ong, Paddy Tobias
## email: gabriel.ong@srcentre.com.au, paddy.tobias@srcentre.com.au
## created: 2020-11-19
## overview: |
##     Shiny dashboard exploring topic model results. 
##     Data comprises imdb movie reviews for all three star wars prequel films.
## ---

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(LDAvis)
library(dashboardthemes)
library(openxlsx)
library(glue)
library(plotly)
source('functions.R')
source('shiny_modules.R')
source('shiny_tabs.R')

# Load any potential additional filters
filter_spec <- read_spec('lookup/filter_vars.xlsx')

# Load main data
dat <- readRDS(glue('data/movie/db_imdb_data.RDS'))

# Load LDAvis object
movie_ldavis <- readRDS('data/movie/db_imdb_vis.RDS')






