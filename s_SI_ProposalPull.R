################################################################################
##### Faculty Mapping -- Awards Pull to Proposals
##### Description: Grabbing identified authors 
################################################################################

libraries <- c("tidyverse",
               "tidytext",
               "tictoc",
               "data.table",
               "here",
               "future", 
               "future.apply",
               "furrr",
               "pryr",
               "httr",
               "jsonlite",
               "xml2",
               "rvest",
               "RSelenium",
               "stringdist",
               "topicmodels",
               "igraph", 
               "network", 
               "sna",
               "ggraph",
               "visNetwork",
               "threejs",
               "networkD3",
               "ndtv")
pacman::p_load(char = libraries)

### Note: Load in data
a_auth_u.dt <- fread(here("Data", "Awards_UniqueAuthors_cor.csv"))
p_auth_u.dt <- fread(here("Data", "Proposals_UniqueAuthors_cor.csv"))
a_auth.dt   <- fread(here("Data", "Awards_Authors.csv"))
p_auth.dt   <- fread(here("Data", "Proposals_Authors.csv"))
rdo_walk.