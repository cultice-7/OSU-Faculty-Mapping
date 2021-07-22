########################################################################################3
##### IGS Energy Faculty Identification
##### Date: 7/15/21
########################################################################################

libraries <- c("tidyverse",
               "tidytext",
               "tictoc",
               "data.table",
               "np",
               "caTools",
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
               "wordcloud2")
pacman::p_load(char = libraries)

###################################################################################################
##### Data
###################################################################################################

### Note: Unique Authors
prop_ua <- read_csv(here("Proposals_UniqueAuthors.csv"))

### Note: Authors to Prop IDs
prop_ap <- read_csv(here("Proposals_Authors.csv"))

### Note: Full authors data
prop_raw <- read_csv(here("RDO_Proposals_2013-2020.csv"))[1:31593,] %>%
  mutate(Title = str_to_lower(Title))                               %>%
  filter(!is.na(Title))

### Note: Keywords
keywords.dt <- fread(here("IGS", "IGS_Topics.csv"))

### Note: Publications
aa_file <- list.files(here("AcademicAnalytics"),
                      pattern = "article")
aa_pubs <- fread(here("AcademicAnalytics", aa_file))

###################################################################################################
##### Functions
###################################################################################################

f_fuzzymatch <- function(phrase, tolerance){
  temp.v   <- phrase 
  temp.df2 <- prop_raw %>%
    mutate(Title = str_to_lower(Title))
  temp.m  <- stringdist(temp.v,
                        temp.df2$Title,
                        method = "jaccard",
                        q = 2)
  
  temp.m[temp.m < tolerance]  <- 0
  temp.m[temp.m >= tolerance] <- 1
  
  temp.df2 <- temp.df2   %>%
    mutate(ind = temp.m) %>%
    filter(ind == 0)
  prop_id_v <- temp.df2$`Proposal ID`
  
  output <- list(phrase, prop_id_v)
  return(output)
}

f_fuzzymatch_pubs <- function(phrase, tolerance){
  temp.v   <- phrase 
  temp.df2 <- aa_pubs %>%
    mutate(articletitle = str_to_lower(articletitle))
  temp.m  <- stringdist(temp.v,
                        temp.df2$articletitle,
                        method = "jaccard",
                        q = 2)
  
  temp.m[temp.m < tolerance]  <- 0
  temp.m[temp.m >= tolerance] <- 1
  
  temp.df2 <- temp.df2   %>%
    mutate(ind = temp.m) %>%
    filter(ind == 0)
  prop_id_v <- temp.df2$articlematchid
  
  output <- list(phrase, prop_id_v)
  return(output)
}

#####################################################################################################
##### Runs
#####################################################################################################

phrase_v <- keywords.dt$Topic
tol_v    <- c(0.8, 0.65, 0.75, 0.8, 0.75, 
              0.75, 0.75, 0.7, 0.7, 0.7, 
              0.8, 0.8, 0.7, 0.8, 0.75, 
              0.8, 0.8, 0.8, 0.7)
inp_list <- list(phrase_v, tol_v)
out_list <- pmap(inp_list, f_fuzzymatch)
test <- purrr::transpose(out_list)

### Note: Translating
test.t <- tibble(phrase        = unlist(test[[1]]),
                 `Proposal ID` = test[[2]])  # Take our two lists and turn them into a dataframe (tibble). Open and check it
test.df <- test.t %>%
  unnest(cols = c(`Proposal ID`)) %>%
  left_join(prop_raw,
            by = "Proposal ID")

### Note: Publications
phrase_v <- keywords.dt$Topic
tol      <- rep(0.7,
                times = length(phrase_v))
inp_list <- list(phrase_v, tol)
out_list <- pmap(inp_list, f_fuzzymatch_pubs)





