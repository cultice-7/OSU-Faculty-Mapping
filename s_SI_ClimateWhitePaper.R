################################################################################
##### Faculty Mapping -- Energy Faculty and Related Folks
##### Description: 
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

################################################################################
### Note: Pulling together separate crosswalks
################################################################################
SI_fac      <- fread(here("Data", "SI_keywords.csv"))
SI_eng_full <- fread(here("Data", "SI_EnergyFaculty_List_edits.csv"))
SI_eng      <- fread(here("Data", "SI_EnergyFaculty_Sal.csv"))
prop_full   <- fread(here("Data", "RDO_Proposals_2013-2020.csv")) %>%
  .[`Proposal ID` != ""]                                          %>%
  .[, .(`Proposal ID`, `Title`, `Prop Spnsr`, `Prop Spnsr Type`, `Fed/Non`)]
prop_us     <- fread(here("Data", "Proposals_UniqueAuthors_cor.csv")) %>%
  .[,c(4:6)] %>%
  .[!is.na(Sal_ID)]
prop_at     <- fread(here("Data", "Proposals_Authors.csv"))
prop_bin    <- fread(here("Data", "Proposals_FacultyPairs.csv"))
sal.dt      <- fread(here("Data", "OSU_FY2020_Sal_Edits.csv"))

### Note: Connect Energy Faculty to Proposals; filter non-matches (for now)
SI_eng_m.dt <- SI_eng %>%
  merge.data.table(prop_us,
                   by = "Sal_ID",
                   all.x = TRUE)  %>%
  .[!is.na(Prop_AuthID)]          %>%

### Note: Connect proposals to authors
prop_eng <- SI_eng_m.dt %>%
  merge.data.table(prop_at,
                   by = "Name_fml")
prop_walk <- prop_eng %>%
  .[,c(1,2,4,5)]      %>%
  unique()

### Note: Grab co-authors on "Energy" Proposals
prop.v    <- unique(prop_eng$`Proposal ID`)
prop_auth <- prop_at %>%
  .[`Proposal ID` %in% prop.v]      %>%
  merge.data.table(prop_us,
                   by = "Name_fml",
                   all.x = TRUE)    %>%
  merge.data.table(prop_full,
                   by = "Proposal ID")

################################################################################
### Note: Authors based on keywords
################################################################################
test <- prop_full %>%
  .[str_detect(Title,
               pattern = paste(keywords4, collapse = "|"))]
