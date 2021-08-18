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
sal.dt      <- fread(here("Data", "OSU_FY2020_Sal_Edits.csv"))              %>%
  .[,tempmax := max(FTE), by = c("Sal_ID")]                                 %>%
  .[FTE == tempmax]                                                         %>%
  .[,SplitAppt := .N-1, by = "Sal_ID"]
a_auth_u.dt <- fread(here("Data", "Awards_UniqueAuthors_cor.csv"))
p_auth_u.dt <- fread(here("Data", "Proposals_UniqueAuthors_cor.csv"))
a_auth.dt   <- fread(here("Data", "Awards_Authors.csv"))
p_auth.dt   <- fread(here("Data", "Proposals_Authors.csv"))
rdo_walk    <- fread(here("Data", "RDO_Crosswalk.csv"))
rdo_select  <- fread(here("Data", "RDO_SelectedAwards.csv"))

### Note: Link unique auths to proposals
award.v <- unique(rdo_select$`Award ID`)
auth_m.dt <- a_auth.dt %>%
  merge.data.table(a_auth_u.dt,
                   by = "Name_fml",
                   suffixes = c("", ".u")) %>%
  .[,c(1:4, 10:12)]                        %>%
  merge.data.table(rdo_walk,
                   by = "Award_AuthID",
                   suffixes = c("", ".u")) %>%
  .[`Award ID` %in% award.v,]
prop_id.v <- unique(auth_m.dt$Prop_AuthID)[-1]

### Note: Grab these authors from proposals
prop_m.dt <- p_auth.dt %>%
  merge.data.table(p_auth_u.dt,
                   by = "Name_fml",
                   suffixes = c("", ".u")) %>%
  .[,c(1:4, 10:11)]                        %>%
  merge.data.table(rdo_walk,
                   by = "Prop_AuthID",
                   suffixes = c("", ".u")) %>%
  .[Prop_AuthID %in% prop_id.v,]           %>%
  .[,c(1:7)]                               %>%
  merge.data.table(sal.dt,
                   by = "Sal_ID",
                   suffixes = c("", ".sal")) %>%
  .[,c(3:6, 14:19, 28)]                          %>%
  .[order(Name, `Proposal ID`)]              
fwrite(prop_m.dt,
       here("Data", "RDO_SelectedPropAuthors.csv"))
