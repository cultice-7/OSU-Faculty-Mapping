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

### Addendum: Pull authors from separate list
other_auth.dt <- fread(here("Data", "RDO_SelectedAwardAuthors.csv")) %>%
  .[`PI Count` == 0]                                                 %>%
  .[,':=' (Name_fl = str_c(`First Name`, " ", `Last Name`))]         %>%
  .[,':=' (Name_fl = str_to_lower(str_replace_all(Name_fl,
                                                  "[:punct:]",
                                                  "")))]             %>%
  .[,':=' (Name_fl  = str_replace_all(Name_fl,
                                       "  ",
                                       " "))]                        %>%
  .[,':=' (Name_l   = str_to_lower(str_replace_all(`Last Name`,
                                                   "[:punct:]",
                                                   "")))]            %>%
  .[,':=' (Name_l   = str_replace_all(Name_l,
                                      "  ",
                                      " "))]                         %>%
  .[,c("Name", "Home College", "Name_fl", "Name_l")]

  # Note: Merge names to proposals data
f_mergesort <- function(id1, tolerance){
  temp.df  <- other_auth.dt[id1, ]
  temp2.df <- p_auth_u.dt
  temp.m   <- stringdist(temp.df$Name_fl,
                         temp2.df$Name_fml,
                         method = "jaccard",
                         q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    A_id.v  <- temp.df$Name_fl[1]
    Sal_id.v  <- NA
  }else{
    if (min_s <= tolerance){
      A_id.v   <- temp.df$Name_fl[1]
      Sal_id.v    <- temp2.df$Prop_AuthID[col_s]
    }else{
      A_id.v   <- temp.df$Name_fl[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}

id1       <- 1:dim(other_auth.dt)[1]
tolerance <- rep(0.34,
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort) %>%
  purrr::transpose(.)
out.df <- data.frame("Name_fl" = unlist(out1[[1]]),
                     "Prop_AuthID" = unlist(out1[[2]]),
                     "Min_s" = unlist(out1[[3]])) %>%
  left_join(other_auth.dt,
            by = "Name_fl")
mis.df <- out.df %>%
  filter(is.na(Prop_AuthID)) %>%
  left_join(p_auth_u.dt,
            by = "Name_l")

rep.v <- c(219, NA_integer_, 578, NA_integer_,
           NA_integer_, NA_integer_, NA_integer_, NA_integer_,
           5305, NA_integer_, 383, NA_integer_, NA_integer_,
           NA_integer_, 1419, NA_integer_, 1570,
           528, 1804, 1911, NA_integer_,
           NA_integer_, NA_integer_, NA_integer_, 3443,
           NA_integer_, NA_integer_, 4195, NA_integer_,
           NA_integer_, NA_integer_, NA_integer_, 5073)
mis2.df <- mis.df[,1:3] %>%
  rename(Prop_AuthID = Prop_AuthID.x) %>%
  unique()                            %>%
  mutate(Prop_AuthID = rep.v)         %>%
  left_join(p_auth_u.dt,
            by = "Prop_AuthID")

newout.df <- out.df %>%
  filter(!(Name_fl %in% mis.df$Name_fl)) %>%
  bind_rows(mis2.df)
  

newprop.dt <- p_auth.dt                                                 %>%
  .[Name_fml %in% newout.df$Name_fml,]                                  %>%
  merge.data.table(p_auth_u.dt[,c("Name_fml", "Prop_AuthID", "Sal_ID")],
                   by = "Name_fml",
                   suffixes = c("", ".d"))                              %>%
  merge.data.table(sal.dt,
                   by = "Sal_ID",
                   suffixes = c("", ".d"))                              %>%
  .[,c("Name_fml", "Proposal ID", "Name", "PI", "VP_COLLEGE",
       "DEPARTMENT",	"ORGANIZATION",	"TITLE",	"FTE", "ANNUAL_BASE_SALARY",
       "SplitAppt")]

out.dt <- rbind(prop_m.dt, newprop.dt)
fwrite(out.dt,
       here("Data", "RDO_SelectedPropAuthors.csv"))
