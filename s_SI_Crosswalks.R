################################################################################
##### Faculty Mapping -- Crossreferences
##### Description: Creating cross references for proposal data, SI faculty data, 
#####              author data
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
### Note: SI Faculty to Salaries Crosswalk
################################################################################

### Note: Changing appointment colleges to align with salaries data
SI_disc       <- c("Healthy Land, Water & Air Systems",
                   "Sustainable Energy",
                   "Smart & Resilient Communities",
                   "Sustainable Resources",
                   "Circular Economy")
SI_energydim  <- c("Energy Technologies",
                   "Energy Resources",
                   "Environmental Technologies and Systems",
                   "Energy Systems Management", 
                   "Economics, Business, and Policy",
                   "Human Behavior and Well-Being")

college_sal   <- c("College of Engineering", "College of Medicine",           
                   "College of Veterinary Med", "Arts and Sciences",             
                   "Ofc of Business and Finance", "Coll of Education & Human Ecol",
                   "Office of Academic Affairs", "Office of Human Resources",     
                   "College of Social Work", "Senior VP Admin & Planning",    
                   "Ofc of Student Life", "Coll of Food,Agr,Envir Science",
                   "College of Law", "College of Pharmacy",           
                   "Senior VP-Executive Officer", "Mansfield Campus",              
                   "University Advancement", "Marion Campus",                 
                   "College of Nursing", "Newark Campus",                 
                   "John Glenn College Public Affa", "Fisher College of Business",    
                   "Lima Campus", "College of Dentistry",          
                   "College of Public Health", "College of Optometry",          
                   "Office of the President", "Office of Legal Affairs",       
                   "Ofc of Government Affairs", "Board of Trustees",              
                   "Strategy Management Office")

college_si    <- c("Engineering", "",
                   "Veterinary Medicine", "ASC",
                   "", "",
                   "OAA", "",
                   "", "",
                   "", "CFAES",
                   "Law", "",
                   "", "Mansfield Campus",
                   "", "",
                   "", "",
                   "Public Affairs", "Business",
                   "", "",
                   "Public Health", "", 
                   "", "",
                   "", "",
                   "")

### Note: Matching faculty to the salaries data
  # Note: Read in SI faculty database and create dot numbers
SI_faculty.df <- read_csv(here("Data", "SI_FacultyAffiliates.csv"))     %>%
  .[,1:33]
SI_faculty.df <- SI_faculty.df[order(SI_faculty.df$`Last Name`),]       %>%
  mutate(SI_faculty_ID       = as.character(1:dim(SI_faculty.df)[1]))   %>%
  mutate(VP_COLLEGE = "")                                               %>%
  filter(!is.na(Email))                                                 %>%
  rowwise()                                                             %>%
  mutate(Name_Dot = str_sub(Email, 
                            end = str_locate(Email,
                                             pattern = "@")[1,1]-1))    %>%
  mutate(Name_fl = str_to_lower(str_c(str_trim(`First Name`),
                                " ",
                                str_trim(`Last Name`))))                %>%
  mutate(Name_fl = str_replace_all(Name_fl,
                                   pattern = "[:punct:]",
                                   replacement = ""))

  # Note: Grab colleges and match them to salary database names
SI_match <- SI_faculty.df$`Primary Appointment College` %>%
  match(college_si)
for (i in 1:length(SI_match)){
  SI_faculty.df$VP_COLLEGE[i] <- college_sal[SI_match[i]]
}

### Note: Read in current salaries data from OSU HR
#source(here("s_OSU_Salaries.R"))
salaries.df <- read_csv(here("Data", "OSU_FY2020_Sal_Edits.csv"))
sal_trim.df <- salaries.df %>%
  .[, c("Sal_ID", "Name_fl", "Name_fm", "Name_fml", "First Name", "Last Name", "Last_Name_e",
        "Last_Name_ls", "VP_COLLEGE", "DEPARTMENT")]

  # Note: Fuzzy matching of names to authors dataset; confirm with colleges
f_mergesort <- function(id1, tolerance){
  temp.df  <- SI_faculty.df[SI_faculty.df$SI_faculty_ID == id1,]
  temp2.df <- salaries.df
  temp.m  <- stringdist(temp.df$Name_fl,
                        temp2.df$Name_fl,
                        method = "jaccard",
                        q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    SI_id.v  <- temp.df$SI_faculty_ID[1]
    A_id.v  <- NA
  }else{
    if (min_s < tolerance){
      SI_id.v   <- temp.df$SI_faculty_ID[1]
      A_id.v    <- temp2.df$Sal_ID[col_s]
    }else{
      SI_id.v   <- temp.df$SI_faculty_ID[1]
      A_id.v    <- NA
    }
  }
  output <- list(SI_id.v, A_id.v)
  return(output)
}

inp1  <- SI_faculty.df$SI_faculty_ID
inp2  <- rep(0.6, times = length(inp1))
inp.l <- list(inp1, inp2)
out.l <- pmap(inp.l,
              f_mergesort) %>%
  purrr::transpose()
out.df <- data.frame("SI_faculty_ID" = unlist(out.l[[1]]),
                     "Sal_ID"        = unlist(out.l[[2]]))

SI_faculty_m.df <- SI_faculty.df    %>%
  left_join(out.df,
            by = "SI_faculty_ID")   %>%
  left_join(sal_trim.df,
            by = "Sal_ID",
            suffix = c("", ".sal"))

  # Note: Check merges; filter if last names don't match
keep.v <- c(34:43)
test <- SI_faculty_m.df %>%
  filter(`Last Name` != `Last Name.sal`) %>%
  .[,keep.v]
wrong.v <- c(1, 4, 5, 6, 7, 9, 10, 11, 13, 14, 15, 16, 17, 19, 21, 22)
wrong.df <- SI_faculty_m.df %>%
  filter(is.na(Sal_ID))     %>%
  .[,34:37]                %>%
  bind_rows(test[wrong.v,1:4])

correct.df <- SI_faculty_m.df %>%
  anti_join(wrong.df,
            by = "SI_faculty_ID")

  # Note: Check merges; filter if first names don't match
test <- correct.df                         %>%
  filter(`First Name` != `First Name.sal`) %>%
  .[, keep.v]
wrong.v <- c(11, 21, 33, 34, 47)
wrong.df <- wrong.df           %>%
  bind_rows(test[wrong.v,1:4])

correct.df <- correct.df %>%
  anti_join(wrong.df,
            by = "SI_faculty_ID")

  # Note: Check merges; filter matching names
test <- correct.df %>%
  mutate(ColMatch = if_else(VP_COLLEGE == VP_COLLEGE.sal,
                            1,
                            0))       %>%
  group_by(Sal_ID)                    %>%
  mutate(ColMatchMax = max(ColMatch)) %>%
  .[, c(keep.v, 46,49)]               %>%
  filter(ColMatchMax != 1 | is.na(ColMatchMax))
wrong.v <- c(1,7,13)
wrong.df <- wrong.df %>%
  bind_rows(test[wrong.v, 1:4])

correct.df <- correct.df %>%
  anti_join(wrong.df,
            by = "SI_faculty_ID")

  # Vector of handcoded for wrong matches
wrongfix.v <- c(23852,     0,     0,     0,  3236, 38109, 19669,   620,     0, 35459,     0, 17511, 
                17950, 19824,     0,  5351, 33784, 25558, 24099, 39647, 5904, 26230, 40473, 40473,
                2328,  5253,     0, 33167)
wrong.df[,"Sal_ID"] <- wrongfix.v
correct.df <- correct.df[, keep.v] %>%
  .[,c(1:5)]                       %>%
  bind_rows(wrong.df)              %>%
  unique()
write_csv(correct.df,
          here("Data", "SI_FacAffiliates_Sal.csv"))

################################################################################
### Note: Proposals dataset; authors to proposals
################################################################################

### Note: Read in data
prop_raw.dt <- fread(here("Data", "RDO_Proposals_2013-2020.csv")) %>%
  .[`Proposal ID`!=""]
prop.dt     <- prop_raw.dt %>%
  .[,c(1,3:4)]

### Note: Grab unique proposal ID's
prop_id.v <- unique(prop.dt)

### Note: for each row, create observed authors
### Note: Creating coinvestigators variables
coI_v    <- prop.dt$`Co-I`
coI_c    <- str_count(coI_v, ";")
coI_l    <- str_locate_all(coI_v, ";")

# Note: Check max 
max.l <- 0
for (k in 1:length(coI_l)){
  if (max.l < dim(coI_l[[k]])[1]){
    max.l <- dim(coI_l[[k]])[1]
  }
}

# Identify max number of co_I and pairs (for later)
n       <- 0
count_v <- vector("numeric",
                  length = length(coI_v))
for (i in 1:length(coI_l)){
  m <- dim(coI_l[[i]])[1]
  if (m>n){
    n <- m
  }
  if (coI_v[i] == ""){
    count_t <- 0
  }else if(coI_v[i] != "" & coI_c[i] == 0){
    count_t <- 1
  }else if(coI_v[i] != "" & coI_c[i] > 0){
    count_t <- factorial(m+1)/((factorial(1))*(factorial((m))))
  }
  count_v[i] <- count_t
}

coI_mat <- matrix(data = "",
                  nrow = length(coI_v),
                  ncol = n+1)

for (i in 1:length(coI_v)){
  if (coI_c[i]==0){
    coI_mat[i, 1] <- coI_v[i]
  } else {
    for (j in 0:coI_c[i]){
      if (j == 0){
        coI_mat[i,j+1] <- str_sub(coI_v[i],
                                  start = 0,
                                  end   = coI_l[[i]][j+1,1]-1) 
      } else if (j==coI_c[i]) {
        coI_mat[i,j+1] <- str_sub(coI_v[i],
                                  start = coI_l[[i]][j,1]+1)
      } else {
        coI_mat[i,j+1] <- str_sub(coI_v[i], 
                                  start = coI_l[[i]][j,1]+1,
                                  end   = coI_l[[i]][j+1,1]-1)
      }
    }
  }
}

# Create investigators dataset
prop_inv.dt <- prop.dt %>%
  .[,c("Proposal ID", "PI Name")]
for (i in 1:(n+1)){
  tempname <- paste0("CoI_", i)
  prop_inv.dt <- prop_inv.dt      %>%
    .[, tempvar := coI_mat[,i]]   %>%
    setnames(old = c("tempvar"),
             new = c(tempname))
}

prop_inv.dt.m <- prop_inv.dt                       %>%
  setnames(.,
           old = c("PI Name"),
           new = c("CoI_0"))                       %>%
  melt(id.vars       = c("Proposal ID"),
       measure       = patterns("^CoI_"),
       variable.name = c("Investigator_Type"),
       value.name    = c("Name"))                  %>%
  .[,':=' (PI = if_else(Investigator_Type == "CoI_0",
                        1,
                        0))]                       %>%
  .[, !c("Investigator_Type")]                     %>%
  .[Name != ""]                                    %>%
  .[,Name := str_trim(Name)]                       %>%
  .[,Name := str_replace_all(Name,
                             pattern = ",",
                             replacement = ", ")]  %>%
  .[,Name := str_replace_all(Name,
                             pattern = "  ",
                             replacement = " ")]   %>%
  .[str_detect(Name, ","),]                        %>%
  unique()

# Note: Create fml version of Name
comma.l <- str_locate_all(prop_inv.dt.m$Name,
                          ",")
comma.v <- unlist(comma.l) %>%
  matrix(ncol = 2,
         byrow = T)
prop_inv.dt.m <- prop_inv.dt.m %>%
  .[,comma_loc := comma.v[,1]]     %>%
  .[,Name_fm := str_trim(str_sub(Name,
                                 start = comma_loc + 1))] %>%
  .[,Name_l := str_trim(str_sub(Name,
                                end = comma_loc - 1))]    %>%
  .[,Name_fml := str_c(Name_fm,
                       " ",
                       Name_l)]                           %>%
  .[,Name_fml := str_to_lower(Name_fml)]                  %>%
  .[,Name_fml := str_replace_all(Name_fml,
                                 "[:punct:]",
                                 "")]                     %>%
  .[,!c("comma_loc")]

fwrite(prop_inv.dt.m,
       here("Data", "Proposals_Authors.csv"))


################################################################################
### Note: Awards dataset; authors to proposals
################################################################################

### Note: Read in data
awards_raw.dt <- fread(here("Data", "RDO_Awards_2015-2020.csv"))
awards.dt <- awards_raw.dt %>%
  .[, c(1, 3:4)]           %>% 
  unique() 

### Note: Grab unique proposal ID's
awards_id.v <- unique(awards.dt)

  # Note: for each row in awards.dt, create observed authors
### Note: Creating coinvestigators variables
coI_v    <- awards.dt$`Co-I`
coI_c    <- str_count(coI_v, ";")
coI_l    <- str_locate_all(coI_v, ";")

  # Note: Check max 
max.l <- 0
for (k in 1:length(coI_l)){
  if (max.l < dim(coI_l[[k]])[1]){
    max.l <- dim(coI_l[[k]])[1]
  }
}

# Identify max number of co_I and pairs (for later)
n       <- 0
count_v <- vector("numeric",
                  length = length(coI_v))
for (i in 1:length(coI_l)){
  m <- dim(coI_l[[i]])[1]
  if (m>n){
    n <- m
  }
  if (coI_v[i] == ""){
    count_t <- 0
  }else if(coI_v[i] != "" & coI_c[i] == 0){
    count_t <- 1
  }else if(coI_v[i] != "" & coI_c[i] > 0){
    count_t <- factorial(m+1)/((factorial(1))*(factorial((m))))
  }
  count_v[i] <- count_t
}

coI_mat <- matrix(data = "",
                  nrow = length(coI_v),
                  ncol = n+1)

for (i in 1:length(coI_v)){
  if (coI_c[i]==0){
    coI_mat[i, 1] <- coI_v[i]
  } else {
    for (j in 0:coI_c[i]){
      if (j == 0){
        coI_mat[i,j+1] <- str_sub(coI_v[i],
                                  start = 0,
                                  end   = coI_l[[i]][j+1,1]-1) 
      } else if (j==coI_c[i]) {
        coI_mat[i,j+1] <- str_sub(coI_v[i],
                                  start = coI_l[[i]][j,1]+1)
      } else {
        coI_mat[i,j+1] <- str_sub(coI_v[i], 
                                  start = coI_l[[i]][j,1]+1,
                                  end   = coI_l[[i]][j+1,1]-1)
      }
    }
  }
}

# Create investigators dataset
awards_inv.dt <- awards.dt %>%
  .[,c("Award ID", "Awd PI")]
for (i in 1:(n+1)){
  tempname <- paste0("CoI_", i)
  awards_inv.dt <- awards_inv.dt      %>%
    .[, tempvar := coI_mat[,i]]   %>%
    setnames(old = c("tempvar"),
             new = c(tempname))
}

awards_inv.dt.m <- awards_inv.dt                   %>%
  setnames(.,
           old = c("Awd PI"),
           new = c("CoI_0"))                       %>%
  melt(id.vars       = c("Award ID"),
       measure       = patterns("^CoI_"),
       variable.name = c("Investigator_Type"),
       value.name    = c("Name"))                  %>%
  .[,':=' (PI = if_else(Investigator_Type == "CoI_0",
                        1,
                        0))]                       %>%
  .[, !c("Investigator_Type")]                     %>%
  .[Name != ""]                                    %>%
  .[,Name := str_trim(Name)]                       %>%
  .[,Name := str_replace_all(Name,
                             pattern = ",",
                             replacement = ", ")]  %>%
  .[,Name := str_replace_all(Name,
                             pattern = "  ",
                             replacement = " ")]   %>%
  .[str_detect(Name, ","),]                        %>%
  unique()

  # Note: Create fml version of Name
comma.l <- str_locate_all(awards_inv.dt.m$Name,
                          ",")
comma.v <- unlist(comma.l) %>%
  matrix(ncol = 2,
         byrow = T)
awards_inv.dt.m <- awards_inv.dt.m %>%
  .[,comma_loc := comma.v[,1]]     %>%
  .[,Name_fm := str_trim(str_sub(Name,
                                 start = comma_loc + 1))] %>%
  .[,Name_l := str_trim(str_sub(Name,
                                end = comma_loc - 1))]    %>%
  .[,Name_fml := str_c(Name_fm,
                       " ",
                       Name_l)]                           %>%
  .[,Name_fml := str_replace_all(Name_fml,
                                 "[:punct:]",
                                 "")]                     %>%
  .[,Name_fml := str_to_lower(Name_fml)]

fwrite(awards_inv.dt.m,
       here("Awards_Authors.csv"))

### Note: Filtering authors related to tagged proposals
propids     <- fread(here("Data", "Awards_ClimateRelated.csv"))
id.v        <- propids$ProposalID
authors_cli <- awards_auth.dt %>%
  .[`Award ID` %in% id.v,]
fwrite(authors_cli,
       here("Data", "Awards_ClimateAuthors.csv"))

################################################################################
### Note: Awards dataset; unique authors connected to proposal ID's, 
###       salaries data, 
################################################################################

### Note: Create unique awards authors dt
awards_auth.dt   <- fread(here("Data", "Awards_Authors.csv"))
awards_auth_u.dt <- awards_auth.dt %>%
  .[,.(Name, Name_fm, Name_fml)]   %>%
  unique()                         %>%
  .[order(Name),]                  %>%
  .[,Award_AuthID := rownames(.)]  %>%
  .[,Name_fm := str_to_lower(str_replace_all(Name_fm,
                                             "[:punct:]",
                                             ""))]

### Note: Unique authors in awards to salaries
f_mergesort <- function(id1, tolerance){
  temp.df  <- awards_auth_u.dt[id1, ]
  temp2.df <- salaries.df
  temp.m  <- stringdist(temp.df$Name_fml,
                        temp2.df$Name_fml,
                        method = "jaccard",
                        q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    A_id.v  <- temp.df$Name_fml[1]
    Sal_id.v  <- NA
  }else{
    if (min_s <= tolerance){
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- temp2.df$Sal_ID[col_s]
    }else{
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}

id1       <- 1:dim(awards_auth_u.dt)[1]
tolerance <- rep(0.5,
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort) %>%
  purrr::transpose(.)
out.df <- data.frame("Name_fml" = unlist(out1[[1]]),
                     "Sal_ID" = unlist(out1[[2]]),
                     "Min_s" = unlist(out1[[3]]))

awards_m.df <- awards_auth_u.dt                                  %>%
  rowwise()                                                      %>%
  mutate(`Last Name` = str_trim(str_to_lower(str_sub(Name,
                                            end = str_locate(Name,
                                                             ",")[1,1]-1)))) %>%
  mutate(`Last Name` = str_replace_all(`Last Name`,
                                       "[:punct:]",
                                       ""))                                  %>%          
  ungroup()                                                                  %>%
  left_join(out.df,
            by = "Name_fml")                                                 %>%
  left_join(sal_trim.df,
            by = "Sal_ID",
            suffix = c("", ".sal"))

# Note: Pull out missing matches
wrong.df <- awards_m.df %>%
  filter(is.na(Sal_ID))

# Note: Check merges; filter if last names don't match. filter those less than 0.3
#       match score and remove from wrong.df
test <- awards_m.df %>%
  filter(`Last Name` != `Last_Name_ls`) %>%
  filter(Min_s > 0.3)

wrong.df <- wrong.df %>%
  bind_rows(test)

# Note: Check merges: filter if first names don't match
test <- awards_m.df                     %>%
  anti_join(wrong.df,
            by = "Award_AuthID")        %>%
  filter(Name_fm != Name_fm.sal)        %>%
  filter(Min_s > (1/3))

wrong.df <- wrong.df %>%
  bind_rows(test)

correct.df <- awards_m.df %>%
  anti_join(wrong.df,
            by = "Award_AuthID")

### Note: From wrong.df, figure out which have last name merges in the sal data
wrongfix.df <- wrong.df %>%
  .[,c(1:5)]            %>%
  left_join(sal_trim.df,
            by     = c("Last Name" = "Last_Name_e"),
            suffix = c("", ".sal"))

### Note: Unique authors in awards to salaries
f_mergesort2 <- function(id1, tolerance){
  temp.df  <- wrongfix.df[wrongfix.df$Award_AuthID == id1, ]
  temp.m  <- stringdist(unique(temp.df$Name_fm),
                        temp.df$Name_fm.sal,
                        method = "jaccard",
                        q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    A_id.v  <- temp.df$Name_fml[1]
    Sal_id.v  <- NA
  }else{
    if (min_s <= tolerance){
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- temp.df$Sal_ID[col_s]
    }else{
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}
id1       <- unique(wrongfix.df$Award_AuthID)
tolerance <- rep((0.25),
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort2) %>%
  purrr::transpose(.)
out.df <- data.frame("Name_fml" = unlist(out1[[1]]),
                     "Sal_ID" = unlist(out1[[2]]),
                     "Min_s" = unlist(out1[[3]]))

  # Note: Turns out none of the last name merges ended up being close enough on 
  #       first names to add correct matches. Letting current corrects ride.
wrong.df <- wrong.df[,c(1:6,15:16)] %>%
  mutate(Sal_ID = NA)               %>%
  mutate_at(.vars = c("VP_COLLEGE",
                      "DEPARTMENT"),
            .funs = ~ NA_character_)
awards_pm.df <- bind_rows(correct.df, wrong.df)
write_csv(awards_pm.df[,c(1:6, 15:16)],
          here("Data", "Awards_UniqueAuthors.csv"))

################################################################################
### Note: Proposals dataset; unique authors connected to proposal ID's, 
###       salaries data, 
################################################################################

### Note: Create unique awards authors dt
prop_auth.dt   <- fread(here("Data", "Proposals_Authors.csv"))
prop_auth_u.dt <- prop_auth.dt     %>%
  .[,.(Name, Name_fm, Name_l, Name_fml)]   %>%
  unique()                                 %>%
  .[order(Name),]                          %>%
  .[,Prop_AuthID := rownames(.)]           %>%
  .[,Name_fm := str_to_lower(str_replace_all(Name_fm,
                                             "[:punct:]",
                                             ""))] %>%
  .[,Name_l := str_to_lower(str_replace_all(Name_l,
                                            "[:punct:]",
                                            ""))]

### Note: Unique authors in proposals to salaries
f_mergesort <- function(id1, tolerance){
  temp.df  <- prop_auth_u.dt[id1, ]
  temp2.df <- salaries.df
  temp.m  <- stringdist(temp.df$Name_fml,
                        temp2.df$Name_fml,
                        method = "jaccard",
                        q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    A_id.v  <- temp.df$Name_fml[1]
    Sal_id.v  <- NA
  }else{
    if (min_s <= tolerance){
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- temp2.df$Sal_ID[col_s]
    }else{
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}

id1       <- 1:dim(prop_auth_u.dt)[1]
tolerance <- rep(0.5,
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort) %>%
  purrr::transpose(.)
out.df <- data.frame("Name_fml" = unlist(out1[[1]]),
                     "Sal_ID" = unlist(out1[[2]]),
                     "Min_s" = unlist(out1[[3]]))

### Note: Bind other datasets to matching results
prop_m.df <- prop_auth_u.dt                                      %>%
  left_join(out.df,
            by = "Name_fml")                                     %>%
  left_join(sal_trim.df,
            by = "Sal_ID",
            suffix = c("", ".sal"))

# Note: Pull out missing matches
wrong.df <- prop_m.df %>%
  filter(is.na(Sal_ID))

# Note: Check merges; filter if last names don't match. filter those less than 0.3
#       match score and remove from wrong.df
test <- prop_m.df %>%
  filter(Name_l != `Last_Name_ls`) %>%
  filter(Min_s > 0.3)

wrong.df <- wrong.df %>%
  bind_rows(test)

# Note: Check merges: filter if first names don't match
test <- prop_m.df                     %>%
  anti_join(wrong.df,
            by = "Prop_AuthID")        %>%
  filter(Name_fm != Name_fm.sal)        %>%
  filter(Min_s > (1/3))

wrong.df <- wrong.df %>%
  bind_rows(test)

correct.df <- prop_m.df %>%
  anti_join(wrong.df,
            by = "Prop_AuthID")

### Note: From wrong.df, figure out which have last name merges in the sal data
wrongfix.df <- wrong.df %>%
  .[,c(1:5)]            %>%
  left_join(sal_trim.df,
            by     = c("Name_l" = "Last_Name_e"),
            suffix = c("", ".sal"))

### Note: Unique authors in awards to salaries
f_mergesort2 <- function(id1, tolerance){
  temp.df  <- wrongfix.df[wrongfix.df$Prop_AuthID == id1, ]
  temp.m  <- stringdist(unique(temp.df$Name_fm),
                        temp.df$Name_fm.sal,
                        method = "jaccard",
                        q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    A_id.v  <- temp.df$Name_fml[1]
    Sal_id.v  <- NA
  }else{
    if (min_s <= tolerance){
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- temp.df$Sal_ID[col_s]
    }else{
      A_id.v   <- temp.df$Name_fml[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}
id1       <- unique(wrongfix.df$Prop_AuthID)
tolerance <- rep((0.34),
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort2) %>%
  purrr::transpose(.)
out.df <- data.frame("Name_fml" = unlist(out1[[1]]),
                     "Sal_ID" = unlist(out1[[2]]),
                     "Min_s" = unlist(out1[[3]]))   %>%
  filter(Min_s < 0.34)                              %>%
  left_join(prop_m.df[,c(1:5)],
            by = "Name_fml")                        %>%
  left_join(sal_trim.df,
            by = "Sal_ID",
            suffix = c("", ".sal"))                 %>%
  filter(Name_fml != 'jin zhao')
wrongfix.df <- data.frame("Name"        = out.df$Name,
                          "Name_fm"     = out.df$Name_fm,
                          "Name_l"      = out.df$Name_l,
                          "Name_fml"    = out.df$Name_fml,
                          "Prop_AuthID" = out.df$Prop_AuthID,
                          "Sal_ID"      = out.df$Sal_ID,
                          "VP_COLLEGE"  = out.df$VP_COLLEGE,
                          "DEPARTMENT"  = out.df$DEPARTMENT)
  # Note: Finish merging correct, wrong, and corrected
wrong.df <- wrong.df[,c(1:6,15:16)]            %>%
  mutate(Sal_ID = NA_real_)                    %>%
  mutate_at(.vars = c("VP_COLLEGE",
                      "DEPARTMENT"),
            .funs = ~ NA_character_)           %>%
  anti_join(wrongfix.df,
            by = "Prop_AuthID")
prop_pm.df <- bind_rows(correct.df, wrong.df, wrongfix.df)
write_csv(prop_pm.df[,c(1:6, 15:16)],
          here("Data", "Proposals_UniqueAuthors.csv"))

################################################################################
##### Note: Merging Proposal Authors to Awards Authors
################################################################################

awards_pm.dt <- fread(here("Data", "Awards_UniqueAuthors.csv")) %>%
  .[,c(1:6)]                                                    %>%
  unique()
prop_pm.dt   <- fread(here("Data", "Proposals_UniqueAuthors.csv")) %>%
  .[,c(1:6)]                                                       %>%
  unique()

### Note: Check for repeated entries; grab error merges
a_check <- awards_pm.dt %>%
  .[,count := .N, by = .(Sal_ID)] %>%
  .[count > 1]                    %>%
  .[!is.na(Sal_ID)]
  # Note: Change mistaken 
mistake.v <- c(1257, 3739, 3473, 3720, 2265)
a_check <- a_check                                                 %>%
  .[Award_AuthID %in% mistake.v, Sal_ID := NA_integer_]            %>%
  .[!is.na(Sal_ID),Award_AuthID := min(Award_AuthID), by = Sal_ID] %>%
  .[,.(Name, Award_AuthID, Sal_ID)]

awards_pm.dt <- awards_pm.dt                                  %>%
  merge.data.table(a_check,
                   by = "Name",
                   suffixes = c("", ".new"),
                   all.x = TRUE)                              %>%
  .[count > 1 & count < 10, Award_AuthID := Award_AuthID.new] %>%
  .[,!c("count", "Award_AuthID.new", "Sal_ID.new")]

fwrite(awards_pm.dt,
       here("Data", "Awards_UniqueAuthors_cor.csv"))

### Note: Check for repeated entries in prop data; grab error merges
p_check <- prop_pm.dt %>%
  .[,count := .N, by = .(Sal_ID)] %>%
  .[count > 1]                    %>%
  .[!is.na(Sal_ID)]
# Note: Change mistaken 
mistake.v <- c(819, 836, 1782, 2240, 3303, 4909, 5281, 5309)
p_check <- p_check                                                 %>%
  .[Prop_AuthID %in% mistake.v, Sal_ID := NA_integer_]             %>%
  .[!is.na(Sal_ID),Prop_AuthID := min(Prop_AuthID), by = Sal_ID]   %>%
  .[,.(Name, Prop_AuthID, Sal_ID)]

prop_pm.dt <- prop_pm.dt                                      %>%
  merge.data.table(p_check,
                   by = "Name",
                   suffixes = c("", ".new"),
                   all.x = TRUE)                              %>%
  .[count > 1 & count < 10, Prop_AuthID := Prop_AuthID.new]   %>%
  .[,!c("count", "Prop_AuthID.new", "Sal_ID.new")]

fwrite(prop_pm.dt,
       here("Data", "Proposals_UniqueAuthors_cor.csv"))

### Note: Merge the two datasets by salary identifier
awards_pm_trim.dt <- awards_pm.dt %>%
  .[,.(Award_AuthID, Sal_ID)]     %>%
  unique()
prop_pm_trim.dt   <- prop_pm.dt   %>%
  .[,.(Prop_AuthID, Sal_ID)]      %>%
  unique()

  # Note: Split non identified and identified entries
a_pm_nm <- awards_pm_trim.dt     %>%
  .[is.na(Sal_ID)]               %>%
  .[,Prop_AuthID := NA_integer_] %>%
  merge.data.table(a_auth_u.dt,
                   by = "Award_AuthID",
                   suffixes = c("", ".d")) %>%
  .[,!c("Sal_ID.d")]
a_pm_m.dt <- awards_pm_trim.dt %>%
  .[!is.na(Sal_ID)]

p_pm_nm   <- prop_pm_trim.dt %>%
  .[is.na(Sal_ID)]           %>%
  .[,Award_AuthID := NA_integer_] %>%
  merge.data.table(p_auth_u.dt,
                   by = "Prop_AuthID",
                   suffixes = c("", ".d")) %>%
  .[,!c("Sal_ID.d")]
p_pm_m.dt <- prop_pm_trim.dt %>%
  .[!is.na(Sal_ID)]

### Note: Merge the two non-matches by name
f_mergesort <- function(id1, tolerance){
  temp.df  <- a_pm_nm[id1, ]
  temp2.df <- p_pm_nm
  temp.m  <- stringdist(temp.df$Name_fml,
                        temp2.df$Name_fml,
                        method = "jaccard",
                        q = 2)
  min_s   <- min(temp.m)
  col_s   <- match(min_s, temp.m)
  if (is.na(min_s)){
    A_id.v  <- temp.df$Award_AuthID[1]
    Sal_id.v  <- NA
  }else{
    if (min_s <= tolerance){
      A_id.v   <- temp.df$Award_AuthID[1]
      Sal_id.v <- temp2.df$Prop_AuthID[col_s]
    }else{
      A_id.v   <- temp.df$Award_AuthID[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}

id1       <- 1:dim(a_pm_nm)[1]
tolerance <- rep(0.33,
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort) %>%
  purrr::transpose(.)
out.df <- data.frame("Award_AuthID" = unlist(out1[[1]]),
                     "Prop_AuthID" = unlist(out1[[2]]))

### Note: Awards positive merges and filtered sal merges
out_a.df <- out.df %>%
  filter(!is.na(Prop_AuthID)) %>%
  mutate(Sal_ID = NA_integer_)
out_na.df <- out.df %>%
  filter(is.na(Prop_AuthID)) %>%
  mutate(Sal_ID = NA_integer_)

out_np.df <- p_pm_nm %>%
  filter(!(Prop_AuthID %in% out_a.df$Prop_AuthID)) %>%
  .[,c("Prop_AuthID", "Sal_ID", "Award_AuthID")]

  
### Note: Attempt to match awards and proposals w/ non salary authors
pm_m.dt <- merge.data.table(a_pm_m.dt,
                            p_pm_m.dt,
                            by  = "Sal_ID",
                            all = TRUE)
inp1 <- list(out_a.df, out_na.df, out_np.df, pm_m.dt)
full.dt <-  rbindlist(inp1,
                      use.names = TRUE)
fwrite(full.dt,
       here("Data", "RDO_Crosswalk.csv"))

################################################################################
### Note: Energy Faculty walks
################################################################################
### Note: Load Salaries DT
sal.dt      <- fread(here("Data", "OSU_FY2020_Sal_Edits.csv"))              %>%
  .[,tempmax := max(FTE), by = c("Sal_ID")]                                 %>%
  .[FTE == tempmax]                                                         %>%
  .[,SplitAppt := .N-1, by = "Sal_ID"]                                      %>%
  setnames(c("Last_Name_e"),
           c("Name_l"))
sal_trim.dt <- sal.dt %>%
  .[,c("NAME", "VP_COLLEGE", "Sal_ID", "Name_fl", "Name_fml", "Name_l")]
### Note: Functions
s_locate <- function(x){
  str_sub(x, 
          end = str_locate(x,
                           "\\.")[1,1]-1)
}
s_replace <- function(x,y){
  out <- str_trim(str_replace_all(x,
                                  y,
                                  ""))
  return(out)
}

### Note: Create cleaned energy faculty master
energy.dt <- fread(here("Data", "SI_EnergyFaculty_List.csv"),
                   header = TRUE)                              %>%
  .[1:288,1:7]                                                 %>%
  .[order(name)]                                               %>%
  .[email == "shelton.1", email := "sheldon.1"]                %>%
  .[,':=' (VP_COLLEGE = "",
           Eng_ID     = row.names(.),
           Name_fl   = str_to_lower(str_replace_all(name,
                                                     "[:punct:]",
                                                     "")))]    %>%
  .[, ':=' (Name_fl  = str_replace_all(Name_fl,
                                        "  ",
                                        " "))]                 %>%
  .[, Name_l := apply(.SD, 1, s_locate), .SDcols = c("email")] %>%
  .[, Name_l := str_replace_all(Name_l, 
                                "[:punct:]",
                                "")]                           %>%
  .[name != "Allen Klaiber"]                                   %>%
  .[name != "Atar Herzinger"]                                  %>%
  .[name != "Jeanie (Chun Ning) Lau"]

# Note: Replace specific names for consistency
col_rename <- matrix(data = c("Arts & Sciences\nEngineering", "Arts & Sciences",
                              "Art & Sciences", "Arts & Sciences",
                              "Byrd Polar", "Arts & Sciences",
                              "Fisher College of Business", "Fisher",
                              "Public Affairs", "John Glenn",
                              "Business", "Fisher"),
                     ncol = 2,
                     byrow = T)
for (i in 1:dim(col_rename)[1]){
  energy.dt <- energy.dt[college == col_rename[i,1], college := col_rename[i,2]]
}  

### Note: Create harmonized college names with salary database
cols_sal <- unique(sal.dt$VP_COLLEGE)
cols_si  <- unique(energy.dt$college)
test <- stringdistmatrix(cols_si, cols_sal,
                         method = "jaccard",
                         q = 2)
match_v <- vector(mode   = "character",
                  length = length(cols_si))
for (i in 1:length(cols_si)){
  test_v <- match(min(test[i,],
                      na.rm = TRUE),
                  test[i,])
  match_v[i] <- cols_sal[test_v]
}
testmat <- matrix(data  = c(cols_si, match_v),
                  ncol  = 2,
                  byrow = F)

for (i in 1:dim(testmat)[1]){
  energy.dt <- energy.dt[college == testmat[i,1], VP_COLLEGE := testmat[i,2]]
}
fwrite(energy.dt,
       here("Data", "SI_EnergyFaculty_List_edits.csv"))
### Note: Matching Process from energy to salaries
f_mergesort <- function(id1, tolerance){
  temp.df  <- energy.dt[id1, ]
  temp2.df <- sal.dt
  temp.m   <- stringdist(temp.df$Name_fl,
                         temp2.df$Name_fl,
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
      Sal_id.v    <- temp2.df$Sal_ID[col_s]
    }else{
      A_id.v   <- temp.df$Name_fl[1]
      Sal_id.v    <- NA
    }
  }
  output <- list(A_id.v, Sal_id.v, min_s)
  return(output)
}

id1       <- 1:dim(energy.dt)[1]
tolerance <- rep(0.34,
                 times = length(id1))
inp <- list(id1, tolerance)
out1   <- pmap(inp, f_mergesort) %>%
  purrr::transpose(.)
out.df <- data.frame("Name_fl" = unlist(out1[[1]]),
                     "Sal_ID" = unlist(out1[[2]]),
                     "Min_s" = unlist(out1[[3]]))

### Note: Bind other datasets to matching results
energy_m.dt <- energy.dt                                     %>%
  merge.data.table(out.df,
                   by = "Name_fl")                           %>%
  merge.data.table(sal_trim.dt,
                   by = "Sal_ID",
                   suffixes = c("", ".sal"),
                   all.x = TRUE)                             %>%
  unique()                                                   %>%
  .[name == "Joyce Chen", Sal_ID := 5253]

# Note: Pull out missing matches
wrong.df <- energy_m.dt %>%
  .[is.na(Sal_ID)]

# Note: Check merges; filter if last names don't match. filter those less than 0.3
#       match score and remove from wrong.df
test <- energy_m.dt %>%
  .[!is.na(Sal_ID)]         %>%
  .[Name_l != `Name_l.sal`] %>%
  .[Min_s > 0.3]            %>%
  .[Name_fl != "antonio j conejo"]
wrong.df <- wrong.df %>%
  rbind(test)

### Note: Hand check correct.dt
correct.dt <- energy_m.dt %>%
  anti_join(wrong.df,
            by = "Eng_ID") %>%
  .[,c("Sal_ID", "Name_fl", "Eng_ID")]

### Note: Correcting wrong matches
wrong.df <- wrong.df[,c(2:12)]                                                        %>%
  merge.data.table(sal_trim.dt,
                   by = "Name_l",
                   suffixes = c("", ".sal"),
                   all.x = TRUE)                                                      %>%
  .[,c("Sal_ID", "Name_fl", "VP_COLLEGE", "VP_COLLEGE.sal", "Eng_ID", "Name_fl.sal")] %>%
  .[,c("Name_fl", "Eng_ID")]                                                          %>%
  unique()
wrong_c.df <- fread(here("Data", "SI_EnergyFaculty_HardCode.csv")) %>%
  .[,Eng_ID := as.character(Eng_ID)]
wrong.df <- wrong.df %>%
  merge.data.table(wrong_c.df,
                   by = "Eng_ID",
                   all = TRUE)
final.dt <- rbind(correct.dt, wrong.df)
fwrite(final.dt,
       here("Data", "SI_EnergyFaculty_Sal.csv"))
