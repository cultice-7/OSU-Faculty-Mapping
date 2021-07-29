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
salaries.dt <- fread(input = here("Data", "OSU_FY2020_Salaries.csv"))
name_v      <- str_trim(str_replace(salaries.dt$NAME, ",", ", "))
salaries.df <- salaries.dt                              %>%
  .[, NAME := name_v]                                   %>%
  setnames(c("LAST_NAME", "FIRST_NAME", "MIDDLE_NAME"),
           c("Last Name", "First Name", "Middle Name")) %>%
  .[POSITION_GROUP == "University"]                     %>%
  as.data.frame()                                       %>%
  rowwise()                                             %>%
  mutate(Last_Name_e = str_replace_all(str_to_lower(str_replace_all(`Last Name`,
                                                                    " ",
                                                                    "")),
                                       pattern     = "[:punct:]",
                                       replacement = ""))     %>%
  mutate(Name_fl   = str_trim(str_c(`First Name`,
                                    `Last Name`,
                                    sep = " ")))              %>%
  mutate(Name_fl   = str_replace_all(Name_fl,
                                     "  ",
                                     " "))                    %>%
  mutate(Name_fl   = str_replace_all(Name_fl,
                                     "[:punct:]",
                                     ""))                     %>%
  mutate(Name_fl   = str_to_lower(Name_fl))                   %>%
  rename(Sal_ID    = IDENTIFIER)
sal_trim.df <- salaries.df %>%
  .[, c("Sal_ID", "Name_fl", "First Name", "Last Name", "VP_COLLEGE", "DEPARTMENT")]
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
            by = "SI_faculty_ID") %>%
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
  .[, c(keep.v, 45)]                  %>%
  filter(ColMatchMax != 1 | is.na(ColMatchMax))
wrong.v <- c(1,2,5, 9, 10, 11, 17)
wrong.df <- wrong.df %>%
  bind_rows(test[wrong.v, 1:4])

correct.df <- correct.df %>%
  anti_join(wrong.df,
            by = "SI_faculty_ID")
