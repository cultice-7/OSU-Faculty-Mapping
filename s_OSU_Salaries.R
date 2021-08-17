################################################################################
##### OSU Salaries Data w/ Edits for Merging
##### Date: 8/9/21
################################################################################

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
  mutate(Last_Name_ls = str_c(Last_Name_e,
                              " ",
                              SUFFIX))                        %>%
  mutate(Last_Name_ls   = str_replace_all(Last_Name_ls,
                                          "  ",
                                          " "))                    %>%
  mutate(Last_Name_ls   = str_to_lower(str_replace_all(Last_Name_ls,
                                                       "[:punct:]",
                                                       "")))  %>%
  mutate(Name_fm   = str_c(`First Name`,
                           " ",
                           `Middle Name`))                    %>%
  mutate(Name_fm   = str_replace_all(Name_fm,
                                     "  ",
                                     " "))                    %>%
  mutate(Name_fm   = str_replace_all(Name_fm,
                                    "[:punct:]",
                                    ""))                      %>%
  mutate(Name_fm   = str_to_lower(Name_fm))                   %>%
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
  rename(Sal_ID    = IDENTIFIER)                              %>%
  mutate(Name_fml  = str_trim(str_c(`First Name`,
                                    " ",
                                    `Middle Name`,
                                    " ",
                                    `Last Name`,
                                    " ",
                                    SUFFIX)))                 %>%
  mutate(Name_fml   = str_replace_all(Name_fml,
                                      "  ",
                                      " "))                    %>%
  mutate(Name_fml   = str_replace_all(Name_fml,
                                      "[:punct:]",
                                      ""))                     %>%
  mutate(Name_fml   = str_to_lower(Name_fml)) 
write_csv(salaries.df,
          here("Data", "OSU_FY2020_Sal_Edits.csv"))