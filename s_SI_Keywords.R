################################################################################
##### Faculty Mapping -- SI Keywords dataset
##### Description: Creating cross references for proposal data, SI faculty data, 
#####              author data
################################################################################

### Note: SI Keywords
data(stop_words)
# Note: Tracker of overall usefulness of keywords. Trashkeywords
keepvar_v  <- names(SI_faculty.df)[c(27:31, 34)]
SI_keywords.df <- SI_faculty.df %>%
  dplyr::select(one_of(keepvar_v))                                                %>%
  pivot_longer(!SI_faculty_ID, names_to = "Keyword_Number", values_to = "Phrase") %>%
  filter(!is.na(Phrase))                                                          %>%
  .[,c("SI_faculty_ID", "Keyword_Number", "Phrase")]                              %>%
  mutate(Phrase = str_to_lower(Phrase))                                           %>%
  unique()                                                                        %>%
  unnest_tokens(word, Phrase)                                                     %>%
  anti_join(stop_words)                                                           %>%
  group_by(SI_faculty_ID, Keyword_Number)                                         %>%
  summarize(across(word,
                   ~ paste(.x, collapse = " ")))                                  %>%
  mutate(Keyword_Number = str_sub(Keyword_Number, start = -1))                    %>%
  mutate(SI_kID = paste0(SI_faculty_ID, "_", Keyword_Number))

# Note: Merge faculty data back to keywords data
SI_faculty.df  <- SI_faculty.df %>%
  .[,c("SI_faculty_ID", "Last Name", "First Name", "Middle Initial",
       "Email", "SI Core Faculty", "Name_Dot", "IDENTIFIER")]
SI_keywords.df <- SI_keywords.df %>%
  left_join(SI_faculty.df,
            by = "SI_faculty_ID")
write_csv(SI_keywords.df,
          file = here("Data", "SI_keywords.csv")) 