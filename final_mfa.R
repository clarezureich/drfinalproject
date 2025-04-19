library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(FactoMineR)
library(factoextra)
library(stringr)
library(janitor)
library(tibble)
library(ggplot2)
library(ggrepel)


# File path
volume_a <- "/Users/clarezureich/Documents/Applied Social Data Science/Dimensionality Reduction/final_project/SP554_Volume_A_QB_clean.xlsx"
volume_b <- "/Users/clarezureich/Documents/Applied Social Data Science/Dimensionality Reduction/final_project/SP554_Volume_B_QB_clean.xlsx"

# Get all sheet names
sheets_a <- excel_sheets(volume_a)
sheets_b <- excel_sheets(volume_b)


###Clean Volume A###
volume_a_clean <- function(sheet) {
  #Read first row for column names
  headers <- read_excel(volume_a, sheet = sheet, col_names = FALSE, range = "A1:ZZ1")
  colnames_vec <- as.character(headers[1, ])
  
  #Read the data from row 2 on
  data <- read_excel(volume_a, sheet = sheet, skip = 1, col_names = FALSE)
  colnames(data) <- colnames_vec
  
  #Add Question column based on sheet name
  data <- data %>%
    mutate(Question = sheet, .before = 1)
  
  return(data)
}

# Apply to all Volume A QB sheets
volume_a_cleaned_list <- map(sheets_a, volume_a_clean)
names(volume_a_cleaned_list) <- sheets_a

volume_a_cleaned_list <- map(volume_a_cleaned_list, ~ {
  names(.x)[1] <- "Question"
  names(.x)[2] <- "label"
  .x
})

#Pivot volume A list long  
volume_a_long <- map(volume_a_cleaned_list, ~ {
  .x %>%
    mutate(across(-c(Question, label), ~ suppressWarnings(as.numeric(.)))) %>%
    pivot_longer(
      cols = -c(Question, label),
      names_to = "group",
      values_to = "value"
    )
})


###Clean Volume B###
volume_b_clean <- function(sheet) {
  headers <- read_excel(volume_b, sheet = sheet, col_names = FALSE, range = "A1:ZZ2")
  main_header <- as.character(headers[1, ])
  sub_header  <- as.character(headers[2, ])
  
  main_header_filled <- na.locf(main_header, na.rm = FALSE)
  
  combined_names <- paste(main_header_filled, sub_header) %>%
    trimws() %>%
    gsub("\\s+", "_", .) %>%
    make.unique()  # handles any duplicates
  
  data <- read_excel(volume_b, sheet = sheet, skip = 2, col_names = FALSE)
  colnames(data) <- combined_names
  
  data <- data %>%
    mutate(Question = sheet, .before = 1)
  
  return(data)
}

# Apply to all Volume B QB sheets
volume_b_cleaned_list <- map(sheets_b, volume_b_clean)
names(volume_b_cleaned_list) <- sheets_b

#Update column names
volume_b_cleaned_list <- map(volume_b_cleaned_list, ~ {
  names(.x)[1] <- "Question"
  names(.x)[2] <- "label"
  .x
})

#Pivot volume B list long  
volume_b_long <- map(volume_b_cleaned_list, ~ {
  .x %>%
    mutate(across(-c(Question, label), ~ suppressWarnings(as.numeric(.)))) %>%
    pivot_longer(
      cols = -c(Question, label),
      names_to = "group",
      values_to = "value"
    )
})




######################
###MFA on Volume A###
######################
# Select MFA questions
mfa_questions <- grep("^QB", names(volume_a_long), value = TRUE)

# Combine into one  data set
mfa_long <- bind_rows(volume_a_long[mfa_questions])

# Combine similarly scaled labels, dropp uncertain/no opinion labels
label_map <- c(
  "A very positive impact" = "Very favorable",
  "Very positively" = "Very favorable",
  "Totally agree" = "Very favorable",
  "Very important" = "Very favorable",
  
  "A fairly positive impact" = "Somewhat favorable",
  "Fairly positively" = "Somewhat favorable",
  "Somewhat positively" = "Somewhat favorable",
  "Tend to agree" = "Somewhat favorable",
  "Somewhat important" = "Somewhat favorable",
  
  "A fairly negative impact" = "Somewhat unfavorable",
  "Fairly negatively" = "Somewhat unfavorable",
  "Somewhat negatively" = "Somewhat unfavorable",
  "Tend to disagree" = "Somewhat unfavorable",
  "Not very important" = "Somewhat unfavorable",
  
  "A very negative impact" = "Very unfavorable",
  "Very negatively" = "Very unfavorable",
  "Totally disagree" = "Very unfavorable",
  "Not at all important" = "Very unfavorable"
)



drop_labels <- c(
  "Don't know", 
  "Don't know enough about the most recent digital technologies (SPONTANEOUS)",
  "Your workplace does not use digital technologies  (SPONTANEOUS)",
  "It depends (SPONTANEOUS)",
  "Not applicable (SPONTANEOUS)",
  "Total 'Positive'", "Total 'Negative'", 
  "Total 'Don't know'", "Total 'Positively'", "Total 'Negatively'", 
  "Total 'Agree'", "Total 'Disagree'",
  "Total 'Aware'", "Total 'Unaware'",
  "Total 'Yes'", "Total 'No'", 
  "Total 'Important'", "Total 'Not important'"
)

#Reverse QB6_1, 6_5, 6_7 due to question wording
rev_map <- tribble(
  ~Question, ~label_orig,         ~label_new,
  "QB6_1",   "Very favorable",       "Very unfavorable",
  "QB6_1",   "Somewhat favorable",   "Somewhat unfavorable",
  "QB6_1",   "Somewhat unfavorable", "Somewhat favorable",
  "QB6_1",   "Very unfavorable",     "Very favorable",
  #
  "QB6_5",   "Very favorable",       "Very unfavorable",
  "QB6_5",   "Somewhat favorable",   "Somewhat unfavorable",
  "QB6_5",   "Somewhat unfavorable", "Somewhat favorable",
  "QB6_5",   "Very unfavorable",     "Very favorable",
  #
  "QB6_7",   "Very favorable",       "Very unfavorable",
  "QB6_7",   "Somewhat favorable",   "Somewhat unfavorable",
  "QB6_7",   "Somewhat unfavorable", "Somewhat favorable",
  "QB6_7",   "Very unfavorable",     "Very favorable"
)


#Analyze the portion of "Don't know/no opinion responses" 
mfa_long %>%
  mutate(response_type = ifelse(label %in% drop_labels, "Non-attitude", "Opinion")) %>%
  group_by(group, response_type) %>%
  summarize(total = sum(value, na.rm = TRUE)) %>%
  mutate(share = total / sum(total)) %>%
  filter(response_type == "Non-attitude") %>%
  arrange(desc(share))


#Apply new label mapping (drop "non-attitude", add relabeled scales, reverse labels for specified QB)
mfa_long <- mfa_long %>%                              
  filter(!label %in% drop_labels) %>%                     
  mutate(label = recode(label, !!!label_map)) %>%        
  left_join(rev_map,
            by = c("Question", "label" = "label_orig")) %>% 
  mutate(label = coalesce(label_new, label)) %>%          
  dplyr::select(-label_new)


# Pivot to wide: row = group (demographic), column = question + label, values = count
mfa_wide <- mfa_long %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  unite("item", Question, label, sep = "_") %>%
  pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
  column_to_rownames("group")

#Convert count data to proportions (controls for population size) 
mfa_wide <- mfa_wide / rowSums(mfa_wide)

#Define attitude vs experience/skill questions 
block_impact <- grep("^(QB1_)|(QB5_)|(QB8_)", colnames(mfa_wide), value = TRUE)
block_skills <- grep("^(QB2_)", colnames(mfa_wide), value = TRUE)
block_mgmt   <- grep("^(QB3_)|(QB4_)|(QB7_)|(QB9_)|(QB10_)", colnames(mfa_wide), value = TRUE)
block_rules  <- grep("^(QB6_)|(QB11_)", colnames(mfa_wide), value = TRUE)

grouped_blocks <- list(
  Impact = block_impact,
  Skills = block_skills,
  Management = block_mgmt,
  Governance = block_rules
)

#Group sizes and name vectors
group_sizes <- sapply(grouped_blocks, length)
group_names <- names(grouped_blocks)

#Perform MFA on question-response matrix
mfa_res <- MFA(
  mfa_wide,
  group = group_sizes,
  type = rep("f", length(group_sizes)),
  name.group = group_names,
  graph = FALSE
)

#Scree plot of explained variance
fviz_screeplot(mfa_res, addlabels = TRUE)

# Variable contributions (response categories)
fviz_mfa_var(mfa_res, "group", repel = TRUE)
fviz_mfa_var(mfa_res,
             choice    = "group",
             axes      = c(1, 2),
             repel     = TRUE,
             col.var   = "contrib") 

# Same for cos²:
fviz_mfa_var(mfa_res,
             choice    = "group",
             axes      = c(1, 2),
             repel     = TRUE,
             col.var   = "cos2")

#Country contribution
fviz_mfa_ind(mfa_res,
             repel = TRUE,
             col.ind = "cos2",     
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Adding ellipses based on country region 
country_region <- c(
  BE = "Western", BG = "Eastern", CZ = "Eastern", DK = "Nordic",
  `D-W` = "Western", DE = "Western", `D-E` = "Eastern", EE = "Eastern",
  IE = "Western", EL = "Southern", ES = "Southern", FR = "Western",
  HR = "Eastern", IT = "Southern", CY = "Southern", LV = "Eastern",
  LT = "Eastern", LU = "Western", HU = "Eastern", MT = "Southern",
  NL = "Western", AT = "Western", PL = "Eastern", PT = "Southern",
  RO = "Eastern", SI = "Eastern", SK = "Eastern", FI = "Nordic", SE = "Nordic"
)

country_region<- factor(country_region[rownames(mfa_res$ind$coord)])

#Centroid calculation
coord_df <- as.data.frame(mfa_res$ind$coord)
coord_df$Region <- country_region[rownames(coord_df)]
centroids <- coord_df %>%
  group_by(Region) %>%
  summarize(Dim1 = mean(Dim.1), Dim2 = mean(Dim.2))

#Ellipses by region with centroids 
fviz_mfa_ind(mfa_res,
             habillage = country_region,  # factor of region
             addEllipses = TRUE,
             repel = TRUE) +
  geom_point(data = centroids,
             aes(x = Dim1, y = Dim2, color = Region),
             shape = 8, size = 4, stroke = 1.5, inherit.aes = FALSE) +
  geom_text(data = centroids,
            aes(x = Dim1, y = Dim2, label = Region, color = Region),
            fontface = "bold", vjust = -1, inherit.aes = FALSE)


###Dimensions 3 and 4 Analysis###
fviz_mfa_ind(mfa_res, axes = c(3, 4))
dimdesc(mfa_res, axes = 3:4, proba = 0.05)
dimdesc(mfa_demo_res, axes = 3:4, proba = 0.05)




###Repeat process above on Volume B###
# Combine into one  data set
demo_long <- bind_rows(volume_b_long[mfa_questions])

# Filter, relabel, and reverse codes based on mappings used above
demo_long <- demo_long %>%
  filter(!label %in% drop_labels) %>%
  mutate(label = recode(label, !!!label_map)) %>%
  left_join(rev_map, by = c("Question", "label" = "label_orig")) %>%
  mutate(label = coalesce(label_new, label)) %>%
  select(-label_new)

# Pivot to wide format: rows = demographic group, cols = question + label
demog_wide <- demo_long %>%
  unite("item", Question, label, sep = "_") %>%
  pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
  column_to_rownames("group")

# Normalize each row to sum to 1
demog_wide <- demog_wide / rowSums(demog_wide)

# Remove rows with NA or zero variance
demog_wide <- demog_wide[rowSums(is.na(demog_wide)) == 0, ]
demog_wide <- demog_wide[apply(demog_wide, 1, var) > 0, ]

# Define blocks again for demog_wide using same logic
block_impact_demo <- grep("^(QB1_)|(QB5_)|(QB8_)", colnames(demog_wide), value = TRUE)
block_skills_demo <- grep("^(QB2_)", colnames(demog_wide), value = TRUE)
block_mgmt_demo   <- grep("^(QB3_)|(QB4_)|(QB7_)|(QB9_)|(QB10_)", colnames(demog_wide), value = TRUE)
block_rules_demo  <- grep("^(QB6_)|(QB11_)", colnames(demog_wide), value = TRUE)


grouped_blocks_demo <- list(
  Impact = block_impact,
  Skills = block_skills,
  Management = block_mgmt,
  Governance = block_rules
)

group_sizes_demo <- sapply(grouped_blocks_demo, length)
group_names_demo <- names(grouped_blocks_demo)

### Run MFA on Demographic Profiles ###
mfa_demo_res <- MFA(
  demog_wide,
  group = group_sizes_demo,
  type = rep("f", length(group_sizes_demo)),
  name.group = group_names_demo,
  graph = FALSE
)

#Scree plot of explained variance
fviz_screeplot(mfa_demo_res, addlabels = TRUE)

# Variable contributions (response categories)
fviz_mfa_var(mfa_demo_res, "group", repel = TRUE)

fviz_mfa_var(mfa_demo_res,
             choice    = "group",
             axes      = c(1, 2),
             repel     = TRUE,
             col.var   = "contrib")

fviz_mfa_var(mfa_demo_res,
             choice    = "group",
             axes      = c(1, 2),
             repel     = TRUE,
             col.var   = "cos2")
#Demographic contribution
fviz_mfa_ind(mfa_demo_res,
             repel = TRUE,
             col.ind = "cos2",     
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#Group mapping to header (overall demographic) and subheader (value of demographic)
label_mapping <- tribble(
  ~full_label,                                                           ~header,                           ~subheader,
  "Gender_Man",                                                          "Gender",                          "Man",
  "Gender_Woman",                                                        "Gender",                          "Woman",
  "Age_15-24",                                                           "Age",                             "Youth",
  "Age_25-34",                                                           "Age",                             "Young Adult",
  "Age_35-44",                                                           "Age",                             "Adult",
  "Age_45-54",                                                           "Age",                             "Adult",
  "Age_55-64",                                                           "Age",                             "Adult",
  "Age_65-74",                                                           "Age",                             "Senior",
  "Generation_1946_-_1964_'BB'",                                         "Generation",                      "Baby Boomers",
  "Generation_1965_-_1980_'X'",                                          "Generation",                      "Gen X",
  "Generation_1981_–_1996_'Millenials'",                                 "Generation",                      "Millennials",
  "Generation_1997_and_beyond_'Generation_Z'",                           "Generation",                      "Gen Z",
  "Education_(End_Of)_15-",                                              "Education_End",                   "Out of School 15 years",
  "Education_(End_Of)_16-19",                                            "Education_End",                   "Out of School 16–19 years",
  "Education_(End_Of)_20+",                                              "Education_End",                   "Out of School 20+ years",
  "Level_of_Diploma_Pre-primary_education_(include_no_education)",      "Level_of_Diploma",                "Pre-primary Education",
  "Level_of_Diploma_Primary_education",                                 "Level_of_Diploma",                "Primary Education",
  "Level_of_Diploma_Lower_secondary_education",                         "Level_of_Diploma",                "Lower secondary Education",
  "Level_of_Diploma_Upper_secondary_education",                         "Level_of_Diploma",                "Upper secondary Education",
  "Level_of_Diploma_Post-secondary_non_tertiary_(including_pre-vocational_or_vocational_education)", "Level_of_Diploma", "Post-secondary Education",
  "Level_of_Diploma_Short-cycle_tertiary",                              "Level_of_Diploma",                "Short-cycle tertiary Education",
  "Level_of_Diploma_Bachelor_or_equivalent",                            "Level_of_Diploma",                "Bachelor Education",
  "Level_of_Diploma_Master_or_equivalent",                              "Level_of_Diploma",                "Master Education",
  "Level_of_Diploma_Doctoral_or_equivalent",                            "Level_of_Diploma",                "Doctoral Education",
  "Socio-professional_category_Self-_employed",                         "Socio-professional_category",     "Self-employed",
  "Socio-professional_category_Managers",                               "Socio-professional_category",     "Managers",
  "Socio-professional_category_Other_white_collars",                    "Socio-professional_category",     "Other white collars",
  "Socio-professional_category_Manual_workers",                         "Socio-professional_category",     "Manual workers",
  "Socio-professional_category_House_persons",                          "Socio-professional_category",     "House persons",
  "Socio-professional_category_Unemployed",                             "Socio-professional_category",     "Unemployed",
  "Socio-professional_category_Retired",                                "Socio-professional_category",     "Retired",
  "Socio-professional_category_Students",                               "Socio-professional_category",     "Students",
  "Marital_status_Married",                                             "Marital_status",                  "Married",
  "Marital_status_Single_living_with_a_partner",                        "Marital_status",                  "Single living w partner",
  "Marital_status_Single",                                              "Marital_status",                  "Single",
  "Marital_status_Divorced_or_separated",                               "Marital_status",                  "Separated",
  "Marital_status_Widow",                                               "Marital_status",                  "Widow",
  "Use_of_the_Internet_Everyday",                                       "Use_of_the_Internet",             "Everyday on Internet ",
  "Use_of_the_Internet_Never",                                          "Use_of_the_Internet",             "Never on Internet",
  "Use_of_the_Internet_Often/_Sometimes",                               "Use_of_the_Internet",             "Often/Sometimes on Internet",
  "Satisfaction_with_democracy_in_one's_country_Satisfied",             "Satisfaction_country",            "Satisfied with Country",
  "Satisfaction_with_democracy_in_one's_country_Not_satisfied",         "Satisfaction_country",            "Not satisfied with Country",
  "Satisfaction_with_democracy_in_the_EU_Satisfied",                    "Satisfaction_EU",                 "Satisfied with EU",
  "Satisfaction_with_democracy_in_the_EU_Not_satisfied",                "Satisfaction_EU",                 "Not satisfied with EU",
  "Perception_of_the_use_of_robots_and_Artificial_Intelligence_Positive", "AI_Robots_Perception",         "AI/Robots are Positive",
  "Perception_of_the_use_of_robots_and_Artificial_Intelligence_Negative", "AI_Robots_Perception",         "AI/Robots are Negative",
  "View_on_the_impact_of_digital_technologies_Totally_positive",        "Digital_Tech_Impact",             "Dig Tech is Totally positive",
  "View_on_the_impact_of_digital_technologies_Totally_negative",        "Digital_Tech_Impact",             "Dig Tech is Totally negative",
  "Including_yourself,_how_many_people_in_total_work_at_your_workplace,_that_is_at_the_local_site_where_you_work?_1", "Workplace_Size", "Workplace of 1",
  "Including_yourself,_how_many_people_in_total_work_at_your_workplace,_that_is_at_the_local_site_where_you_work?_2-9", "Workplace_Size", "Workplace of 2–9",
  "Including_yourself,_how_many_people_in_total_work_at_your_workplace,_that_is_at_the_local_site_where_you_work?_10-49", "Workplace_Size", "Workplace of 10–49",
  "Including_yourself,_how_many_people_in_total_work_at_your_workplace,_that_is_at_the_local_site_where_you_work?_50-250", "Workplace_Size", "Workplace of 50–250",
  "Including_yourself,_how_many_people_in_total_work_at_your_workplace,_that_is_at_the_local_site_where_you_work?_More_than_250", "Workplace_Size", "Workplace of 250+",
  "Household_situation_Single_Household_without_children",              "Household_situation",             "Single Household (no children)",
  "Household_situation_Single_Household_with_children",                 "Household_situation",             "Single_Household (Children)",
  "Household_situation_Multiple_Household_without_children",            "Household_situation",             "Multiple Household (no children)",
  "Household_situation_Household_with_children",                        "Household_situation",             "Household (children)",
  "Household_composition_1",                                            "Household_composition",           "1 in Household",
  "Household_composition_2",                                            "Household_composition",           "2 in Household",
  "Household_composition_3",                                            "Household_composition",           "3 in Household",
  "Household_composition_4+",                                           "Household_composition",           "4+ in Household",
  "Difficulties_paying_bills_Most_of_the_time",                        "Difficulties_paying_bills",       "Difficulty paying bills",
  "Difficulties_paying_bills_From_time_to_time",                        "Difficulties_paying_bills",       "Some Difficulty paying bills ",
  "Difficulties_paying_bills_Almost_never/_Never",                      "Difficulties_paying_bills",       "No Difficulty paying bills ",
  "Consider_belonging_to_The_working_class",                            "Consider_belonging_to",           "Working Class",
  "Consider_belonging_to_The_lower_middle_class",                       "Consider_belonging_to",           "Lower/Middle Class",
  "Consider_belonging_to_The_middle_class",                             "Consider_belonging_to",           "Middle Class",
  "Consider_belonging_to_The_upper_middle_class",                       "Consider_belonging_to",           "Upper/middle Class",
  "Consider_belonging_to_The_upper_class",                              "Consider_belonging_to",           "Upper Class",
  "Subjective_urbanisation_Rural_village",                              "Subjective_urbanisation",         "Rural",
  "Subjective_urbanisation_Small/_mid_size_town",                       "Subjective_urbanisation",         "Small/mid size town",
  "Subjective_urbanisation_Large_town",                                 "Subjective_urbanisation",         "Large town",
  "Political_interest_index_++",                                        "Political_interest_index",        "Very politically interested",
  "Political_interest_index_+",                                         "Political_interest_index",        "Somewhat politically interested",
  "Political_interest_index_-",                                         "Political_interest_index",        "Somewhat politically disinterested",
  "Political_interest_index_--",                                        "Political_interest_index",        "Very politically disinterested",
  "Left-right_political_scale_Left",                                    "Left-right_political_scale",      "Politically Left",
  "Left-right_political_scale_Centre",                                  "Left-right_political_scale",      "Politically Centre",
  "Left-right_political_scale_Right",                                   "Left-right_political_scale",      "Politically Right",
  "Talk_about_European_political_matters_Frequently",                  "Talk_about_European_political_matters", "Frequently talk about politics",
  "Talk_about_European_political_matters_Occasionally",                "Talk_about_European_political_matters", "Occasionally talk about politics",
  "Talk_about_European_political_matters_Never",                       "Talk_about_European_political_matters", "Never talk about politics",
  "Image_of_the_EU_Positive",                                           "Image_of_the_EU",                 "Positive EU Image",
  "Image_of_the_EU_Neutral",                                            "Image_of_the_EU",                 "Neutral EU Image",
  "Image_of_the_EU_Negative",                                           "Image_of_the_EU",                 "Negative EU Image",
  "Things_in_country_are_going_in…_Right_direction",                    "Things_in_country_are_going_in…", "My country is going in the right direction",
  "Things_in_country_are_going_in…_Wrong_direction",                    "Things_in_country_are_going_in…", "My country is going in the wrong direction",
  "Things_in_country_are_going_in…_Neither",                            "Things_in_country_are_going_in…", "My country is going in a neutral direction",
  "Things_in_the_EU_are_going_in…_Right_direction",                     "Things_in_the_EU_are_going_in…",  "The EU is going in the right direction",
  "Things_in_the_EU_are_going_in…_Wrong_direction",                     "Things_in_the_EU_are_going_in…",  "The EU is giong in the wrong direction",
  "Things_in_the_EU_are_going_in…_Neither",                             "Things_in_the_EU_are_going_in…",  "The EU is going in a neutral direction",
  "Things_in_your_life_are_going_in…_Right_direction",                  "Things_in_your_life_are_going_in…", "My life is going in the right direction",
  "Things_in_your_life_are_going_in…_Wrong_direction",                  "Things_in_your_life_are_going_in…", "My life is going in the wrong direction",
  "Things_in_your_life_are_going_in…_Neither",                           "Things_in_your_life_are_going_in…", "My life is going in a neutral direction",
  "My_voice_counts_in_the_EU_Total_'Agree'",                            "EU Voice",                          "My voice counts in the EU",
  "My_voice_counts_in_the_EU_Total_'Disagree'",                         "EU Voice",                          "My voice doesn't count in the EU",
  "My_voice_counts_in_(OUR_COUNTRY)_Total_'Agree'",                     "Country Voice",                      "My voice counts in my country",
  "My_voice_counts_in_(OUR_COUNTRY)_Total_'Disagree'",                  "Country Voice",                     "My voice doesn't count in my country", 
  "Awareness_of_the_use_that_your_employer_makes_of_digital_technologies,_including_Artificial_Intelligence_Aware", "AI in the Workplace Awareness", "Aware", 
  "Awareness_of_the_use_that_your_employer_makes_of_digital_technologies,_including_Artificial_Intelligence_Unaware", "AI in the Workplace Awareness", "Unaware", 
  "Current_working_status_Currently_not_working",                        "Current_working_status",          "Currently not working",
  "Current_working_status_Currently_working",                            "Current_working_status",          "Currently working",
  "Currently_not_working_Previously_working",                            "Currently_not_working",           "Previously worked",
  "Currently_not_working_Never_did_any_paid_work",                       "Currently_not_working",           "Never worked",
  "What_sector,_business_or_industry_is_the_company_or_organisation_where_you_work_mainly_active_in?_Agriculture,_forestry_and_fishing", 
  "Sector",                           "Agriculture/Forestry/Fishing Sector",
  "What_sector,_business_or_industry_is_the_company_or_organisation_where_you_work_mainly_active_in?_Manufacturing", 
  "Sector",                           "Manufacturing Sector",
  "What_sector,_business_or_industry_is_the_company_or_organisation_where_you_work_mainly_active_in?_Logistics", 
  "Sector",                           "Logistics Sector",
  "What_sector,_business_or_industry_is_the_company_or_organisation_where_you_work_mainly_active_in?_Service,_including_retail_trade,_accommodation,_transportation,_food_services", 
  "Sector",                           "Retail/Accommodation/Food Sector",
  "What_sector,_business_or_industry_is_the_company_or_organisation_where_you_work_mainly_active_in?_Public_sector", 
  "Sector",                           "Public sector Sector"
  )

# Keep only this version (it's complete and adds header info):
coords_demo <- as.data.frame(mfa_demo_res$ind$coord) %>%
  rownames_to_column("full_label") %>%
  left_join(label_mapping, by = "full_label") %>% 
  mutate(predictor = header)  # consistent terminology
coords_demo$group <- rownames(coords_demo)

# Extract categorical predictors
coords_demo$predictor <- demo_predictors


# Means (centroids) per demographic group
group_centroids <- coords_demo %>%
  group_by(header) %>%
  summarize(across(starts_with("Dim."), mean)) %>%
  mutate(dist_from_origin = sqrt(Dim.1^2 + Dim.2^2)) %>%
  arrange(desc(dist_from_origin))


# Select top N most influential demographic categories
top_headers <- group_centroids %>%
  slice_max(order_by = dist_from_origin, n = 5)  # adjust `n` if you want more or fewer

# Plot centroids of the most important headers
ggplot(top_headers, aes(x = Dim.1, y = Dim.2, label = header)) +
  geom_point(color = "#FC4E07", size = 4) +
  geom_text_repel(size = 4.5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Most Influential Demographic Categories",
    x = "MFA Dimension 1",
    y = "MFA Dimension 2"
  )

# For each of the top 5 headers, find the 2 most extreme subheaders
extreme_coords <- coords_demo %>%
  filter(header %in% top_headers$header) %>%
  group_by(header) %>%
  mutate(dist_to_centroid = sqrt((Dim.1 - mean(Dim.1))^2 + (Dim.2 - mean(Dim.2))^2)) %>%
  slice_max(order_by = dist_to_centroid, n = 2) %>%  # take the two furthest levels per group
  ungroup()


ggplot(extreme_coords, aes(x = Dim.1, y = Dim.2, color = header, label = subheader)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, max.overlaps = Inf, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Most Distinctive Subgroups within Top Demographic Categories",
    x = "MFA Dimension 1",
    y = "MFA Dimension 2",
    color = "Demographic Group"
  )

#All Demographic values 
# Use 'header' as the grouping (not predictor)
ggplot(coords_demo, aes(x = Dim.1, y = Dim.2, label = subheader, color = header)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(size = 2, max.overlaps = 100) +
  theme_minimal() +
  labs(
    title = "All Demographic Subgroups in MFA Space",
    x = "MFA Dimension 1",
    y = "MFA Dimension 2",
    color = "Demographic Group"  # This renames the legend to "Demographic Group"
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))  # optional: make legend dots clearer

# Add distance from origin (for influence)
coords_demo <- coords_demo %>%
  mutate(dist_from_origin = sqrt(Dim.1^2 + Dim.2^2))

# Select the 25 most extreme subgroups in MFA space
top_coords <- coords_demo %>%
  slice_max(order_by = dist_from_origin, n = 25)

# Plot those top 25
ggplot(top_coords, aes(x = Dim.1, y = Dim.2, label = subheader, color = header)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3.5, max.overlaps = 100) +
  theme_minimal() +
  labs(
    title = "Top 25 Most Distinctive Demographic Subgroups in MFA Space",
    x = "MFA Dimension 1",
    y = "MFA Dimension 2",
    color = "Demographic Group"
  )
#Dimension Descriptions 
dimdesc(mfa_demo_res, axes = 1:2, proba = 0.05)

#Dimensions 3 and 4
# Filter top contributing demographic coords on Dim 3 or 4
coords_demo_34 <- coords_demo %>%
  mutate(
    cos2_34 = mfa_demo_res$ind$cos2[, 3] + mfa_demo_res$ind$cos2[, 4]
  ) %>%
  slice_max(order_by = cos2_34, n = 20)  # adjust n if needed

# Plot just the top contributors
ggplot(coords_demo_34, aes(x = Dim.3, y = Dim.4, label = subheader, color = header)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3.5, max.overlaps = 100, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Top 20 Demographic Contributors to Dimensions 3 & 4",
    x = "MFA Dimension 3",
    y = "MFA Dimension 4",
    color = "Demographic Group"
  )





###Dimension Labeling###
# 1/K threshold (%)

K <- ncol(mfa_wide)              
crit <- 100 / K                         # 1 / K  (%)

contrib <- mfa_res$freq$contrib   

dim1_keep <- contrib[ contrib[,1] >= crit , 1]
dim2_keep <- contrib[ contrib[,2] >= crit , 2]

#Contribution of active category to total inertia on that axis
cat("\nAxis‑1 drivers:\n"); print(sort(dim1_keep, TRUE))
cat("\nAxis‑2 drivers:\n"); print(sort(dim2_keep, TRUE))

#Signed coordinates for interpretation 
coord_mat <- mfa_res$freq$coord
coord_dim1 <- coord_mat[names(dim1_keep), 1]
coord_dim2 <- coord_mat[names(dim2_keep), 2]

#Full descriptive tables - Volume A 
axis_descr <- dimdesc(mfa_res, axes = 1:2, proba = 0.05)
# Positive and negative poles; correlation table 
axis_descr$Dim.1$quanti
axis_descr$Dim.2$quanti

#Full descriptive tables - Volume B
axis_descr_demo <- dimdesc(mfa_demo_res, axes = 1:2, proba = 0.05)
# Positive and negative poles; correlation table 
axis_descr_demo$Dim.1$quanti
axis_descr_demo$Dim.2$quanti

# Hierarchical clustering on principal components - countries 
hcpc_res <- HCPC(mfa_res, consol = TRUE, graph = FALSE)
fviz_cluster(hcpc_res, repel = TRUE)
# Euclidean distance on the first two MFA dimensions
d <- dist(mfa_res$ind$coord[, 1:2])
h <- hclust(d, method = "ward.D2")
plot(h, labels = rownames(mfa_res$ind$coord), main = "Ward dendrogram (Dim 1–2 space)")



# Hierarchical clustering on principal components - demographics 
hcpc_demo_res <- HCPC(mfa_demo_res, consol = TRUE, graph = FALSE)
fviz_cluster(hcpc_demo_res, repel = TRUE)
# Euclidean distance on the first two MFA dimensions
d_demo <- dist(mfa_demo_res$ind$coord[, 1:2])
h_demo <- hclust(d_demo, method = "ward.D2")
plot(h_demo, labels = rownames(mfa_demo_res$ind$coord), main = "Ward dendrogram (Dim 1–2 space)")

