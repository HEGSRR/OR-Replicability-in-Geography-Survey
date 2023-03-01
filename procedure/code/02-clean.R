# list of required packages
packages = c("here","tidyverse","table1","officer")

# load and install required packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# save the R processing environment to r_environment.txt
writeLines(capture.output(sessionInfo()),here("procedure","environment","r_environment.txt"))

#--------------------------------------------#

#- Read in and limit data file to completes -#

#--------------------------------------------#
raw_hegs_rpl <- readRDS(here("data","raw","public","raw_hegs_rpl.rds"))

int_hegs_rpl <- raw_hegs_rpl %>% filter(Progress > 70 & Q1_age != "Under 18")
summary(int_hegs_rpl$Progress)
table(int_hegs_rpl$Progress)

table(toupper(int_hegs_rpl$Q3_subfield_5_TEXT))

#-Open response areas of specialization to recode-#
hum_list  <- c("CULTURAL GEOGRAPHY", "URBAN GEOGRAPHY", "URBAN PLANNING AND GEOGRAPHY", "URBAN STUDIES",
               "MEDICAL GEOGRAPHY", "POLITICAL SCIENCE", "PSYCHOLOGY", "REDISTRICTING", "REGIONAL SCIENCE", "SOCIOLOGY",
               "CRIMINAL JUSTICE & CRIMINOLOGY", "ECONOMIC GEOGRAPHY", "ECONOMIC GEOGRAPHY, URBAN STUDIES, PLANNING", 
               "ECONOMICS", "ECONOMY", "INNOVATION", "INNOVATION STUDIES",
               "BUILT ENVIRONMENT AND TRAVEL BEHAVIOR; ACCESSIBILITY; SUSTAINABLE MOBILITY", 
               "MULTI REGIONAL INPUT-OUTPUT", "REAL ESTATE", "TOURISM AND LEISURE", "TRANSPORTATION")

phys_list <- c("PHYSICAL OCEANOGRAPHY", "PHYSICAL SCIENCES", "OCEANOGRAPHY, CLIMATE", "PALEOCLIMATE AND PALEOENVIRONMENT", 
                "BIOGEOGRAPHY","PALEOBOTANY","PALEOCLIMATOLOGY",
               "LANDSCAPE GENETICS GENOMICS", "PHYLOGEOGRAPHY", "BIOGEOGRAPHY", "BIOGEOGRAPHY, LANDSCAPE ECOLOGY" , "ECOLOGY", "ECOLOGY AND EVOLUTION", 
               "ECOLOGY/BIOGEOGRAPHY", "PLANT ECOLOGY", "WILDLIFE ECOLOGY")

nat_list  <- c("ARCHAEOLOGY", "HAZARD AND RISK",
               "LAN DUSE PLANNING SYSTEMS", "LAND USE", "RESILIENCE AND CLIMATE CHANGE",
               "ENVIRONMENTAL ECONOMICS", "ENVIRONMENTAL PLANNING", "ENVIRONMENTAL SOCIOLOGY")

gis_list  <- c("SPATIAL ANALYSIS OF LABOR MARKET", "GEOGRAPHY EDUCATION (SPECIFICALLY GIS)", 
               "VOLUNTEERED GEOGRAPHIC INFORMATION", "CQRTOGRAPHY, SEMIOLOGY OF GRAPHICS", "MAP PROJECTION")

int_hegs_rpl <- int_hegs_rpl %>% 
  mutate(Q3_recoded = as.factor(ifelse(toupper(Q3_subfield_5_TEXT) %in% hum_list, 1, 
                                ifelse(toupper(Q3_subfield_5_TEXT) %in% phys_list, 2, 
                                ifelse(toupper(Q3_subfield_5_TEXT) %in% nat_list, 3, 
                                ifelse(toupper(Q3_subfield_5_TEXT) %in% gis_list, 4, Q3_subfield))))))
levels(int_hegs_rpl$Q3_recoded) <- c("Human", 
                                     "Physical",
                                     "Nature/society", 
                                     "Methods",
                                     "Other",
                                     "NA")

table(int_hegs_rpl$Q3_recoded,int_hegs_rpl$Q3_subfield)
## Note: 3 respondents skipped Q3 

#---------------------------------------------------------------------------------------------------------------------------

table(toupper(int_hegs_rpl$Q25_title_9_TEXT))

full_prof <- c("RETIRED PROFESSOR","RETIRED","SENIOR LECTURER")
asst_prof <- c("ADJUNCT PROFESSOR")
res_scientist <- c("SENIOR RESEARCH FELLOW","PART-TIME RESEARCHER","RESEARCH FELLOW","INDEPENDENT RESEARCHER")
other <- c("UNDERGRAD ACADEMIC PROGRAM DIRECTOR", "USED TO BE A POSTDOC, NOW NOT IN DEVELOPMENT WORK")

int_hegs_rpl <- int_hegs_rpl %>% 
  mutate(Q25_recoded = as.factor(ifelse(toupper(Q25_title_9_TEXT) %in% res_scientist, 5, 
                                 ifelse(toupper(Q25_title_9_TEXT) == "Lecturer/teacher at a university of applied sciences.", 2, Q25_title))))

levels(int_hegs_rpr$Q25_recoded) <- c("Full professor/lecturer",
                                      "Associate professor/lecturer",
                                      "Assistant professor/lecturer",
                                      "Laboratory director/head",
                                      "Research scientist",
                                      "Post-doctoral fellow",
                                      "Graduate student (PhD, masters)",
                                      "Technician/research assistant",
                                      "Other (specify)",
                                      "NA")

table(int_hegs_rpl$Q25_recoded,toupper(int_hegs_rpl$Q25_title))

#----------------------------------------------------------#

#- Update NAs with valid values to reflect question logic -#

#----------------------------------------------------------#



#----------------------------------------------------------#

#- Convert character to ordered factors-#

#----------------------------------------------------------#
# convert agree questions into ordered factors
values <- c("Strongly agree", "Agree", "Disagree","Strongly disagree","Don't Know")
questions <- c("Q7_value_1", "Q7_value_2", "Q7_value_3", "Q7_value_4","Q7_value_5")
int_hegs_rpl[questions] <- lapply(int_hegs_rpl[questions], 
                                  factor, 
                                  levels=values,
                                  ordered = TRUE,
                                  exclude=c(""))

# convert increase questions into ordered factors
values <- c("Very likely to increase", "Somewhat likely to increase", "Not likely to affect", "Somewhat likely to decrease","Very likely to decrease","Don't Know")
questions <- c("Q8_study_factors_1", "Q8_study_factors_2", "Q8_study_factors_3", "Q8_study_factors_4",
               "Q8_study_factors_5", "Q8_study_factors_6", "Q8_study_factors_7", "Q8_study_factors_8",
               "Q8_study_factors_9", "Q8_study_factors_10",
               "Q10_phen_factors_1", "Q10_phen_factors_2", "Q10_phen_factors_3", "Q10_phen_factors_4",
               "Q10_phen_factors_5", "Q10_phen_factors_6")
int_hegs_rpl[questions] <- lapply(int_hegs_rpl[questions], 
                                  factor, 
                                  levels=values,
                                  ordered = TRUE,
                                  exclude=c(""))

# convert frequency questions into ordered factors
values <- c("Always", "Frequently", "Occasionally", "Rarely", "Never", "Don't Know")
questions <- c("Q15_decision_factors_1", "Q15_decision_factors_2", "Q15_decision_factors_3", 
               "Q15_decision_factors_4", "Q15_decision_factors_5", "Q15_decision_factors_6", 
               "Q15_decision_factors_7", "Q15_decision_factors_8", "Q15_decision_factors_9", 
               "Q15_decision_factors_10", "Q15_decision_factors_11", "Q15_decision_factors_12")
int_hegs_rpl[questions] <- lapply(int_hegs_rpl[questions], 
                                  factor, 
                                  levels=values,
                                  ordered = TRUE,
                                  exclude="")


#--------------------------------#

#- Perform Qualitative Coding -#

#--------------------------------#

analysis_hegs_rpl %>% 
  select(c("ResponseId","Q6_definition")) %>% 
  write.csv(here("data","derived","public","q6_coding.csv"))

saveRDS(int_hegs_rpl, here("data","derived","public","analysis_hegs_rpl.rds"))
