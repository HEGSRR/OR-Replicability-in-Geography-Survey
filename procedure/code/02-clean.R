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
completes <- read.csv(here("data","raw","private","final_classification_Survey-Response-Tracking_qualtrics_update_v4.csv"))
raw_hegs_rpl <- left_join(raw_hegs_rpl, completes, by = c("ResponseId" = "Response.ID"))

int_hegs_rpl <- raw_hegs_rpl %>% filter(Disposition.Code %in% c(1.1,1.2))
#int_hegs_rpl <- raw_hegs_rpl %>% filter(Progress > 70 & Q1_age != "Under 18")
summary(int_hegs_rpl$Progress)


table(toupper(int_hegs_rpl$Q3_subfield_5_TEXT))

#Not sure how to classify
# "INNOVATION", "INNOVATION STUDIES"


#-Open response areas of specialization to recode-#
hum_list  <- c("CULTURAL GEOGRAPHY", "URBAN GEOGRAPHY", "URBAN PLANNING AND GEOGRAPHY", "URBAN STUDIES",
               "MEDICAL GEOGRAPHY", "POLITICAL SCIENCE", "PSYCHOLOGY", "REDISTRICTING", "REGIONAL SCIENCE", "SOCIOLOGY",
               "CRIMINAL JUSTICE & CRIMINOLOGY", "ECONOMIC GEOGRAPHY", "ECONOMIC GEOGRAPHY, URBAN STUDIES, PLANNING", 
               "ECONOMICS", "ECONOMY")

phys_list <- c("PHYSICAL OCEANOGRAPHY", "PHYSICAL SCIENCES", "OCEANOGRAPHY, CLIMATE", "PALEOCLIMATE AND PALEOENVIRONMENT", 
               "RESILIENCE AND CLIMATE CHANGE", "BIOGEOGRAPHY","PALEOBOTANY","PALEOCLIMATOLOGY",
               "ENVIRONMENTAL ECONOMICS", "ENVIRONMENTAL PLANNING", "ENVIRONMENTAL SOCIOLOGY")

nat_list  <- c("ARCHAEOLOGY","LANDSCAPE GENETICS GENOMICS", "PHYLOGEOGRAPHY", "HAZARD AND RISK",
               "BIOGEOGRAPHY", "BIOGEOGRAPHY, LANDSCAPE ECOLOGY" , "ECOLOGY", "ECOLOGY AND EVOLUTION", 
               "ECOLOGY/BIOGEOGRAPHY", "PLANT ECOLOGY", "WILDLIFE ECOLOGY", 
               "BUILT ENVIRONMENT AND TRAVEL BEHAVIOR; ACCESSIBILITY; SUSTAINABLE MOBILITY", 
               "LAN DUSE PLANNING SYSTEMS", "LAND USE","MULTI REGIONAL INPUT-OUTPUT",
               "REAL ESTATE", "TOURISM AND LEISURE", "TRANSPORTATION")

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


