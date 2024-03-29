# Code for downloading survey data from Qualtrics API

library(qualtRics)
library(svDialogs)
library(here)

qualtrics_url <- dlgInput("Qualtrics URL:","asu.co1.qualtrics.com")$res
qualtrics_key <- dlgInput("Qualtrics API Key:","daspM3PJaaoQtTXfZbWsWHpfDEbXELO3Cm5OgEh1")$res 
rpl_survey_id <- dlgInput("Survey ID:","SV_3X9E43dL6bbogyq")$res
# find API key and survey ID in Qualtrics "my account" info --> Qualtrics IDs menu
# find URL in the address bar while you're logged into your Qualtrics projects

qualtrics_api_credentials(api_key = qualtrics_key, 
                          base_url = qualtrics_url, # e.g. "middlebury.az1.qualtrics.com"
                          install = TRUE,
                          overwrite = TRUE)

readRenviron("~/.Renviron")

# get survey metadata as nested lists, incl questions and possible responses
rpl_survey_metadata <- metadata(rpl_survey_id)

# get data frame with four columns with question ID, name, text and required 
rpl_survey_questions <- survey_questions(rpl_survey_id)

# get survey responses as data frame - ordinal responses are ordered factors
rpl_responses <- fetch_survey(rpl_survey_id)

saveRDS(rpl_responses, here("data","raw","private","raw_rpl_responses.rds"))
saveRDS(rpl_survey_questions, here("data","raw","private","raw_rpl_survey_questions.rds"))
saveRDS(rpl_survey_metadata, here("data","raw","private","raw_rpl_survey_metadata.rds"))

saveRDS(rpl_survey_questions, here("data","raw","public","raw_rpl_survey_questions.rds"))
saveRDS(rpl_survey_metadata, here("data","raw","public","raw_rpl_survey_metadata.rds"))

