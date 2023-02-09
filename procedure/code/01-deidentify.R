library(here)
library(tidyverse)

raw_rpl_responses <- readRDS(here("data","raw","private","raw_rpl_responses.rds"))
head(raw_rpl_responses)
str(raw_rpl_responses)

#Pick prize draw winners among those who want to be considered
table(raw_rpl_responses$Q27_1) ## 161 would like to be included in prize draw
set.seed(1114)
floor(runif(3,1,161))

raw_rpl_responses %>% 
                  filter(!is.na(Q27_1))  %>% 
                  slice(32, 98, 128) %>% 
                  select(c(RecipientLastName,RecipientEmail,Q26_country))

hegs_rpl_deidentified <- raw_rpl_responses %>% 
  select(-c(RecipientLastName, 
            RecipientFirstName, 
            RecipientEmail,
            IPAddress,
            Status,
            ExternalReference,
            LocationLatitude,
            LocationLongitude,
            DistributionChannel,
            UserLanguage))

saveRDS(hegs_rpl_deidentified, here("data","raw","public","raw_hegs_rpl.rds"))
