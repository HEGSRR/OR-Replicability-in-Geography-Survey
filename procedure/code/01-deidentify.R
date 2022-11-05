library(here)
library(tidyverse)

raw_rpl_responses <- readRDS(here("data","raw","private","raw_rpl_responses.rds"))
head(raw_rpl_responses)
str(raw_rpl_responses)

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
