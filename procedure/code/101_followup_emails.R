library(tidyverse)
library(stringr)
library(here)


sample <- read.csv(here("data","raw","private","replicability_sample.csv"))
distribution <- read.csv(here("data","raw","private","HEGSReplicability-Distribution_History-4.csv"))
disposition <- read.csv(here("data","raw","private","Survey-Response-Tracking_qualtrics_update_v4.csv"))

disposition <- disposition %>% mutate(email_name = paste(LastName,Email,""))
distribution <- distribution %>% mutate(email_name = paste(Last.Name,Email,""))

combined <- left_join(distribution, disposition, by = "Email")

follow_up_status <- combined %>%
  transmute(hegs_id,
            AuthID,
            Response.ID = Response.ID.x,
            LastName,
            Email,
            Disposition.Code,
            New.Email,
            Qualtrics_Status_Rd1,
            Qualtrics_Status_Rd2,
            Qualtrics_Status_Rd3 = Status,
            Notes)

table(follow_up_status$Qualtrics_Status_Rd3)
write.csv(follow_up_status, here("data","raw","private","Survey-Response-Tracking_qualtrics_update_v5.csv"))
