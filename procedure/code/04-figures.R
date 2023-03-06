library(here)
library(tidyverse)
library(table1)
library(officer)


#--------------------------------#
#-                              -#
#- PERFORM DESCRIPTIVE ANALYSIS -#
#-                              -#
#--------------------------------#
analysis_hegs_rpl <- readRDS(here("data","derived","public","analysis_hegs_rpl.rds"))

#Recode Q8 and Q10 
analysis_hegs_rpl <- analysis_hegs_rpl %>% 
  mutate_at(c("Q8_study_factors_1","Q8_study_factors_2","Q8_study_factors_3","Q8_study_factors_4",
              "Q8_study_factors_5","Q8_study_factors_6","Q8_study_factors_7","Q8_study_factors_8",
              "Q8_study_factors_9","Q8_study_factors_10",
              "Q10_phen_factors_1","Q10_phen_factors_2","Q10_phen_factors_3","Q10_phen_factors_4",
              "Q10_phen_factors_5","Q10_phen_factors_6"), funs(dplyr::recode(., 
                                                                       `Very likely to decrease`= -2, 
                                                                       `Somewhat likely to decrease`= -1, 
                                                                       `Not likely to affect` = 0,
                                                                       `Somewhat likely to increase` = 1,
                                                                       `Very likely to increase` = 2,
                                                                       .default = NaN))) %>% 
  mutate_at(c("Q15_decision_factors_1","Q15_decision_factors_2","Q15_decision_factors_3","Q15_decision_factors_4",
              "Q15_decision_factors_5","Q15_decision_factors_6","Q15_decision_factors_7","Q15_decision_factors_8",
              "Q15_decision_factors_9","Q15_decision_factors_10","Q15_decision_factors_11","Q15_decision_factors_12"), funs(dplyr::recode(., 
                                                                      `Never`= 0, 
                                                                      `Rarely`= 1, 
                                                                      `Occasionally` = 2,
                                                                      `Frequently` = 3,
                                                                      `Always` = 4,
                                                                      .default = NaN)))



# Label Q8, Q10, and Q15 variables
table1::label(analysis_hegs_rpl$Q8_study_factors_1) <- "Multiple hypotheses were tested"
table1::label(analysis_hegs_rpl$Q8_study_factors_2) <- "Quantitative methods were used"
table1::label(analysis_hegs_rpl$Q8_study_factors_3) <- "Qualitative methods were used"
table1::label(analysis_hegs_rpl$Q8_study_factors_4) <- "Mixed methods were used"
table1::label(analysis_hegs_rpl$Q8_study_factors_5) <- "Poor documentation of study methods"
table1::label(analysis_hegs_rpl$Q8_study_factors_6) <- "Restricted access data were used"
table1::label(analysis_hegs_rpl$Q8_study_factors_7) <- "Data were gathered from multiple sites"
table1::label(analysis_hegs_rpl$Q8_study_factors_8) <- "A large research team conducted the study"
table1::label(analysis_hegs_rpl$Q8_study_factors_9) <- "Relied on expertise unique to the researcher"
table1::label(analysis_hegs_rpl$Q8_study_factors_10) <- "Relied on the unique position of the researcher"
table1::label(analysis_hegs_rpl$Q10_phen_factors_1) <- "Spatially dependent upon itself"
table1::label(analysis_hegs_rpl$Q10_phen_factors_2) <- "Strongly related with local conditions"
table1::label(analysis_hegs_rpl$Q10_phen_factors_3) <- "Exhibits variation across locations"
table1::label(analysis_hegs_rpl$Q10_phen_factors_4) <- "Cannot be directly measured"
table1::label(analysis_hegs_rpl$Q10_phen_factors_5) <- "Cannot be directly manipulated"
table1::label(analysis_hegs_rpl$Q10_phen_factors_6) <- "Has multiple competing theoretical explanations"
table1::label(analysis_hegs_rpl$Q15_decision_factors_1) <- "Pressure to publish original research"
table1::label(analysis_hegs_rpl$Q15_decision_factors_2) <- "Low value of replication studies"
table1::label(analysis_hegs_rpl$Q15_decision_factors_3) <- "Low chances of replicating a result"
table1::label(analysis_hegs_rpl$Q15_decision_factors_4) <- "Lack of experience conducting replications"
table1::label(analysis_hegs_rpl$Q15_decision_factors_5) <- "Difficult publishing peer-reviewed replications"
table1::label(analysis_hegs_rpl$Q15_decision_factors_6) <- "Difficulty accessing/creating relevant data"
table1::label(analysis_hegs_rpl$Q15_decision_factors_7) <- "Insufficient information about original methods"
table1::label(analysis_hegs_rpl$Q15_decision_factors_8) <- "Difficulty recreating similar procedures"
table1::label(analysis_hegs_rpl$Q15_decision_factors_9) <- "Lack of funding for replication studies"
table1::label(analysis_hegs_rpl$Q15_decision_factors_10) <- "Fabrication of data or results by original authors"
table1::label(analysis_hegs_rpl$Q15_decision_factors_11) <- "Ethical concerns"
table1::label(analysis_hegs_rpl$Q15_decision_factors_12) <- "Known spatial variation in phenomena being studied"

#Create tables for Q8, Q10, and Q15
Q3_table4a <- table1::table1(~Q10_phen_factors_1 + Q10_phen_factors_2 + Q10_phen_factors_3 +
                              Q10_phen_factors_4 + Q10_phen_factors_5 + Q10_phen_factors_6 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table4a <- table1::table1(~Q10_phen_factors_1 + Q10_phen_factors_2 + Q10_phen_factors_3 +
                              Q10_phen_factors_4 + Q10_phen_factors_5 + Q10_phen_factors_6 | Q4_quantqual , data = analysis_hegs_rpl)

Q3_table3a <- table1::table1(~Q8_study_factors_1 + Q8_study_factors_2 + Q8_study_factors_3 + Q8_study_factors_4 +
                               Q8_study_factors_5 + Q8_study_factors_6 + Q8_study_factors_7 + Q8_study_factors_8 +
                               Q8_study_factors_9 + Q8_study_factors_10 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table3a <- table1::table1(~Q8_study_factors_1 + Q8_study_factors_2 + Q8_study_factors_3 + Q8_study_factors_4 +
                               Q8_study_factors_5 + Q8_study_factors_6 + Q8_study_factors_7 + Q8_study_factors_8 +
                               Q8_study_factors_9 + Q8_study_factors_10 | Q4_quantqual , data = analysis_hegs_rpl)

Q3_table5a <- table1::table1(~Q15_decision_factors_1 + Q15_decision_factors_2 + Q15_decision_factors_3 +
                              Q15_decision_factors_4 + Q15_decision_factors_5 + Q15_decision_factors_6 +
                              Q15_decision_factors_7 + Q15_decision_factors_8 + Q15_decision_factors_9 +
                              Q15_decision_factors_10 + Q15_decision_factors_11 + Q15_decision_factors_12 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table5a <- table1::table1(~Q15_decision_factors_1 + Q15_decision_factors_2 + Q15_decision_factors_3 +
                              Q15_decision_factors_4 + Q15_decision_factors_5 + Q15_decision_factors_6 +
                              Q15_decision_factors_7 + Q15_decision_factors_8 + Q15_decision_factors_9 +
                              Q15_decision_factors_10 + Q15_decision_factors_11 + Q15_decision_factors_12 | Q4_quantqual , data = analysis_hegs_rpl)


# Output tables using write table and flextable
write.table(Q3_table4a , here("results","tables","Table4a_PhenomChar_discipline.csv"), col.names = T, row.names=F, append= F, sep=',')
write.table(Q4_table4a , here("results","tables","Table4a_PhenomChar_method.csv"), col.names = T, row.names=F, append= F, sep=',')
write.table(Q3_table3a , here("results","tables","Table3a_StudyChar_discipline.csv"), col.names = T, row.names=F, append= F, sep=',')
write.table(Q4_table3a , here("results","tables","Table3a_StudyChar_method.csv"), col.names = T, row.names=F, append= F, sep=',')
write.table(Q3_table5a , here("results","tables","Table5a_Decision_discipline.csv"), col.names = T, row.names=F, append= F, sep=',')
write.table(Q4_table5a , here("results","tables","Table5a_Decision_method.csv"), col.names = T, row.names=F, append= F, sep=',')


#Create figures

#Convert to factors
q8_10_levels <- paste(c("Very likely to decrease","Somewhat likely to decrease","Not likely to affect",
                        "Somewhat likely to increase", "Very likely to increase"))
q15_levels <- paste(c("Never","Rarely","Occasionally", "Frequently", "Always"))

analysis_hegs_rpl[20:29] = lapply(analysis_hegs_rpl[20:29], factor, levels = -2:2, labels=q8_10_levels)
analysis_hegs_rpl[31:36] = lapply(analysis_hegs_rpl[31:36], factor, levels = -2:2, labels=q8_10_levels)
analysis_hegs_rpl[41:52] = lapply(analysis_hegs_rpl[41:52], factor, levels = 0:4, labels=q15_levels)
analysis_hegs_rpl <- as.data.frame(analysis_hegs_rpl) 

table(analysis_hegs_rpl$Q15_decision_factors_1)

library(likert)
q8_figures <- likert(analysis_hegs_rpl[20:29])
q10_figures <-likert(analysis_hegs_rpl[31:36])
q15_figures <-likert(analysis_hegs_rpl[41:52])

#Overall figure
png(here("results","figures","Q8_overall.png"), width = 450, height = 250, units='mm', res = 500)
plot(q8_figures, ordered = TRUE, group.order = names(analysis_hegs_rpl[20:29]))
dev.off() 

png(here("results","figures","Q10_overall.png"), width = 450, height = 250, units='mm', res = 500)
plot(q10_figures, ordered = TRUE, group.order = names(analysis_hegs_rpl[31:36]))
dev.off() 

png(here("results","figures","Q15_overall.png"), width = 450, height = 250, units='mm', res = 500)
plot(q15_figures, ordered = TRUE, centered = FALSE, group.order = names(analysis_hegs_rpl[41:52]))
dev.off() 


#Discipline figure

q8_disc_a = likert(analysis_hegs_rpl[, c(20:24), drop = FALSE], grouping = analysis_hegs_rpl$Q3_recoded)
png(here("results","figures","Q8_disc_1.png"), width = 450, height = 250, units='mm', res = 500)
plot(q8_disc_a, ordered = TRUE)
dev.off() 

q8_disc_b = likert(analysis_hegs_rpl[, c(25:29), drop = FALSE], grouping = analysis_hegs_rpl$Q3_recoded)
png(here("results","figures","Q8_disc_2.png"), width = 450, height = 250, units='mm', res = 500)
plot(q8_disc_b, ordered = TRUE)
dev.off() 

q10_disc = likert(analysis_hegs_rpl[, c(31:36), drop = FALSE], grouping = analysis_hegs_rpl$Q3_recoded)
png(here("results","figures","Q10_disc.png"), width = 450, height = 250, units='mm', res = 500)
plot(q10_disc, ordered = TRUE)
dev.off() 

q15_disc_a = likert(analysis_hegs_rpl[, c(41:46), drop = FALSE], grouping = analysis_hegs_rpl$Q3_recoded)
png(here("results","figures","Q15_disc_1.png"), width = 450, height = 250, units='mm', res = 500)
plot(q15_disc_a, ordered = TRUE)
dev.off() 

q15_disc_b = likert(analysis_hegs_rpl[, c(47:52), drop = FALSE], grouping = analysis_hegs_rpl$Q3_recoded)
png(here("results","figures","Q15_disc_2.png"), width = 450, height = 250, units='mm', res = 500)
plot(q15_disc_b)
dev.off() 

#Methods figure
analysis_hegs_rpl <- analysis_hegs_rpl %>% filter(Q4_quantqual %in% c("Quantitative","Qualitative","Mixed Methods"))

q8_meth_a = likert(analysis_hegs_rpl[, c(20:24), drop = FALSE], grouping = analysis_hegs_rpl$Q4_quantqual)
png(here("results","figures","Q8_methods_1.png"), width = 450, height = 250, units='mm', res = 500)
plot(q8_meth_a, ordered = TRUE)
dev.off() 

q8_meth_b = likert(analysis_hegs_rpl[, c(25:29), drop = FALSE], grouping = analysis_hegs_rpl$Q4_quantqual)
png(here("results","figures","Q8_methods_2.png"), width = 450, height = 250, units='mm', res = 500)
plot(q8_meth_b, ordered = TRUE)
dev.off() 

q10_meth = likert(analysis_hegs_rpl[, c(31:36), drop = FALSE], grouping = analysis_hegs_rpl$Q4_quantqual)
png(here("results","figures","Q10_methods.png"), width = 450, height = 250, units='mm', res = 500)
plot(q10_meth, ordered = TRUE)
dev.off() 

q15_meth_a = likert(analysis_hegs_rpl[, c(41:46), drop = FALSE], grouping = analysis_hegs_rpl$Q4_quantqual)
png(here("results","figures","Q15_methods_1.png"), width = 450, height = 250, units='mm', res = 500)
plot(q15_meth_a, ordered = TRUE)
dev.off() 

q15_meth_b = likert(analysis_hegs_rpl[, c(47:52), drop = FALSE], grouping = analysis_hegs_rpl$Q4_quantqual)
png(here("results","figures","Q15_methods_2.png"), width = 450, height = 250, units='mm', res = 500)
plot(q15_meth_b)
dev.off() 




## Q12-Q14 Histograms and Density Plots ##
# Has
ggplot(analysis_hegs_rpl, aes(x=Q12_pcnt_have_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  labs(x = "Percent of recent studies that have been replicated",
       y = "Density",
       title = "Overall") +
  theme_classic()
ggsave(here("results","figures","q12_hist_overall.png"))

ggplot(analysis_hegs_rpl, aes(x=Q12_pcnt_have_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  labs(x = "Percent of recent studies that have been replicated",
       y = "Density") +
  facet_grid(. ~ Q3_recoded) +
  theme_classic()
ggsave(here("results","figures","q12_hist_discipline.png"))

ggplot(analysis_hegs_rpl, aes(x=Q12_pcnt_have_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  labs(x = "Percent of recent studies that have been replicated",
       y = "Density") +
  facet_grid(. ~ Q4_quantqual) +
  theme_classic()
ggsave(here("results","figures","q12_hist_method.png"))

# Could
ggplot(analysis_hegs_rpl, aes(x=Q13_pcnt_could_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.1, fill="green")  +
  labs(x = "Percent of recent studies that could be replicated",
       y = "Density",
       title = "Overall") +
  theme_classic()
ggsave(here("results","figures","q13_hist_overall.png"))

ggplot(analysis_hegs_rpl, aes(x=Q13_pcnt_could_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.1, fill="green")  +
  labs(x = "Percent of recent studies that could be replicated",
       y = "Density",
       title = "Overall") +
  facet_grid(. ~ Q3_recoded) +
  theme_classic()
ggsave(here("results","figures","q13_hist_discipline.png"))

ggplot(analysis_hegs_rpl, aes(x=Q13_pcnt_could_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.1, fill="green")  +
  labs(x = "Percent of recent studies that could be replicated",
       y = "Density",
       title = "Overall") +
  facet_grid(. ~ Q4_quantqual) +
  theme_classic()
ggsave(here("results","figures","q13_hist_method.png"))



# Should
ggplot(analysis_hegs_rpl, aes(x=Q14_pcnt_should_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.1, fill="blue")  +
  labs(x = "Percent of recent studies that should be replicated",
       y = "Density",
       title = "Overall") +
  theme_classic()
ggsave(here("results","figures","q14_hist_overall.png"))

ggplot(analysis_hegs_rpl, aes(x=Q14_pcnt_should_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.1, fill="blue")  +
  labs(x = "Percent of recent studies that should be replicated",
       y = "Density") +
  facet_grid(. ~ Q3_recoded) +
  theme_classic()
ggsave(here("results","figures","q14_hist_discipline.png"))

ggplot(analysis_hegs_rpl, aes(x=Q14_pcnt_should_rep_1)) + 
  geom_histogram(aes(y=..density..), colour="darkgrey", fill="white")+
  geom_density(alpha=.1, fill="blue")  +
  labs(x = "Percent of recent studies that should be replicated",
       y = "Density") +
  facet_grid(. ~ Q4_quantqual) +
  theme_classic()
ggsave(here("results","figures","q14_hist_method.png"))

library(reshape)
plots <- analysis_hegs_rpl %>% 
  transmute(Has = Q12_pcnt_have_rep_1,
            Could = Q13_pcnt_could_rep_1,
            Should = Q14_pcnt_should_rep_1,
            ResponseId,
            Q3_recoded,
            Q4_quantqual) %>% 
  as.data.frame %>%
  melt(., id = c('ResponseId','Q3_recoded','Q4_quantqual'))

ggplot(plots, aes(x=value, fill=variable)) +
  geom_density(alpha=.3) +
  labs(x = "Percentage of research in subfield",
       y = "Density",
       fill = "Replicability",
       title = "Overall") +
  theme_classic()
ggsave(here("results","figures","q12-q14_stacked_overall.png"))

ggplot(plots, aes(x=value, fill=variable)) +
  geom_density(alpha=.3) +
  labs(x = "Percentage of research in subfield",
       y = "Density",
       fill = "Replicability")  +
  facet_grid(. ~ Q3_recoded) +
  theme_classic()
ggsave(here("results","figures","q12-q14_stacked_discpline.png"))


ggplot(plots, aes(x=value, fill=variable)) +
  geom_density(alpha=.3) +
  labs(x = "Percentage of research in subfield",
       y = "Density",
       fill = "Replicability")  +
  facet_grid(. ~ Q4_quantqual) +
  theme_classic()
ggsave(here("results","figures","q12-q14_stacked_method.png"))


