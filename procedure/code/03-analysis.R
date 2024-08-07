library(here)
library(tidyverse)
library(table1)
library(flextable)
library(officer)
library(readxl)

#--------------------------------#
#-                              -#
#- PERFORM DESCRIPTIVE ANALYSIS -#
#-                              -#
#--------------------------------#
analysis_hegs_rpl <- readRDS(here("data","derived","public","analysis_hegs_rpl.rds"))

#Define MS Word table layout options
sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape", 
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)

#-----------------------#
#-- Cross tabulations --#
#-----------------------#
#- Table 1) Summary Characteristics of Respondents -#

table1::label(analysis_hegs_rpl$Q3_recoded)   <- "Subdiscipline"
table1::label(analysis_hegs_rpl$Q4)           <- "Methods"
table1::label(analysis_hegs_rpl$Q24_lab_size) <- "Size of lab"
table1::label(analysis_hegs_rpl$Q25_title)    <- "Title"
table1::label(analysis_hegs_rpl$Q1_age)       <- "Age"


Q3_table1 <- table1::table1(~Q4_quantqual + Q24_lab_size + Q25_title + Q1_age  | Q3_recoded, data = analysis_hegs_rpl)
Q4_table1 <- table1::table1(~Q3_recoded + Q24_lab_size + Q25_title + Q1_age  | Q4_quantqual, data = analysis_hegs_rpl)

# Output tables using write table and flextable
write.table(Q3_table1 , here("results","tables","Table1_Summary_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table1 , here("results","tables","Table1_Summary_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table1) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table1_Summary_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table1) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table1_Summary_method.docx"),
               pr_section = sect_properties)

#----------------------------------------------------------------#
#- Q7 Tables -#
table1::label(analysis_hegs_rpl$Q7_value_1) <- "Claim is the product of chance"
table1::label(analysis_hegs_rpl$Q7_value_2) <- "Result is the product of a flawed research design"
table1::label(analysis_hegs_rpl$Q7_value_3) <- "Obs and analyses reflect the concepts they intended to"
table1::label(analysis_hegs_rpl$Q7_value_4) <- "Claim will hold in other populations"
table1::label(analysis_hegs_rpl$Q7_value_5) <- "Claim will hold in other locations"

Q3_table2 <- table1::table1(~Q7_value_1 + Q7_value_2 + Q7_value_3 + Q7_value_4 + Q7_value_5 | Q3_recoded , data = analysis_hegs_rpl)
Q4_table2 <- table1::table1(~Q7_value_1 + Q7_value_2 + Q7_value_3 + Q7_value_4 + Q7_value_5 | Q4_quantqual , data = analysis_hegs_rpl)

# Output tables using write table and flextable
write.table(Q3_table2 , here("results","tables","Table2_RepValue_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table2 , here("results","tables","Table2_RepValue_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table2) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table2_RepValue_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table2) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table2_RepValue_method.docx"),
               pr_section = sect_properties)

#----------------------------------------------------------------#
#- Q8 Tables -#
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

Q3_table3 <- table1::table1(~Q8_study_factors_1 + Q8_study_factors_2 + Q8_study_factors_3 + Q8_study_factors_4 +
                              Q8_study_factors_5 + Q8_study_factors_6 + Q8_study_factors_7 + Q8_study_factors_8 +
                              Q8_study_factors_9 + Q8_study_factors_10 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table3 <- table1::table1(~Q8_study_factors_1 + Q8_study_factors_2 + Q8_study_factors_3 + Q8_study_factors_4 +
                              Q8_study_factors_5 + Q8_study_factors_6 + Q8_study_factors_7 + Q8_study_factors_8 +
                              Q8_study_factors_9 + Q8_study_factors_10 | Q4_quantqual , data = analysis_hegs_rpl)


# Output tables using write table and flextable
write.table(Q3_table3 , here("results","tables","Table3_StudyChar_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table3 , here("results","tables","Table3_StudyChar_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table3) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table3_StudyChar_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table3) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table3_StudyChar_method.docx"),
               pr_section = sect_properties)



#----------------------------------------------------------------#
#- Q10 Tables -#
table1::label(analysis_hegs_rpl$Q10_phen_factors_1) <- "Spatially dependent upon itself"
table1::label(analysis_hegs_rpl$Q10_phen_factors_2) <- "Strongly related with local conditions"
table1::label(analysis_hegs_rpl$Q10_phen_factors_3) <- "Exhibits variation across locations"
table1::label(analysis_hegs_rpl$Q10_phen_factors_4) <- "Cannot be directly measured"
table1::label(analysis_hegs_rpl$Q10_phen_factors_5) <- "Cannot be directly manipulated"
table1::label(analysis_hegs_rpl$Q10_phen_factors_6) <- "Has multiple competing theoretical explanations"

Q3_table4 <- table1::table1(~Q10_phen_factors_1 + Q10_phen_factors_2 + Q10_phen_factors_3 +
                              Q10_phen_factors_4 + Q10_phen_factors_5 + Q10_phen_factors_6 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table4 <- table1::table1(~Q10_phen_factors_1 + Q10_phen_factors_2 + Q10_phen_factors_3 +
                              Q10_phen_factors_4 + Q10_phen_factors_5 + Q10_phen_factors_6 | Q4_quantqual , data = analysis_hegs_rpl)

# Output tables using write table and flextable
write.table(Q3_table4 , here("results","tables","Table4_PhenomChar_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table4 , here("results","tables","Table4_PhenomChar_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table4) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table4_PhenomChar_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table4) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table4_PhenomChar_method.docx"),
               pr_section = sect_properties)

#----------------------------------------------------------------#
#- Q15 Tables -#
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

Q3_table5 <- table1::table1(~Q15_decision_factors_1 + Q15_decision_factors_2 + Q15_decision_factors_3 +
                              Q15_decision_factors_4 + Q15_decision_factors_5 + Q15_decision_factors_6 +
                              Q15_decision_factors_7 + Q15_decision_factors_8 + Q15_decision_factors_9 +
                              Q15_decision_factors_10 + Q15_decision_factors_11 + Q15_decision_factors_12 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table5 <- table1::table1(~Q15_decision_factors_1 + Q15_decision_factors_2 + Q15_decision_factors_3 +
                              Q15_decision_factors_4 + Q15_decision_factors_5 + Q15_decision_factors_6 +
                              Q15_decision_factors_7 + Q15_decision_factors_8 + Q15_decision_factors_9 +
                              Q15_decision_factors_10 + Q15_decision_factors_11 + Q15_decision_factors_12 | Q4_quantqual , data = analysis_hegs_rpl)


# Output tables using write table and flextable
write.table(Q3_table5 , here("results","tables","Table5_Decision_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table5 , here("results","tables","Table5_Decision_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table5) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table5_Decision_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table5) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table5_Decision_method.docx"),
               pr_section = sect_properties)

#----------------------------------------------------------------#
# Table 6

table1::label(analysis_hegs_rpl$Q12_pcnt_have_rep_1) <- "Percent of recent studies that have been replicated"
table1::label(analysis_hegs_rpl$Q13_pcnt_could_rep_1) <- "Percent of recent studies that could be replicated"
table1::label(analysis_hegs_rpl$Q14_pcnt_should_rep_1) <- "Percent of recent studies that should be replicated"

Q3_table6 <- table1::table1(~Q12_pcnt_have_rep_1 + Q13_pcnt_could_rep_1 + Q14_pcnt_should_rep_1 | Q3_recoded , data = analysis_hegs_rpl)
Q4_table6 <- table1::table1(~Q12_pcnt_have_rep_1 + Q13_pcnt_could_rep_1 + Q14_pcnt_should_rep_1 | Q4_quantqual , data = analysis_hegs_rpl)

# Output tables using write table and flextable
write.table(Q3_table6 , here("results","tables","Table6_Q12-Q14_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table6 , here("results","tables","Table6_Q12-Q14_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table6) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table6_Q12-Q14_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table6) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table6_Q12-Q14_method.docx"),
               pr_section = sect_properties)


#----------------------------------------------------------------#
# Table 7

table1::label(analysis_hegs_rpl$Q17_rep_behavior_1) <- "Thought about the replicability of your research"
table1::label(analysis_hegs_rpl$Q17_rep_behavior_2) <- "Spoken with colleagues about replicability"
table1::label(analysis_hegs_rpl$Q17_rep_behavior_3) <- "Questioned the replicability of published research"
table1::label(analysis_hegs_rpl$Q17_rep_behavior_4) <- "Considered replicability while peer reviewing a research proposal or publication"
table1::label(analysis_hegs_rpl$Q17_rep_behavior_5) <- "Attempted to replicate prior research claims"


Q3_table7 <- table1::table1(~Q17_rep_behavior_1 + Q17_rep_behavior_2 + Q17_rep_behavior_3 +
                             Q17_rep_behavior_4 + Q17_rep_behavior_5 | Q3_recoded , data = analysis_hegs_rpl)

Q4_table7 <- table1::table1(~Q17_rep_behavior_1 + Q17_rep_behavior_2 + Q17_rep_behavior_3 +
                              Q17_rep_behavior_4 + Q17_rep_behavior_5 | Q4_quantqual , data = analysis_hegs_rpl)

# Output tables using write table and flextable
write.table(Q3_table7 , here("results","tables","Table7_Behavior_discipline.csv"), col.names = T, row.names=F, append= T, sep=',')
write.table(Q4_table7 , here("results","tables","Table7_Behavior_method.csv"), col.names = T, row.names=F, append= T, sep=',')

t1flex(Q3_table7) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table7_Behavior_discipline.docx"),
               pr_section = sect_properties)

t1flex(Q4_table7) %>% 
  save_as_docx(path = here("results","tables","MSWord","Table7_Behavior_method.docx"),
               pr_section = sect_properties)



#--------------------------------#
#- New Tables -#
#--------------------------------#

#Identify set of respodents who reported attempting a replication
completed_rep <- read_xlsx(here("data","derived","public","Q17_coding.xlsx"))

reason_no_pub <- completed_rep %>%  filter(!is.na(completed_rep$Q22_code)) %>% select(Q22_reason_no_pub, Q22_code)

completed_rep <- completed_rep %>% 
  filter(rep_flag == 1) %>%
  transmute(ResponseId, rep_flag)

replication_sample <- left_join(completed_rep,analysis_hegs_rpl,by="ResponseId")


#-------------------------------------------------------------------------------#
#Table 1 - Demographics of individuals who attempted a replication
#-------------------------------------------------------------------------------#
table1::label(replication_sample$Q3_recoded)   <- "Subdiscipline"
table1::label(replication_sample$Q4_quantqual) <- "Methods"
table1::label(replication_sample$Q24_lab_size) <- "Size of lab"
table1::label(replication_sample$Q25_title)    <- "Title"
table1::label(replication_sample$Q1_age)       <- "Age"

rep_sample_demo <- table1::table1(~Q4_quantqual + Q24_lab_size + Q25_title + Q1_age +  Q3_recoded + Q4_quantqual, data = replication_sample)

# Output tables using write table and flextable
write.table(rep_sample_demo , here("results","tables","rep_sample_demo.csv"), col.names = T, row.names=F, append= T, sep=',')

#-------------------------------------------------------------------------------#
#Table 2 - Summary stats for individuals who attempted a replication
#-------------------------------------------------------------------------------#
table1::label(replication_sample$Q18_same_loc)      <- "Same location"
table1::label(replication_sample$Q20_rep_results_1) <- "Exactly replicated"
table1::label(replication_sample$Q20_rep_results_2) <- "Partially replicated"
table1::label(replication_sample$Q20_rep_results_3) <- "Not replicated"
table1::label(replication_sample$Q21_rep_process_1) <- "Access to data"
table1::label(replication_sample$Q21_rep_process_2) <- "Access to code"
table1::label(replication_sample$Q21_rep_process_3) <- "Follow procedures exactly"
table1::label(replication_sample$Q21_rep_process_4) <- "Recreate computational environment"
table1::label(replication_sample$Q21_rep_process_5) <- "Find details about geographic location and extent"
table1::label(replication_sample$Q21_rep_process_6) <- "Publish findings"
table1::label(replication_sample$Q23_eval_criteria) <- "Can evaluate when a study is replicated"


rep_sum_char <- table1::table1(~Q18_same_loc + Q20_rep_results_1 + Q20_rep_results_2 + Q20_rep_results_3 +  Q21_rep_process_1 + Q21_rep_process_2 + 
                                  Q21_rep_process_3 +Q21_rep_process_4 + Q21_rep_process_5 + Q21_rep_process_6 + Q23_eval_criteria, data = replication_sample)

# Output tables using write table and flextable
write.table(rep_sum_char , here("results","tables","rep_summary_chars.csv"), col.names = T, row.names=F, append= T, sep=',')

#-------------------------------------------------------------------------------#
#Table 3 - Survey completes
#-------------------------------------------------------------------------------#

