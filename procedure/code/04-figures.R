library(here)
library(tidyverse)
library(table1)
library(officer)
library(ggthemes)
library(ggridges)
library("ggtext")
library(extrafont)
download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
system("mkdir ~/.fonts")
system("cp xkcd.ttf  ~/.fonts")
font_import(paths = "~/.fonts", pattern="[X/x]kcd")
fonts()
loadfonts()
library(readxl)

#--------------------------------#
#- Figures -#
#--------------------------------#
analysis_hegs_rpl <- readRDS(here("data","derived","public","analysis_hegs_rpl.rds"))

#-------------------------------------------------------------------------------#
#Fig A - Diverging bar graph from Q7
#      - Larger font for the percentage values embedded in the bars
#      - Confirm that percentages are being generated correctly
#-------------------------------------------------------------------------------#

#Subset data to Q7 and apply descriptive variable names
Fig_A <-analysis_hegs_rpl[,substr(names(analysis_hegs_rpl), 1,2) == "Q7"]
names(Fig_A) <- c("Chance",
                  "Flawed Design",
                  "Accurately Reflect Concepts",
                  "Holds Across Populations",
                  "Holds Across Location")

Fig_A <- as.data.frame(Fig_A[,c(1,2,3,4,5)])

jpeg(here("results","figures","Fig_A_labeled.jpeg"), units="in", width=11, height=7, res=300)
pivot_longer(Fig_A, everything()) %>%
  group_by(name) %>%
  dplyr::count(value) %>%
  mutate(percentage = (n/283),
         percentage_diverging = ifelse(value %in% c("Strongly disagree","Disagree"),
                                       -1*percentage,
                                       percentage),
         name = factor(name, levels=c("Chance",
                                      "Flawed Design",
                                      "Accurately Reflect Concepts",
                                      "Holds Across Populations",
                                      "Holds Across Location"
         )),
         label = paste0(round(percentage*100,0),""),
         value_ordered=fct_relevel(value,c("Strongly disagree","Disagree","Strongly agree","Agree","Don't know"))
  ) %>%
  filter(!is.na(value) & value != "Don't know") %>%
  ggplot(aes(x = name, y = percentage_diverging, fill = value_ordered)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(label = label), size = 14, colour="white",
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(-1, 1)) +
  scale_fill_manual(values = c("#003660","#04859B","#CD2311","#EF5645")) +
  ylab("") + 
  xlab("") +
  scale_x_discrete(expand = c(0, 0))  +
  theme(axis.line.x = element_line(size=0.5, colour = "black"),
        axis.ticks.x = element_line(size=0.5, colour = "black"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 10),
        legend.position = "bottom",
        legend.title =element_blank())
dev.off()

#-------------------------------------------------------------------------------#
#Fig 2 
#-------------------------------------------------------------------------------#
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
                                                                                                                                          `Never`= 4, 
                                                                                                                                          `Rarely`= 3, 
                                                                                                                                          `Occasionally` = 2,
                                                                                                                                          `Frequently` = 1,
                                                                                                                                          `Always` = 0,
                                                                                                                                          `Don't know` = 5,
                                                                                                                                          .default = NaN)))



#Convert to factors
q8_10_levels <- paste(c("Very likely to decrease","Somewhat likely to decrease","Not likely to affect",
                        "Somewhat likely to increase", "Very likely to increase"))

analysis_hegs_rpl[20:29] = lapply(analysis_hegs_rpl[20:29], factor, levels = -2:2, labels=q8_10_levels)
analysis_hegs_rpl[31:36] = lapply(analysis_hegs_rpl[31:36], factor, levels = -2:2, labels=q8_10_levels)

#Subset data to Q8 and Q10 and apply descriptive variable names
Fig_2 <-analysis_hegs_rpl[,substr(names(analysis_hegs_rpl), 1,2) == "Q8" | 
                            substr(names(analysis_hegs_rpl), 1,3) == 'Q10']

names(Fig_2) <- c("Multiple Hypotheses", 
                      "Quantitative Methods", 
                      "Qualitative Methods", 
                      "Mixed Methods",
                      "Poor Documentation",
                      "Restricted Access",
                      "Multiple Sites",
                      "Large Team",
                      "Unique Expertise",
                      "Researcher Position",
                      "Spatially Dependent",
                      "Linked to Place",
                      "Spatially Variable",
                      "Not Measurable",
                      "Not Manipulable",
                      "Many Explanations")

Fig_2 <- as.data.frame(Fig_2[,c(5,6,7,8,9,10,1,2,3,4,11,12,13,14,15,16)])

jpeg(here("results","figures","Fig_2_unlabeled.jpeg"), units="in", width=11, height=7, res=300)
pivot_longer(Fig_2, everything()) %>%
  group_by(name) %>%
  dplyr::count(value) %>%
  mutate(percentage = (n/283),
         percentage_diverging = ifelse(value %in% c("Very likely to decrease","Somewhat likely to decrease"),
                                       -1*percentage,
                                       percentage),
         name = factor(name, levels=c("Many Explanations",
                                      "Not Manipulable",
                                      "Not Measurable",
                                      "Spatially Variable",
                                      "Linked to Place",
                                      "Spatially Dependent",
                                      "Mixed Methods",
                                      "Qualitative Methods", 
                                      "Quantitative Methods", 
                                      "Multiple Hypotheses", 
                                      "Researcher Position",
                                      "Unique Expertise",
                                      "Large Team",
                                      "Multiple Sites",
                                      "Restricted Access",
                                      "Poor Documentation"
         )),
         label = paste0(round(percentage*100,0),""),
         value_ordered = relevel(value, "Very likely to increase")
  ) %>%
  filter(!is.na(value) & value != "Not likely to affect" & value !="Don't know") %>%
  ggplot(aes(x = name, y = percentage_diverging, fill = value_ordered)) +
  geom_bar(stat = "identity", width = 0.9) +
#  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6, colour="white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(-1, 1)) +
  scale_fill_manual(values = c("#CD2311","#003660","#04859B","#EF5645")) +
  ylab("") + 
  xlab("") +
  scale_x_discrete(expand = c(0, 0))  +
  theme(axis.line.x = element_line(size=0.5, colour = "black"),
        axis.ticks.x = element_line(size=0.5, colour = "black"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 10),
        legend.position = "bottom",
        legend.title =element_blank())
dev.off()

#-------------------------------------------------------------------------------#
#Fig 3 - Diverging bar graph from Q15
#-------------------------------------------------------------------------------#
q15_levels <- paste(c("Always","Frequently","Occasionally","Rarely",  "Never"))
analysis_hegs_rpl[41:52] = lapply(analysis_hegs_rpl[41:52], factor, levels = 0:4, labels=q15_levels)

Fig_3 <-analysis_hegs_rpl[,substr(names(analysis_hegs_rpl), 1,3) == c('Q15')]
names(Fig_3) <- c("Original Research",
                    "Perceived Value",
                    "Low Chance of Success",
                    "Inexperience",
                    "Difficulty Publishing",
                    "Data Access",
                    "Method Access",
                    "Recreating Methods",
                    "Lack of Funding",
                    "Fraud",
                    "Ethical Concerns",
                    "Geographic Variation")

Fig_3 <- as.data.frame(Fig_3[,c(1,9,2,5,10, 6,7,8, 4,12,3,11)])


jpeg(here("results","figures","Fig_3.jpeg"), units="in", width=11, height=7, res=300)
pivot_longer(Fig_3, everything()) %>%
  group_by(name) %>%
  dplyr::count(value) %>%
  mutate(percentage = (n/283),
         name = factor(name, levels=c("Ethical Concerns",
                                      "Low Chance of Success",
                                      "Geographic Variation",
                                      "Inexperience",
                                      "Recreating Methods",
                                      "Method Access",
                                      "Data Access",
                                      "Fraud",
                                      "Difficulty Publishing",
                                      "Perceived Value",
                                      "Lack of Funding",
                                      "Original Research"
         ))) %>%
  ggplot(aes(x = name, y = percentage, fill = value)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity", width = 0.8) +
  coord_flip() + 
  scale_fill_manual(values = c("#CD2311","#EF5645","#BFBFBF","#D9D9D9","#F2F2F2"), na.value="white") +
  ylab("") + 
  xlab("") +
  scale_x_discrete(expand = c(0, 0))  +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.line.x = element_line(size=0.5, colour = "black"),
        axis.ticks.x = element_line(size=0.5, colour = "black"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 10),
        legend.position = "bottom",
        legend.title =element_blank()) 
dev.off()

fig3_values <- pivot_longer(Fig_3, everything()) %>%
  group_by(name) %>%
  dplyr::count(value) %>%
  mutate(percentage = (n/283),
         name = factor(name, levels=c("Ethical Concerns",
                                      "Low Chance of Success",
                                      "Geographic Variation",
                                      "Inexperience",
                                      "Recreating Methods",
                                      "Method Access",
                                      "Data Access",
                                      "Fraud",
                                      "Difficulty Publishing",
                                      "Perceived Value",
                                      "Lack of Funding",
                                      "Original Research"
         )))
write_csv(fig3_values, "fig3_labels.csv")

