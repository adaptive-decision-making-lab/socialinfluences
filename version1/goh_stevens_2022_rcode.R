## ---
##
## Script name: goh_stevens_2022_rcode.R
##
## Purpose of script: Analyze social influence on choice data
##
## Authors: Francine W. Goh (francinegoh@gmail.com), Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Finalized: 2022-06-14
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##  Share — copy and redistribute the material in any medium or format.
##  Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##  Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
##  No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
## ---
##
## Notes:
##   
## Instructions: Create folders called "data" and "figures". Place this file in the main 
## 	directory. Place the data file in the data directory. Set the R working directory to 
##  the main directory. At the R command prompt, type
## 	> source("goh_stevens_2022_rcode.R")
## 	This will run the script, adding all of the calculated variables to the workspace and 
##  saving figures in the figures directory. If packages do not load properly, install them 
##  with install.packages("package_name").
## 
## Data file:
## goh_stevens_2022_data.csv
##  study = study number
##  subject_nr = participant number
##  amount_pair = number pair in the amount similarity judgment task
##  delay_pair = number pair in the delay similarity judgment task
##  amount_similarity = participant's similarity judgment in the amount similarity judgment task (0 = dissimilar, 1 = similar)
##  delay_similarity = participant's similarity judgment in the delay similarity judgment task (0 = dissimilar, 1 = similar)
##  itc_control_choice = participant's choice for non-social intertemporal choice questions (0 = smaller, sooner option, 1 = larger, later option)
##  itc_amount_social_info = similarity judgment shown for the amount pair in social intertemporal choice questions (0 = dissimilar, 1 = similar)
##  itc_delay_social_info = similarity judgment shown for the delay pair in social intertemporal choice questions (0 = dissimilar, 1 = similar)
##  amount_delay_social_info = social information condition shown for social intertemporal choice questions (D_D = amount dissimilar, delay dissimilar; S_S = amount similar, delay similar; D_S = amount dissimilar, delay similar (amount-focused condition); S_D = amount similar, delay dissimilar (delay-focused condition))
##  itc_social_choice = participant's choice for social intertemporal choice questions (0 = smaller, sooner option, 1 = larger, later option)
##  miss_score = participant's suggestibility score
##  median_miss_score = median suggestibility score
##  suggestibility_level = participant's suggestibility level
##  sns_score = participant's subjective numeracy score
##  sns_level = participant's subjective numeracy level
##  overall_bnt_score = participant's objective numeracy score
##  age = participant age
##  ethnicity = participant ethnicity
##  gender = participant gender
##  control_social =	non-social (control) or social intertemporal choice question followed by the number pair for the question. For social questions, this is followed by an indication of the similarity judgment shown for the number pair (d = dissimilar, s = similar). For non-social questions, this is followed by "NA" since there is no similarity judgment shown for the number pair.
##  amount_delay = similarity judgment task type (amount or delay)
##  similarity_judgment = participant's similarity judgment in the corresponding similarity judgment task (0 = dissimilar, 1 = similar)
##  task_social_info = type of social information shown for similarity judgment (amount or delay)
##  social_info = similarity judgment shown in intertemporal choice question (0 = no similarity judgment shown, 1 = similar, 2 = dissimilar)
##
## ---


# Packages ----------------------------------------------------------------
library(afex)
library(BayesFactor)
library(bayestestR)
library(broom)
library(emmeans)

library(lsr)
library(patchwork)
library(tidyverse)


# Import and process data ---------------------------------------------
all_data <- read_csv("goh_stevens_2022_data.csv")  # import data
all_data_1 <- filter(all_data, study == 1) %>%  # separate study 1 data
  select(subject_nr:gender) %>% 
  mutate(amount_delay_social_info = as.factor(amount_delay_social_info))
all_data_2 <- filter(all_data, study == 2) %>%  # separate study 2 data
  select(subject_nr:gender) %>% 
  mutate(amount_delay_social_info = as.factor(amount_delay_social_info))
all_data_3 <- filter(all_data, study == 3) %>%  # separate study 3 data
  select(subject_nr, control_social, amount_delay, similarity_judgment, task_social_info, social_info, miss_score, suggestibility_level, overall_bnt_score, sns_score, sns_level, age, ethnicity, gender) %>% 
  mutate(control_social = as.factor(control_social),
         amount_delay = as.factor(amount_delay),
         suggestibility_level = as.factor(suggestibility_level),
         sns_level = as.factor(sns_level))

set.seed(100)


# Study 1: Similarity judgments & ITC choice comparisons --------------------------------
# Create dataset to compare choices in control & social ITC tasks across the 4 social information conditions 
itc_choice_data_1 <- all_data_1 %>%
  select(subject_nr:itc_social_choice) %>% 
  mutate(subject_nr = as.factor(subject_nr)) %>%   # convert into factor type
  group_by(subject_nr, amount_delay_social_info) %>% 
  summarise(`Non-social` = mean(itc_control_choice),  # calculate mean value of choices in control ITC task for each social info condition
            Social = mean(itc_social_choice)) %>%  # calculate mean value of choices in social ITC task for each social info condition
  pivot_longer(c(`Non-social`, Social), names_to = "itc_task", values_to = "choice") %>% 
  mutate(itc_task = as.factor(itc_task))


# _Plot: ITC task & social info condition effects -------------------------
itc_socialinfo_plot_1 <- ggplot(itc_choice_data_1, aes(x = amount_delay_social_info, y = choice, fill = amount_delay_social_info)) +
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_normal, size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~itc_task) +  # split graph by control & social ITC task type
  labs(x = "Amount-delay social information", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_x_discrete(labels = c("D_D" = "Dissimilar", "D_S" = "Amount\n-focused", "S_D" = "Delay\n-focused", "S_S" = "Similar")) +  # change x-axis tick mark labels
  scale_fill_manual(values=c("#44AA99", "#88CCEE", "#DDCC77", "#CC6677")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 23),  # change x & y axes tick mark font & size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph
ggsave("figures/itc_social_info_1.png", width = 14, height = 7.5)


# _Analyses: ITC task & social info condition effects -------------------------------
# Within-subjects ANOVA for ITC task and social info condition
itc_socialinfo_anova_1 <- aov_car(choice ~ itc_task * amount_delay_social_info + Error(subject_nr / (itc_task * amount_delay_social_info)), data = itc_choice_data_1)  # ANOVA
summary(itc_socialinfo_anova_1)

emm_itc_socialinfo_interaction_1 <- emmeans(itc_socialinfo_anova_1, ~ itc_task | amount_delay_social_info)  # post-hoc contrasts
emm_itc_socialinfo_interaction_results_1 <- pairs(emm_itc_socialinfo_interaction_1, adjust = "tukey")  # pairwise comparisons

itc_socialinfo_anova_1_bf <- anovaBF(choice ~ itc_task * amount_delay_social_info + subject_nr, data = itc_choice_data_1, whichRandom = "subject_nr")  # Bayes factor analyses

# Compare preference for LL option by social info condition for non-social & social ITC tasks
## Non-social ITC
nonsocial_itc_data_1 <- itc_choice_data_1 %>% 
  filter(itc_task == "Non-social")
nonsocial_itc_comparisons_anova_1 <- aov_car(choice ~ amount_delay_social_info + Error(subject_nr / amount_delay_social_info), data = nonsocial_itc_data_1)  # ANOVA
nonsocial_itc_comparisons_bf_1 <- anovaBF(choice ~ amount_delay_social_info + subject_nr, data = nonsocial_itc_data_1, whichRandom = "subject_nr")  # Bayes factor analysis

## Social ITC
social_itc_data_1 <- itc_choice_data_1 %>% 
  filter(itc_task == "Social")
social_itc_comparisons_anova_1 <- aov_car(choice ~ amount_delay_social_info + Error(subject_nr / amount_delay_social_info), data = social_itc_data_1)  # ANOVA
social_itc_comparisons_bf_1 <- anovaBF(choice ~ amount_delay_social_info + subject_nr, data = social_itc_data_1, whichRandom = "subject_nr")  # Bayes factor analysis


# _Plot: Amount-focused & delay-focused social info condition comparisons --------
nonsocial_social_analysis_1 <- itc_choice_data_1 %>% 
  pivot_wider(names_from = itc_task, values_from = choice)

nonsocial_social_DS_SD_plot_1 <- nonsocial_social_analysis_1 %>% 
  filter(amount_delay_social_info == "D_S" | amount_delay_social_info == "S_D") %>% 
  pivot_longer(c(`Non-social`, Social), names_to = "itc_task", values_to = "choice") %>% 
  pivot_wider(names_from = amount_delay_social_info, values_from = choice) %>% 
  rename("Amount-focused" = D_S, "Delay-focused" = S_D) %>%  # rename columns
  pivot_longer(c(`Amount-focused`, `Delay-focused`), names_to = "amount_delay_social_info", values_to = "choice") %>%
  mutate(itc_task = as.factor(itc_task),
         amount_delay_social_info = as.factor(amount_delay_social_info))  # convert variables to factor type

ggplot(nonsocial_social_DS_SD_plot_1, aes(x = itc_task, y = choice, fill = itc_task)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_boot", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~amount_delay_social_info) +  # split graph by social info condition (D-S/S-D)
  labs(x = "Intertemporal choice questions", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_y_continuous(limits = c(0, 1)) +  # expand y-axis limit to include "0" & "1" tick marks
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial")+
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  #  change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # x & y axes tick mark size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph
ggsave("figures/socialinfo_ds_sd_1.png", width = 6.5, height = 6.5)  # save plot to "figures" folder


# _Analyses: Amount-focused & delay-focused social info condition comparisons --------
# Compare participant preference for LL option in nonsocial & social ITC tasks: DS (amount-focused) condition
## If social info = amount dissimilar, delay similar: participants should prefer LL option in social ITC task
nonsocial_social_analysis_DS_1 <- filter(nonsocial_social_analysis_1, amount_delay_social_info == "D_S")  # create dataframe for amount dissimilar-delay similar condition analysis

nonsocial_social_comparison_ds_1 <- t.test(nonsocial_social_analysis_DS_1$`Non-social`, nonsocial_social_analysis_DS_1$Social, paired = TRUE)  # paired t-test: compare participant preference for LL option in amount-focused condition for nonsocial & social ITC
nonsocial_social_comparison_ds_bf_1 <- ttestBF(nonsocial_social_analysis_DS_1$`Non-social`, nonsocial_social_analysis_DS_1$Social, paired = TRUE)  # Bayes factor analysis
nonsocial_social_comparison_ds_cohensd_1 <- cohensD(nonsocial_social_analysis_DS_1$`Non-social`, nonsocial_social_analysis_DS_1$Social)  # calculate effect size (Cohen's d)

# Compare participant preference for LL option in nonsocial & social ITC tasks: SD (delay-focused) condition
## If social info = amount similar, delay dissimilar: participants should pick SS option in social ITC task
nonsocial_social_analysis_SD_1 <- filter(nonsocial_social_analysis_1, amount_delay_social_info == "S_D")  # create dataframe for amount similar-delay dissimilar condition analysis

nonsocial_social_comparison_sd_1 <- t.test(nonsocial_social_analysis_SD_1$`Non-social`, nonsocial_social_analysis_SD_1$Social, paired = TRUE)  # paired t-test: compare participant preference for LL option in delay-focused condition for nonsocial & social ITC
nonsocial_social_comparison_sd_bf_1 <- ttestBF(nonsocial_social_analysis_SD_1$`Non-social`, nonsocial_social_analysis_SD_1$Social, paired = TRUE)  # Bayes factor analysis
nonsocial_social_comparison_sd_cohensd_1 <- cohensD(nonsocial_social_analysis_SD_1$`Non-social`, nonsocial_social_analysis_SD_1$Social)  # calculate effect size (Cohen's d)

# Compare amount-focused & delay-focused conditions in social ITC
ds_sd_social_itc_comparison_data_1 <- nonsocial_social_DS_SD_plot_1 %>% 
  filter(itc_task == "Social") %>% 
  pivot_wider(names_from = amount_delay_social_info, values_from = choice)

ds_sd_social_itc_comparison_1 <- t.test(ds_sd_social_itc_comparison_data_1$`Amount-focused`, ds_sd_social_itc_comparison_data_1$`Delay-focused`, paired = TRUE)  # paired t-test: compare participant preference for LL option in amount-focused and delay-focused social ITC
ds_sd_social_itc_comparison_bf_1 <- ttestBF(ds_sd_social_itc_comparison_data_1$`Amount-focused`, ds_sd_social_itc_comparison_data_1$`Delay-focused`, paired = TRUE)  # Bayes factor analysis
ds_sd_social_itc_comparison_cohensd_1 <- cohensD(ds_sd_social_itc_comparison_data_1$`Amount-focused`, ds_sd_social_itc_comparison_data_1$`Delay-focused`)  # calculate effect size (Cohen's d)


# Study 1: Predictive ability of similarity judgments ---------------------
# _Analyses: Predictive ability of similarity judgments for non-social vs. social ITC tasks --------
# AMOUNTS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: DS
# Participants should prefer LL option in non-social ITC task
amount_d_itc_1 <- all_data_1 %>%
  select(subject_nr:itc_social_choice) %>%
  mutate(amount_congruence = ifelse(is.na(amount_similarity) | is.na(itc_amount_social_info), NA,  # remove any NA responses
                                    ifelse(amount_similarity == itc_amount_social_info, 1, 0)),   # check if amount similarity judgment is the same as the presented social info in the social ITC task. Code response as "1" if participant's amount similarity judgment in amount judgment task is the same as the presented social info (i.e., if amount is stated as similar or dissimilar) for each amount_pair in the social ITC task; otherwise response will be coded "0"
         observed_control_choice_prediction = ifelse(is.na(amount_similarity) | is.na(itc_control_choice), NA,  # remove any NA responses
                                                     ifelse(amount_similarity == 0 & itc_control_choice == 1, 1, 0)),  # test if similarity judgment predicts choice in control ITC task. Code response as "1" if participant indicated amount_pair is dissimilar & chose LL option for control ITC question; otherwise response will be coded "0"
         observed_social_choice_prediction = ifelse(is.na(amount_similarity) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                    ifelse(amount_similarity == 0 & itc_social_choice == 1, 1, 0)),  # test if similarity judgment predicts choice in social ITC task. Code response as "1" if participant indicated amount_pair is dissimilar & chose LL option for social ITC question; otherwise response will be coded "0"
         social_info_social_choice_prediction = ifelse(is.na(itc_amount_social_info) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                       ifelse(itc_amount_social_info == 0 & itc_social_choice == 1, 1, 0)),   # test if presented social info predicts choice in social ITC task. Code response as "1" if the presented social info for amounts is dissimilar & participant chose LL option for social ITC question; otherwise response will be coded "0"
         personal_similarity_domain = ifelse(is.na(amount_similarity) | is.na(delay_similarity), NA,  # remove any NA responses
                                             ifelse(amount_similarity != delay_similarity, 1, 0)),  # test if participant's amount & delay similarity judgments are the same for both similarity judgment tasks. Code response as "0" if participant's amount & delay similarity judgments are the same; otherwise response will be coded "1"
         social_similarity_domain = ifelse(itc_amount_social_info != itc_delay_social_info, 1, 0))  # test if presented social info for amount & delay in social ITC are different. Code response as "0" if amount & delay social info are the same; otherwise response will be coded "1"

amount_ds_personal_domain_1 <- amount_d_itc_1 %>% 
  filter(amount_similarity == 0 & personal_similarity_domain == 1)  # select trials where participants judged amount pairs to be dissimilar in amount similarity judgment task, and amount similarity judgment is different from delay similarity judgment

amount_ds_personal_domain_means_1 <- amount_ds_personal_domain_1 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

personaldomain_amount_ds_personal_predicts_control_1 <- t.test(amount_ds_personal_domain_means_1$personal_predicts_control, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in control ITC task more than 50% of the time
personaldomain_amount_ds_personal_predicts_control_bf_1 <- ttestBF(amount_ds_personal_domain_means_1$personal_predicts_control, mu = 0.5)  # Bayes factor analysis

personaldomain_amount_ds_personal_predicts_social_1 <- t.test(amount_ds_personal_domain_means_1$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
personaldomain_amount_ds_personal_predicts_social_bf_1 <- ttestBF(amount_ds_personal_domain_means_1$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

personaldomain_amount_ds_personal_vs_social_1 <- t.test(amount_ds_personal_domain_means_1$personal_predicts_control, amount_ds_personal_domain_means_1$personal_predicts_social, paired = TRUE)  # paired t-test: compare abilities of participants' similarity judgments to predict choice in control vs. social ITC task
personaldomain_amount_ds_personal_vs_social_bf_1 <- ttestBF(amount_ds_personal_domain_means_1$personal_predicts_control, amount_ds_personal_domain_means_1$personal_predicts_social, paired = TRUE)  # Bayes factor analysis


# DELAYS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: SD
# Participants should prefer SS option in non-social ITC task
delay_d_itc_1 <- all_data_1 %>%
  select(subject_nr:itc_social_choice) %>% 
  mutate(delay_congruence = ifelse(is.na(delay_similarity) | is.na(itc_delay_social_info), NA,  # remove any NA responses
                                   ifelse(delay_similarity == itc_delay_social_info, 1, 0)),   # check if delay similarity judgment is the same as the presented social info in the social ITC task. Code response as "1" if participant's delay similarity judgment in delay judgment task is the same as the presented social info (i.e., if delay is stated as similar or dissimilar) for each delay_pair in the social ITC task; otherwise response will be coded "0"
         observed_control_choice_prediction = ifelse(is.na(delay_similarity) | is.na(itc_control_choice), NA,  # remove any NA responses
                                                     ifelse(delay_similarity == 0 & itc_control_choice == 0, 1, 0)),  # test if similarity judgment predicts choice in control ITC task. Code response as "1" if participant indicated delay_pair is dissimilar & chose SS option for control ITC question; otherwise response will be coded "0"
         observed_social_choice_prediction = ifelse(is.na(delay_similarity) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                    ifelse(delay_similarity == 0 & itc_social_choice == 0, 1, 0)),  # test if similarity judgment predicts choice in social ITC task. Code response as "1" if participant indicated delay_pair is dissimilar & chose SS option for social ITC question; otherwise response will be coded "0"
         social_info_social_choice_prediction = ifelse(is.na(itc_delay_social_info) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                       ifelse(itc_delay_social_info == 0 & itc_social_choice == 0, 1, 0)),   # test if presented social info predicts choice in social ITC task. Code response as "1" if the presented social info for delays is dissimilar & participant chose SS option for social ITC question; otherwise response will be coded "0"
         personal_similarity_domain = ifelse(is.na(amount_similarity) | is.na(delay_similarity), NA,  # remove any NA responses
                                             ifelse(amount_similarity != delay_similarity, 1, 0)),  # test if participant's amount & delay similarity judgments are the same for both similarity judgment tasks. Code response as "0" if participant's amount & delay similarity judgments are the same; otherwise response will be coded "1"
         social_similarity_domain = ifelse(amount_delay_social_info == "D_S" | amount_delay_social_info == "S_D", 1, 0))  # test if social info condition in social ITC is either DS or SD. Code response as "1" if not; otherwise response will be coded "0"

delay_sd_personal_domain_1 <- delay_d_itc_1 %>% 
  filter(delay_similarity == 0 & personal_similarity_domain == 1)  # select trials where participants judged delay pairs to be dissimilar in delay similarity judgment task, and amount similarity judgment is different from delay similarity judgment

delay_sd_personal_domain_means_1 <- delay_sd_personal_domain_1 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

personaldomain_delay_sd_personal_predicts_control_1 <- t.test(delay_sd_personal_domain_means_1$personal_predicts_control, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in control ITC task more than 50% of the time
personaldomain_delay_sd_personal_predicts_control_bf_1 <- ttestBF(delay_sd_personal_domain_means_1$personal_predicts_control, mu = 0.5)  # Bayes factor analysis

personaldomain_delay_sd_personal_predicts_social_1 <- t.test(delay_sd_personal_domain_means_1$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
personaldomain_delay_sd_personal_predicts_social_bf_1 <- ttestBF(delay_sd_personal_domain_means_1$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

personaldomain_delay_sd_personal_vs_social_1 <- t.test(delay_sd_personal_domain_means_1$personal_predicts_control, delay_sd_personal_domain_means_1$personal_predicts_social, paired = TRUE)  # paired t-test: compare abilities of participants' similarity judgments to predict choice in control vs. social ITC task
personaldomain_delay_sd_personal_vs_social_bf_1 <- ttestBF(delay_sd_personal_domain_means_1$personal_predicts_control, delay_sd_personal_domain_means_1$personal_predicts_social, paired = TRUE)  # Bayes factor analysis


# _Analyses: Predictive ability of similarity judgments vs. social info in social ITC task --------
# AMOUNTS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: DS
# Participants should prefer LL option in non-social ITC task
amount_ds_social_domain_1 <- amount_d_itc_1 %>% 
  filter(amount_delay_social_info == "D_S") # select trials where presented social info = amount dissimilar, delay similar in social ITC task

amount_ds_social_domain_means_1 <- amount_ds_social_domain_1 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

socialdomain_amount_ds_personal_predicts_social_1 <- t.test(amount_ds_social_domain_means_1$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
socialdomain_amount_ds_personal_predicts_social_bf_1 <- ttestBF(amount_ds_social_domain_means_1$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_amount_ds_social_predicts_social_1 <- t.test(amount_ds_social_domain_means_1$social_predicts_social, mu = 0.5)  # one sample t-test: test if social info can predict choice in social ITC task more than 50% of the time
socialdomain_amount_ds_social_predicts_social_bf_1 <- ttestBF(amount_ds_social_domain_means_1$social_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_amount_ds_personal_vs_social_1 <- t.test(amount_ds_social_domain_means_1$personal_predicts_social, amount_ds_social_domain_means_1$social_predicts_social, paired = TRUE)  # paired t-test: compare abilities of participants' similarity judgments vs. social info to predict choice in social ITC task
socialdomain_amount_ds_personal_vs_social_bf_1 <- ttestBF(amount_ds_social_domain_means_1$personal_predicts_social, amount_ds_social_domain_means_1$social_predicts_social, paired = TRUE)  # Bayes factor analysis


# DELAYS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: SD
# Participants should prefer SS option in non-social ITC task
delay_sd_social_domain_1 <- delay_d_itc_1 %>% 
  filter(amount_delay_social_info == "S_D")  # select trials where presented social info = amount similar, delay dissimilar in social ITC task

delay_sd_social_domain_means_1 <- delay_sd_social_domain_1 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

socialdomain_delay_sd_personal_predicts_social_1 <-t.test(delay_sd_social_domain_means_1$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
socialdomain_delay_sd_personal_predicts_social_bf_1 <- ttestBF(delay_sd_social_domain_means_1$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_delay_sd_social_predicts_social_1 <- t.test(delay_sd_social_domain_means_1$social_predicts_social, mu = 0.5)  # one sample t-test: test if social info can predict choice in social ITC task more than 50% of the time
socialdomain_delay_sd_social_predicts_social_bf_1 <- ttestBF(delay_sd_social_domain_means_1$social_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_delay_sd_personal_vs_social_1 <- t.test(delay_sd_social_domain_means_1$personal_predicts_social, delay_sd_social_domain_means_1$social_predicts_social, paired = TRUE)  #  paired t-test: compare abilities of participants' similarity judgments vs. social info to predict choice in social ITC task
socialdomain_delay_sd_personal_vs_social_bf_1 <- ttestBF(delay_sd_social_domain_means_1$personal_predicts_social, delay_sd_social_domain_means_1$social_predicts_social, paired = TRUE)  # Bayes factor analysis


# _Plot: Predictive ability of similarity judgments -----------------------
# Plot: Predictive ability of similarity judgments for non-social vs. social ITC tasks
amount_ds_personal_domain_plot_1 <- amount_ds_personal_domain_means_1 %>% 
  pivot_longer(c(personal_predicts_control, personal_predicts_social), names_to = "comparison", values_to = "prediction") %>%
  mutate(comparison = as.factor(comparison))

delay_sd_personal_domain_plot_1 <- delay_sd_personal_domain_means_1 %>% 
  pivot_longer(c(personal_predicts_control, personal_predicts_social), names_to = "comparison", values_to = "prediction") %>%
  mutate(comparison = as.factor(comparison))

personal_domain_plot_1 <- amount_ds_personal_domain_plot_1 %>% 
  inner_join(delay_sd_personal_domain_plot_1, by = c("subject_nr", "comparison")) %>%
  rename("Amount-focused" = prediction.x, "Delay-focused" = prediction.y) %>%  # rename columns
  pivot_longer(c(`Amount-focused`, `Delay-focused`), names_to = "condition", values_to = "prediction") %>% 
  mutate(condition = as.factor(condition))

sim_judgments_itc_predictions_personal_domain_1 <- ggplot(personal_domain_plot_1, aes(x = comparison, y = prediction, fill = comparison)) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = 1) +  # add horizontal line at y = 0.50 (line options: 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1) +  # generate mean and confidence level bars
  facet_wrap(~condition) +  # split graph by social info condition
  labs(x = "Type of choice prediction", y = "Proportion of choices predicted") +  # change x & y axes labels
  scale_x_discrete(labels = c("personal_predicts_control" = "Personal\npredicts\nnon-social", "personal_predicts_social" = "Personal\npredicts\nsocial", "social_predicts_social" = "Social\npredicts\nsocial")) +  # change x-axis tick mark labels
  scale_fill_manual(values=c("#528bcc", "#ccbe52", "#cc526e")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 20, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 18),  # change y axis tick mark font & size
        axis.text.x = element_text(face = "bold", size = 13),  # change x axis tick mark font & size
        strip.text.x = element_text(size = 20),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: Predictive ability of similarity judgments vs. social info in social ITC task
amount_ds_social_domain_plot_1 <- amount_ds_social_domain_means_1 %>% 
  pivot_longer(c(personal_predicts_social, social_predicts_social), names_to = "comparison", values_to = "prediction") %>% 
  mutate(comparison = as.factor(comparison))

delay_sd_social_domain_plot_1 <- delay_sd_social_domain_means_1 %>% 
  pivot_longer(c(personal_predicts_social, social_predicts_social), names_to = "comparison", values_to = "prediction") %>% 
  mutate(comparison = as.factor(comparison))

social_domain_plot_1 <- amount_ds_social_domain_plot_1 %>% 
  inner_join(delay_sd_social_domain_plot_1, by = c("subject_nr", "comparison")) %>%
  rename("Amount-focused" = prediction.x, "Delay-focused" = prediction.y) %>%  # rename columns
  pivot_longer(c(`Amount-focused`, `Delay-focused`), names_to = "condition", values_to = "prediction") %>% 
  mutate(condition = as.factor(condition))

sim_judgments_itc_predictions_social_domain_1 <- ggplot(social_domain_plot_1, aes(x = comparison, y = prediction, fill = comparison)) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = 1) +  # add horizontal line at y = 0.50 (line options: 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1) +  # generate mean and confidence level bars
  facet_wrap(~condition) +  # split graph by social info condition
  labs(x = "Type of choice prediction", y = "Proportion of choices predicted") +  # change x & y axes labels
  scale_x_discrete(labels = c("personal_predicts_control" = "Personal\npredicts\nnon-social", "personal_predicts_social" = "Personal\npredicts\nsocial", "social_predicts_social" = "Social\npredicts\nsocial")) +  # change x-axis tick mark labels
  scale_fill_manual(values=c("#ccbe52", "#cc526e")) +  # change boxplot fill colors
  # scale_fill_manual(values=c("#528bcc", "#cc9f52", "#cc6252")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 20, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 18),  # change y axis tick mark font & size
        axis.text.x = element_text(face = "bold", size = 13),  # change x axis tick mark font & size
        strip.text.x = element_text(size = 20),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(sim_judgments_itc_predictions_personal_domain_1 + sim_judgments_itc_predictions_social_domain_1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 25))
ggsave("figures/sim_judgment_predictions_combined_1.png", width = 15, height = 6.5)


# Study 1: Suggestibility effects on social influence ---------------------
# _Plot: Suggestibility effects -------------------------------------------
# Plot: differences in participant choices in non-social & social ITC tasks by suggestibility level for social info = delay similar, amount dissimilar
socialinfo_delaysimilar_amtdissimilar_1 <- all_data_1 %>%
  select(subject_nr, delay_pair, itc_control_choice, itc_amount_social_info, itc_delay_social_info, itc_social_choice) %>%
  filter(itc_delay_social_info == "1" & itc_amount_social_info == "0") %>%  # filter trials where social info in social ITC says delays are similar & amounts are dissimilar (coding key: "1"= similar, "0"= dissimilar)
  group_by(subject_nr) %>%
  summarise(control_itc_choice = mean(itc_control_choice, na.rm = TRUE),  # calculate mean value of participant choices in control ITC task
            social_itc_choice = mean(itc_social_choice, na.rm = TRUE)) %>%  # calculate mean value of participant choices in social ITC task
  pivot_longer(c(control_itc_choice, social_itc_choice), names_to = "itc_task", values_to = "choice")

suggestibility_socialinfo_delaysimilar_amtdissimilar_1 <- all_data_1 %>%
  select(subject_nr, miss_score, median_miss_score, suggestibility_level) %>% 
  distinct() %>% 
  inner_join(socialinfo_delaysimilar_amtdissimilar_1, by = "subject_nr") %>% 
  select(subject_nr, itc_task, choice, suggestibility_level) %>% 
  mutate(subject_nr = as.factor(subject_nr),
         itc_task = as.factor(itc_task),
         suggestibility_level = as.factor(suggestibility_level))  # convert these variables into factor type

suggestibility_ds_plot_1 <- ggplot(suggestibility_socialinfo_delaysimilar_amtdissimilar_1, aes(x = itc_task, y = choice, fill = itc_task)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~suggestibility_level) +  # split graph by participant suggestibility (high/low) level
  labs(x = "Intertemporal choice questions", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_x_discrete(labels = c("control_itc_choice" = "Non-social", "social_itc_choice" = "  Social")) +  # change x-axis tick mark labels
  scale_y_continuous(limits = c(0, 1)) +  # specify y-axis limits
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  #  change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),  #  change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: differences in participant choices in non-social & social ITC tasks by suggestibility level for social info = amount similar, delay dissimilar
socialinfo_amtsimilar_delaydissimilar_1 <- all_data_1 %>%
  select(subject_nr, amount_pair, itc_control_choice, itc_amount_social_info, itc_delay_social_info, itc_social_choice) %>%
  filter(itc_amount_social_info == "1" & itc_delay_social_info == "0") %>%  # select trials where social info in social ITC says amount is similar & delays are dissimilar (coding key: "1"= similar, "0"= dissimilar)
  group_by(subject_nr) %>%
  summarise(control_itc_choice = mean(itc_control_choice, na.rm = TRUE),  # calculate mean value of participant choices in control ITC task
            social_itc_choice = mean(itc_social_choice, na.rm = TRUE)) %>%  # calculate mean value of participant choices in social ITC task
  pivot_longer(c(control_itc_choice, social_itc_choice), names_to = "itc_task", values_to = "choice")

suggestibility_socialinfo_amtsimilar_delaydissimilar_1 <- all_data_1 %>%
  select(subject_nr, miss_score, median_miss_score, suggestibility_level) %>% 
  distinct() %>% 
  inner_join(socialinfo_amtsimilar_delaydissimilar_1, by = "subject_nr") %>% 
  select(subject_nr, itc_task, choice, suggestibility_level) %>% 
  mutate(subject_nr = as.factor(subject_nr),  # convert variables to factor type
         itc_task = as.factor(itc_task),
         suggestibility_level = as.factor(suggestibility_level))

suggestibility_sd_plot_1 <- ggplot(suggestibility_socialinfo_amtsimilar_delaydissimilar_1, aes(x = itc_task, y = choice, fill = itc_task)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~suggestibility_level) +  # split graph by participant suggestibility (high/low) level
  labs(x = "Intertemporal choice questions", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_x_discrete(labels = c("control_itc_choice" = "Non-social", "social_itc_choice" = "  Social")) +  # change x-axis tick mark labels
  scale_y_continuous(limits = c(0, 1)) +  # specify y-axis limits
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  #  change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),  #  change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(suggestibility_ds_plot_1 + suggestibility_sd_plot_1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/suggestibility_itc_1.png", width = 14, height = 7.5)


# _Analyses: Non-social vs. social ITC comparisons ------------------------
# Compare participant choices in non-social & social ITC by suggestibility level for social info = delay similar, amount dissimilar
suggestibility_itc_ds_1 <- aov_car(choice ~ itc_task * suggestibility_level + Error(subject_nr / itc_task), data = suggestibility_socialinfo_delaysimilar_amtdissimilar_1)  # ANOVA
summary(suggestibility_itc_ds_1)

suggestibility_itc_ds_bf_1 <- anovaBF(choice ~ itc_task * suggestibility_level, data = suggestibility_socialinfo_delaysimilar_amtdissimilar_1)  # Bayes factor analyses

emm_suggestibility_itc_ds_1 <- emmeans(suggestibility_itc_ds_1, ~ itc_task | suggestibility_level)  # post-hoc contrasts: interaction
emm_suggestibility_itc_ds_results_1 <- pairs(emm_suggestibility_itc_ds_1, adjust = "tukey")  # pairwise comparisons

# Compare participant choices in non-social & social ITC by suggestibility level: social info = amount similar, delay dissimilar
suggestibility_itc_sd_1 <- aov_car(choice ~ itc_task * suggestibility_level + Error(subject_nr / itc_task), data = suggestibility_socialinfo_amtsimilar_delaydissimilar_1)  # ANOVA
summary(suggestibility_itc_sd_1)

suggestibility_itc_sd_bf_1 <- anovaBF(choice ~ itc_task * suggestibility_level, data = suggestibility_socialinfo_amtsimilar_delaydissimilar_1)  # Bayes factor analyses


# Study 1: Numeracy effects -----------------------------------------------
# _Analyses: Numeracy & similarity judgments ------------------------------
numeracy_similarity_itc_1 <- all_data_1 %>% 
  select(subject_nr, amount_pair, amount_similarity, delay_pair, delay_similarity, itc_control_choice, itc_social_choice, sns_score, sns_level, overall_bnt_score)

# Compare subjective numeracy & similarity judgments
## Compare amount similarity judgments between high & low SNS participants 
numeracy_amountsimilarity_1 <- numeracy_similarity_itc_1 %>% 
  select(subject_nr, amount_pair, amount_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         amount_pair = as.factor(amount_pair),
         sns_level = as.factor(sns_level)) %>% 
  drop_na()

sns_judgments_amount_1 <- glmer(amount_similarity ~ sns_level + (1 | subject_nr), data = numeracy_amountsimilarity_1, family = binomial)
summary(sns_judgments_amount_1)

### Calculate BF
sns_judgments_amount_random_1 <- glmer(amount_similarity ~ (1 | subject_nr), data = numeracy_amountsimilarity_1, family = binomial)  # subject_nr random model
sns_judgments_amount_anova_1 <- anova(sns_judgments_amount_1, sns_judgments_amount_random_1)
sns_judgments_amount_anova_tidy_1 <- tidy(sns_judgments_amount_anova_1)
sns_judgments_amount_bf_1 <- bf_models(sns_judgments_amount_random_1, sns_judgments_amount_1)

## Compare delay similarity judgments between high & low SNS participants 
numeracy_delaysimilarity_1 <- numeracy_similarity_itc_1 %>% 
  select(subject_nr, delay_pair, delay_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         delay_pair = as.factor(delay_pair),
         sns_level = as.factor(sns_level)) %>% 
  drop_na()

sns_judgments_delay_1 <- glmer(delay_similarity ~ sns_level + (1 | subject_nr), data = numeracy_delaysimilarity_1, family = binomial)
summary(sns_judgments_delay_1)

### Calculate BF
sns_judgments_delay_random_1 <- glmer(delay_similarity ~ (1 | subject_nr), data = numeracy_delaysimilarity_1, family = binomial)  # subject_nr random model
sns_judgments_delay_anova_1 <- anova(sns_judgments_delay_1, sns_judgments_delay_random_1)
sns_judgments_delay_anova_tidy_1 <- tidy(sns_judgments_delay_anova_1)
sns_judgments_delay_bf_1 <- bf_models(sns_judgments_delay_random_1, sns_judgments_delay_1)

## Results for Markdown
sns_judgments_amount_result_1 <- sns_judgments_amount_anova_tidy_1 %>% 
  filter(term == "sns_judgments_amount_1") %>% 
  rename(pvalue = `p.value`)

sns_judgments_delay_result_1 <- sns_judgments_delay_anova_tidy_1 %>% 
  filter(term == "sns_judgments_delay_1") %>% 
  rename(pvalue = `p.value`)


# Compare objective numeracy & similarity judgments
## Compare amount similarity judgments by BNT scores 
numeracy_amountsimilarity_bnt_1 <- numeracy_similarity_itc_1 %>% 
  select(subject_nr, amount_pair, amount_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         amount_pair = as.factor(amount_pair),
         sns_level = as.factor(sns_level)) %>%  # convert variables to factor type
  drop_na()  # remove participants with any NA responses

bnt_judgments_amount_1 <- glmer(amount_similarity ~ overall_bnt_score + (1 | subject_nr), data = numeracy_amountsimilarity_bnt_1, family = binomial)
summary(bnt_judgments_amount_1)

### Calculate BF
bnt_judgments_amount_random_1 <- glmer(amount_similarity ~ (1 | subject_nr), data = numeracy_amountsimilarity_bnt_1, family = binomial)  # subject_nr random model
bnt_judgments_amount_anova_1 <- anova(bnt_judgments_amount_1, bnt_judgments_amount_random_1)
bnt_judgments_amount_anova_tidy_1 <- tidy(bnt_judgments_amount_anova_1)
bnt_judgments_amount_bf_1 <- bf_models(bnt_judgments_amount_random_1, bnt_judgments_amount_1)

## Compare delay similarity judgments by BNT scores 
numeracy_delaysimilarity_bnt_1 <- numeracy_similarity_itc_1 %>% 
  select(subject_nr, delay_pair, delay_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         delay_pair = as.factor(delay_pair),
         sns_level = as.factor(sns_level)) %>%  # convert variables to factor type
  drop_na()  # remove participants with any NA responses

bnt_judgments_delay_1 <- glmer(delay_similarity ~ overall_bnt_score + (1 | subject_nr), data = numeracy_delaysimilarity_bnt_1, family = binomial)
summary(bnt_judgments_delay_1)

### Calculate BF
bnt_judgments_delay_random_1 <- glmer(delay_similarity ~ (1 | subject_nr), data = numeracy_delaysimilarity_bnt_1, family = binomial)  # subject_nr random model
bnt_judgments_delay_anova_1 <- anova(bnt_judgments_delay_1, bnt_judgments_delay_random_1)
bnt_judgments_delay_anova_tidy_1 <- tidy(bnt_judgments_delay_anova_1)
bnt_judgments_delay_bf_1 <- bf_models(bnt_judgments_delay_random_1, bnt_judgments_delay_1)

## Results for Markdown
bnt_judgments_amount_result_1 <- bnt_judgments_amount_anova_tidy_1 %>% 
  filter(term == "bnt_judgments_amount_1") %>% 
  rename(pvalue = `p.value`)

bnt_judgments_delay_result_1 <- bnt_judgments_delay_anova_tidy_1 %>% 
  filter(term == "bnt_judgments_delay_1") %>% 
  rename(pvalue = `p.value`)


# _Plot: Numeracy & similarity judgments ----------------------------------
# Plot: combined plots for amount and delay similarity judgments between high & low SNS participants
numeracy_amount_similarity_label_1 <- numeracy_amountsimilarity_1 %>% 
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  pivot_wider(names_from = amount_pair, values_from = amount_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:44])) %>%  # calculate the proportion for the total number of times participants judged amount pairs to be similar
  select(subject_nr, sns_score, sns_level, similarity_total) %>% 
  mutate(label = rep("Amount", length(subject_nr)))  # create "label" column with "amount" indicated for each row

numeracy_delay_similarity_label_1 <- numeracy_delaysimilarity_1 %>%
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  pivot_wider(names_from = delay_pair, values_from = delay_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:44])) %>%  # calculate the proportion for the total number of times participants judged delay pairs to be similar
  select(subject_nr, sns_score, sns_level, similarity_total) %>% 
  mutate(label = rep("Delay", length(subject_nr)))  # create "label" column with "delay" indicated for each row

numeracy_amount_delay_similarity_graph_1 <- numeracy_amount_similarity_label_1 %>%
  full_join(numeracy_delay_similarity_label_1, by = c("subject_nr", "sns_score", "sns_level", "similarity_total", "label"))

sns_judgments_plot_1 <- ggplot(numeracy_amount_delay_similarity_graph_1, aes(x = sns_level, y = similarity_total, fill = sns_level)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~label) +  # split graph by amount & delay conditions
  labs(x = "Subjective numeracy", y = "Proportion judged similar") +  # change x & y axes labels
  scale_fill_manual(values=c("#E1BE6A", "#40B0A6")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        strip.text.x = element_text(size = 22),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: combined plots for amount and delay similarity judgments by BNT score
numeracy_amountsimilarity_bnt_label_1 <- numeracy_amountsimilarity_bnt_1 %>% 
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  pivot_wider(names_from = amount_pair, values_from = amount_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:44])) %>% # calculate the proportion for the total number of times participants judged amount pairs to be similar
  select(subject_nr, overall_bnt_score, similarity_total)

numeracy_delaysimilarity_bnt_label_1 <- numeracy_delaysimilarity_bnt_1 %>% 
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  pivot_wider(names_from = delay_pair, values_from = delay_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:44])) %>% # calculate the proportion for the total number of times participants judged amount pairs to be similar
  select(subject_nr, overall_bnt_score, similarity_total)

numeracy_amount_delay_combined_1 <- numeracy_amountsimilarity_bnt_label_1 %>%
  inner_join(numeracy_delaysimilarity_bnt_label_1, by = "subject_nr") %>%
  select(subject_nr, overall_bnt_score.x, similarity_total.x, similarity_total.y) %>%
  rename("Amount" = similarity_total.x, "Delay" = similarity_total.y, "overall_bnt_score" = overall_bnt_score.x) %>%  # rename columns
  gather("Amount", "Delay", key = similarity_judgment_task, value = similarity_total)

bnt_judgments_plot_1 <- ggplot(numeracy_amount_delay_combined_1, aes(x = overall_bnt_score, y = similarity_total)) +  # create graph axes labels
  geom_jitter(width = 0.01, height = 0.01, color = "grey") +
  # geom_smooth(method = "lm", se = FALSE) +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~similarity_judgment_task) +  # split graph by similarity judgment task type
  labs(x = "Objective numeracy score", y = "Proportion judged similar") +  # change x & y axes labels
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +  # set y-axis limit, specify tick mark breaks
  theme_classic(base_family = "Arial") +
  theme(panel.background = element_rect(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 18),  # change y axis tick mark font & size
        axis.text.x = element_text(face = "bold", size = 18),  # change x axis tick mark font & size
        strip.text.x = element_text(size = 20),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(bnt_judgments_plot_1 + sns_judgments_plot_1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/numeracy_judgments_1.png", width = 14, height = 7.5)


# _Analyses: Numeracy & ITC -----------------------------------------------
# Compare subjective numeracy & non-social ITC preference
numeracy_nonsocial_itc_1 <- numeracy_similarity_itc_1 %>% 
  select(subject_nr, amount_pair, delay_pair, itc_control_choice, sns_score, sns_level, overall_bnt_score) %>%
  unite(amount_delay, amount_pair, delay_pair, sep = "--") %>%  # combine amount & delay pairs
  mutate(subject_nr = as.factor(subject_nr),
         amount_delay = as.factor(amount_delay),
         sns_level = as.factor(sns_level))  # convert variables to factor type

sns_itc_1 <- glmer(itc_control_choice ~ sns_level + (1 | subject_nr), data = numeracy_nonsocial_itc_1, family = binomial)
summary(sns_itc_1)

## Calculate BF
sns_itc_random_1 <- glmer(itc_control_choice ~ (1 | subject_nr), data = numeracy_nonsocial_itc_1, family = binomial)  # subject_nr random model
sns_itc_anova_1 <- anova(sns_itc_1, sns_itc_random_1)
sns_itc_anova_tidy_1 <- tidy(sns_itc_anova_1)
sns_itc_bf_1 <- bf_models(sns_itc_random_1, sns_itc_1)

# Compare objective numeracy & non-social ITC preference
numeracy_nonsocial_itc_bnt_1 <- numeracy_similarity_itc_1 %>% 
  select(subject_nr, amount_pair, delay_pair, itc_control_choice, sns_score, sns_level, overall_bnt_score) %>%
  unite(amount_delay, amount_pair, delay_pair, sep = "--") %>%  # combine amount & delay pairs
  mutate(subject_nr = as.factor(subject_nr),
         amount_delay = as.factor(amount_delay),
         sns_level = as.factor(sns_level))

bnt_itc_1 <- glmer(itc_control_choice ~ overall_bnt_score + (1 | subject_nr), data = numeracy_nonsocial_itc_bnt_1, family = binomial)
summary(bnt_itc_1)

## Calculate BF
bnt_itc_random_1 <- glmer(itc_control_choice ~ (1 | subject_nr), data = numeracy_nonsocial_itc_bnt_1, family = binomial)  # subject_nr random model
bnt_itc_anova_1 <- anova(bnt_itc_1, bnt_itc_random_1)
bnt_itc_anova_tidy_1 <- tidy(bnt_itc_anova_1)
bnt_itc_bf_1 <- bf_models(bnt_itc_random_1, bnt_itc_1)

# Results for Markdown
bnt_itc_result_1 <- bnt_itc_anova_tidy_1 %>% 
  filter(term == "bnt_itc_1") %>% 
  rename(pvalue = `p.value`)

sns_itc_result_1 <- sns_itc_anova_tidy_1 %>% 
  filter(term == "sns_itc_1") %>% 
  rename(pvalue = `p.value`)


# _Plot: Numeracy & ITC ---------------------------------------------------
# Plot: compare SNS & non-social ITC preference
bnt_sns_itc_plot_1 <- numeracy_nonsocial_itc_1 %>% 
  pivot_wider(names_from = amount_delay, values_from = itc_control_choice) %>%  # pivot data to wide format
  mutate(LL_total = rowMeans(.[5:68]))  # calculate the proportion that participants chose LL option in non-social ITC task

sns_itc_plot_1 <- ggplot(bnt_sns_itc_plot_1, aes(x = sns_level, y = LL_total, fill = sns_level)) +  # create graph axes labels
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Subjective numeracy", y = "Proportion for larger-later") +  # change x & y axes labels
  scale_fill_manual(values=c("#E1BE6A", "#40B0A6")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        legend.position = "none")  # remove legend from graph

# Plot: compare BNT & non-social ITC preference
bnt_itc_plot_1 <- ggplot(bnt_sns_itc_plot_1, aes(x = overall_bnt_score , y = LL_total, fill = overall_bnt_score)) +  # create graph axes labels
  geom_jitter(width = 0.01, height = 0.01, color = "grey") +
  # geom_smooth(method = "lm", se = FALSE) +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Objective numeracy score", y = "Proportion for larger-later") +  # change x & y axes labels
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic(base_family = "Arial") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 26, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 22),  # change x & y axes tick mark font & size
        legend.position = "none")  # remove legend from graph

# Combine figures
(bnt_itc_plot_1 + sns_itc_plot_1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/numeracy_itc_1.png", width = 14, height = 7.5)


# Study 1: Demographic data -----------------------------------------------
demographic_data_1 <- all_data_1 %>% 
  select(subject_nr, age, ethnicity, gender) %>%
  distinct()  # remove duplicate rows
  
# Participant age descriptives
age_1 <- mean(demographic_data_1$age, na.rm = TRUE)  # calculate participant mean age
age_sd_1 <- sd(demographic_data_1$age, na.rm = TRUE) # calculate standard deviation for participant mean age

# Participant gender descriptives
gender_1 <- demographic_data_1 %>% 
  count(gender)

# Participant ethnicity descriptives
ethnicity_1 <- demographic_data_1 %>%
  count(ethnicity)


# Study 2: Similarity judgments & ITC choice comparisons --------------------------------
# Create dataset to compare choices in control & social ITC tasks across the 2 social information conditions 
itc_choice_data_2 <- all_data_2 %>%
  select(subject_nr:itc_social_choice) %>% 
  mutate(subject_nr = as.factor(subject_nr)) %>%   # convert into factor type
  group_by(subject_nr, amount_delay_social_info) %>% 
  summarise(`Non-social` = mean(itc_control_choice),  # calculate mean value of choices in control ITC task across the 2 social info versions
            Social = mean(itc_social_choice)) %>%  # calculate mean value of choices in social ITC task across the 2 social info versions
  pivot_longer(c(`Non-social`, Social), names_to = "itc_task", values_to = "choice") %>% 
  mutate(itc_task = as.factor(itc_task))


# _Plot: ITC task & social info condition effects -------------------------
itc_socialinfo_plot_2 <- ggplot(itc_choice_data_2, aes(x = amount_delay_social_info, y = choice, fill = amount_delay_social_info)) +
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_normal, size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~itc_task) +  # split graph by control & social ITC task type
  labs(x = "Amount-delay social information", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_x_discrete(labels = c("D_S" = "Amount\n-focused", "S_D" = "Delay\n-focused")) +  # change x-axis tick mark labels
  scale_fill_manual(values=c("#88CCEE", "#DDCC77")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 19),  # change x & y axes tick mark font & size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph
ggsave("figures/itc_social_info_2.png", width = 6.5, height = 6.5)


# _Analyses: ITC task & social info condition effects ---------------------
# Within-subjects ANOVA for ITC task and social info condition
itc_socialinfo_anova_2 <- aov_car(choice ~ itc_task * amount_delay_social_info + Error(subject_nr / (itc_task * amount_delay_social_info)), data = itc_choice_data_2)  # ANOVA
summary(itc_socialinfo_anova_2)

emm_itc_socialinfo_interaction_2 <- emmeans(itc_socialinfo_anova_2, ~ itc_task | amount_delay_social_info)  # post-hoc contrasts
emm_itc_socialinfo_interaction_results_2 <- pairs(emm_itc_socialinfo_interaction_2, adjust = "tukey")  # pairwise comparisons

itc_socialinfo_anova_2_bf <- anovaBF(choice ~ itc_task * amount_delay_social_info + subject_nr, data = itc_choice_data_2, whichRandom = "subject_nr")  # Bayes factor analyses


# _Plot: Amount-focused & delay-focused social info condition comparisons --------
nonsocial_social_analysis_2 <- itc_choice_data_2 %>% 
  pivot_wider(names_from = itc_task, values_from = choice)

nonsocial_social_DS_SD_plot_2 <- nonsocial_social_analysis_2 %>% 
  pivot_longer(c(`Non-social`, Social), names_to = "itc_task", values_to = "choice") %>% 
  pivot_wider(names_from = amount_delay_social_info, values_from = choice) %>% 
  rename("Amount-focused" = D_S, "Delay-focused" = S_D) %>%  # rename columns
  pivot_longer(c(`Amount-focused`, `Delay-focused`), names_to = "amount_delay_social_info", values_to = "choice") %>%
  mutate(itc_task = as.factor(itc_task),
         amount_delay_social_info = as.factor(amount_delay_social_info))  # convert variables to factor type

socialinfo_ds_sd_2 <- ggplot(nonsocial_social_DS_SD_plot_2, aes(x = itc_task, y = choice, fill = itc_task)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_boot", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~amount_delay_social_info) +  # split graph by social info condition (D-S/S-D)
  labs(x = "Intertemporal choice questions", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_y_continuous(limits = c(0, 1)) +  # expand y-axis limit to include "0" & "1" tick marks
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial")+
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  #  change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # x & y axes tick mark size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph
# ggsave("figures/socialinfo_ds_sd_2.png", width = 6.5, height = 6.5)  # save plot to "figures" folder


# _Analyses: Amount-focused & delay-focused social info condition comparisons --------
# Compare participant preference for LL option in nonsocial & social ITC tasks: DS (amount-focused) condition
## If social info = amount dissimilar, delay similar: participants should prefer LL option in social ITC task
nonsocial_social_analysis_DS_2 <- filter(nonsocial_social_analysis_2, amount_delay_social_info == "D_S")  # create dataframe for amount dissimilar-delay similar condition analysis

nonsocial_social_comparison_ds_2 <- t.test(nonsocial_social_analysis_DS_2$`Non-social`, nonsocial_social_analysis_DS_2$Social, paired = TRUE)  # paired t-test: compare participant preference for LL option in amount-focused condition for nonsocial & social ITC
nonsocial_social_comparison_ds_bf_2 <- ttestBF(nonsocial_social_analysis_DS_2$`Non-social`, nonsocial_social_analysis_DS_2$Social, paired = TRUE)  # Bayes factor analysis
nonsocial_social_comparison_ds_cohensd_2 <- cohensD(nonsocial_social_analysis_DS_2$`Non-social`, nonsocial_social_analysis_DS_2$Social)  # calculate effect size (Cohen's d)

# Compare participant preference for LL option in nonsocial & social ITC tasks: SD (delay-focused) condition
## If social info = amount similar, delay dissimilar: participants should pick SS option in social ITC task
nonsocial_social_analysis_SD_2 <- filter(nonsocial_social_analysis_2, amount_delay_social_info == "S_D")  # create dataframe for amount similar-delay dissimilar condition analysis

nonsocial_social_comparison_sd_2 <- t.test(nonsocial_social_analysis_SD_2$`Non-social`, nonsocial_social_analysis_SD_2$Social, paired = TRUE)  # paired t-test: compare participant preference for LL option in delay-focused condition for nonsocial & social ITC
nonsocial_social_comparison_sd_bf_2 <- ttestBF(nonsocial_social_analysis_SD_2$`Non-social`, nonsocial_social_analysis_SD_2$Social, paired = TRUE)  # Bayes factor analysis
nonsocial_social_comparison_sd_cohensd_2 <- cohensD(nonsocial_social_analysis_SD_2$`Non-social`, nonsocial_social_analysis_SD_2$Social)  # calculate effect size (Cohen's d)

# Compare amount-focused & delay-focused conditions in social ITC
ds_sd_social_itc_comparison_data_2 <- nonsocial_social_DS_SD_plot_2 %>% 
  filter(itc_task == "Social") %>% 
  pivot_wider(names_from = amount_delay_social_info, values_from = choice)

ds_sd_social_itc_comparison_2 <- t.test(ds_sd_social_itc_comparison_data_2$`Amount-focused`, ds_sd_social_itc_comparison_data_2$`Delay-focused`, paired = TRUE)  # paired t-test: compare participant preference for LL option in amount-focused and delay-focused social ITC
ds_sd_social_itc_comparison_bf_2 <- ttestBF(ds_sd_social_itc_comparison_data_2$`Amount-focused`, ds_sd_social_itc_comparison_data_2$`Delay-focused`, paired = TRUE)  # Bayes factor analysis
ds_sd_social_itc_comparison_cohensd_2 <- cohensD(ds_sd_social_itc_comparison_data_2$`Amount-focused`, ds_sd_social_itc_comparison_data_2$`Delay-focused`)  # calculate effect size (Cohen's d)


# Study 2: Predictive ability of similarity judgments ---------------------
# _Analyses: Predictive ability of similarity judgments for non-social vs. social ITC tasks --------
## AMOUNTS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: DS
# Participants should prefer LL option in non-social ITC task
amount_d_itc_2 <- all_data_2 %>%
  select(subject_nr:itc_social_choice) %>%
  mutate(amount_congruence = ifelse(is.na(amount_similarity) | is.na(itc_amount_social_info), NA,  # remove any NA responses
                                    ifelse(amount_similarity == itc_amount_social_info, 1, 0)),   # check if amount similarity judgment is the same as the presented social info in the social ITC task. Code response as "1" if participant's amount similarity judgment in amount judgment task is the same as the presented social info (i.e., if amount is stated as similar or dissimilar) for each amount_pair in the social ITC task; otherwise response will be coded "0"
         observed_control_choice_prediction = ifelse(is.na(amount_similarity) | is.na(itc_control_choice), NA,  # remove any NA responses
                                                     ifelse(amount_similarity == 0 & itc_control_choice == 1, 1, 0)),  # test if similarity judgment predicts choice in control ITC task. Code response as "1" if participant indicated amount_pair is dissimilar & chose LL option for control ITC question; otherwise response will be coded "0"
         observed_social_choice_prediction = ifelse(is.na(amount_similarity) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                    ifelse(amount_similarity == 0 & itc_social_choice == 1, 1, 0)),  # test if similarity judgment predicts choice in social ITC task. Code response as "1" if participant indicated amount_pair is dissimilar & chose LL option for social ITC question; otherwise response will be coded "0"
         social_info_social_choice_prediction = ifelse(is.na(itc_amount_social_info) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                       ifelse(itc_amount_social_info == 0 & itc_social_choice == 1, 1, 0)),   # test if presented social info predicts choice in social ITC task. Code response as "1" if the presented social info for amounts is dissimilar & participant chose LL option for social ITC question; otherwise response will be coded "0"
         personal_similarity_domain = ifelse(is.na(amount_similarity) | is.na(delay_similarity), NA,  # remove any NA responses
                                             ifelse(amount_similarity != delay_similarity, 1, 0)),  # test if participant's amount & delay similarity judgments are the same for both similarity judgment tasks. Code response as "0" if participant's amount & delay similarity judgments are the same; otherwise response will be coded "1"
         social_similarity_domain = ifelse(itc_amount_social_info != itc_delay_social_info, 1, 0))  # test if presented social info for amount & delay in social ITC are different. Code response as "0" if amount & delay social info are the same; otherwise response will be coded "1"

amount_ds_personal_domain_2 <- amount_d_itc_2 %>% 
  filter(amount_similarity == 0 & personal_similarity_domain == 1)  # select trials where participants judged amount pairs to be dissimilar in amount similarity judgment task, and amount similarity judgment is different from delay similarity judgment

amount_ds_personal_domain_means_2 <- amount_ds_personal_domain_2 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

personaldomain_amount_ds_personal_predicts_control_2 <- t.test(amount_ds_personal_domain_means_2$personal_predicts_control, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in control ITC task more than 50% of the time
personaldomain_amount_ds_personal_predicts_control_bf_2 <- ttestBF(amount_ds_personal_domain_means_2$personal_predicts_control, mu = 0.5)  # Bayes factor analysis

personaldomain_amount_ds_personal_predicts_social_2 <- t.test(amount_ds_personal_domain_means_2$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
personaldomain_amount_ds_personal_predicts_social_bf_2 <- ttestBF(amount_ds_personal_domain_means_2$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

personaldomain_amount_ds_personal_vs_social_2 <- t.test(amount_ds_personal_domain_means_2$personal_predicts_control, amount_ds_personal_domain_means_2$personal_predicts_social, paired = TRUE)  # paired t-test: compare abilities of participants' similarity judgments to predict choice in control vs. social ITC task
personaldomain_amount_ds_personal_vs_social_bf_2 <- ttestBF(amount_ds_personal_domain_means_2$personal_predicts_control, amount_ds_personal_domain_means_2$personal_predicts_social, paired = TRUE)  # Bayes factor analysis


# DELAYS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: SD
# Participants should prefer SS option in non-social ITC task
delay_d_itc_2 <- all_data_2 %>%
  select(subject_nr:itc_social_choice) %>%
  mutate(delay_congruence = ifelse(is.na(delay_similarity) | is.na(itc_delay_social_info), NA,  # remove any NA responses
                                   ifelse(delay_similarity == itc_delay_social_info, 1, 0)),   # check if delay similarity judgment is the same as the presented social info in the social ITC task. Code response as "1" if participant's delay similarity judgment in delay judgment task is the same as the presented social info (i.e., if delay is stated as similar or dissimilar) for each delay_pair in the social ITC task; otherwise response will be coded "0"
         observed_control_choice_prediction = ifelse(is.na(delay_similarity) | is.na(itc_control_choice), NA,  # remove any NA responses
                                                     ifelse(delay_similarity == 0 & itc_control_choice == 0, 1, 0)),  # test if similarity judgment predicts choice in control ITC task. Code response as "1" if participant indicated delay_pair is dissimilar & chose SS option for control ITC question; otherwise response will be coded "0"
         observed_social_choice_prediction = ifelse(is.na(delay_similarity) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                    ifelse(delay_similarity == 0 & itc_social_choice == 0, 1, 0)),  # test if similarity judgment predicts choice in social ITC task. Code response as "1" if participant indicated delay_pair is dissimilar & chose SS option for social ITC question; otherwise response will be coded "0"
         social_info_social_choice_prediction = ifelse(is.na(itc_delay_social_info) | is.na(itc_social_choice), NA,  # remove any NA responses
                                                       ifelse(itc_delay_social_info == 0 & itc_social_choice == 0, 1, 0)),   # test if presented social info predicts choice in social ITC task. Code response as "1" if the presented social info for delays is dissimilar & participant chose SS option for social ITC question; otherwise response will be coded "0"
         personal_similarity_domain = ifelse(is.na(amount_similarity) | is.na(delay_similarity), NA,  # remove any NA responses
                                             ifelse(amount_similarity != delay_similarity, 1, 0)),  # test if participant's amount & delay similarity judgments are the same for both similarity judgment tasks. Code response as "0" if participant's amount & delay similarity judgments are the same; otherwise response will be coded "1"
         social_similarity_domain = ifelse(amount_delay_social_info == "D_S" | amount_delay_social_info == "S_D", 1, 0))  # test if social info condition in social ITC is either DS or SD. Code response as "1" if not; otherwise response will be coded "0"

delay_sd_personal_domain_2 <- delay_d_itc_2 %>% 
  filter(delay_similarity == 0 & personal_similarity_domain == 1)  # select trials where participants judged delay pairs to be dissimilar in delay similarity judgment task, and amount similarity judgment is different from delay similarity judgment

delay_sd_personal_domain_means_2 <- delay_sd_personal_domain_2 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

personaldomain_delay_sd_personal_predicts_control_2 <- t.test(delay_sd_personal_domain_means_2$personal_predicts_control, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in control ITC task more than 50% of the time
personaldomain_delay_sd_personal_predicts_control_bf_2 <- ttestBF(delay_sd_personal_domain_means_2$personal_predicts_control, mu = 0.5)  # Bayes factor analysis

personaldomain_delay_sd_personal_predicts_social_2 <- t.test(delay_sd_personal_domain_means_2$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
personaldomain_delay_sd_personal_predicts_social_bf_2 <- ttestBF(delay_sd_personal_domain_means_2$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

personaldomain_delay_sd_personal_vs_social_2 <- t.test(delay_sd_personal_domain_means_2$personal_predicts_control, delay_sd_personal_domain_means_2$personal_predicts_social, paired = TRUE)  # paired t-test: compare abilities of participants' similarity judgments to predict choice in control vs. social ITC task
personaldomain_delay_sd_personal_vs_social_bf_2 <- ttestBF(delay_sd_personal_domain_means_2$personal_predicts_control, delay_sd_personal_domain_means_2$personal_predicts_social, paired = TRUE)  # Bayes factor analysis


# _Analyses: Predictive ability of similarity judgments vs. social info in social ITC task --------
# AMOUNTS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: DS
# Participants should prefer LL option in non-social ITC task
amount_ds_social_domain_2 <- amount_d_itc_2 %>% 
  filter(amount_delay_social_info == "D_S") # select trials where presented social info = amount dissimilar, delay similar in social ITC task

amount_ds_social_domain_means_2 <- amount_ds_social_domain_2 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

socialdomain_amount_ds_personal_predicts_social_2 <- t.test(amount_ds_social_domain_means_2$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
socialdomain_amount_ds_personal_predicts_social_bf_2 <- ttestBF(amount_ds_social_domain_means_2$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_amount_ds_social_predicts_social_2 <- t.test(amount_ds_social_domain_means_2$social_predicts_social, mu = 0.5)  # one sample t-test: test if social info can predict choice in social ITC task more than 50% of the time
socialdomain_amount_ds_social_predicts_social_bf_2 <- ttestBF(amount_ds_social_domain_means_2$social_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_amount_ds_personal_vs_social_2 <- t.test(amount_ds_social_domain_means_2$personal_predicts_social, amount_ds_social_domain_means_2$social_predicts_social, paired = TRUE)  # paired t-test: compare abilities of participants' similarity judgments vs. social info to predict choice in social ITC task
socialdomain_amount_ds_personal_vs_social_bf_2 <- ttestBF(amount_ds_social_domain_means_2$personal_predicts_social, amount_ds_social_domain_means_2$social_predicts_social, paired = TRUE)  # Bayes factor analysis


# DELAYS
# Calculate predictive ability of similarity judgments for non-social and social ITC tasks: SD
# Participants should prefer SS option in non-social ITC task
delay_sd_social_domain_2 <- delay_d_itc_2 %>% 
  filter(amount_delay_social_info == "S_D")  # select trials where presented social info = amount similar, delay dissimilar in social ITC task

delay_sd_social_domain_means_2 <- delay_sd_social_domain_2 %>% 
  group_by(subject_nr) %>%
  summarise(personal_predicts_control = mean(observed_control_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in control ITC task
            personal_predicts_social = mean(observed_social_choice_prediction, na.rm = TRUE),  # calculate mean value of amount similarity judgments' ability to predict choice in social ITC task
            social_predicts_social = mean(social_info_social_choice_prediction, na.rm = TRUE))  # calculate mean value of social information's ability to predict choice in social ITC task

socialdomain_delay_sd_personal_predicts_social_2 <- t.test(delay_sd_social_domain_means_2$personal_predicts_social, mu = 0.5)  # one sample t-test: test if participants' similarity judgments can predict choice in social ITC task more than 50% of the time
socialdomain_delay_sd_personal_predicts_social_bf_2 <- ttestBF(delay_sd_social_domain_means_2$personal_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_delay_sd_social_predicts_social_2 <- t.test(delay_sd_social_domain_means_2$social_predicts_social, mu = 0.5)  # one sample t-test: test if social info can predict choice in social ITC task more than 50% of the time
socialdomain_delay_sd_social_predicts_social_bf_2 <- ttestBF(delay_sd_social_domain_means_2$social_predicts_social, mu = 0.5)  # Bayes factor analysis

socialdomain_delay_sd_personal_vs_social_2 <- t.test(delay_sd_social_domain_means_2$personal_predicts_social, delay_sd_social_domain_means_2$social_predicts_social, paired = TRUE)  #  paired t-test: compare abilities of participants' similarity judgments vs. social info to predict choice in social ITC task
socialdomain_delay_sd_personal_vs_social_bf_2 <- ttestBF(delay_sd_social_domain_means_2$personal_predicts_social, delay_sd_social_domain_means_2$social_predicts_social, paired = TRUE)  # Bayes factor analysis


# _Plot: Predictive ability of similarity judgments -----------------------
# Plot: Predictive ability of similarity judgments for non-social vs. social ITC tasks
amount_ds_personal_domain_plot_2 <- amount_ds_personal_domain_means_2 %>%
  pivot_longer(c(personal_predicts_control, personal_predicts_social), names_to = "comparison", values_to = "prediction") %>%
  mutate(comparison = as.factor(comparison))

delay_sd_personal_domain_plot_2 <- delay_sd_personal_domain_means_2 %>%
  pivot_longer(c(personal_predicts_control, personal_predicts_social), names_to = "comparison", values_to = "prediction") %>%
  mutate(comparison = as.factor(comparison))

personal_domain_plot_2 <- amount_ds_personal_domain_plot_2 %>% 
  inner_join(delay_sd_personal_domain_plot_2, by = c("subject_nr", "comparison")) %>%
  rename("Amount-focused" = prediction.x, "Delay-focused" = prediction.y) %>%  # rename columns
  pivot_longer(c(`Amount-focused`, `Delay-focused`), names_to = "condition", values_to = "prediction") %>% 
  mutate(condition = as.factor(condition))

sim_judgments_itc_predictions_personal_domain_2 <- ggplot(personal_domain_plot_2, aes(x = comparison, y = prediction, fill = comparison)) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = 1) +  # add horizontal line at y = 0.50 (line options: 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1) +  # generate mean and confidence level bars
  facet_wrap(~condition) +  # split graph by social info condition
  labs(x = "Type of choice prediction", y = "Proportion of choices predicted") +  # change x & y axes labels
  scale_x_discrete(labels = c("personal_predicts_control" = "Personal\npredicts\nnon-social", "personal_predicts_social" = "Personal\npredicts\nsocial", "social_predicts_social" = "Social\npredicts\nsocial")) +  # change x-axis tick mark labels
  scale_fill_manual(values=c("#528bcc", "#ccbe52", "#cc526e")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 20, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 18),  # change y axis tick mark font & size
        axis.text.x = element_text(face = "bold", size = 13),  # change x axis tick mark font & size
        strip.text.x = element_text(size = 20),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: Predictive ability of similarity judgments vs. social info in social ITC task
amount_ds_social_domain_plot_2 <- amount_ds_social_domain_means_2 %>%
  pivot_longer(c(personal_predicts_social, social_predicts_social), names_to = "comparison", values_to = "prediction") %>%
  mutate(comparison = as.factor(comparison))

delay_sd_social_domain_plot_2 <- delay_sd_social_domain_means_2 %>%
  pivot_longer(c(personal_predicts_social, social_predicts_social), names_to = "comparison", values_to = "prediction") %>%
  mutate(comparison = as.factor(comparison))

social_domain_plot_2 <- amount_ds_social_domain_plot_2 %>%
  inner_join(delay_sd_social_domain_plot_2, by = c("subject_nr", "comparison")) %>%
  rename("Amount-focused" = prediction.x, "Delay-focused" = prediction.y) %>%  # rename columns
  pivot_longer(c(`Amount-focused`, `Delay-focused`), names_to = "condition", values_to = "prediction") %>% 
  mutate(condition = as.factor(condition))

sim_judgments_itc_predictions_social_domain_2 <- ggplot(social_domain_plot_2, aes(x = comparison, y = prediction, fill = comparison)) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey", size = 1) +  # add horizontal line at y = 0.50 (line options: 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1) +  # generate mean and confidence level bars
  facet_wrap(~condition) +  # split graph by social info condition
  labs(x = "Type of choice prediction", y = "Proportion of choices predicted") +  # change x & y axes labels
  scale_x_discrete(labels = c("personal_predicts_control" = "Personal\npredicts\nnon-social", "personal_predicts_social" = "Personal\npredicts\nsocial", "social_predicts_social" = "Social\npredicts\nsocial")) +  # change x-axis tick mark labels
  scale_fill_manual(values=c("#ccbe52", "#cc526e")) +  # change boxplot fill colors
  # scale_fill_manual(values=c("#528bcc", "#cc9f52", "#cc6252")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 20, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 18),  # change y axis tick mark font & size
        axis.text.x = element_text(face = "bold", size = 13),  # change x axis tick mark font & size
        strip.text.x = element_text(size = 20),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(sim_judgments_itc_predictions_personal_domain_2 + sim_judgments_itc_predictions_social_domain_2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 25))
ggsave("figures/sim_judgment_predictions_combined_2.png", width = 15, height = 6.5)


# Study 2: Suggestibility effects on social influence ---------------------
# _Plot: Suggestibility effects -------------------------------------------
# Plot: differences in participant choices in non-social & social ITC tasks by suggestibility level for social info = delay similar, amount dissimilar
socialinfo_delaysimilar_amtdissimilar_2 <- all_data_2 %>%
  select(subject_nr, delay_pair, itc_control_choice, itc_amount_social_info, itc_delay_social_info, itc_social_choice) %>%
  filter(itc_delay_social_info == "1" & itc_amount_social_info == "0") %>%  # filter trials where social info in social ITC says delays are similar & amounts are dissimilar (coding key: "1"= similar, "0"= dissimilar)
  group_by(subject_nr) %>%
  summarise(control_itc_choice = mean(itc_control_choice, na.rm = TRUE),  # calculate mean value of participant choices in control ITC task
            social_itc_choice = mean(itc_social_choice, na.rm = TRUE)) %>%  # calculate mean value of participant choices in social ITC task
  pivot_longer(c(control_itc_choice, social_itc_choice), names_to = "itc_task", values_to = "choice")

suggestibility_socialinfo_delaysimilar_amtdissimilar_2 <- all_data_2 %>% 
  select(subject_nr, miss_score, median_miss_score, suggestibility_level) %>% 
  distinct() %>% 
  inner_join(socialinfo_delaysimilar_amtdissimilar_2, by = "subject_nr") %>% 
  select(subject_nr, itc_task, choice, suggestibility_level) %>% 
  mutate(subject_nr = as.factor(subject_nr),
         itc_task = as.factor(itc_task),
         suggestibility_level = as.factor(suggestibility_level))  # convert these variables into factor type

suggestibility_ds_plot_2 <- ggplot(suggestibility_socialinfo_delaysimilar_amtdissimilar_2, aes(x = itc_task, y = choice, fill = itc_task)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~suggestibility_level) +  # split graph by participant suggestibility (high/low) level
  labs(x = "Intertemporal choice questions", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_x_discrete(labels = c("control_itc_choice" = "Non-social", "social_itc_choice" = "  Social")) +  # change x-axis tick mark labels
  scale_y_continuous(limits = c(0, 1)) +  # specify y-axis limits
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  #  change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),  #  change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: differences in participant choices in non-social & social ITC tasks by suggestibility level for social info = amount similar, delay dissimilar
socialinfo_amtsimilar_delaydissimilar_2 <- all_data_2 %>%
  select(subject_nr, amount_pair, itc_control_choice, itc_amount_social_info, itc_delay_social_info, itc_social_choice) %>%
  filter(itc_amount_social_info == "1" & itc_delay_social_info == "0") %>%  # select trials where social info in social ITC says amount is similar & delays are dissimilar (coding key: "1"= similar, "0"= dissimilar)
  group_by(subject_nr) %>%
  summarise(control_itc_choice = mean(itc_control_choice, na.rm = TRUE),  # calculate mean value of participant choices in control ITC task
            social_itc_choice = mean(itc_social_choice, na.rm = TRUE)) %>%  # calculate mean value of participant choices in social ITC task
  pivot_longer(c(control_itc_choice, social_itc_choice), names_to = "itc_task", values_to = "choice")

suggestibility_socialinfo_amtsimilar_delaydissimilar_2 <- all_data_2 %>% 
  select(subject_nr, miss_score, median_miss_score, suggestibility_level) %>% 
  distinct() %>% 
  inner_join(socialinfo_amtsimilar_delaydissimilar_2, by = "subject_nr") %>% 
  select(subject_nr, itc_task, choice, suggestibility_level) %>% 
  mutate(subject_nr = as.factor(subject_nr),  # convert variables to factor type
         itc_task = as.factor(itc_task),
         suggestibility_level = as.factor(suggestibility_level))

suggestibility_sd_plot_2 <- ggplot(suggestibility_socialinfo_amtsimilar_delaydissimilar_2, aes(x = itc_task, y = choice, fill = itc_task)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~suggestibility_level) +  # split graph by participant suggestibility (high/low) level
  labs(x = "Intertemporal choice questions", y = "Proportion for larger, later") +  # change x & y axes labels
  scale_x_discrete(labels = c("control_itc_choice" = "Non-social", "social_itc_choice" = "  Social")) +  # change x-axis tick mark labels
  scale_y_continuous(limits = c(0, 1)) +  # specify y-axis limits
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  #  change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),  #  change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(suggestibility_ds_plot_2 + suggestibility_sd_plot_2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/suggestibility_itc_2.png", width = 14, height = 7.5)


# _Analyses: Non-social vs. social ITC comparisons ------------------------
# Compare participant choices in non-social & social ITC by suggestibility level for social info = delay similar, amount dissimilar
suggestibility_itc_ds_2 <- aov_car(choice ~ itc_task * suggestibility_level + Error(subject_nr / itc_task), data = suggestibility_socialinfo_delaysimilar_amtdissimilar_2)  # ANOVA
summary(suggestibility_itc_ds_2)

suggestibility_itc_ds_bf_2 <- anovaBF(choice ~ itc_task * suggestibility_level, data = suggestibility_socialinfo_delaysimilar_amtdissimilar_2)  # Bayes factor analyses

# Compare participant choices in non-social & social ITC by suggestibility level: social info = amount similar, delay dissimilar
suggestibility_itc_sd_2 <- aov_car(choice ~ itc_task * suggestibility_level + Error(subject_nr / itc_task), data = suggestibility_socialinfo_amtsimilar_delaydissimilar_2)  # ANOVA
summary(suggestibility_itc_sd_2)

suggestibility_itc_sd_bf_2 <- anovaBF(choice ~ itc_task * suggestibility_level, data = suggestibility_socialinfo_amtsimilar_delaydissimilar_2)  # Bayes factor analyses


# Study 2: Numeracy effects -----------------------------------------------
# _Analyses: Numeracy & similarity judgments ------------------------------
numeracy_similarity_itc_2 <- all_data_2 %>% 
  select(subject_nr, amount_pair, amount_similarity, delay_pair, delay_similarity, itc_control_choice, itc_social_choice, sns_score, sns_level, overall_bnt_score)

# Compare subjective numeracy & similarity judgments
## Compare amount similarity judgments between high & low SNS participants 
numeracy_amountsimilarity_2 <- numeracy_similarity_itc_2 %>% 
  select(subject_nr, amount_pair, amount_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         amount_pair = as.factor(amount_pair),
         sns_level = as.factor(sns_level)) %>% 
  drop_na()

sns_judgments_amount_2 <- glmer(amount_similarity ~ sns_level + (1 | subject_nr), data = numeracy_amountsimilarity_2, family = binomial)
summary(sns_judgments_amount_2)

### Calculate BF
sns_judgments_amount_random_2 <- glmer(amount_similarity ~ (1 | subject_nr), data = numeracy_amountsimilarity_2, family = binomial)  # subject_nr random model
sns_judgments_amount_anova_2 <- anova(sns_judgments_amount_2, sns_judgments_amount_random_2)
sns_judgments_amount_anova_tidy_2 <- tidy(sns_judgments_amount_anova_2)
sns_judgments_amount_bf_2 <- bf_models(sns_judgments_amount_random_2, sns_judgments_amount_2)

## Compare delay similarity judgments between high & low SNS participants 
numeracy_delaysimilarity_2 <- numeracy_similarity_itc_2 %>% 
  select(subject_nr, delay_pair, delay_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         delay_pair = as.factor(delay_pair),
         sns_level = as.factor(sns_level)) %>% 
  drop_na()

sns_judgments_delay_2 <- glmer(delay_similarity ~ sns_level + (1 | subject_nr), data = numeracy_delaysimilarity_2, family = binomial)
summary(sns_judgments_delay_2)

### Calculate BF
sns_judgments_delay_random_2 <- glmer(delay_similarity ~ (1 | subject_nr), data = numeracy_delaysimilarity_2, family = binomial)  # subject_nr random model
sns_judgments_delay_anova_2 <- anova(sns_judgments_delay_2, sns_judgments_delay_random_2)
sns_judgments_delay_anova_tidy_2 <- tidy(sns_judgments_delay_anova_2)
sns_judgments_delay_bf_2 <- bf_models(sns_judgments_delay_random_2, sns_judgments_delay_2)

## Results for Markdown
sns_judgments_amount_result_2 <- sns_judgments_amount_anova_tidy_2 %>% 
  filter(term == "sns_judgments_amount_2") %>% 
  rename(pvalue = `p.value`)

sns_judgments_delay_result_2 <- sns_judgments_delay_anova_tidy_2 %>% 
  filter(term == "sns_judgments_delay_2") %>% 
  rename(pvalue = `p.value`)


# Compare objective numeracy & similarity judgments
## Compare amount similarity judgments by BNT scores 
numeracy_amountsimilarity_bnt_2 <- numeracy_similarity_itc_2 %>% 
  select(subject_nr, amount_pair, amount_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         amount_pair = as.factor(amount_pair),
         sns_level = as.factor(sns_level)) %>%  # convert variables to factor type
  drop_na()  # remove participants with any NA responses

bnt_judgments_amount_2 <- glmer(amount_similarity ~ overall_bnt_score + (1 | subject_nr), data = numeracy_amountsimilarity_bnt_2, family = binomial)
summary(bnt_judgments_amount_2)

### Calculate BF
bnt_judgments_amount_random_2 <- glmer(amount_similarity ~ (1 | subject_nr), data = numeracy_amountsimilarity_bnt_2, family = binomial)  # subject_nr random model
bnt_judgments_amount_anova_2 <- anova(bnt_judgments_amount_2, bnt_judgments_amount_random_2)
bnt_judgments_amount_anova_tidy_2 <- tidy(bnt_judgments_amount_anova_2)
bnt_judgments_amount_bf_2 <- bf_models(bnt_judgments_amount_random_2, bnt_judgments_amount_2)

## Compare delay similarity judgments by BNT scores 
numeracy_delaysimilarity_bnt_2 <- numeracy_similarity_itc_2 %>% 
  select(subject_nr, delay_pair, delay_similarity, sns_score, sns_level, overall_bnt_score) %>%
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  mutate(subject_nr = as.factor(subject_nr),
         delay_pair = as.factor(delay_pair),
         sns_level = as.factor(sns_level)) %>%  # convert variables to factor type
  drop_na()  # remove participants with any NA responses

bnt_judgments_delay_2 <- glmer(delay_similarity ~ overall_bnt_score + (1 | subject_nr), data = numeracy_delaysimilarity_bnt_2, family = binomial)
summary(bnt_judgments_delay_2)

### Calculate BF
bnt_judgments_delay_random_2 <- glmer(delay_similarity ~ (1 | subject_nr), data = numeracy_delaysimilarity_bnt_2, family = binomial)  # subject_nr random model
bnt_judgments_delay_anova_2 <- anova(bnt_judgments_delay_2, bnt_judgments_delay_random_2)
bnt_judgments_delay_anova_tidy_2 <- tidy(bnt_judgments_delay_anova_2)
bnt_judgments_delay_bf_2 <- bf_models(bnt_judgments_delay_random_2, bnt_judgments_delay_2)

## Results for Markdown
bnt_judgments_amount_result_2 <- bnt_judgments_amount_anova_tidy_2 %>% 
  filter(term == "bnt_judgments_amount_2") %>% 
  rename(pvalue = `p.value`)

bnt_judgments_delay_result_2 <- bnt_judgments_delay_anova_tidy_2 %>% 
  filter(term == "bnt_judgments_delay_2") %>% 
  rename(pvalue = `p.value`)


# _Plot: Numeracy & similarity judgments ----------------------------------
# Plot: combined plots for amount and delay similarity judgments between high & low SNS participants
numeracy_amount_similarity_label_2 <- numeracy_amountsimilarity_2 %>% 
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  pivot_wider(names_from = amount_pair, values_from = amount_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:28])) %>%  # calculate the proportion for the total number of times participants judged amount pairs to be similar
  select(subject_nr, sns_score, sns_level, similarity_total) %>% 
  mutate(label = rep("Amount", length(subject_nr)))  # create "label" column with "amount" indicated for each row

numeracy_delay_similarity_label_2 <- numeracy_delaysimilarity_2 %>%
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  pivot_wider(names_from = delay_pair, values_from = delay_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:28])) %>%  # calculate the proportion for the total number of times participants judged delay pairs to be similar
  select(subject_nr, sns_score, sns_level, similarity_total) %>% 
  mutate(label = rep("Delay", length(subject_nr)))  # create "label" column with "delay" indicated for each row

numeracy_amount_delay_similarity_graph_2 <- numeracy_amount_similarity_label_2 %>%
  full_join(numeracy_delay_similarity_label_2, by = c("subject_nr", "sns_score", "sns_level", "similarity_total", "label"))

sns_judgments_plot_2 <- ggplot(numeracy_amount_delay_similarity_graph_2, aes(x = sns_level, y = similarity_total, fill = sns_level)) +  # create graph axes labels, add fill color to boxplot
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~label) +  # split graph by amount & delay conditions
  labs(x = "Subjective numeracy", y = "Proportion judged similar") +  # change x & y axes labels
  scale_fill_manual(values=c("#E1BE6A", "#40B0A6")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        strip.text.x = element_text(size = 22),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: combined plots for amount and delay similarity judgments by BNT score
numeracy_amountsimilarity_bnt_label_2 <- numeracy_amountsimilarity_bnt_2 %>% 
  distinct(amount_pair, subject_nr, .keep_all = TRUE) %>%  # remove amount_pair duplicates
  pivot_wider(names_from = amount_pair, values_from = amount_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:28])) %>% # calculate the proportion for the total number of times participants judged amount pairs to be similar
  select(subject_nr, overall_bnt_score, similarity_total)

numeracy_delaysimilarity_bnt_label_2 <- numeracy_delaysimilarity_bnt_2 %>% 
  distinct(delay_pair, subject_nr, .keep_all = TRUE) %>%  # remove delay_pair duplicates
  pivot_wider(names_from = delay_pair, values_from = delay_similarity) %>%  # pivot data to wide format
  mutate(similarity_total = rowMeans(.[5:28])) %>% # calculate the proportion for the total number of times participants judged amount pairs to be similar
  select(subject_nr, overall_bnt_score, similarity_total)

numeracy_amount_delay_combined_2 <- numeracy_amountsimilarity_bnt_label_2 %>%
  inner_join(numeracy_delaysimilarity_bnt_label_2, by = "subject_nr") %>%
  select(subject_nr, overall_bnt_score.x, similarity_total.x, similarity_total.y) %>%
  rename("Amount" = similarity_total.x, "Delay" = similarity_total.y, "overall_bnt_score" = overall_bnt_score.x) %>%  # rename columns
  gather("Amount", "Delay", key = similarity_judgment_task, value = similarity_total)

bnt_judgments_plot_2 <- ggplot(numeracy_amount_delay_combined_2, aes(x = overall_bnt_score, y = similarity_total)) +  # create graph axes labels
  geom_jitter(width = 0.01, height = 0.01, color = "grey") +
  # geom_smooth(method = "lm", se = FALSE) +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  facet_wrap(~similarity_judgment_task) +  # split graph by similarity judgment task type
  labs(x = "Objective numeracy score", y = "Proportion judged similar") +  # change x & y axes labels
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +  # set y-axis limit, specify tick mark breaks
  theme_classic(base_family = "Arial") +
  theme(panel.background = element_rect(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 18),  # change y axis tick mark font & size
        axis.text.x = element_text(face = "bold", size = 18),  # change x axis tick mark font & size
        strip.text.x = element_text(size = 20),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(bnt_judgments_plot_2 + sns_judgments_plot_2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/numeracy_judgments_2.png", width = 14, height = 7.5)


# _Analyses: Numeracy & ITC -----------------------------------------------
# Compare subjective numeracy & non-social ITC preference
numeracy_nonsocial_itc_2 <- numeracy_similarity_itc_2 %>% 
  select(subject_nr, amount_pair, delay_pair, itc_control_choice, sns_score, sns_level, overall_bnt_score) %>%
  unite(amount_delay, amount_pair, delay_pair, sep = "--") %>%  # combine amount & delay pairs
  mutate(subject_nr = as.factor(subject_nr),
         amount_delay = as.factor(amount_delay),
         sns_level = as.factor(sns_level)) %>%  # convert variables to factor type
  drop_na()

sns_itc_2 <- glmer(itc_control_choice ~ sns_level + (1 | subject_nr), data = numeracy_nonsocial_itc_2, family = binomial)
summary(sns_itc_2)

## Calculate BF
sns_itc_random_2 <- glmer(itc_control_choice ~ (1 | subject_nr), data = numeracy_nonsocial_itc_2, family = binomial)  # subject_nr random model
sns_itc_anova_2 <- anova(sns_itc_2, sns_itc_random_2)
sns_itc_anova_tidy_2 <- tidy(sns_itc_anova_2)
sns_itc_bf_2 <- bf_models(sns_itc_random_2, sns_itc_2)

# Compare objective numeracy & non-social ITC preference
numeracy_nonsocial_itc_bnt_2 <- numeracy_similarity_itc_2 %>% 
  select(subject_nr, amount_pair, delay_pair, itc_control_choice, sns_score, sns_level, overall_bnt_score) %>%
  unite(amount_delay, amount_pair, delay_pair, sep = "--") %>%  # combine amount & delay pairs
  mutate(subject_nr = as.factor(subject_nr),
         amount_delay = as.factor(amount_delay),
         sns_level = as.factor(sns_level)) %>% 
  drop_na()

bnt_itc_2 <- glmer(itc_control_choice ~ overall_bnt_score + (1 | subject_nr), data = numeracy_nonsocial_itc_bnt_2, family = binomial)
summary(bnt_itc_2)

## Calculate BF
bnt_itc_random_2 <- glmer(itc_control_choice ~ (1 | subject_nr), data = numeracy_nonsocial_itc_bnt_2, family = binomial)  # subject_nr random model
bnt_itc_anova_2 <- anova(bnt_itc_2, bnt_itc_random_2)
bnt_itc_anova_tidy_2 <- tidy(bnt_itc_anova_2)
bnt_itc_bf_2 <- bf_models(bnt_itc_random_2, bnt_itc_2)

# Results for Markdown
bnt_itc_result_2 <- bnt_itc_anova_tidy_2 %>%
  filter(term == "bnt_itc_2") %>%
  rename(pvalue = `p.value`)

sns_itc_result_2 <- sns_itc_anova_tidy_2 %>% 
  filter(term == "sns_itc_2") %>% 
  rename(pvalue = `p.value`)


# _Plot: Numeracy & ITC ---------------------------------------------------
# Plot: compare SNS & non-social ITC preference
bnt_sns_itc_plot_2 <- numeracy_nonsocial_itc_2 %>% 
  pivot_wider(names_from = amount_delay, values_from = itc_control_choice) %>%  # pivot data to wide format
  mutate(LL_total = rowMeans(.[5:36]))  # calculate the proportion that participants chose LL option in non-social ITC task

sns_itc_plot_2 <- ggplot(bnt_sns_itc_plot_2, aes(x = sns_level, y = LL_total, fill = sns_level)) +  # create graph axes labels
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Subjective numeracy", y = "Proportion for larger-later") +  # change x & y axes labels
  scale_fill_manual(values=c("#E1BE6A", "#40B0A6")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 23, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 23, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        legend.position = "none")  # remove legend from graph

# Plot: compare BNT & non-social ITC preference
bnt_itc_plot_2 <- ggplot(bnt_sns_itc_plot_2, aes(x = overall_bnt_score , y = LL_total, fill = overall_bnt_score)) +  # create graph axes labels
  geom_jitter(width = 0.01, height = 0.01, color = "grey") +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Objective numeracy score", y = "Proportion for larger-later") +  # change x & y axes labels
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic(base_family = "Arial") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 26, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 22),  # change x & y axes tick mark font & size
        legend.position = "none")  # remove legend from graph

# Combine figures
(bnt_itc_plot_2 + sns_itc_plot_2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/numeracy_itc_2.png", width = 14, height = 7.5)


# Study 2: Demographic data -----------------------------------------------
demographic_data_2 <- all_data_2 %>% 
  select(subject_nr, age, ethnicity, gender) %>%
  distinct()  # remove duplicate rows

age_2 <- mean(demographic_data_2$age, na.rm = TRUE)  # calculate participant mean age
age_sd_2 <- sd(demographic_data_2$age, na.rm = TRUE) # calculate standard deviation for participant mean age

# Participant gender descriptives
gender_2 <- demographic_data_2 %>% 
  count(gender)

# Participant ethnicity descriptives
ethnicity_2 <- demographic_data_2 %>%
  count(ethnicity)


# Study 3: Effect of social information on similarity judgments ------
# _Analyses: Effect of social info on similarity judgments ---------------
similarity_judgments_data_3 <- all_data_3 %>% 
  separate(control_social, into = "condition", sep = "_") %>% 
  mutate(condition = as.factor(condition),
         subject_nr = as.factor(subject_nr))

# Social info is stated as "similar" in similarity judgment tasks
similarity_judgments_info_similar_3 <- similarity_judgments_data_3 %>% 
  filter(social_info == "1" | social_info == "0")  # select control & social info "similar" similarity judgment task questions

## Hypothesis testing
judgments_similarity_task_full_similar_3 <- glmer(similarity_judgment ~ condition * amount_delay + (1 | subject_nr), data = similarity_judgments_info_similar_3, family = binomial)  # full model with social info and similarity judgment task type interaction
summary(judgments_similarity_task_full_similar_3)

judgments_similarity_task_condition_similar_3 <- glmer(similarity_judgment ~ condition + (1 | subject_nr), data = similarity_judgments_info_similar_3, family = binomial)  # main effect of social info 
summary(judgments_similarity_task_condition_similar_3)

judgments_similarity_task_amount_delay_similar_3 <- glmer(similarity_judgment ~ amount_delay + (1 | subject_nr), data = similarity_judgments_info_similar_3, family = binomial)  # main effect of similarity judgment task type
summary(judgments_similarity_task_amount_delay_similar_3)

## Likelihood ratio tests for model comparison
# Random effects
judgments_similarity_intercept_similar_3 <- glm(similarity_judgment ~ 1, data = similarity_judgments_info_similar_3, family = binomial)  # empty random model
judgments_similarity_random_similar_3 <- glmer(similarity_judgment ~ (1 | subject_nr), data = similarity_judgments_info_similar_3, family = binomial)  # subject_nr random model

judgments_similarity_random_anova_similar_3 <- anova(judgments_similarity_random_similar_3, judgments_similarity_intercept_similar_3)
judgments_similarity_random_anova_tidy_similar_3 <- tidy(judgments_similarity_random_anova_similar_3)

judgments_similarity_anova_similar_3 <- anova(judgments_similarity_task_full_similar_3, judgments_similarity_task_condition_similar_3, judgments_similarity_task_amount_delay_similar_3, judgments_similarity_random_similar_3)
judgments_similarity_anova_tidy_similar_3 <- tidy(judgments_similarity_anova_similar_3)

# Calculate BFs
judgments_similarity_full_bf_similar_3 <- bf_models(judgments_similarity_random_similar_3, judgments_similarity_task_full_similar_3)
judgments_similarity_condition_bf_similar_3 <- bf_models(judgments_similarity_random_similar_3, judgments_similarity_task_condition_similar_3)
judgments_similarity_amount_delay_bf_similar_3 <- bf_models(judgments_similarity_random_similar_3, judgments_similarity_task_amount_delay_similar_3)

## Results for Markdown
social_info_present_similar_result_3 <- judgments_similarity_anova_tidy_similar_3 %>% 
  filter(term == "judgments_similarity_task_condition_similar_3") %>% 
  rename(pvalue = `p.value`)

social_info_present_similar_interaction_3 <- judgments_similarity_anova_tidy_similar_3 %>% 
  filter(term == "judgments_similarity_task_full_similar_3") %>% 
  rename(pvalue = `p.value`)


# Social info is stated as "dissimilar" in similarity judgment tasks
similarity_judgments_info_dissimilar_3 <- similarity_judgments_data_3 %>% 
  filter(social_info == "2" | social_info == "0")  # select control & social info "dissimilar" similarity judgment task questions

## Hypothesis testing
judgments_similarity_task_full_dissimilar_3 <- glmer(similarity_judgment ~ condition * amount_delay + (1 | subject_nr), data = similarity_judgments_info_dissimilar_3, family = binomial)  # full model with social info and similarity judgment task type interaction
summary(judgments_similarity_task_full_dissimilar_3)

judgments_similarity_task_condition_dissimilar_3 <- glmer(similarity_judgment ~ condition + (1 | subject_nr), data = similarity_judgments_info_dissimilar_3, family = binomial)  # main effect of social info 
summary(judgments_similarity_task_condition_dissimilar_3)

judgments_similarity_task_amount_delay_dissimilar_3 <- glmer(similarity_judgment ~ amount_delay + (1 | subject_nr), data = similarity_judgments_info_dissimilar_3, family = binomial)  # main effect of similarity judgment task type
summary(judgments_similarity_task_amount_delay_dissimilar_3)

## Likelihood ratio tests for model comparison
# Random effects
judgments_similarity_intercept_dissimilar_3 <- glm(similarity_judgment ~ 1, data = similarity_judgments_info_dissimilar_3, family = binomial)  # empty random model
judgments_similarity_random_dissimilar_3 <- glmer(similarity_judgment ~ (1 | subject_nr), data = similarity_judgments_info_dissimilar_3, family = binomial)  # subject_nr random model

judgments_similarity_random_anova_dissimilar_3 <- anova(judgments_similarity_random_dissimilar_3, judgments_similarity_intercept_dissimilar_3)
judgments_similarity_random_anova_tidy_dissimilar_3 <- tidy(judgments_similarity_random_anova_dissimilar_3)

judgments_similarity_anova_dissimilar_3 <- anova(judgments_similarity_task_full_dissimilar_3, judgments_similarity_task_condition_dissimilar_3, judgments_similarity_task_amount_delay_dissimilar_3, judgments_similarity_random_dissimilar_3)
judgments_similarity_anova_tidy_dissimilar_3 <- tidy(judgments_similarity_anova_dissimilar_3)

# Calculate BFs
judgments_similarity_full_bf_dissimilar_3 <- bf_models(judgments_similarity_random_dissimilar_3, judgments_similarity_task_full_dissimilar_3)
judgments_similarity_condition_bf_dissimilar_3 <- bf_models(judgments_similarity_random_dissimilar_3, judgments_similarity_task_condition_dissimilar_3)
judgments_similarity_amount_delay_bf_dissimilar_3 <- bf_models(judgments_similarity_random_dissimilar_3, judgments_similarity_task_amount_delay_dissimilar_3)

## Results for Markdown
social_info_present_dissimilar_result_3 <- judgments_similarity_anova_tidy_dissimilar_3 %>% 
  filter(term == "judgments_similarity_task_condition_dissimilar_3") %>% 
  rename(pvalue = `p.value`)

social_info_present_dissimilar_interaction_3 <- judgments_similarity_anova_tidy_dissimilar_3 %>% 
  filter(term == "judgments_similarity_task_full_dissimilar_3") %>% 
  rename(pvalue = `p.value`)


# _Plot: Effect of social info on similarity judgments --------------------
amount_data_plot_3 <- all_data_3 %>%
  select(subject_nr:social_info) %>% 
  filter(task_social_info == "amount_social_info" & amount_delay == "amount_similarity") %>% 
  select(subject_nr, control_social, similarity_judgment, social_info) %>% 
  pivot_wider(names_from = social_info, values_from = similarity_judgment) %>%  # pivot data to wide format
  group_by(subject_nr) %>% 
  summarise(amount_control_mean = mean(`0`, na.rm = TRUE),  # calculate mean of similarity judgments for each social info condition
            amount_sim_mean = mean(`1`, na.rm = TRUE),
            amount_dis_mean = mean(`2`, na.rm = TRUE))

delay_data_plot_3 <- all_data_3 %>%
  select(subject_nr:social_info) %>% 
  filter(task_social_info == "delay_social_info" & amount_delay == "delay_similarity") %>% 
  select(subject_nr, control_social, similarity_judgment, social_info) %>% 
  pivot_wider(names_from = social_info, values_from = similarity_judgment) %>%  # pivot data to wide format
  group_by(subject_nr) %>% 
  summarise(delay_control_mean = mean(`0`, na.rm = TRUE), # calculate mean of similarity judgments for each social info condition
            delay_sim_mean = mean(`1`, na.rm = TRUE),
            delay_dis_mean = mean(`2`, na.rm = TRUE))

judgments_plot_3 <- amount_data_plot_3 %>%
  full_join(delay_data_plot_3, by = "subject_nr") %>%  # add delay similarity judgments data
  pivot_longer(c(amount_control_mean:delay_dis_mean), names_to = "judgment_task", values_to = "mean_similarity_judgment") %>%  # pivot data to long format
  mutate(social_info_condition = ifelse(grepl("control", judgment_task), "Control",  # insert "Control" label in "social_info_condition" column if social info condition is listed as "control" in "judgment_task" column
                                        ifelse(grepl("sim", judgment_task), "Similar",  # insert "Similar" label in "social_info_condition" column if social info condition is listed as "sim" in "judgment_task" column
                                               ifelse(grepl("dis", judgment_task), "Dissimilar", NA)))) %>%  # insert "Dissimilar" label in "social_info_condition" column if social info condition is listed as "dis" in "judgment_task" column
  mutate(amount_delay = ifelse(grepl("amount", judgment_task), "Amount",  # insert "Amount" label in "amount_delay" column if similarity judgment task is listed as "amount" in "judgment_task" column
                               ifelse(grepl("delay", judgment_task), "Delay", NA))) %>%  # insert "Delay" label in "amount_delay" column if similarity judgment task is listed as "delay" in "judgment_task" column
  mutate(social_info_condition = as.factor(social_info_condition),
         amount_delay = as.factor(amount_delay))

# Plot: social info condition is listed as "similar"
judgments_plot_info_similar_3 <- judgments_plot_3 %>% 
  filter(social_info_condition == "Control" | social_info_condition == "Similar")  # select control & social info "similar" similarity judgment task questions

judgments_plot_similar_interaction_plot_3 <- ggplot(judgments_plot_info_similar_3, aes(x = social_info_condition, y = mean_similarity_judgment, fill = social_info_condition)) +
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Social information: similar", y = "Proportion judged similar") +  # change x & y axes labels
  scale_x_discrete(labels = c("Control" = "Non-social", "Similar" = "Social")) +  # change x-axis tick mark labels
  facet_wrap(~amount_delay) +
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Plot: social info condition is listed as "dissimilar"
judgments_plot_info_dissimilar_3 <- judgments_plot_3 %>% 
  filter(social_info_condition == "Control" | social_info_condition == "Dissimilar")  # select control & social info "similar" similarity judgment task questions

judgments_plot_dissimilar_interaction_plot_3 <- ggplot(judgments_plot_info_dissimilar_3, aes(x = social_info_condition, y = mean_similarity_judgment, fill = social_info_condition)) +
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Social information: dissimilar", y = "Proportion judged similar") +  # change x & y axes labels
  scale_x_discrete(labels = c("Control" = "Non-social", "Dissimilar" = "Social")) +  # change x-axis tick mark labels
  facet_wrap(~amount_delay) +
  scale_fill_manual(values=c("#F0E442", "#009E73")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph

# Combine figures
(judgments_plot_similar_interaction_plot_3 + judgments_plot_dissimilar_interaction_plot_3) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 25))
ggsave("figures/judgments_socialinfo_combined_3.png", width = 15, height = 6.5)


# Study 3: Suggestibility effects on social influence ---------------------
# _Analyses: Suggestibility effects on social influence -------------------
# Social info is stated as "similar" in similarity judgment tasks
all_data_suggestibility_similar_3 <- similarity_judgments_info_similar_3 %>% 
  filter(suggestibility_level != "NA")  # remove participants with NA values for suggestibility column

judgments_miss_similar_full_3 <- glmer(similarity_judgment ~ condition * suggestibility_level + (1 | subject_nr), data = all_data_suggestibility_similar_3, family = binomial)  # full model with social info condition and suggestibility level interaction
summary(judgments_miss_similar_full_3)
judgments_miss_similar_full_task_type_3 <- glmer(similarity_judgment ~ condition * amount_delay * suggestibility_level + (1 | subject_nr), data = all_data_suggestibility_similar_3, family = binomial)  # full model with social info condition, similarity judgment task type & suggestibility level interaction
summary(judgments_miss_similar_full_task_type_3)

## Calculate BFs
judgments_miss_similar_random_3 <- glmer(similarity_judgment ~ condition + (1 | subject_nr), data = all_data_suggestibility_similar_3, family = binomial)  # random model
judgments_miss_similar_anova_3 <- anova(judgments_miss_similar_full_3, judgments_miss_similar_full_task_type_3, judgments_miss_similar_random_3)
judgments_miss_similar_anova_tidy_3 <- tidy(judgments_miss_similar_anova_3)

judgments_miss_similar_full_bf_3 <- bf_models(judgments_miss_similar_random_3, judgments_miss_similar_full_3)
judgments_miss_similar_full_task_type_bf_3 <- bf_models(judgments_miss_similar_random_3, judgments_miss_similar_full_task_type_3)
## Result: Individual suggestibility level did not interact with social information condition to predict similarity judgments when the social information presented was "similar" (i.e., participants high in suggestibility did not judge value pairs as more similar in the social information condition compared to participants low in suggestibility).


# Social info is stated as "dissimilar" in similarity judgment tasks
all_data_suggestibility_dissimilar_3 <- similarity_judgments_info_dissimilar_3 %>% 
  filter(suggestibility_level != "NA")  # remove participants with NA values for suggestibility column

judgments_miss_dissimilar_full_3 <- glmer(similarity_judgment ~ condition * suggestibility_level + (1 | subject_nr), data = all_data_suggestibility_dissimilar_3, family = binomial)  # full model with social info condition and suggestibility level interaction
summary(judgments_miss_dissimilar_full_3)
judgments_miss_dissimilar_full_task_type_3 <- glmer(similarity_judgment ~ condition * amount_delay * suggestibility_level + (1 | subject_nr), data = all_data_suggestibility_dissimilar_3, family = binomial)  # full model with social info condition, similarity judgment task type & suggestibility level interaction
summary(judgments_miss_dissimilar_full_task_type_3)

## Calculate BFs
judgments_miss_dissimilar_random_3 <- glmer(similarity_judgment ~ condition + (1 | subject_nr), data = all_data_suggestibility_dissimilar_3, family = binomial)  # random model
judgments_miss_dissimilar_anova_3 <- anova(judgments_miss_dissimilar_full_3, judgments_miss_dissimilar_full_task_type_3, judgments_miss_dissimilar_random_3)
judgments_miss_dissimilar_anova_tidy_3 <- tidy(judgments_miss_dissimilar_anova_3)

judgments_miss_dissimilar_full_bf_3 <- bf_models(judgments_miss_dissimilar_random_3, judgments_miss_dissimilar_full_3)
judgments_miss_dissimilar_full_task_type_bf_3 <- bf_models(judgments_miss_dissimilar_random_3, judgments_miss_dissimilar_full_task_type_3)
## Result: Individual suggestibility level did not interact with social information condition to predict similarity judgments when the social information presented was "dissimilar" (i.e., participants high in suggestibility did not judge value pairs as more dissimilar in the social information condition compared to participants low in suggestibility).

## Results for Markdown
suggestibility_similar_result_3 <- judgments_miss_similar_anova_tidy_3 %>% 
  filter(term == "judgments_miss_similar_full_3") %>% 
  rename(pvalue = `p.value`)

suggestibility_dissimilar_result_3 <- judgments_miss_dissimilar_anova_tidy_3 %>% 
  filter(term == "judgments_miss_dissimilar_full_3") %>% 
  rename(pvalue = `p.value`)


# _Plot: Suggestibility effects -------------------------------------------
suggestibility_plot_label_3 <- all_data_3 %>% 
  select(subject_nr, miss_score, suggestibility_level) %>% 
  distinct()

suggestibility_plot_3 <- judgments_plot_3 %>% 
  left_join(suggestibility_plot_label_3, by = "subject_nr") %>% 
  mutate(social_info_condition_overall = ifelse(grepl("control", social_info_condition), "control",  # insert "control" label in "social_info_condition_overall" column if social info condition is listed as "control" in "social_info_condition" column
                                                ifelse(grepl("similar", social_info_condition), "social", NA))) %>%  # insert "social" label in "social_info_condition_overall" column if social info condition is listed as "similar/dissimilar" in "social_info_condition" column
  mutate(suggestibility_level = as.factor(suggestibility_level),  # convert variables to factor type
         social_info_condition_overall =  as.factor(social_info_condition_overall)) %>% 
  filter(suggestibility_level != "NA" & mean_similarity_judgment != "NA")  # remove participants with no suggestibility scores and NA responses for similarity judgments

## Plot: effect of suggestibility level on social information effect
ggplot(suggestibility_plot_3, aes(x = social_info_condition, y = mean_similarity_judgment, fill = social_info_condition)) +
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_boot, size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Social information condition", y = "Proportion judged similar") +  # change x & y axes labels
  scale_x_discrete(labels = c("Control" = "Non-social", "Dissimilar" = "  Dissimilar")) +  # change x-axis tick mark labels
  facet_wrap(~suggestibility_level) +
  scale_fill_manual(values=c("#ff8336", "#3694ff", "#8f4af0")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text.y = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        axis.text.x = element_text(face = "bold", size = 12),
        strip.text.x = element_text(size = 23),  # change facet label size
        legend.position = "none")  # remove legend from graph
ggsave("figures/suggestibility_social_info_3.png", width = 6.5, height = 6.5)  # save plot to "figures" folder


# Study 3: Numeracy effects on similarity judgments -----------------------
# _Analyses: Numeracy effects on similarity judgments ---------------------
# Objective numeracy
all_data_bnt_3 <- all_data_3 %>%
  filter(overall_bnt_score != "NA" & similarity_judgment != "NA") %>%   # remove participants with no objective numeracy scores and NA responses for similarity judgments
  select(-c(miss_score, suggestibility_level, sns_score, sns_level)) %>% 
  mutate(subject_nr = as.factor(subject_nr)) %>% 
  drop_na()

bnt_judgments_3 <- glmer(similarity_judgment ~ overall_bnt_score + (1 | subject_nr), data = all_data_bnt_3, family = binomial)
summary(bnt_judgments_3)

## Calculate BF for objective numeracy
bnt_random_3 <- glmer(similarity_judgment ~ (1 | subject_nr), data = all_data_bnt_3, family = binomial)  # subject_nr random model
bnt_anova_3 <- anova(bnt_judgments_3, bnt_random_3)
bnt_anova_tidy_3 <- tidy(bnt_anova_3)
bnt_judgments_bf_3 <- bf_models(bnt_random_3, bnt_judgments_3)

# Subjective numeracy
all_data_sns_3 <- all_data_3 %>%
  filter(sns_level != "NA" & similarity_judgment != "NA" ) %>%   # remove participants with no subjective numeracy scores and NA responses for similarity judgments
  select(-c(miss_score, suggestibility_level, overall_bnt_score)) %>% 
  mutate(subject_nr = as.factor(subject_nr)) %>% 
  drop_na()


sns_judgments_3 <- glmer(similarity_judgment ~ sns_level + (1 | subject_nr), data = all_data_sns_3, family = binomial)
summary(sns_judgments_3)

## Calculate BF for subjective numeracy
sns_random_3 <- glmer(similarity_judgment ~ (1 | subject_nr), data = all_data_sns_3, family = binomial)  # subject_nr random model
sns_anova_3 <- anova(sns_judgments_3, sns_random_3)
sns_anova_tidy_3 <- tidy(sns_anova_3)
sns_judgments_bf_3 <- bf_models(sns_random_3, sns_judgments_3)

## Results for Markdown
bnt_judgments_result_3 <- bnt_anova_tidy_3 %>% 
  filter(term == "bnt_judgments_3") %>% 
  rename(pvalue = `p.value`)

sns_judgments_result_3 <- sns_anova_tidy_3 %>% 
  filter(term == "sns_judgments_3") %>% 
  rename(pvalue = `p.value`)


# _Plot: Numeracy effects -------------------------------------------------
# Plot: proportion of similarity judgments according to BNT level
bnt_plot_label_3 <- all_data_3 %>% 
  select(subject_nr, overall_bnt_score) %>% 
  distinct()

bnt_plot_3 <- judgments_plot_3 %>% 
  left_join(bnt_plot_label_3, by = "subject_nr") %>% 
  filter(overall_bnt_score != "NA" & mean_similarity_judgment != "NA") %>%  # remove participants with no BNT score and NA responses for similarity judgments
  mutate(subject_nr = as.factor(subject_nr))

bnt_judgments_plot_3 <- ggplot(bnt_plot_3, aes(x = overall_bnt_score, y = mean_similarity_judgment, fill = overall_bnt_score)) +
  geom_jitter(width = 0.01, height = 0.01, color = "grey") +
  stat_summary(fun.data = "mean_cl_normal", size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Objective numeracy score", y = "Proportion judged similar") +  # change x & y axes labels
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        legend.position = "none")  # remove legend from graph

# Plot: proportion of similarity judgments according to SNS level
sns_plot_label_3 <- all_data_3 %>% 
  select(subject_nr, sns_score, sns_level) %>% 
  distinct()

sns_plot_3 <- judgments_plot_3 %>% 
  left_join(sns_plot_label_3, by = "subject_nr") %>% 
  mutate(sns_level = as.factor(sns_level)) %>% 
  filter(sns_level != "NA" & mean_similarity_judgment != "NA") %>%  # remove participants with no SNS level and NA responses for similarity judgments
  mutate(subject_nr = as.factor(subject_nr))

sns_judgments_plot_3 <- ggplot(sns_plot_3, aes(x = sns_level, y = mean_similarity_judgment, fill = sns_level)) +
  geom_boxplot() +  # create boxplot
  stat_summary(fun.data = mean_cl_normal, size = 1.25) +  # generate mean and confidence level bars
  labs(x = "Subjective numeracy", y = "Proportion judged similar") +  # change x & y axes labels
  scale_fill_manual(values=c("#E1BE6A", "#40B0A6")) +  # change boxplot fill colors
  theme_classic(base_family = "Arial") +
  theme(axis.title.y = element_text(face = "bold", size = 26, margin = margin(t = 0, r = 10, b = 0, l = 0)),  # change y axis title font, size & margin
        axis.title.x = element_text(face = "bold", size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),  # change x axis title font, size & margin
        axis.text = element_text(face = "bold", size = 20),  # change x & y axes tick mark font & size
        legend.position = "none")  # remove legend from graph

# Combine figures
(bnt_judgments_plot_3 + sns_judgments_plot_3) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20))
ggsave("figures/numeracy_judgments_3.png", width = 14, height = 7.5)


# Study 3: Demographic data -----------------------------------------------
demographic_data_3 <- all_data_3 %>% 
  select(subject_nr, age, ethnicity, gender) %>%
  distinct()  # remove duplicate rows

# Participant age descriptives
age_3 <- mean(demographic_data_3$age, na.rm = TRUE)  # calculate participant mean age
age_sd_3 <- sd(demographic_data_3$age, na.rm = TRUE)  # calculate standard deviation for participant age

# Participant gender descriptives
gender_3 <- demographic_data_3 %>%
  count(gender)

# Participant ethnicity descriptives
ethnicity_3 <- demographic_data_3 %>% 
  count(ethnicity)

