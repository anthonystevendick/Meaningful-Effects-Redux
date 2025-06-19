
###################################
#                                 #
#       Bilingual Re-analysis     #
#           ABCD 5.1 release      #
#                                 #
###################################

#### Load libraries
library(mosaic)
library(ggplot2)
library(WRS2)
library(lm.beta)
library(gamm4)
library(dplyr)
library(plyr)
library(rstatix)
library(jtools)
library(pbkrtest)
library(r2glmm)

# Set working directory
setwd("your directory")

# Load datasets (separated into imaging and non-imaging due to size)
nda5.1_nonimaging <- readRDS("nonimaging_data_excluding_nt_5.1.rds")
nda5.1_imaging <- readRDS("all_imaging_data_5.1.rds")

# Recode household income
nda5.1_nonimaging$household.income <- cut(nda5.1_nonimaging$demo_comb_income_v2,
                         breaks = c(0, 5, 8, 10),
                         labels = c("household.income[<50K]", "household.income[>=50K & <100K]", "household.income[>=100K]"),
                         include.lowest = TRUE)

# Recode education
education_mapping <- c(
  "0" = "high.educ<HS Diploma", "1" = "high.educ<HS Diploma", "2" = "high.educ<HS Diploma",
  "3" = "high.educ<HS Diploma", "4" = "high.educ<HS Diploma", "5" = "high.educ<HS Diploma",
  "6" = "high.educ<HS Diploma", "7" = "high.educ<HS Diploma", "8" = "high.educ<HS Diploma",
  "9" = "high.educ<HS Diploma", "10" = "high.educ<HS Diploma", "11" = "high.educ<HS Diploma",
  "12" = "high.educ<HS Diploma", "13" = "high.educHS Diploma/GED", "14" = "high.educHS Diploma/GED",
  "15" = "high.educSome College", "16" = "high.educSome College", "17" = "high.educSome College",
  "18" = "high.educBachelor", "19" = "high.educPost Graduate Degree",
  "20" = "high.educPost Graduate Degree", "21" = "high.educPost Graduate Degree",
  "777" = "Refused to answer", "999" = "Don't Know"
)

nda5.1_nonimaging$high_educ_category <- education_mapping[as.character(nda5.1_nonimaging$demo_prnt_ed_v2)]

# Remove "Refused to answer" and "Don't Know"
nda5.1_nonimaging <- nda5.1_nonimaging %>%
  mutate(high_educ_category = ifelse(
    high_educ_category %in% c("Refused to answer", "Don't Know"),
    NA,
    high_educ_category
  ))

# Set ordered levels for education
education_levels_ordered <- c(
  "high.educ<HS Diploma",
  "high.educHS Diploma/GED",
  "high.educSome College",
  "high.educBachelor",
  "high.educPost Graduate Degree"
)

nda5.1_nonimaging$high_educ_category <- factor(
  nda5.1_nonimaging$high_educ_category,
  levels = education_levels_ordered,
  ordered = TRUE
)

# Remove sex == 3 at baseline
nda5.1_nonimaging <- nda5.1_nonimaging %>%
  group_by(src_subject_id) %>%
  filter(!(demo_sex_v2.x == 3 & eventname == "baseline_year_1_arm_1")) %>%
  ungroup()

# Recode race/ethnicity missing codes
nda5.1_nonimaging <- nda5.1_nonimaging %>%
  mutate(demo_ethn_v2 = ifelse(demo_ethn_v2 %in% c(777, 999), NA, demo_ethn_v2))

# Subset nonimaging data
selected_columns <- c("src_subject_id", "eventname", "rel_family_id.x", "site_id_l", "interview_age",
                      "genetic_pc_1", "genetic_pc_2", "genetic_pc_3", "genetic_pc_4",
                      "genetic_pc_5", "genetic_pc_6", "genetic_pc_7", "genetic_pc_8",
                      "genetic_pc_9", "genetic_pc_10", "demo_sex_v2.x",
                      "high_educ_category", "household.income", "demo_ethn_v2",
                      "nihtbx_reading_uncorrected", "nihtbx_picture_uncorrected",
                      "nihtbx_list_uncorrected", "nihtbx_pattern_uncorrected",
                      "nihtbx_picvocab_uncorrected", "nihtbx_flanker_uncorrected",
                      "nihtbx_cardsort_uncorrected", "accult_q1_y", "accult_q2_y",
                      "accult_q3_dropdwn_y", "accult_q4_y", "accult_q5_y")

nda5.1_nonimaging_subset <- nda5.1_nonimaging[selected_columns]

# Subset imaging data without filtering
imaging_columns <- c("src_subject_id", "eventname",
                     "tfmri_sst_all_beh_total_mssrt",
                     "tfmri_sst_all_beh_total_issrt",
                     "tfmri_sst_beh_performflag")

nda5.1_imaging_subset <- nda5.1_imaging[imaging_columns]

# Subset to baseline only
nda5.1_nonimaging_subset_baseline <- subset(nda5.1_nonimaging_subset, eventname == "baseline_year_1_arm_1")
nda5.1_imaging_subset_baseline <- subset(nda5.1_imaging_subset, eventname == "baseline_year_1_arm_1")

# Merge datasets and retain one version of `eventname`
nda5.1merged <- merge(
  nda5.1_nonimaging_subset_baseline,
  nda5.1_imaging_subset_baseline[, !(names(nda5.1_imaging_subset_baseline) %in% c("eventname"))],
  by = "src_subject_id",
  all = TRUE
)

# Rename columns (due to combining, these were renamed in the merge)
names(nda5.1merged)[names(nda5.1merged) == "rel_family_id.x"] <- "rel_family_id"
names(nda5.1merged)[names(nda5.1merged) == "demo_sex_v2.x"] <- "demo_sex_v2"


###VERY IMPORTANT! MUST DO QC FLAG AND REMOVE NEGATIVE SSRT VALUES
###OTHERWISE YOU WILL GET JUNK RESULTS

# Create SST QC flag
nda5.1merged$sst_pass_qc <- ifelse(nda5.1merged$tfmri_sst_beh_performflag == 1, 1, 0)

# Reverse score SST variables, excluding invalid (e.g., negative) SSRT values
nda5.1merged$mean_SSRTr <- ifelse(
  nda5.1merged$sst_pass_qc == 1 &
    nda5.1merged$tfmri_sst_all_beh_total_mssrt >= 0,
  -nda5.1merged$tfmri_sst_all_beh_total_mssrt,
  NA
)

nda5.1merged$integration_SSRTr <- ifelse(
  nda5.1merged$sst_pass_qc == 1 &
    nda5.1merged$tfmri_sst_all_beh_total_issrt >= 0,
  -nda5.1merged$tfmri_sst_all_beh_total_issrt,
  NA
)

# Filter to valid (non-NA) reversed SSRT values
valid_ssrt <- nda5.1merged %>%
    filter(!is.na(mean_SSRTr))

# Summary statistics
summary(valid_ssrt$mean_SSRTr)

# Histogram
hist(
    valid_ssrt$mean_SSRTr,
    breaks = 50,
    main = "Reversed SSRT (mean_SSRTr)",
    xlab = "Reversed SSRT (ms)",
    col = "lightblue",
    border = "white"
)

# Save and reload merged dataset in case you want to start here
#saveRDS(nda5.1merged, "nda5.1merged.rds")
#nda5.1merged <- readRDS("nda5.1merged.rds")

###Start here after initial cleaning

#recode the accult_q2_y variable into a binary "Bilingual Status", 0 = not bilingual; 1 = bilingual, changing 777 to NA

nda5.1merged$bilingual_status<-factor(ifelse(nda5.1merged$accult_q2_y == 777, NA, ifelse(nda5.1merged$accult_q2_y == 0, 0, ifelse(nda5.1merged$accult_q2_y == 1, 1, NA))))
table(nda5.1merged$bilingual_status)

#dimension a 'bilingual degree' variable, where 1 = participant said they were bilingual, and they speak the other language with friends all the time, most of the time,
#or equally, OR they speak the other language with family all the time, most of the time, or equally. Otherwise they get a 0.

nda5.1merged$bilingual_degree <- ifelse(nda5.1merged$bilingual_status == 0, 0, ifelse(nda5.1merged$bilingual_status == 1 & (as.numeric(nda5.1merged$accult_q4_y) <= 3 | as.numeric(nda5.1merged$accult_q5_y) <= 3), 1, 0))

table(nda5.1merged$bilingual_degree)

#dimension a continuous 'bilingual use' variable, and reverse-score so that if participants speak the other language with friends all the time, most of the time...,
#they will receive high scores on this measure (range 0-8, with 8 indicating a high-degree of other language use)

nda5.1merged$bilingual_use<-10-(as.numeric(nda5.1merged$accult_q4_y)+as.numeric(nda5.1merged$accult_q5_y))
table(nda5.1merged$bilingual_use)

#compute new IQ measure

nda5.1merged$fluidIQ<-scale((nda5.1merged$nihtbx_picture_uncorrected+nda5.1merged$nihtbx_list_uncorrected+nda5.1merged$nihtbx_pattern_uncorrected)/3)
hist(nda5.1merged$fluidIQ)

###################################################
###################################################
##Select Variables for OLS and Multilevel Models###
###################################################
###################################################

#https://cran.r-project.org/web/packages/partR2/vignettes/Using_partR2.html
#https://www.theanalysisfactor.com/r-squared-for-mixed-effects-models/


abcd_subset<-nda5.1merged # create a new data frame

#Descriptive Statistics by bilingual grouping
stat_vocab<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(nihtbx_picvocab_uncorrected, na.rm = TRUE),sd=sd(nihtbx_picvocab_uncorrected, na.rm = TRUE))
stat_flanker<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(nihtbx_flanker_uncorrected, na.rm = TRUE),sd=sd(nihtbx_flanker_uncorrected, na.rm = TRUE))
stat_card<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(nihtbx_cardsort_uncorrected, na.rm = TRUE),sd=sd(nihtbx_cardsort_uncorrected, na.rm = TRUE))
stat_SST<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(mean_SSRTr, na.rm = TRUE),sd=sd(mean_SSRTr, na.rm = TRUE))

degree_vocab<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(nihtbx_picvocab_uncorrected, na.rm = TRUE),sd=sd(nihtbx_picvocab_uncorrected, na.rm = TRUE))
degree_flanker<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(nihtbx_flanker_uncorrected, na.rm = TRUE),sd=sd(nihtbx_flanker_uncorrected, na.rm = TRUE))
degree_card<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(nihtbx_cardsort_uncorrected, na.rm = TRUE),sd=sd(nihtbx_cardsort_uncorrected, na.rm = TRUE))
degree_SST<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(mean_SSRTr, na.rm = TRUE),sd=sd(mean_SSRTr, na.rm = TRUE))

use_vocab<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(nihtbx_picvocab_uncorrected, na.rm = TRUE),sd=sd(nihtbx_picvocab_uncorrected, na.rm = TRUE))
use_flanker<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(nihtbx_flanker_uncorrected, na.rm = TRUE),sd=sd(nihtbx_flanker_uncorrected, na.rm = TRUE))
use_card<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(nihtbx_cardsort_uncorrected, na.rm = TRUE),sd=sd(nihtbx_cardsort_uncorrected, na.rm = TRUE))
use_SST<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(mean_SSRTr, na.rm = TRUE),sd=sd(mean_SSRTr, na.rm = TRUE))

############Individual Models############
#I tried doing a loop for this but it was a mess

##Bilingual Status

mod_vocab_status<-lmer(nihtbx_picvocab_uncorrected ~ bilingual_status + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_vocab_status)
summ(mod_vocab_status, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_vocab_status, partial = TRUE)

mod_SSRTr_status<-lmer(mean_SSRTr ~ bilingual_status + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_SSRTr_status)
summ(mod_SSRTr_status, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)

mod_flanker_status<-lmer(nihtbx_flanker_uncorrected ~ bilingual_status + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_flanker_status)
summ(mod_flanker_status, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_flanker_status, partial = TRUE)

mod_cardsort_status<-lmer(nihtbx_cardsort_uncorrected ~ bilingual_status + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_cardsort_status)
summ(mod_cardsort_status, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_cardsort_status, partial = TRUE)

##Bilingual Degree

mod_vocab_degree<-lmer(nihtbx_picvocab_uncorrected ~ bilingual_degree + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_vocab_degree)
summ(mod_vocab_degree, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_vocab_degree, partial = TRUE)

mod_SSRTr_degree<-lmer(mean_SSRTr ~ bilingual_degree + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_SSRTr_degree)
summ(mod_SSRTr_degree, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_SSRTr_degree, partial = TRUE)

mod_flanker_degree<-lmer(nihtbx_flanker_uncorrected ~ bilingual_degree + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_flanker_degree)
summ(mod_flanker_degree, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_flanker_degree, partial = TRUE)

mod_cardsort_degree<-lmer(nihtbx_cardsort_uncorrected ~ bilingual_degree + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_cardsort_degree)
summ(mod_cardsort_degree, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_cardsort_degree, partial = TRUE)

##Bilingual Use

mod_vocab_use<-lmer(nihtbx_picvocab_uncorrected ~ bilingual_use + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_vocab_use)
summ(mod_vocab_use, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_vocab_use, partial = TRUE)

mod_SSRTr_use<-lmer(mean_SSRTr ~ bilingual_use + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_SSRTr_use)
summ(mod_SSRTr_use, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_SSRTr_use, partial = TRUE)

mod_flanker_use<-lmer(nihtbx_flanker_uncorrected ~ bilingual_use + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_flanker_use)
summ(mod_flanker_use, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_flanker_use, partial = TRUE)

mod_cardsort_use<-lmer(nihtbx_cardsort_uncorrected ~ bilingual_use + interview_age + 
         demo_sex_v2 + genetic_pc_1 + genetic_pc_2 + genetic_pc_3 + 
         genetic_pc_4 + genetic_pc_5 + genetic_pc_6 + genetic_pc_7 + 
         genetic_pc_8 + genetic_pc_9 + genetic_pc_10 + high_educ_category + 
         household.income + nihtbx_reading_uncorrected + fluidIQ + (1| site_id_l / rel_family_id ), data = abcd_subset)

summary(mod_cardsort_use)
jtools::summ(mod_cardsort_use, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)
r2beta(mod_cardsort_use, partial = TRUE)

# Load required packages
library(lme4)
library(jtools)
library(xtable)

####VERY IMPORTANT: r is calculated from r squared in this code, so 
####need to look at beta from model to get direction of effect.

# Define the models with remapped names
mod_list <- list(
  "Bilingual Status: Vocabulary" = mod_vocab_status,
  "Bilingual Status: SSRT" = mod_SSRTr_status,
  "Bilingual Status: Flanker" = mod_flanker_status,
  "Bilingual Status: Cardsort" = mod_cardsort_status,
  "Bilingual Degree: Vocabulary" = mod_vocab_degree,
  "Bilingual Degree: SSRT" = mod_SSRTr_degree,
  "Bilingual Degree: Flanker" = mod_flanker_degree,
  "Bilingual Degree: Cardsort" = mod_cardsort_degree,
  "Bilingual Use: Vocabulary" = mod_vocab_use,
  "Bilingual Use: SSRT" = mod_SSRTr_use,
  "Bilingual Use: Flanker" = mod_flanker_use,
  "Bilingual Use: Cardsort" = mod_cardsort_use
)

# Initialize an empty data frame to store results
results <- data.frame(Model = character(),
                      `Semipartial r` = numeric(),
                      `Semipartial r^2` = numeric(),
                      `95% CI (Lower)` = numeric(),
                      `95% CI (Upper)` = numeric(),
                      `Degrees of Freedom` = numeric(), # Add new column for degrees of freedom
                      `p-value` = numeric(), # Moved p-value to far right
                      stringsAsFactors = FALSE)

# Initialize list to store beta and SE for TOST
beta_se_list <- list()

# Loop through each model and extract the required information
for (mod_name in names(mod_list)) {
  mod <- mod_list[[mod_name]]
  # Get semipartial r values
  r_values <- data.frame(r2beta(mod, partial = TRUE))
  sorted_r_values <- r_values[order(as.numeric(rownames(r_values)), decreasing = FALSE), ]

  # Get p-values and standardized estimates

  # Standardized model summary with CIs
  coefs <- data.frame(jtools::summ(mod, scale = TRUE, confint = TRUE, transform.response = TRUE, pvals = TRUE)$coeftable)

  # Get the second row after intercept = bilingual predictor
  beta_std <- coefs$Est.[2]
  
  # Calculate SE from CI (95% => z = 1.96)
  ci_lower <- coefs$X2.5.[2]
  ci_upper <- coefs$X97.5.[2]
  se_std <- (ci_upper - ci_lower) / (2 * 1.96)

  # Store for TOST later
  beta_se_list[[mod_name]] <- list(beta = beta_std, se = se_std)

  # Get p-value and df for bilingual predictor
  p_value <- round(coefs$p[2], 3)
  degrees_of_freedom <- round(coefs$d.f.[2], 3)

  # Extract required information for the second variable
  semipartial_r <- round(sqrt(sorted_r_values$Rsq[2]), 3)
  semipartial_r2 <- round(sorted_r_values$Rsq[2], 3)
  ci_lower <- round(sorted_r_values$lower.CL[2], 3)
  ci_upper <- round(sorted_r_values$upper.CL[2], 3)
  
  # Add information to results data frame
  results <- rbind(results, data.frame(Model = mod_name,
                                      `\\textit{\\(r_{sp}\\)}` = semipartial_r,
                                      `\\textit{\\(R^2_{sp}\\)}` = semipartial_r2,
                                      `\\textit{\\(R^2_{sp}\\) 95\\% CI Lower}` = ci_lower,
                                      `\\textit{\\(R^2_{sp}\\) 95\\% CI Upper}` = ci_upper,
                                      `\\textit{Degrees of Freedom}` = degrees_of_freedom,
                                      `\\textit{p-value}` = p_value))
}

# Print results data frame
print(results)

# Convert results data frame to LaTeX format using xtable
library(xtable)
results_table <- xtable(
  results,
  caption = "Summary of models with \\textit{$r_{sp}$}, \\textit{$R^2_{sp}$} with 95\\% CIs, and \\textit{p}.",
  digits = c(0, 0, 3, 3, 3, 3, 1, 4)
)

# Set alignment and column names
align(results_table) <- rep("r", ncol(results) + 1)
colnames(results_table) <- c(
  "Model",
  "\\textit{$r_{sp}$}",
  "\\textit{$R^2_{sp}$}",
  "\\textit{$R^2_{sp}$} 95\\% CI Lower",
  "\\textit{$R^2_{sp}$} 95\\% CI Upper",
  "Degrees of Freedom",
  "\\textit{p}"
)

# Print LaTeX table
print(results_table, caption.placement = "top", include.rownames = FALSE, sanitize.text.function = identity)


# Define TOST equivalence testing function. Change the interval as desired.
equivbs <- function(beta, se, equiv_int = c(-0.1, 0.1), alpha = 0.05) {
  # Calculate standard error difference
  z_upper <- (beta - equiv_int[2]) / se
  z_lower <- (beta - equiv_int[1]) / se
  
  # p-value using Two One-Sided Tests
  p.value <- pnorm(z_upper) - pnorm(z_lower)

  # Confidence interval for beta
  lower <- beta - qnorm(1 - alpha) * se
  upper <- beta + qnorm(1 - alpha) * se
  CI <- round(c(lower, upper), 4)

  # Decision
  decision <- if (CI[1] > equiv_int[1] & CI[2] < equiv_int[2]) {
    "✅ Equivalent: Beta lies within the equivalence interval."
  } else {
    "❌ Not Equivalent: Beta is outside the equivalence interval."
  }

  list(`P value` = round(p.value, 4), CI = CI, Decision = decision)
}

# Equivalence bounds
equiv_bounds <- c(-0.1, 0.1)

# Initialize and populate plot_df
plot_df <- data.frame(
    Model = character(),
    Beta = numeric(),
    SE = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    stringsAsFactors = FALSE
)

for (mod_name in names(beta_se_list)) {
    beta <- beta_se_list[[mod_name]]$beta
    se <- beta_se_list[[mod_name]]$se
    eq <- equivbs(beta, se, equiv_int = equiv_bounds)
    
    plot_df <- rbind(plot_df, data.frame(
        Model = mod_name,
        Beta = beta,
        SE = se,
        CI_Lower = eq$CI[1],
        CI_Upper = eq$CI[2],
        stringsAsFactors = FALSE
    ))
}

# Assign Decision based on CI
plot_df$Decision <- ifelse(
    plot_df$CI_Lower > -0.1 & plot_df$CI_Upper < 0.1,
    "Equivalent",
    "Not Equivalent"
)
plot_df$Decision <- factor(plot_df$Decision, levels = c("Not Equivalent", "Equivalent"))

# Add visual spacer rows
spacers <- data.frame(
    Model = c(" ", "  "),  # Different space characters to create unique factor levels
    Beta = NA,
    SE = NA,
    CI_Lower = NA,
    CI_Upper = NA,
    Decision = NA,
    stringsAsFactors = FALSE
)

# Insert spacer rows at desired positions
plot_df_with_gaps <- rbind(
    plot_df[1:4, ],        # Bilingual Status
    spacers[1, ],
    plot_df[5:8, ],        # Bilingual Degree
    spacers[2, ],
    plot_df[9:12, ]        # Bilingual Use
)

# Redefine factor levels to preserve vertical ordering (top-to-bottom)
plot_df_with_gaps$Model <- factor(plot_df_with_gaps$Model, levels = rev(plot_df_with_gaps$Model))

# Define Ravenclaw colors
ravenclaw_blue <- "#222F5B"
ravenclaw_bronze <- "#B08D57"
ravenclaw_colors <- c("Equivalent" = ravenclaw_blue, "Not Equivalent" = ravenclaw_bronze)

# Create the plot
plot <- ggplot(plot_df_with_gaps, aes(x = Model, y = Beta, color = Decision)) +
    geom_pointrange(aes(ymin = CI_Lower, ymax = CI_Upper), size = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "gray40") +
    coord_flip() +
    scale_color_manual(values = ravenclaw_colors, name = "Equivalence Decision", na.translate = FALSE) +
    labs(
        x = "Model (Predictor → Outcome)",
        y = expression(Standardized~beta),
        title = "Equivalence Test for \u03B2\n95% CI vs Equivalence Bounds (±0.10)"
    ) +
    theme_bw() +
    theme(
        text = element_text(size = 18),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, lineheight = 1.2)
    )


# Save the plot as a TIFF file with 600 dpi
ggsave("equivalence_plot.tiff", plot, dpi = 600)
ggsave("equivalence_plot.png", plot, dpi = 600)

###
###
###