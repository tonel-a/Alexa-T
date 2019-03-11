rm(list = ls())

library(haven)

# Adam's WD
setwd("/home/adam/MEGA/Research Projects/Interpersonal Processes and Depression (Cross-sectional)/Data Analysis/")
# Alexa's WD
setwd("C:\\Users\\lexto\\OneDrive\\Desktop\\Research")

# Read in data
d <- read_sav("./cross sectional study on depression.sav")

# Remove useless columns
d <- d[, !colnames(d) %in% c("RecordedDate", "ResponseId", "DistributionChannel", "UserLanguage")]


colnames(d) <- c("start", "end", "status", "progress", "duration", "finished", "consent",
                 "gender", "gender_other", "age", "birthcountry", "agemoveus", "ethnicity",
                 paste0("ethnicity_hisp_", c("col", "cub", "dom", "ecu", "mex", "pue", "other", "NoAns", "otherText")),
                 paste0("race_", c("asian", "aiaa", "black", "pacisl", "white", "other", "NA", "otherText")),
                 "sexorient", "sexorient_other", "relstat" ,"relstat_other",
                 paste0("lang_", c("eng", "spa", "chi", "jap", "kor", "other", "NA", "otherText")),
                 paste0("empl_", c("studft", "studpt", "emplft", "emplpt", "other", "NA", "otherText")),
                 "income", "income_other", paste0("rel_", c("ath", "agn", "chr", "jew", "mus", "bud", "hin", "other", "NA", "otherText")),
                 "rel_imp", "politics", paste0("acrs_", 1:24), paste0("sis_", 1:14), paste0("bads_", 1:25), paste0("diri_", 1:26),
                 paste0("iros_", 1:6), "attn_1", paste0("iros_", 7:10), paste0("inq_", 1:15), "attn_2", "isq_1", "isq_1_freq",
                 paste0("isq_1_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "tea", "bos", "other", "NA", "otherText")),
                 "isq_2", "isq_3_freq", paste0("isq_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "tea", "bos", "other", "NA", "otherText")),
                 "isq_3", "isq_3_freq", "isq_3_event", "isq_3_intent", "isq_4", "isq_4_freq",
                 paste0("isq_4_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "tea", "bos", "other", "NA", "otherText")),
                 "isq_5", "isq_5_count", paste0("isq_5_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "other", "NA", "otherText")),
                 "isq_6", "isq_6_count", paste0("isq_6_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "tea", "bos", "other", "NA", "otherText")),
                 "isq_7", "isq_7_count", paste0("isq_7_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "tea", "bos", "other", "NA", "otherText")),
                 "isq_8", "isq_8_count", paste0("isq_8_whom_", c("rom", "fri", "car", "sib", "roo", "cla", "tea", "bos", "other", "NA", "otherText")),
                 "isq_9", "isq_9_count", "isq_9_desc", "isq_10", paste0("ids_", 1:9), "ids_9a", "id_9b", "ids_10", "ids_app", paste0("ids_", 11:12),
                 "ids_weight", paste0("ids_", 13:30), paste0("sbq_", 1:4), paste0("fsq_", 1:12), paste0("uclals_", 1:20),
                 paste0("rio_", 1:6), paste0("phq_", 1:3), "attn_3", paste0("phq_", 4:10), "emhs_1",
                 paste0("emhs_1_type_", c("PSYcbt", "PSYbeh", "PSYcog", "PSYdyn", "PSYidk", "MDboth", "MDtalk", "MDmed", "other", "NA", "otherText")),
                 "emhs_1_dx", "emhs_1_length", "emhs_1_help", "emhs_2",
                 paste0("emhs_2_type_", c("PSYcbt", "PSYbeh", "PSYcog", "PSYdyn", "PSYidk", "MDboth", "MDtalk", "MDmed", "other", "NA", "otherText")),
                 "emhs_2_dx", "emhs_2_length", "emhs_2_help", "emhs_3", "emhs_3_dx", "emhs_3_reason", paste0("scs_", 1:20), paste0("sss_", 1:19),
                 "attn_4", "pid", "mobile")

d$birthcountry[d$birthcountry == 1] <- 190


# TASK 1: Make categorical variables into factors

d$gender <- factor(x = d$gender, levels = attributes(d$gender)$labels, labels = names(attributes(d$gender)$labels))
d$birthcountry <- factor(x = d$birthcountry, levels = attributes(d$birthcountry)$labels, labels = names(attributes(d$birthcountry)$labels))
d$ethnicity <- factor(x = d$ethnicity, levels = attributes(d$ethnicity)$labels, labels = names(attributes(d$ethnicity)$labels))
d$race <- factor(x = d$race, levels = attributes(d$race)$labels, labels = names(attributes(d$race)$labels))
d$language <- factor(x = d$language, levels = attributes(d$language)$labels, labels = names(attributes(d$language)$labels))
d$employment <- factor(x = d$employment, levels = attributes(d$employment)$labels, labels = names(attributes(d$employment)$labels))
d$religion_vars <- factor(x = d$religion_vars, levels = attributes(d$religion_vars)$labels, labels = names(attributes(d$religion_vars)$labels))
d$income <- factor(x = d$income, levels = attributes(d$income)$labels, labels = names(attributes(d$income)$labels))
# ethnicity
# race
# language
# employment
# religion vars
# income



# TASK 2: Find the scoring instructions for each of the measuures and put them in this document
# ACRS
# SIS 
# BADS: 4 subscales- AC, AR, WS, SI reverse-scored items 1,2,6,8-10,13-22,23,25 -> sum the items for BADS score total 
# DIRI: reassurance-seeking subscale has 4 items -> scores are averaged across items 
# IROS: items 2,5,6,7, and 9 are reversed scored -> sum up the 10 items 
# INQ: items 7,8,10,13,14, and 15 are reverse coded -> sum up the 15 items 
# ISQ: 
# IDS:sum up all the scores checked by respondents for a total score (range 0-84)
# SBQ: sum all the scores checked by respondents (range 3-18)
# FSQ: 
# UCLALS: items 1,5,6,9,10,15,16,19, and 20 are reverse scored -> sum all the items (range 20-80)
# RIO: sum up all the scores checked by respondents (range 6-30)
# PHQ: sum all items (0 = not at all, 3 = nearly everyday)
# EMHS
# SCS: items 3,6,7,9,11,13,15,17,18, and 20 are reverse coded -> sum the items (range 6-48)
# SSS: calculate average of the scores for all items