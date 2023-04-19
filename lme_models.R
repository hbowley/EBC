library(tidyverse)
library(lme4)
library(rstatix)
library(ggpubr)

EBC_master_file <- read.csv("C:/Users/hanna/OneDrive - Florida International University/PLSD/seahorse_r/EBC/EBC_master_file.csv")

EBC_master_file$AGE_int <- as.integer(EBC_master_file$age_at_study)
EBC_master_file <- subset(EBC_master_file, EBC_master_file$AGE_int >= 4)

EBC_CR <- EBC_master_file %>%
  group_by(subjectid)%>%
  select(CR_percentage_2,
         CR_percentage_3,
         CR_percentage_4,
         CR_percentage_5,
         CR_percentage_6,
         CR_percentage_7,
         CR_percentage_8,
         CR_percentage_9,
         CR_percentage_10,
         CR_percentage_11,
         age_at_study,
         child_sex,
         AGE_int
         
  )




EBC_CR_long <- gather(EBC_CR, Block, CR_AVG, c(CR_percentage_2,
                                               CR_percentage_3,
                                               CR_percentage_4,
                                               CR_percentage_5,
                                               CR_percentage_6,
                                               CR_percentage_7,
                                               CR_percentage_8,
                                               CR_percentage_9,
                                               CR_percentage_10,
                                               CR_percentage_11
))%>%




# latency -----------------------------------------------------------------

## calculate overall mean of latency in the master file
EBC_master_file$Onset_Latency_Avg <- rowMeans(EBC_master_file[,c("CR_Onest_avg_2",
                                                                 "CR_Onest_avg_3",
                                                                 "CR_Onest_avg_4",
                                                                 "CR_Onest_avg_5",
                                                                 "CR_Onest_avg_6",
                                                                 "CR_Onest_avg_7",
                                                                 "CR_Onest_avg_8",
                                                                 "CR_Onest_avg_9",
                                                                 "CR_Onest_avg_10",
                                                                 "CR_Onest_avg_11")], na.rm = TRUE)


EBC_master_file$Latency_SD_AVG <- rowMeans(EBC_master_file[,c("CR_Onset_sd_2",
                                                              "CR_Onset_sd_3",
                                                              "CR_Onset_sd_4",
                                                              "CR_Onset_sd_5",
                                                              "CR_Onset_sd_6",
                                                              "CR_Onset_sd_7",
                                                              "CR_Onset_sd_8",
                                                              "CR_Onset_sd_9",
                                                              "CR_Onset_sd_10",
                                                              "CR_Onset_sd_11")], na.rm = TRUE)


EBC_latency_sd_age <- EBC_master_file %>%
  group_by(subjectid)%>%
  select(CR_Onset_sd_2,
         CR_Onset_sd_3,
         CR_Onset_sd_4,
         CR_Onset_sd_5,
         CR_Onset_sd_6,
         CR_Onset_sd_7,
         CR_Onset_sd_8,
         CR_Onset_sd_9,
         CR_Onset_sd_10,
         CR_Onset_sd_11,
         age_at_study,
         child_sex,
         AGE_int,
         
  )
# EBC_latency_sd_age <- na.omit(EBC_latency_sd_age)


## change data to long format
EBC_latency_sd_age_long <- gather(EBC_latency_sd_age, "Block", "Onset_Latency", c(CR_Onset_sd_2,
                                                                                  CR_Onset_sd_3,
                                                                                  CR_Onset_sd_4,
                                                                                  CR_Onset_sd_5,
                                                                                  CR_Onset_sd_6,
                                                                                  CR_Onset_sd_7,
                                                                                  CR_Onset_sd_8,
                                                                                  CR_Onset_sd_9,
                                                                                  CR_Onset_sd_10,
                                                                                  CR_Onset_sd_11
))





library(lme4)
model <- lmer(Onset_Latency ~ Block * child_sex + (1|subjectid), data = EBC_latency_sd_age_long)
anova(model)

# Type III Analysis of Variance Table with Satterthwaite's method
#                 Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
# Block           358550   39839     9 369.37  1.9750 0.04112 *
# child_sex          146     146     1  52.24  0.0072 0.93259  
# Block:child_sex 107262   11918     9 369.37  0.5908 0.80470  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model <- lmer(Onset_Latency ~ Block * age_at_study + (1|subjectid), data = EBC_latency_sd_age_long)
anova(model)

# Type III Analysis of Variance Table with Satterthwaite's method
#                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
# Block              315785   35087     9 370.51  1.7857 0.06943 .
# age_at_study         2663    2663     1  52.90  0.1355 0.71425  
# Block:age_at_study 298441   33160     9 370.39  1.6876 0.09025 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model <- lmer(CR_AVG ~ Block * age_at_study + (1|subjectid), data = EBC_CR_long)
anova(model)
# Type III Analysis of Variance Table with Satterthwaite's method
#                    Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
# Block              2932.7  325.85     9 500.52  1.6538 0.09747 .
# age_at_study         66.5   66.48     1  59.95  0.3374 0.56350  
# Block:age_at_study 3279.8  364.42     9 500.36  1.8495 0.05735 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model <- lmer(CR_AVG ~ Block * child_sex + (1|subjectid), data = EBC_CR_long)
anova(model)

# Type III Analysis of Variance Table with Satterthwaite's method
#                  Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
# Block           1354.60 150.511     9 499.04  0.7455 0.6671
# child_sex         12.23  12.235     1  58.24  0.0606 0.8064
# Block:child_sex  872.46  96.940     9 499.04  0.4802 0.8882