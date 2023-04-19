## Hannah Bowley
## 09/19/2022
## map function to calculate scores of all files


# load in libraries -------------------------------------------------------

library(tidyverse)
library(lme4)
library(rstatix)
library(ggpubr)


# load in functions -------------------------------------------------------

source("EBC_functions.R")


# get folder working directory --------------------------------------------

## set working directory to external shared drive with SEA files

setwd("Y:/Project Seahorse/Participant Folders/EBC_Seahorse")


# create file list for csv-----------------------------------------------------
file_list_1 <- list.files(path = ".", "scored).csv")

lst_1 <- vector("list", length(file_list_1))

for (i in 1:length(file_list_1)) {
  lst_1[[i]] <- read.csv(file_list_1[i])
}

## apply function to file list for csv 

EBC_master_file <- lapply(lst_1, EBC_function_v6) %>%
  bind_rows()

# create file list for txt ------------------------------------------------
file_list_2 <- list.files(path = ".", "scored).txt")

lst_2 <- vector("list", length(file_list_2))

for (i in 1:length(file_list_2)) {
  lst_2[[i]] <- read.delim(file_list_2[i])
}

## apply function for file list for txt
EBC_text_file <- lapply(lst_2, EBC_function_v5) %>%
  bind_rows()

# join text and csv -------------------------------------------------------

EBC_master_file <- EBC_master_file %>%
  full_join(EBC_text_file)


# save data frame as csv --------------------------------------------------

# write.csv(EBC_master_file, "C:/Users/hanna/OneDrive - Florida International University/PLSD/seahorse_r/EBC/EBC_master_file.csv")


# subject id --------------------------------------------------------------
EBC_master_file$subjectid <- paste(
  EBC_master_file$ID.,
  EBC_master_file$Subject.,
  EBC_master_file$Group.
)

EBC_master_file <- EBC_master_file[-c(1:3)]

# EBC_master_file <- na.omit(EBC_master_file)

## file with only CR

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
         CR_percentage_11
         )
# EBC_CR <- na.omit(EBC_CR)

# repeated measures ANOVA -------------------------------------------------

## change data to long format 
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
))

## box plot: Conditioned response percentage across blocks

bxp <- ggplot(EBC_CR_long, aes(x = Block, y = CR_AVG))+
  geom_boxplot() + 
  ggtitle("Conditioned Response Accuracy Across Blocks") +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

bxp

## anova: conditioned response across blocks 
model <- aov(
  CR_AVG ~ factor(Block) + Error(factor(subjectid)),
  data = EBC_CR_long
)

summary(model)

# Error: factor(subjectid)
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Block)  2    249   124.4   0.077  0.926
# Residuals     58  94084  1622.1               
# 
# Error: Within
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Block)   9   1267   140.8   0.707  0.703
# Residuals     545 108542   199.2      


## bin the data into groups
EBC_CR_bins <-  EBC_CR %>%
  transmute(
    bin1 = mean(c(CR_percentage_2, CR_percentage_3)),
    bin2 = mean(c(CR_percentage_4, CR_percentage_5)),
    bin3 = mean(c(CR_percentage_6, CR_percentage_7)),
    bin4 = mean(c(CR_percentage_8, CR_percentage_9)),
    bin5 = mean(c(CR_percentage_10, CR_percentage_11))
    )

## change bin data to long format
EBC_CR_bins_long <- EBC_CR_bins %>% 
  pivot_longer(
    bin1:bin5, names_to = "Bin", values_to = "CR_AVG"
  )

## Boxplot: Conditioned Response Accuracy Across Bins
##     suggests no significant changes

bxp <- ggplot(EBC_CR_bins_long, aes(x = Bin, y = CR_AVG))+
  geom_boxplot() + 
  ggtitle("Conditioned Response Accuracy Across Bins") +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
bxp

## Repeated Measures Anova change in Conditioned Response Across Bins
model <- aov(
  CR_AVG ~ factor(Bin) + Error(factor(subjectid)), data = EBC_CR_bins_long
  )

summary(model)

# Error: factor(subjectid)
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Bin)  1     42    41.7    0.05  0.824
# Residuals   56  46972   838.8               
# 
# Error: Within
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Bin)   4    108   27.07   0.247  0.911
# Residuals   244  26694  109.40    


# apply function to only examine tone alone trials ------------------------

EBC_tonealone_file <- lapply(lst_1, EBC_function_v3) %>%
  discard(~ nrow(.x) == 0) %>%
  bind_rows()


EBC_text_file_tonealone <- lapply(lst_2, EBC_function_v4)%>%
  discard(~ nrow(.x) == 0) %>%
  bind_rows()

# join text and csv tonealone-----------------------------------------------

EBC_tonealone_file <- EBC_tonealone_file %>%
  full_join(EBC_text_file_tonealone)


# repeated measures anova TONEALONE ---------------------------------------

## Deal with messy subjectid

EBC_tonealone_file$subjectid <- paste(
  EBC_tonealone_file$ID., EBC_tonealone_file$Subject., EBC_tonealone_file$Group.
  )

EBC_tonealone_file <- EBC_tonealone_file[-c(1:3)]

# EBC_master_file <- na.omit(EBC_master_file)

## Data frame with only CR for tonealone trials 

EBC_CR_tonealone <- EBC_tonealone_file %>%
  group_by(subjectid) %>%
  select(
    CR_percentage_2,
    CR_percentage_3,
    CR_percentage_4,
    CR_percentage_5,
    CR_percentage_6,
    CR_percentage_7,
    CR_percentage_8,
    CR_percentage_9,
    CR_percentage_10,
    CR_percentage_11
  )
# EBC_CR_tonealone <- na.omit(EBC_CR_tonealone)

# repeated measures ANOVA -------------------------------------------------

## change data to long format 
EBC_CR_long_tonealone <- gather(
  EBC_CR_tonealone, Block, CR_AVG, c(
    CR_percentage_2,
    CR_percentage_3,
    CR_percentage_4,
    CR_percentage_5,
    CR_percentage_6,
    CR_percentage_7,
    CR_percentage_8,
    CR_percentage_9,
    CR_percentage_10,
    CR_percentage_11
  )
)

## box plot suggests no differences over time for tonealone trials

bxp <- ggplot(EBC_CR_long_tonealone, aes(x = Block, y = CR_AVG)) +
  geom_boxplot() +
  ggtitle("Conditioned Response Accuracy Across Blocks (TONEALONE TRIALS)") +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

bxp

## anova
model <- aov(
  CR_AVG ~ factor(Block) + Error(factor(subjectid)),
  data = EBC_CR_long_tonealone
)
summary(model)

# Error: factor(subjectid)
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Block)  1     97      97   0.028  0.869
# Residuals     56 195984    3500               
# 
# Error: Within
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Block)   9   5902   655.7   1.052  0.397
# Residuals     544 339098   623.3 


## bin the tonealone data into groups
EBC_CR_bins_tonealone <- EBC_CR_tonealone %>%
  transmute(
    bin1 = mean(c(CR_percentage_2, CR_percentage_3)),
    bin2 = mean(c(CR_percentage_4, CR_percentage_5)),
    bin3 = mean(c(CR_percentage_6, CR_percentage_7)),
    bin4 = mean(c(CR_percentage_8, CR_percentage_9)),
    bin5 = mean(c(CR_percentage_10, CR_percentage_11))
  )

## change bin data to long format
EBC_CR_bins_tonealone_long <- EBC_CR_bins_tonealone %>%
  pivot_longer(bin1:bin5, names_to = "Bin", values_to = "CR_AVG")


## box plots are probably not the best way to visualize tonealone trials
bxp <- ggplot(EBC_CR_bins_tonealone_long, aes(x = Bin, y = CR_AVG))+
  geom_boxplot() + 
  ggtitle("Conditioned Response Accuracy Across Bins (Tone alone)") +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
bxp

model <- aov(
  CR_AVG~factor(Bin)+Error(factor(subjectid)),data=EBC_CR_bins_tonealone_long
  )
summary(model)

# Error: factor(subjectid)
# Df Sum Sq Mean Sq F value Pr(>F)
# Residuals 56  97992    1750               
# 
# Error: Within
# Df Sum Sq Mean Sq F value Pr(>F)
# factor(Bin)   4   1803   450.8   1.613  0.172
# Residuals   244  68197   279.5 


# latency analysis --------------------------------------------------------
# repeated measures ANOVA -------------------------------------------------

## file with only Onset of Conditioned Response

EBC_CR_onset <- EBC_master_file %>%
  group_by(subjectid)%>%
  select(CR_Onest_avg_2,
         CR_Onest_avg_3,
         CR_Onest_avg_4,
         CR_Onest_avg_5,
         CR_Onest_avg_6,
         CR_Onest_avg_7,
         CR_Onest_avg_8,
         CR_Onest_avg_9,
         CR_Onest_avg_10,
         CR_Onest_avg_11
  )
# EBC_CR <- na.omit(EBC_CR)

# repeated measures ANOVA -------------------------------------------------

## change data to long format 
EBC_CR_onset_long <- gather(EBC_CR_onset, Block, CR_Onset_AVG, c(
  CR_Onest_avg_2,
  CR_Onest_avg_3,
  CR_Onest_avg_4,
  CR_Onest_avg_5,
  CR_Onest_avg_6,
  CR_Onest_avg_7,
  CR_Onest_avg_8,
  CR_Onest_avg_9,
  CR_Onest_avg_10,
  CR_Onest_avg_11
))


## box plot suggests no differences over time 

bxp <- ggplot(EBC_CR_onset_long, aes(x = Block, y = CR_Onset_AVG))+
  geom_boxplot() + 
  ggtitle("Conditioned Response Onset Across Blocks") +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
bxp

## anova
model <- aov(
  CR_Onset_AVG ~ factor(Block) + Error(factor(subjectid)),
  data = EBC_CR_onset_long
)

summary(model)


# Error: factor(subjectid)
# Df   Sum Sq Mean Sq F value Pr(>F)  
# factor(Block)  9  7143871  793763   2.501 0.0188 *
#   Residuals     51 16187891  317410                 
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Error: Within
# Df   Sum Sq Mean Sq F value Pr(>F)
# factor(Block)   9   772000   85778   1.599  0.113
# Residuals     486 26071305   53645    


## bin the data

## bin the data into groups
EBC_Onset_bins <- EBC_CR_onset %>%
  transmute(
    bin1 = mean(c(
      CR_Onest_avg_2,
      CR_Onest_avg_3
    )),
    bin2 = mean(c(
      CR_Onest_avg_4,
      CR_Onest_avg_5
    )),
    bin3 = mean(c(
      CR_Onest_avg_6,
      CR_Onest_avg_7
    )),
    bin4 = mean(c(
      CR_Onest_avg_8,
      CR_Onest_avg_9
    )),
    bin5 = mean(c(
      CR_Onest_avg_10,
      CR_Onest_avg_11
    ))
  )

## change bin data to long format
EBC_Onset_bins_long <- pivot_longer(
  EBC_Onset_bins, bin1:bin5, names_to = "Bins", values_to = "CR_Onset_AVG")


bxp <- ggplot(EBC_Onset_bins_long, aes(x = Bins, y = CR_Onset_AVG))+
  geom_boxplot() + 
  ggtitle("Conditioned Response Onset Across Bins") +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
bxp


model <- aov(CR_Onset_AVG ~ factor(Bins) + Error(factor(subjectid)),
  data = EBC_Onset_bins_long
)

summary(model)


# Error: factor(subjectid)
# Df  Sum Sq Mean Sq F value Pr(>F)
# factor(Bins)  4  688759  172190    0.98  0.427
# Residuals    51 8959424  175675               
# 
# Error: Within
# Df  Sum Sq Mean Sq F value Pr(>F)
# factor(Bins)   4   21334    5333   0.191  0.943
# Residuals    188 5261322   27986  


# rename subject id variables ---------------------------------------------

## renamed the variables within csv file because too many names had issues

## Corrected subject ID EBC master file
EBC_master_file$subjectid_corrected <- EBC_master_file$subjectid
EBC_master_file$subjectid_corrected <- c("SEA1024",
                                         "SEA1025",
                                         "SEA1026",
                                         "SEA1027",
                                         "SEA1028",
                                         "SEA1029",
                                         "SEA1030",
                                         "SEA1031",
                                         "SEA1032",
                                         "SEA1034",
                                         "SEA1035",
                                         "SEA1036",
                                         "SEA1037",
                                         "SEA1038",
                                         "SEA1039",
                                         "-",
                                         "SEA1041",
                                         "SEA1042",
                                         "SEA1043",
                                         "101D",
                                         "102D",
                                         "103D",
                                         "104D",
                                         "105D",
                                         "106D",
                                         "107D",
                                         "108D",
                                         "109D",
                                         "110D",
                                         "111D",
                                         "112D",
                                         "114D",
                                         "115D",
                                         "116D",
                                         "117D",
                                         "118D",
                                         "119D",
                                         "120D",
                                         "121D",
                                         "122D",
                                         "123D",
                                         "124D",
                                         "125D",
                                         "126D",
                                         "127D",
                                         "129D",
                                         "130D",
                                         "131D",
                                         "132D",
                                         "133D",
                                         "134D",
                                         "135D",
                                         "136D",
                                         "137D",
                                         "138D",
                                         "139D",
                                         "140D",
                                         "SEA1001",
                                         "SEA1002",
                                         "SEA1003",
                                         "SEA1005",
                                         "SEA1008",
                                         "-",
                                         "SEA1022",
                                         "SEA1023"
                                         )

EBC_master_file$subjectid <- EBC_master_file$subjectid_corrected


SEA_REEB_DEMO <- readRDS("C:/Users/hanna/OneDrive - Florida International University/PLSD/seahorse_r/Seahorse/merged_SEA_REEB_demo.Rds")

SEA_REEB_DEMO$subjectid <- str_trunc(SEA_REEB_DEMO$subjectid, 7, ellipsis = '')


EBC_master_file$subjectid <- str_replace_all(
  EBC_master_file$subjectid, "[^[:alnum:]]", ""
  )

EBC_master_file$subjectid <- str_replace_all(EBC_master_file$subjectid, " ", "")

## remove columns not being used 
SEA_REEB_DEMO <- as.data.frame(
  subset(
    SEA_REEB_DEMO,
    select = -c(age_of_visit, child_primary_language, date_of_study)
  )
)

## remove rows with na
SEA_REEB_DEMO <- na.omit(SEA_REEB_DEMO)

# join demographics to EBC master file ------------------------------------

EBC_master_file <- merge(EBC_master_file, SEA_REEB_DEMO, by="subjectid")



# two way repeated measures anova -----------------------------------------

## file with only latency and child sex

EBC_latency_sex <- EBC_master_file %>%
  group_by(subjectid)%>%
  select(CR_Onest_avg_2,
         CR_Onest_avg_3,
         CR_Onest_avg_4,
         CR_Onest_avg_5,
         CR_Onest_avg_6,
         CR_Onest_avg_7,
         CR_Onest_avg_8,
         CR_Onest_avg_9,
         CR_Onest_avg_10,
         CR_Onest_avg_11,
         child_sex
         
  )
EBC_latency_sex <- na.omit(EBC_latency_sex)

# repeated measures ANOVA -------------------------------------------------

## change data to long format 
EBC_latency_sex_long <- gather(EBC_latency_sex, "Block", "Onset_Latency", c(
  CR_Onest_avg_2,
  CR_Onest_avg_3,
  CR_Onest_avg_4,
  CR_Onest_avg_5,
  CR_Onest_avg_6,
  CR_Onest_avg_7,
  CR_Onest_avg_8,
  CR_Onest_avg_9,
  CR_Onest_avg_10,
  CR_Onest_avg_11
))

# EBC_latency_sex_long <- na.omit(EBC_latency_sex_long)

## histogram of EBC latency long 
hist(EBC_latency_sex_long$Onset_Latency)


## two way repeated measures anova: between subject sex & mean onset latency across blocks

bxp <- ggboxplot(EBC_latency_sex_long, x = "Block", y = "Onset_Latency",
  color = "child_sex", palette = "jco"
)
bxp

model <- aov(Onset_Latency~ factor(Block) * factor(child_sex) +Error(subjectid / (Block * child_sex)),data=EBC_latency_sex_long)
summary(model)

# Error: factor(subjectid)
# Df  Sum Sq Mean Sq F value Pr(>F)
# factor(child_sex)  1    3368    3368   0.013  0.911
# Residuals         30 7937085  264570               
# 
# Error: Within
# Df   Sum Sq Mean Sq F value Pr(>F)
# factor(Block)                     9   356661   39629   0.926  0.503
# factor(Block):factor(child_sex)   9   192997   21444   0.501  0.873
# Residuals                       270 11553345   42790 

library(lmerTest)
model <- lmer(Onset_Latency ~ child_sex * Block + (1|subjectid), data = EBC_latency_sex_long, REML = TRUE)

summary(model)

anova(model)

# ## file with only latency and child age
# 
# EBC_latency_age <- EBC_master_file %>%
#   group_by(subjectid)%>%
#   select(CR_Onest_avg_2,
#          CR_Onest_avg_3,
#          CR_Onest_avg_4,
#          CR_Onest_avg_5,
#          CR_Onest_avg_6,
#          CR_Onest_avg_7,
#          CR_Onest_avg_8,
#          CR_Onest_avg_9,
#          CR_Onest_avg_10,
#          CR_Onest_avg_11,
#          age_at_study
#          
#   )
# EBC_latency_age <- na.omit(EBC_latency_age)
# 
# # repeated measures ANOVA -------------------------------------------------
# 
# ## change data to long format 
# EBC_latency_age_long <- gather(EBC_latency_age, "Block", "Onset_Latency", c(CR_Onest_avg_2,
#                                                                             CR_Onest_avg_3,
#                                                                             CR_Onest_avg_4,
#                                                                             CR_Onest_avg_5,
#                                                                             CR_Onest_avg_6,
#                                                                             CR_Onest_avg_7,
#                                                                             CR_Onest_avg_8,
#                                                                             CR_Onest_avg_9,
#                                                                             CR_Onest_avg_10,
#                                                                             CR_Onest_avg_11
# ))%>%
#   convert_as_factor(subjectid, Block)
# 
# EBC_latency_age_long <- na.omit(EBC_latency_age_long)
# 
# 
# ## two way repeated measures anova: between subject sex & mean onset latency across blocks
# 
# bxp <- ggboxplot(EBC_latency_age_long, x = "Block", y = "Onset_Latency",
#                  color = "age_at_study", palette = "jco"
# )
# bxp
# 
# model <- aov(Onset_Latency~ factor(Block) * age_at_study +Error(subjectid / (Block * age_at_study)),data=EBC_latency_age_long)
# summary(model)
# 


## onset latency standard deviation ----------------------------------------

## file with only latency sd and child age

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
         child_sex

  )
# EBC_latency_sd_age <- na.omit(EBC_latency_sd_age)

# repeated measures ANOVA -------------------------------------------------

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

# EBC_latency_sd_age_long <- na.omit(EBC_latency_sd_age_long)


## two way repeated measures anova: between subject sex & mean onset latency across blocks

dev.off()

plot <- ggplot(
  EBC_latency_sd_age_long,
  aes(x = Block, y = Onset_Latency, fill = factor(child_sex))
) +
  scale_x_discrete(
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  ) +
  labs(
    y = "Onset Latency Std Dev",
    title = "Onset Latency Std Dev across blocks by Sex"
  ) +
  scale_fill_manual(values = c("#ee4276", "#fbaa19"), name = "Child Age") +
  geom_boxplot(alpha = 0.5) +
  theme(
    panel.background = element_rect(
      fill = "transparent",
      colour = "#c6d940",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  )

plot

model <- aov(Onset_Latency~ factor(Block) * factor(child_sex) +Error(subjectid / (Block * child_sex)),data=EBC_latency_sd_age_long)
summary(model)

model <- lmer(Onset_Latency ~ child_sex * Block + (1|subjectid), data = EBC_latency_sd_age_long, REML = TRUE)

summary(model)

anova(model)

# anova(model)
# Analysis of Variance Table
# npar Sum Sq Mean Sq F value
# child_sex          1     50      50  0.0025
# Block              9 320676   35631  1.7755
# child_sex:Block    9 107630   11959  0.5959

# one way anova sd latency ------------------------------------------------

## box plot suggests no differences over time 
# bxp <- ggboxplot(EBC_latency_sd_age_long, x = "Block", y = "Onset_Latency", add= "point")
# bxp
# 
# ## anova
# model <- aov(Onset_Latency~factor(Block)+Error(factor(subjectid)),data=EBC_latency_sd_age_long)
# summary(model)

# ## bin the data
# 
# ## bin the data into groups
# EBC_latency_sd_bin <-  EBC_latency_sd_age %>%
#   transmute(
#     bin1 = mean(c(CR_Onset_sd_2,
#                   CR_Onset_sd_3)),
#     bin2 = mean(c(CR_Onset_sd_4,
#                   CR_Onset_sd_5)),
#     bin3 = mean(c(CR_Onset_sd_6,
#                   CR_Onset_sd_7)),
#     bin4 = mean(c(CR_Onset_sd_8,
#                   CR_Onset_sd_9)),
#     bin5 = mean(c(CR_Onset_sd_10,
#                   CR_Onset_sd_11)))
# 
# ## change bin data to long format
# EBC_latency_sd_bin_long <- pivot_longer(EBC_latency_sd_bin, bin1:bin5)
# 
# bxp <- ggboxplot(EBC_Onset_bins_long, x = "name", y = "value", add= "point")
# bxp
# 
# model <- aov(value~factor(name)+Error(factor(subjectid)),data=EBC_Latency_sd_bin_long)
# summary(model)
# 







