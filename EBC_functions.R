## Author: Hannah Bowley
## Date: 2/13/2023
## Script with EBC Functions



# function for original ebc software output -------------------------------

## optimal for original text files
## to test function use this sample file: 

# scored_file <- read.delim("101-D(scored).txt")

EBC_function_v1 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v1 <- mean(
    rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    3*(sd(rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.), na.rm = TRUE))
  

  ## subset by trial type
  
  ## change trial.name into a factor variable
  
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## filter by tonepuff (paired trials)
  scored_file_tonepuff <- scored_file %>%
    filter(Trial.name == "1250-tonepuff")
  
  ## filter by puff alone
  scored_file_puffalone <- scored_file %>%
    filter(Trial.name == "1250-puffalone")
  

  # score the file ----------------------------------------------------------
  
  ## score the tone puff paired trials
  scored_file_tonepuff$scored <- ifelse(
    scored_file_tonepuff$X..Onset. >= 800 &
      scored_file_tonepuff$X..Onset.<= 1300 &
      scored_file_tonepuff$X..Peak.Amp.>= AMP_sd_v1, 1, 0)
  
  
  
  ## score the puff alone trials
  scored_file_puffalone$scored <- ifelse(
    scored_file_puffalone$X..Onset. >= 800 &
      scored_file_puffalone$X..Onset.<= 1300 &
      scored_file_puffalone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
    
  
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$X..Onset. >= 800 &
      scored_file_tonealone$X..Onset.>= 1600 &
      scored_file_tonealone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
    
  
  
  ## join the files
  
  scored_file<- full_join(scored_file_puffalone, scored_file_tonealone)
  scored_file <- full_join(scored_file, scored_file_tonepuff)
  
  ## change scored NA to 0
  scored_file$scored[is.na(scored_file$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file$CR_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file$UR_Onset_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Onset., NA
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file$UR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak..t.., NA
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file$UR_Peak_Amp_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak.Amp., NA
  )
  
  ## add final CR onset time to entire file
  scored_file$CR_Onset_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$X..Onset., NA
  )
  
  ## add final CR peak time to entire file
  scored_file$CR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$X..Peak..t.., NA
  )
  
  ## add final CR Peak Amp to entire file
  scored_file$CR_Peak_Amp_final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$X..Peak.Amp., NA
  )
  
  ## change added variables to numeric
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  scored_file_block1 <- scored_file %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(CR_percentage_1 = mean(CR_Final)*100,
              UR_onset_avg_1 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_1 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_1 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_1 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_1 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_1 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 2
  
  scored_file_block2 <- scored_file %>% 
    filter(Trial...>=5 & Trial...< 15) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_2 = mean(CR_Final)*100,
              UR_onset_avg_2 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_2 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_2 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_2 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_2 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_2 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ## scored block 3
  scored_file_block3 <- scored_file %>% 
    filter(Trial...>= 15 & Trial... < 25) %>% 
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_3 = mean(CR_Final)*100,
              UR_onset_avg_3 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_3 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_3 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_3 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_3 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_3 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 4
  scored_file_block4 <- scored_file %>%
    filter(Trial... >= 25 & Trial... < 35) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_4 = mean(CR_Final)*100,
              UR_onset_avg_4 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_4 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_4 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_4 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_4 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_4 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 5
  scored_file_block5 <- scored_file %>% 
    filter(Trial... >= 35 & Trial... < 45) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_5 = mean(CR_Final)*100,
              UR_onset_avg_5 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_5 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_5 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_5 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_5 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_5 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 6
  scored_file_block6 <- scored_file %>% 
    filter(Trial... >= 45 & Trial... < 55) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_6 = mean(CR_Final)*100,
              UR_onset_avg_6 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_6 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_6 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_6 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_6 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_6 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 7
  scored_file_block7 <- scored_file %>% 
    filter(Trial... >= 55 & Trial... < 65) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group. , Date.)%>%
    summarise(CR_percentage_7 = mean(CR_Final)*100,
              UR_onset_avg_7 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_7 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_7 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_7 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_7 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_7 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 8
  scored_file_block8 <- scored_file %>%
    filter(Trial... >= 65 & Trial... < 75) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_8 = mean(CR_Final)*100,
              UR_onset_avg_8 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_8 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_8 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_8 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_8 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_8 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 9
  scored_file_block9 <- scored_file %>% 
    filter(Trial... >= 75 & Trial... < 85) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_9 = mean(CR_Final)*100,
              UR_onset_avg_9 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_9 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_9 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_9 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_9 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_9 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 10
  scored_file_block10 <- scored_file %>% 
    filter(Trial... >= 85 & Trial... < 95) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_10 = mean(CR_Final)*100,
              UR_onset_avg_10 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_10 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_10 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_10 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_10 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_10 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 11
  scored_file_block11 <- scored_file %>%
    filter(Trial... >= 95 & Trial... < 105) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_11 = mean(CR_Final)*100,
              UR_onset_avg_11 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_11 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_11 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_11 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_11 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_11 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ## join to master participant file
  ebc_master_file <- scored_file_block2 %>%
    left_join(scored_file_block3) %>%
    left_join(scored_file_block4) %>%
    left_join(scored_file_block5) %>%
    left_join(scored_file_block6) %>%
    left_join(scored_file_block7) %>%
    left_join(scored_file_block8) %>%
    left_join(scored_file_block9) %>%
    left_join(scored_file_block10) %>%
    left_join(scored_file_block11)
  
  
  
  
  
}


# function for newer ebc software output ----------------------------------

## optimal for newer csv files
## to test function use this sample file: 
# scored_file <- read.csv("SEA1024MB(scored).csv")


EBC_function_v2 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v2 <- mean(
    rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    sd(rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.), na.rm = TRUE)
  
  ## subset by trial type
  
  ## change trial.name into a factor variable
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## filter by tonepuff (paired trials)
  scored_file_tonepuff <- scored_file %>%
    filter(Trial.name == "1250-tonepuff")
  
  ## filter by puff alone
  scored_file_puffalone <- scored_file %>%
    filter(Trial.name == "1250-puffalone")
  
  
  # score the file ----------------------------------------------------------
  
  ## score the tone puff paired trials
  scored_file_tonepuff$scored <- ifelse(
    scored_file_tonepuff$CR.Onset. >= 800 &
    scored_file_tonepuff$CR.Onset. <= 1300 &
    scored_file_tonepuff$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
    
  
  
  
  ## score the puff alone trials
  scored_file_puffalone$scored <- ifelse(
  scored_file_puffalone$CR.Onset. >= 800 &
  scored_file_puffalone$CR.Onset. <= 1300 &
  scored_file_puffalone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
    
  
  
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$CR.Onset. >= 800 &
    scored_file_tonealone$CR.Onset. <= 1300 &
    scored_file_tonealone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
    
  
  
  
  ## join the files
  
  scored_file<- full_join(scored_file_puffalone, scored_file_tonealone)
  scored_file <- full_join(scored_file, scored_file_tonepuff)
  
  ## change scored NA to 0
  scored_file$scored[is.na(scored_file$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file$CR_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file$UR_Onset_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Onset., NA
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file$UR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak..t.., NA
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file$UR_Peak_Amp_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak.Amp., NA
  )
  
  ## add final CR onset time to entire file
  scored_file$CR_Onset_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$CR.Onset. , NA
  )
  
  ## add final CR peak time to entire file
  scored_file$CR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$CR.Peak..t.. , NA
  )
  
  ## add final CR Peak Amp to entire file
  scored_file$CR_Peak_Amp_final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$CR.Peak.Amp., NA
  )
  
 
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  scored_file_block1 <- scored_file %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(CR_percentage_1 = mean(CR_Final)*100,
              UR_onset_avg_1 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_1 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_1 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_1 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_1 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_1 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 2
  
  scored_file_block2 <- scored_file %>% 
    filter(Trial...>=5 & Trial...< 15) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_2 = mean(CR_Final)*100,
              UR_onset_avg_2 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_2 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_2 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_2 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_2 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_2 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ## scored block 3
  scored_file_block3 <- scored_file %>% 
    filter(Trial...>= 15 & Trial... < 25) %>% 
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_3 = mean(CR_Final)*100,
              UR_onset_avg_3 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_3 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_3 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_3 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_3 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_3 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 4
  scored_file_block4 <- scored_file %>%
    filter(Trial... >= 25 & Trial... < 35) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_4 = mean(CR_Final)*100,
              UR_onset_avg_4 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_4 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_4 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_4 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_4 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_4 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 5
  scored_file_block5 <- scored_file %>% 
    filter(Trial... >= 35 & Trial... < 45) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_5 = mean(CR_Final)*100,
              UR_onset_avg_5 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_5 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_5 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_5 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_5 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_5 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 6
  scored_file_block6 <- scored_file %>% 
    filter(Trial... >= 45 & Trial... < 55) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_6 = mean(CR_Final)*100,
              UR_onset_avg_6 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_6 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_6 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_6 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_6 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_6 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 7
  scored_file_block7 <- scored_file %>% 
    filter(Trial... >= 55 & Trial... < 65) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group. , Date.)%>%
    summarise(CR_percentage_7 = mean(CR_Final)*100,
              UR_onset_avg_7 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_7 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_7 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_7 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_7 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_7 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 8
  scored_file_block8 <- scored_file %>%
    filter(Trial... >= 65 & Trial... < 75) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_8 = mean(CR_Final)*100,
              UR_onset_avg_8 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_8 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_8 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_8 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_8 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_8 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 9
  scored_file_block9 <- scored_file %>% 
    filter(Trial... >= 75 & Trial... < 85) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_9 = mean(CR_Final)*100,
              UR_onset_avg_9 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_9 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_9 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_9 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_9 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_9 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 10
  scored_file_block10 <- scored_file %>% 
    filter(Trial... >= 85 & Trial... < 95) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_10 = mean(CR_Final)*100,
              UR_onset_avg_10 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_10 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_10 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_10 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_10 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_10 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 11
  scored_file_block11 <- scored_file %>%
    filter(Trial... >= 95 & Trial... < 105) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_11 = mean(CR_Final)*100,
              UR_onset_avg_11 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_11 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_11 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_11 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_11 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_11 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ## join to master participant file
  ebc_master_file <- scored_file_block2 %>%
    left_join(scored_file_block3) %>%
    left_join(scored_file_block4) %>%
    left_join(scored_file_block5) %>%
    left_join(scored_file_block6) %>%
    left_join(scored_file_block7) %>%
    left_join(scored_file_block8) %>%
    left_join(scored_file_block9) %>%
    left_join(scored_file_block10) %>%
    left_join(scored_file_block11)
  
  
  
  
  
}



# function v3 csv-tonealone -----------------------------------------------



EBC_function_v3 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v2 <- mean(
    rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    sd(rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.), na.rm = TRUE)
  
  ## subset by trial type
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$CR.Onset. >= 800 &
      scored_file_tonealone$CR.Onset. <= 1300 &
      scored_file_tonealone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  ## change scored NA to 0
  scored_file_tonealone$scored[is.na(scored_file_tonealone$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file_tonealone$CR_Final <- as.numeric(ifelse(
    scored_file_tonealone$Good.Base.Line.== "TRUE", scored_file_tonealone$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file_tonealone$UR_Onset_Final <- as.numeric(ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$UR.Onset., 0
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file_tonealone$UR_Peak_T_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$UR.Peak..t.., 0
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file_tonealone$UR_Peak_Amp_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$UR.Peak.Amp., 0
  )
  
  ## add final CR onset time to entire file
  scored_file_tonealone$CR_Onset_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$CR.Onset. , 0
  )
  
  ## add final CR peak time to entire file
  scored_file_tonealone$CR_Peak_T_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line.== "TRUE", scored_file_tonealone$CR.Peak..t.. , 0
  )
  
  ## add final CR Peak Amp to entire file
  scored_file_tonealone$CR_Peak_Amp_final <- ifelse(
    scored_file_tonealone$Good.Base.Line.== "TRUE", scored_file_tonealone$CR.Peak.Amp., 0
  )
  
## Subset by block
  
  scored_file_block2 <- scored_file_tonealone %>% 
    filter(Trial...>=5 & Trial...< 15) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_2 = mean(CR_Final)*100,
              UR_onset_avg_2 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_2 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_2 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_2 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_2 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_2 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  scored_file_block3 <- scored_file_tonealone %>% 
    filter(Trial...>= 15 & Trial... < 25) %>% 
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_3 = mean(CR_Final)*100,
              UR_onset_avg_3 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_3 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_3 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_3 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_3 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_3 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 4
  scored_file_block4 <- scored_file_tonealone %>%
    filter(Trial... >= 25 & Trial... < 35) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_4 = mean(CR_Final)*100,
              UR_onset_avg_4 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_4 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_4 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_4 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_4 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_4 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 5
  scored_file_block5 <- scored_file_tonealone %>% 
    filter(Trial... >= 35 & Trial... < 45) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_5 = mean(CR_Final)*100,
              UR_onset_avg_5 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_5 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_5 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_5 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_5 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_5 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 6
  scored_file_block6 <- scored_file_tonealone %>% 
    filter(Trial... >= 45 & Trial... < 55) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_6 = mean(CR_Final)*100,
              UR_onset_avg_6 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_6 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_6 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_6 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_6 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_6 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 7
  scored_file_block7 <- scored_file_tonealone %>% 
    filter(Trial... >= 55 & Trial... < 65) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group. , Date.)%>%
    summarise(CR_percentage_7 = mean(CR_Final)*100,
              UR_onset_avg_7 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_7 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_7 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_7 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_7 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_7 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 8
  scored_file_block8 <- scored_file_tonealone %>%
    filter(Trial... >= 65 & Trial... < 75) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_8 = mean(CR_Final)*100,
              UR_onset_avg_8 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_8 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_8 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_8 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_8 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_8 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 9
  scored_file_block9 <- scored_file_tonealone %>% 
    filter(Trial... >= 75 & Trial... < 85) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_9 = mean(CR_Final)*100,
              UR_onset_avg_9 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_9 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_9 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_9 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_9 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_9 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 10
  scored_file_block10 <- scored_file_tonealone %>% 
    filter(Trial... >= 85 & Trial... < 95) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_10 = mean(CR_Final)*100,
              UR_onset_avg_10 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_10 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_10 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_10 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_10 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_10 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 11
  scored_file_block11 <- scored_file_tonealone %>%
    filter(Trial... >= 95 & Trial... < 105) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_11 = mean(CR_Final)*100,
              UR_onset_avg_11 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_11 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_11 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_11 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_11 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_11 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ## join to master participant file
  ebc_master_file_tonealone <- scored_file_block2 %>%
    left_join(scored_file_block3)%>%
    left_join(scored_file_block4) %>%
    left_join(scored_file_block5) %>%
    left_join(scored_file_block6) %>%
    left_join(scored_file_block7) %>%
    left_join(scored_file_block8) %>%
    left_join(scored_file_block9) %>%
    left_join(scored_file_block10) %>%
    left_join(scored_file_block11)
  
  
  
}


# function v4 text-tonealone ----------------------------------------------


EBC_function_v4 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v1 <- mean(
    rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    sd(rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.), na.rm = TRUE)
  
  
  ## subset by trial type
  
  ## change trial.name into a factor variable
  
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  
  
  # score the file ----------------------------------------------------------

  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$X..Onset. >= 800 &
      scored_file_tonealone$X..Onset.>= 1600 &
      scored_file_tonealone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
  
  
  ## change scored NA to 0
  scored_file_tonealone$scored[is.na(scored_file_tonealone$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file_tonealone$CR_Final <- as.numeric(ifelse(
    scored_file_tonealone$Good.Base.Line.== "TRUE", scored_file_tonealone$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file_tonealone$UR_Onset_Final <- as.numeric(ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$UR.Onset., 0
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file_tonealone$UR_Peak_T_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$UR.Peak..t.., 0
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file_tonealone$UR_Peak_Amp_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$UR.Peak.Amp., 0
  )
  
  ## add final CR onset time to entire file
  scored_file_tonealone$CR_Onset_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line. == "TRUE", scored_file_tonealone$X..Onset., 0
  )
  
  ## add final CR peak time to entire file
  scored_file_tonealone$CR_Peak_T_Final <- ifelse(
    scored_file_tonealone$Good.Base.Line.== "TRUE", scored_file_tonealone$X..Peak..t.., 0
  )
  
  ## add final CR Peak Amp to entire file
  scored_file_tonealone$CR_Peak_Amp_final <- ifelse(
    scored_file_tonealone$Good.Base.Line.== "TRUE", scored_file_tonealone$X..Peak.Amp., 0
  )
  
  ## change added variables to numeric
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  scored_file_block1 <- scored_file_tonealone %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(CR_percentage_1 = mean(CR_Final)*100,
              UR_onset_avg_1 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_1 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_1 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_1 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_1 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_1 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 2
  
  scored_file_block2 <- scored_file_tonealone %>% 
    filter(Trial...>=5 & Trial...< 15) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_2 = mean(CR_Final)*100,
              UR_onset_avg_2 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_2 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_2 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_2 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_2 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_2 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ## scored block 3
  scored_file_block3 <- scored_file_tonealone %>% 
    filter(Trial...>= 15 & Trial... < 25) %>% 
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_3 = mean(CR_Final)*100,
              UR_onset_avg_3 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_3 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_3 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_3 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_3 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_3 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 4
  scored_file_block4 <- scored_file_tonealone %>%
    filter(Trial... >= 25 & Trial... < 35) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_4 = mean(CR_Final)*100,
              UR_onset_avg_4 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_4 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_4 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_4 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_4 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_4 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 5
  scored_file_block5 <- scored_file_tonealone %>% 
    filter(Trial... >= 35 & Trial... < 45) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_5 = mean(CR_Final)*100,
              UR_onset_avg_5 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_5 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_5 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_5 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_5 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_5 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 6
  scored_file_block6 <- scored_file_tonealone %>% 
    filter(Trial... >= 45 & Trial... < 55) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_6 = mean(CR_Final)*100,
              UR_onset_avg_6 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_6 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_6 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_6 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_6 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_6 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 7
  scored_file_block7 <- scored_file_tonealone %>% 
    filter(Trial... >= 55 & Trial... < 65) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group. , Date.)%>%
    summarise(CR_percentage_7 = mean(CR_Final)*100,
              UR_onset_avg_7 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_7 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_7 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_7 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_7 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_7 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 8
  scored_file_block8 <- scored_file_tonealone %>%
    filter(Trial... >= 65 & Trial... < 75) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_8 = mean(CR_Final)*100,
              UR_onset_avg_8 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_8 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_8 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_8 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_8 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_8 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  ##  scored block 9
  scored_file_block9 <- scored_file_tonealone %>% 
    filter(Trial... >= 75 & Trial... < 85) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_9 = mean(CR_Final)*100,
              UR_onset_avg_9 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_9 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_9 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_9 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_9 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_9 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 10
  scored_file_block10 <- scored_file_tonealone %>% 
    filter(Trial... >= 85 & Trial... < 95) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_10 = mean(CR_Final)*100,
              UR_onset_avg_10 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_10 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_10 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_10 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_10 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_10 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ##  scored block 11
  scored_file_block11 <- scored_file_tonealone %>%
    filter(Trial... >= 95 & Trial... < 105) %>%
    filter(Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_11 = mean(CR_Final)*100,
              UR_onset_avg_11 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_11 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_11 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_11 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_11 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_11 = mean(CR_Peak_Amp_final, na.rm = TRUE)
    )
  
  
  ## join to master participant file
  ebc_master_file <- scored_file_block2 %>%
    left_join(scored_file_block3) %>%
    left_join(scored_file_block4) %>%
    left_join(scored_file_block5) %>%
    left_join(scored_file_block6) %>%
    left_join(scored_file_block7) %>%
    left_join(scored_file_block8) %>%
    left_join(scored_file_block9) %>%
    left_join(scored_file_block10) %>%
    left_join(scored_file_block11)
  
  
  
  
  
}


# latency sd analysis for text --------------------------------------------


EBC_function_v5 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v1 <- mean(
    rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    3*(sd(rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.), na.rm = TRUE))
  
  
  ## subset by trial type
  
  ## change trial.name into a factor variable
  
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## filter by tonepuff (paired trials)
  scored_file_tonepuff <- scored_file %>%
    filter(Trial.name == "1250-tonepuff")
  
  ## filter by puff alone
  scored_file_puffalone <- scored_file %>%
    filter(Trial.name == "1250-puffalone")
  
  
  # score the file ----------------------------------------------------------
  
  ## score the tone puff paired trials
  scored_file_tonepuff$scored <- ifelse(
    scored_file_tonepuff$X..Onset. >= 800 &
      scored_file_tonepuff$X..Onset.<= 1300 &
      scored_file_tonepuff$X..Peak.Amp.>= AMP_sd_v1, 1, 0)
  
  
  
  ## score the puff alone trials
  scored_file_puffalone$scored <- ifelse(
    scored_file_puffalone$X..Onset. >= 800 &
      scored_file_puffalone$X..Onset.<= 1300 &
      scored_file_puffalone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
  
  
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$X..Onset. >= 800 &
      scored_file_tonealone$X..Onset.>= 1600 &
      scored_file_tonealone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
  
  
  
  ## join the files
  
  scored_file<- full_join(scored_file_puffalone, scored_file_tonealone)
  scored_file <- full_join(scored_file, scored_file_tonepuff)
  
  ## change scored NA to 0
  scored_file$scored[is.na(scored_file$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file$CR_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file$UR_Onset_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Onset., NA
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file$UR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak..t.., NA
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file$UR_Peak_Amp_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak.Amp., NA
  )
  
  ## add final CR onset time to entire file
  scored_file$CR_Onset_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$X..Onset., NA
  )
  
  ## add final CR peak time to entire file
  scored_file$CR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$X..Peak..t.., NA
  )
  
  ## add final CR Peak Amp to entire file
  scored_file$CR_Peak_Amp_final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$X..Peak.Amp., NA
  )
  
  ## change added variables to numeric
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  scored_file_block1 <- scored_file %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(CR_percentage_1 = mean(CR_Final)*100,
              UR_onset_avg_1 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_1 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_1 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_1 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_1 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_1 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_1 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  ##  scored block 2
  
  scored_file_block2 <- scored_file %>% 
    filter(Trial...>=5 & Trial...< 15) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_2 = mean(CR_Final)*100,
              UR_onset_avg_2 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_2 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_2 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_2 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_2 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_2 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_2 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ## scored block 3
  scored_file_block3 <- scored_file %>% 
    filter(Trial...>= 15 & Trial... < 25) %>% 
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_3 = mean(CR_Final)*100,
              UR_onset_avg_3 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_3 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_3 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_3 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_3 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_3 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_3 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 4
  scored_file_block4 <- scored_file %>%
    filter(Trial... >= 25 & Trial... < 35) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_4 = mean(CR_Final)*100,
              UR_onset_avg_4 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_4 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_4 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_4 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_4 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_4 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_4 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 5
  scored_file_block5 <- scored_file %>% 
    filter(Trial... >= 35 & Trial... < 45) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_5 = mean(CR_Final)*100,
              UR_onset_avg_5 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_5 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_5 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_5 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_5 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_5 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_5 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 6
  scored_file_block6 <- scored_file %>% 
    filter(Trial... >= 45 & Trial... < 55) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_6 = mean(CR_Final)*100,
              UR_onset_avg_6 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_6 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_6 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_6 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_6 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_6 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_6 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 7
  scored_file_block7 <- scored_file %>% 
    filter(Trial... >= 55 & Trial... < 65) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group. , Date.)%>%
    summarise(CR_percentage_7 = mean(CR_Final)*100,
              UR_onset_avg_7 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_7 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_7 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_7 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_7 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_7 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_7 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 8
  scored_file_block8 <- scored_file %>%
    filter(Trial... >= 65 & Trial... < 75) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_8 = mean(CR_Final)*100,
              UR_onset_avg_8 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_8 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_8 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_8 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_8 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_8 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_8 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  ##  scored block 9
  scored_file_block9 <- scored_file %>% 
    filter(Trial... >= 75 & Trial... < 85) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_9 = mean(CR_Final)*100,
              UR_onset_avg_9 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_9 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_9 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_9 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_9 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_9 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_9 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 10
  scored_file_block10 <- scored_file %>% 
    filter(Trial... >= 85 & Trial... < 95) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_10 = mean(CR_Final)*100,
              UR_onset_avg_10 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_10 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_10 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_10 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_10 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_10 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_10 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 11
  scored_file_block11 <- scored_file %>%
    filter(Trial... >= 95 & Trial... < 105) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_11 = mean(CR_Final)*100,
              UR_onset_avg_11 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_11 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_11 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_11 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_11 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_11 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_11 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ## join to master participant file
  ebc_master_file <- scored_file_block2 %>%
    left_join(scored_file_block3) %>%
    left_join(scored_file_block4) %>%
    left_join(scored_file_block5) %>%
    left_join(scored_file_block6) %>%
    left_join(scored_file_block7) %>%
    left_join(scored_file_block8) %>%
    left_join(scored_file_block9) %>%
    left_join(scored_file_block10) %>%
    left_join(scored_file_block11)
  
  
  
  
  
}



# function for newer ebc software output ----------------------------------

## optimal for newer csv files
## to test function use this sample file: 
# scored_file <- read.csv("SEA1024MB(scored).csv")


EBC_function_v6 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v2 <- mean(
    rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    3*(sd(rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.), na.rm = TRUE))
  
  ## subset by trial type
  
  ## change trial.name into a factor variable
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## filter by tonepuff (paired trials)
  scored_file_tonepuff <- scored_file %>%
    filter(Trial.name == "1250-tonepuff")
  
  ## filter by puff alone
  scored_file_puffalone <- scored_file %>%
    filter(Trial.name == "1250-puffalone")
  
  
  # score the file ----------------------------------------------------------
  
  ## score the tone puff paired trials
  scored_file_tonepuff$scored <- ifelse(
    scored_file_tonepuff$CR.Onset. >= 800 &
      scored_file_tonepuff$CR.Onset. <= 1300 &
      scored_file_tonepuff$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  
  
  
  ## score the puff alone trials
  scored_file_puffalone$scored <- ifelse(
    scored_file_puffalone$CR.Onset. >= 800 &
      scored_file_puffalone$CR.Onset. <= 1300 &
      scored_file_puffalone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  
  
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$CR.Onset. >= 800 &
      scored_file_tonealone$CR.Onset. <= 1600 &
      scored_file_tonealone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  
  
  
  ## join the files
  
  scored_file<- full_join(scored_file_puffalone, scored_file_tonealone)
  scored_file <- full_join(scored_file, scored_file_tonepuff)
  
  ## change scored NA to 0
  scored_file$scored[is.na(scored_file$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file$CR_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file$UR_Onset_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Onset., NA
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file$UR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak..t.., NA
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file$UR_Peak_Amp_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak.Amp., NA
  )
  
  ## add final CR onset time to entire file
  scored_file$CR_Onset_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$CR.Onset. , NA
  )
  
  ## add final CR peak time to entire file
  scored_file$CR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$CR.Peak..t.. , NA
  )
  
  ## add final CR Peak Amp to entire file
  scored_file$CR_Peak_Amp_final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$CR.Peak.Amp., NA
  )
  
  
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  scored_file_block1 <- scored_file %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(CR_percentage_1 = mean(CR_Final)*100,
              UR_onset_avg_1 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_1 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_1 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_1 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_1 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_1 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_1 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  ##  scored block 2
  
  scored_file_block2 <- scored_file %>% 
    filter(Trial...>=5 & Trial...< 15) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_2 = mean(CR_Final)*100,
              UR_onset_avg_2 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_2 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_2 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_2 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_2 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_2 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_2 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ## scored block 3
  scored_file_block3 <- scored_file %>% 
    filter(Trial...>= 15 & Trial... < 25) %>% 
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_3 = mean(CR_Final)*100,
              UR_onset_avg_3 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_3 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_3 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_3 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_3 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_3 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_3 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 4
  scored_file_block4 <- scored_file %>%
    filter(Trial... >= 25 & Trial... < 35) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_4 = mean(CR_Final)*100,
              UR_onset_avg_4 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_4 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_4 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_4 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_4 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_4 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_4 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 5
  scored_file_block5 <- scored_file %>% 
    filter(Trial... >= 35 & Trial... < 45) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_5 = mean(CR_Final)*100,
              UR_onset_avg_5 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_5 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_5 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_5 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_5 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_5 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_5 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  ##  scored block 6
  scored_file_block6 <- scored_file %>% 
    filter(Trial... >= 45 & Trial... < 55) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_6 = mean(CR_Final)*100,
              UR_onset_avg_6 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_6 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_6 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_6 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_6 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_6 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_6 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 7
  scored_file_block7 <- scored_file %>% 
    filter(Trial... >= 55 & Trial... < 65) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group. , Date.)%>%
    summarise(CR_percentage_7 = mean(CR_Final)*100,
              UR_onset_avg_7 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_7 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_7 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_7 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_7 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_7 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_7 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 8
  scored_file_block8 <- scored_file %>%
    filter(Trial... >= 65 & Trial... < 75) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_8 = mean(CR_Final)*100,
              UR_onset_avg_8 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_8 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_8 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_8 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_8 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_8 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_8 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  ##  scored block 9
  scored_file_block9 <- scored_file %>% 
    filter(Trial... >= 75 & Trial... < 85) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_9 = mean(CR_Final)*100,
              UR_onset_avg_9 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_9 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_9 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_9 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_9 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_9 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_9 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 10
  scored_file_block10 <- scored_file %>% 
    filter(Trial... >= 85 & Trial... < 95) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_10 = mean(CR_Final)*100,
              UR_onset_avg_10 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_10 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_10 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_10 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_10 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_10 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_10 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ##  scored block 11
  scored_file_block11 <- scored_file %>%
    filter(Trial... >= 95 & Trial... < 105) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group., Date.)%>%
    summarise(CR_percentage_11 = mean(CR_Final)*100,
              UR_onset_avg_11 = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_11 = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_11 = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_11 = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_11 = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_11 = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_11 = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  ## join to master participant file
  ebc_master_file <- scored_file_block2 %>%
    left_join(scored_file_block3) %>%
    left_join(scored_file_block4) %>%
    left_join(scored_file_block5) %>%
    left_join(scored_file_block6) %>%
    left_join(scored_file_block7) %>%
    left_join(scored_file_block8) %>%
    left_join(scored_file_block9) %>%
    left_join(scored_file_block10) %>%
    left_join(scored_file_block11)
  
  
  
  
  
}



## optimal for newer csv files
## to test function use this sample file: 
# scored_file <- read.csv("SEA1024MB(scored).csv")
EBC_function_v7 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v2 <- mean(
    rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    3*(sd(rbind(scored_file$CR.Peak.Amp,scored_file$UR.Peak.Amp.), na.rm = TRUE))
  
  ## subset by trial type
  
  ## change trial.name into a factor variable
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## filter by tonepuff (paired trials)
  scored_file_tonepuff <- scored_file %>%
    filter(Trial.name == "1250-tonepuff")
  
  ## filter by puff alone
  scored_file_puffalone <- scored_file %>%
    filter(Trial.name == "1250-puffalone")
  
  
  # score the file ----------------------------------------------------------
  
  ## score the tone puff paired trials
  scored_file_tonepuff$scored <- ifelse(
    scored_file_tonepuff$CR.Onset. >= 800 &
      scored_file_tonepuff$CR.Onset. <= 1300 &
      scored_file_tonepuff$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  
  
  
  ## score the puff alone trials
  scored_file_puffalone$scored <- ifelse(
    scored_file_puffalone$CR.Onset. >= 800 &
      scored_file_puffalone$CR.Onset. <= 1300 &
      scored_file_puffalone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  
  
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$CR.Onset. >= 800 &
      scored_file_tonealone$CR.Onset. <= 1600 &
      scored_file_tonealone$CR.Peak.Amp >= AMP_sd_v2, 1, 0)
  
  
  
  
  ## join the files
  
  scored_file<- full_join(scored_file_puffalone, scored_file_tonealone)
  scored_file <- full_join(scored_file, scored_file_tonepuff)
  
  ## change scored NA to 0
  scored_file$scored[is.na(scored_file$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file$CR_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file$UR_Onset_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Onset., NA
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file$UR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak..t.., NA
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file$UR_Peak_Amp_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak.Amp., NA
  )
  
  ## add final CR onset time to entire file
  scored_file$CR_Onset_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$CR.Onset. , NA
  )
  
  ## add final CR peak time to entire file
  scored_file$CR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$CR.Peak..t.. , NA
  )
  
  ## add final CR Peak Amp to entire file
  scored_file$CR_Peak_Amp_final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$CR.Peak.Amp., NA
  )
  
  
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  Final_scores <- scored_file %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(CR_percentage_Final = mean(CR_Final, na.rm=TRUE),
              UR_onset_avg_Final = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_Final = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_Final= mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_Final = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_Final= mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_Final= mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_Final = sd(CR_Onset_Final, na.rm = TRUE)
    )
  
  
  
  
  
  
  
}


EBC_function_v8 <- function(
    scored_file
)
{
  
  # subset file -------------------------------------------------------------
  
  ## filter by channel
  scored_file <- scored_file %>% filter(Chan.== 1)
  
  ## calculate amp mean-1sd for participant 
  AMP_sd_v1 <- mean(
    rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.),na.rm = TRUE)-
    3*(sd(rbind(scored_file$X..Peak.Amp.,scored_file$UR.Peak.Amp.), na.rm = TRUE))
  
  
  ## subset by trial type
  
  ## change trial.name into a factor variable
  
  scored_file$Trial.name <- as.factor(scored_file$Trial.name)
  
  ## filter by tonealone trial
  scored_file_tonealone <- scored_file %>% 
    filter(Trial.name == "1250-tonealone")
  
  ## filter by tonepuff (paired trials)
  scored_file_tonepuff <- scored_file %>%
    filter(Trial.name == "1250-tonepuff")
  
  ## filter by puff alone
  scored_file_puffalone <- scored_file %>%
    filter(Trial.name == "1250-puffalone")
  
  
  # score the file ----------------------------------------------------------
  
  ## score the tone puff paired trials
  scored_file_tonepuff$scored <- ifelse(
    scored_file_tonepuff$X..Onset. >= 800 &
      scored_file_tonepuff$X..Onset.<= 1300 &
      scored_file_tonepuff$X..Peak.Amp.>= AMP_sd_v1, 1, 0)
  
  
  
  ## score the puff alone trials
  scored_file_puffalone$scored <- ifelse(
    scored_file_puffalone$X..Onset. >= 800 &
      scored_file_puffalone$X..Onset.<= 1300 &
      scored_file_puffalone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
  
  
  
  ## score the tone alone trials
  scored_file_tonealone$scored <- ifelse(
    scored_file_tonealone$X..Onset. >= 800 &
      scored_file_tonealone$X..Onset.>= 1600 &
      scored_file_tonealone$X..Peak.Amp.>= AMP_sd_v1, 1,0)
  
  
  
  ## join the files
  
  scored_file<- full_join(scored_file_puffalone, scored_file_tonealone)
  scored_file <- full_join(scored_file, scored_file_tonepuff)
  
  ## change scored NA to 0
  scored_file$scored[is.na(scored_file$scored)] <- 0
  
  ## add final CR score to entire file
  
  scored_file$CR_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$scored, 0
  ))
  
  ## add final UR onset Time score to entire file
  scored_file$UR_Onset_Final <- as.numeric(ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Onset., NA
  ))
  
  ## add final UR Peak Time score to entire file
  scored_file$UR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak..t.., NA
  )
  
  ## add final UR Peak Amp score to entire file
  scored_file$UR_Peak_Amp_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$UR.Peak.Amp., NA
  )
  
  ## add final CR onset time to entire file
  scored_file$CR_Onset_Final <- ifelse(
    scored_file$Good.Base.Line. == "TRUE", scored_file$X..Onset., NA
  )
  
  ## add final CR peak time to entire file
  scored_file$CR_Peak_T_Final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$X..Peak..t.., NA
  )
  
  ## add final CR Peak Amp to entire file
  scored_file$CR_Peak_Amp_final <- ifelse(
    scored_file$Good.Base.Line.== "TRUE", scored_file$X..Peak.Amp., NA
  )
  
  ## change added variables to numeric
  
  
  # subset by block ---------------------------------------------------------
  
  ##  scored block 1
  Final_scores <- scored_file %>% 
    filter(Trial...< 5) %>%
    filter(Trial.name == "1250-tonepuff" | Trial.name == "1250-tonealone") %>%
    group_by(ID., Subject., Group.)%>%
    summarise(
              UR_onset_avg_Final = mean(UR_Onset_Final, na.rm = TRUE),
              UR_peak_avg_Final = mean(UR_Peak_T_Final, na.rm = TRUE),
              UR_amp_avg_Final = mean(UR_Peak_Amp_Final, na.rm = TRUE),
              CR_Onest_avg_Final = mean(CR_Onset_Final, na.rm = TRUE),
              CR_Peak_avg_Final = mean(CR_Peak_T_Final, na.rm = TRUE),
              CR_Amp_avg_Final = mean(CR_Peak_Amp_final, na.rm = TRUE),
              CR_Onset_sd_Final = sd(CR_Onset_Final, na.rm = TRUE)
    )

  
  
  
  
}

