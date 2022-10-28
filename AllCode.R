# packages I used 

library(tidyverse)
library(readxl)
library(ggplot2)
library(simputation)
# reading and filtering original dataset to use for problems 1-3

pitch_data <- read_excel("2023 Analytics Internship Problem Dataset.xlsx")
# remove unknown pitches 

pitch_data <- pitch_data|> filter(PITCH_TYPE_KEY != "UN", PITCH_TYPE_KEY != "NULL" , PITCH_NUMBER != "NULL") 
pitcherAmounts <- pitch_data |> 
  group_by(pitch_data$PITCH_TYPE_KEY) |> 
  summarize(AmountOfPitches = n())

# adding a column for woba and for gravity's approximate effect on pitches

pitch_data <- pitch_data|> 
  mutate(woba_value = case_when(EVENT_RESULT_KEY == "single" ~ .9,
                                EVENT_RESULT_KEY == "double" ~ 1.25,
                                EVENT_RESULT_KEY == "triple" ~ 1.6,
                                EVENT_RESULT_KEY == "home_run" ~ 2,
                                (PITCH_RESULT_KEY == "InPlay" & pitch_data$O > 0) ~ 0)) |> 
  mutate(GRAVITY = 36)

#converting all pitch starting and finishing locations to inches 

pitch_data$RELEASE_HEIGHT <- pitch_data$RELEASE_HEIGHT * 12
pitch_data$PLATE_Z <- pitch_data$PLATE_Z * 12
pitch_data$PLATE_X <- pitch_data$PLATE_X * 12

#creating another dataframe just for pitcher A and B, and removing sinkers due to 
# minimal data

two_Pitchers <- pitch_data|>
  select(PITCHER_KEY,PITCH_TYPE_KEY,PLATE_X,PLATE_Z,PITCH_RESULT_KEY,RELEASE_SPEED,
         HORIZONTAL_BREAK,INDUCED_VERTICAL_BREAK,RELEASE_HEIGHT,woba_value,GRAVITY) |>
  filter(PITCHER_KEY == "A"  | PITCHER_KEY == "B" , PITCH_TYPE_KEY != "SI")

# 1. Imputing the missing values of Induced Vertical Break 

two_Pitchers$INDUCED_VERTICAL_BREAK <- as.numeric(as.character(two_Pitchers$INDUCED_VERTICAL_BREAK))
da1 <- impute_lm(two_Pitchers, INDUCED_VERTICAL_BREAK ~ RELEASE_HEIGHT-GRAVITY-PLATE_Z)
two_Pitchers <- da1




#2a
#pitcher A 
changeups <- pitch_data |> 
  filter(PITCH_TYPE_KEY == "CH", RELEASE_SPEED <= "89", RELEASE_SPEED >= "86", PITCH_RESULT_KEY != "NULL" , PITCH_RESULT_KEY!= "HitByPitch" , PITCH_RESULT_KEY != "BallCalled") 
sum(changeups$PITCH_RESULT_KEY == "StrikeSwinging")


#pitcher B

cutters <- pitch_data |> 
  filter(PITCH_TYPE_KEY == "CF", RELEASE_SPEED <= "88", RELEASE_SPEED >= "83", HORIZONTAL_BREAK >= 3.5, HORIZONTAL_BREAK <= 5.5, PITCH_RESULT_KEY != "NULL" , PITCH_RESULT_KEY!= "HitByPitch" , PITCH_RESULT_KEY != "BallCalled") 
sum(cutters$PITCH_RESULT_KEY == "StrikeSwinging")


#2B. 

two_PitcherAmounts <- two_Pitchers |> 
  group_by(PITCHER_KEY,PITCH_TYPE_KEY) |> 
  summarize(AmountOfPitches = n())

ggplot(two_PitcherAmounts, aes(x = PITCHER_KEY, y = AmountOfPitches, 
                               fill = two_PitcherAmounts$PITCH_TYPE_KEY)) + 
  geom_bar(stat = "identity", position = "dodge")


two_Pitchers  |> 
  ggplot(two_Pitchers,mapping = aes(PITCHER_KEY,RELEASE_SPEED)) + geom_jitter() + 
  facet_wrap(~ two_Pitchers$PITCH_TYPE_KEY)


ggplot(two_Pitchers,aes(PLATE_X, PLATE_Z, col = PITCHER_KEY)) + geom_point()  +
  facet_wrap(~ two_Pitchers$PITCH_TYPE_KEY) + geom_rect(mapping = aes(ymax = 40.68,
                                                                      ymin = 18.6, xmax = -12, xmin = 12), alpha = 0, size = 1.2, colour = "black")

ggplot(two_Pitchers,aes(PLATE_X,PLATE_Z)) + geom_point() + facet_grid(two_Pitchers$PITCH_TYPE_KEY ~ PITCHER_KEY) + 
  geom_rect(mapping = aes(ymax = 40.68,ymin = 18.6, xmax = -12, xmin = 12), alpha = 0, size = 1.2, colour = "red")

ggplot(two_Pitchers,aes(HORIZONTAL_BREAK,INDUCED_VERTICAL_BREAK,col = PITCHER_KEY))+
  geom_point(alpha = 0.7) + facet_wrap(~ two_Pitchers$PITCH_TYPE_KEY) 


two_Pitchers_inPlay <- two_Pitchers
two_Pitchers_inPlay <- two_Pitchers_inPlay |> 
  filter(PITCH_RESULT_KEY == "InPlay" ,PITCH_TYPE_KEY != "FF")

ggplot(two_Pitchers_inPlay, aes(PLATE_X,PLATE_Z)) + geom_point(aes(color = as.character(woba_value))) + 
  facet_grid(two_Pitchers_inPlay$PITCH_TYPE_KEY ~ two_Pitchers_inPlay$PITCHER_KEY) + 
  geom_rect(mapping = aes(ymax = 40.68, ymin = 18.6, xmax = -12, xmin = 12), alpha = 0, size = 1.2, colour = "black")
