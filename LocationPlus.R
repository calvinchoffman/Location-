#Load Libraries
library(tidyverse)
library(randomForest)
library(glm2)

#Read the data 
df <- read.csv("College_Updated_5_24TH.csv")

#Select Columns
dft <- df[c('Pitcher', 'PitcherTeam', 'TaggedPitchType', 'PitchCall', 'PlayResult', 'RelHeight', 'RelSide', 'Extension', 'PlateLocHeight', 'PlateLocSide', 'VertApprAngle','HorzApprAngle')]

# Filter and transform the data
Swings <- dft %>% filter(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))
Whiff <- Swings %>% mutate(whiff=ifelse(PitchCall=="StrikeSwinging", 1, 0))
HAA <- Whiff %>% mutate(HorzApprAngle = abs(HorzApprAngle))
RS <- HB %>% mutate(RelSide = abs(RelSide))
FBDiffBreak <- HB[DiffBreak$TaggedPitchType %in% c('Fastball', 'Sinker', 'TwoSeamFastBall', 'FourSeamFastBall', 'OneSeamFastBall'),] %>% na.omit()

# Prepare predictor variables and response variable
X_fb <- FBDiffBreak[,c('PlateLocHeight','PlateLocSide',  'RelHeight', 'RelSide', 'Extension','HorzApprAngle','VertApprAngle')]
Y_fb <- FBDiffBreak$whiff  # Note: No need to convert this to factor now

# Bind the predictor variables and response variable in a data frame
data_fb <- cbind(X_fb, Y_fb)

# Split the data
set.seed(12345678)
Split= 0.7
N <- nrow(data_fb)
TrainingSize <- round(N*Split)
TrainingCases <- sample(N, TrainingSize)
Training = data_fb[TrainingCases, ]
Test= data_fb[-TrainingCases, ]

# Fit the logistic regression model
logistic_model <- glm(Y_fb ~ ., data = Training, family = binomial())

# Predict probabilities
FBDiffBreak$predicted_whiff <- predict(logistic_model, newdata = X_fb, type = "response")

#find mean of predicted whiff
mean(FBDiffBreak$predicted_whiff, na.rm = TRUE)

#create a new column called location+ which is (predicted_whiff / mean of predicted_whiff * 100)
library(dplyr)
mean_predicted_whiff <- mean(FBDiffBreak$predicted_whiff, na.rm = TRUE)

#Calculate Location+ 
FBDiffBreak <- FBDiffBreak %>%
  mutate(`location+` = (predicted_whiff / mean_predicted_whiff) * 100)

#groupby Pitcher
FBDiffBreak_summary <- FBDiffBreak %>%
  group_by(Pitcher) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), count = n())

#Picther Values 
Avitia_Daniel_FBData <- FBDiffBreak_summary %>%
  filter(Pitcher == "Avitia, Daniel")