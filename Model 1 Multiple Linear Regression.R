#Load libraries
library(dplyr)
library(leaps)
library(gains)
library(forecast)

#Read and store CSV
st_math <- read.csv("student-mat.csv", sep = ";")

#Check variable types
sapply(st_math, class)

#Change various variable types to factors
st_math <- st_math %>%
  mutate(across(c("school", "sex","Medu", "Fedu", "traveltime", "studytime","famrel","freetime", 
                  "goout","Dalc", "Walc", "health", "address", "famsize", "Pstatus", "Mjob", 
                  "Fjob", "reason", "guardian", "schoolsup", "famsup", "paid", "activities", 
                  "nursery", "higher", "internet", "romantic"), as.factor))

#Check variable classes
sapply(st_math, class)

#Build starting model
st_model <- lm(G3 ~ + studytime + absences + failures + G1 + G2 + schoolsup + famsup, data = st_math)
summary(st_model)

#Split training and test data
set.seed(1)
nrow(st_math)
train.index <- sample(1:395, size = .6 * 395)
train.df <- st_math[train.index, ]
valid.df <- st_math[-train.index, ]

#Create fitted model and use backwards regression
reg2 <- lm(G3 ~ ., data = train.df)
step.backward <- step(reg2, direction="backward")
summary(step.backward)

#Conduct predictions and calculate accuracy of the predicted models to G3
model_pred <- predict(step.backward, valid.df)
accuracy(model_pred, valid.df$G3)

#Build final model
final_model <- lm(G3 ~ romantic + absences + G1 + G2, data = train.df)
summary(final_model)

#Conduct predictions and calculate accuracy of the final model
model_pred <- predict(final_model, valid.df)
accuracy(model_pred, valid.df$G3)