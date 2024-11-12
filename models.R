# Load necessary libraries
library(mgcv)
library(randomForest)
library(caret)  # For splitting the data into training/testing sets

library(duckdb)
library(tidyverse)

# Connect to db
dpv = duckdb("project.db")
con = dbConnect(dpv)

# Set tables
rosterfact = dbGetQuery(con, "SELECT * FROM rosterfact") %>% 
  mutate(SeasonDisplay = as.numeric(substr(Season, 1, 4))) %>% 
  rename(PctRetention = PctTurnover)

dimteam = dbGetQuery(con, "SELECT * FROM dimteam")

colors = read_csv("color_teams.csv")
dimteam$HexColor = colors$Hex_Code

tenures = dbGetQuery(con, "SELECT * FROM playertenures")

# Close db
dbDisconnect(con)

data = rosterfact %>% filter(!(Season %in% c("2014-15", "2024-25")))
# Step 2: Split Data into Training and Test Sets (70% train, 30% test)
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(data$WinPct, p = 0.9, list = FALSE)  # 70% training
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Step 3: Linear Regression Model (First model)
linear_model <- lm(WinPct ~ PtsReturning + MinReturning + Experience, data = train_data)
summary(linear_model)

# Predict using linear model
linear_predictions <- predict(linear_model, newdata = test_data)

linear_predictions

summary(linear_model)

