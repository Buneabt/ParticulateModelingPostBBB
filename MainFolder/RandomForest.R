# --- Step 1: Set up your R environment and load the data ---
set.seed(100)

library(randomForest)
library(janitor)
library(here)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Assuming your data is already in a data frame called 'df'
# (The previous steps for loading the data and converting columns to numeric apply)

df <- read.csv("data/df.csv")


df$Carbon.dioxide <- as.numeric(gsub(",", "", df$Carbon.dioxide))
df$RenPer <- as.numeric(gsub("%", "", df$RenPer))



# --- Step 2: Handle autocorrelation with a lagged feature for the new response variable ---
#df$Particulates_lag1 <- c(NA, df$Pm2.5[-nrow(df)])

#df <- df[-1,]



# --- Step 3: Split the data chronologically ---
# Assuming you want to train on data up to 2018 and test on 2019-2022


train_data <- subset(df, Year <= 2015)
test_data <- subset(df, Year >= 2016) 
test_data <- subset(test_data, Year <= 2019)










# Assuming 'train_data' is ready from your previous steps
# train_data is a subset of your full dataset (e.g., all years up to 2018)

# --- Step 2: Define the range of hyperparameters to test ---
# We'll test a range of tree counts and mtry values.
# You can adjust these ranges based on your needs.
ntree_values <- c(100, 250, 500)  # Example: Test 100, 250, and 500 trees
mtry_values <- 1:4                # Test mtry values from 1 to 4 (your number of predictors)

# --- Step 3: Create a data frame to store the results ---
# We'll store the R-squared for each model we build.
results_df <- data.frame(
    ntree = integer(),
    mtry = integer(),
    oob_r_squared = numeric()
)

# --- Step 4: Perform the grid search with a nested loop ---
# The outer loop iterates through the number of trees (ntree)
for (n in ntree_values) {
    # The inner loop iterates through the number of variables to sample (mtry)
    for (m in mtry_values) {
        
        cat("Training model with ntree =", n, "and mtry =", m, "\n")
        
        # Build the random forest model using the current ntree and mtry values
        # We are predicting Particulates from all other variables
        rf_model <- randomForest(
            Pm2.5 ~ Carbon.dioxide + Nitrous.oxide +  RenPer + Methane + Others,
            data = train_data,
            ntree = n,
            mtry = m
        )
        
        
        # Get the OOB R-squared from the model summary
        oob_r_squared <- rf_model$rsq
        
        # Store the results in the data frame
        results_df <- rbind(results_df, data.frame(ntree = n, mtry = m, oob_r_squared = oob_r_squared))
    }
}

# --- Step 5: Analyze the results ---
# Print the data frame to see the performance of each model
print(results_df)

# Find the row with the highest OOB R-squared
best_model_params <- results_df[which.max(results_df$oob_r_squared), ]

# FINAL MODEL 


# --- Step 1: Train the final model with the optimal hyperparameters ---

final_rf_model <- randomForest(
    Pm2.5 ~ Carbon.dioxide + Nitrous.oxide +  RenPer + Methane + Others,
    data = train_data,
    ntree = best_model_params$ntree,
    mtry = best_model_params$mtry
)

# --- Step 2: Make predictions on the test data ---
# This is the data from 2019-2022 that the model has never seen
predictions <- predict(final_rf_model, newdata = test_data)

# --- Step 3: Evaluate the final model using R-squared and RMSE ---
# Calculate R-squared (This line is correct)
final_r_squared <- 1 - (sum((predictions - test_data$Pm2.5)^2) / sum((test_data$Pm2.5 - mean(test_data$Pm2.5))^2))

# Calculate RMSE (Corrected line)
final_rmse <- sqrt(mean((predictions - test_data$Pm2.5)^2))

# --- Step 4: Visual Diagnostics (Plotting Actual vs. Predicted) ---
plot(test_data$Pm2.5, predictions, 
     main = "Actual vs. Predicted Particulates (Test Data)",
     xlab = "Actual Particulates (PM 2.5)", 
     ylab = "Predicted Particulates (PM 2.5)")
abline(a = 0, b = 1, col = "red", lty = 2) # Add a diagonal line for reference

#Final Output for Easy

print(results_df)
print(best_model_params)
cat("Final R-squared on Test Data:", final_r_squared, "\n")
cat("Final RMSE on Test Data:", final_rmse, "\n")



