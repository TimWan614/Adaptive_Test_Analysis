# Load necessary libraries
library(ltm)
library(dplyr)
library(tidyr)

# Define response thresholds (n >= 10, n >= 30, n >= 50)
thresholds <- c(15, 30, 50)

# Step 1: Load and Prepare the Dataset
PreTask <- read.csv("data/Simulated_Adaptive_Test_Data.csv")



# Remove parent items (since they have no scores)
PreTaskNP <- PreTask %>% filter(item_type != "parent")

# Create item-specific column names (e.g., 733S for standalone, 733C for child)
PreTaskNP <- PreTaskNP %>%
  mutate(item_col = paste0(item_id, ifelse(item_type == "standalone", "S", "C")))

# Convert to wide format
PreTaskWide <- PreTaskNP %>%
  select(user_id, item_col, score) %>%
  pivot_wider(names_from = item_col, values_from = score)

# Compute Response Counts
item_response_counts <- colSums(!is.na(PreTaskWide[-1]))  # Count non-missing responses

# Initialize results table (AIC & BIC only)
model_comparison <- data.frame(
  Response_Threshold = thresholds,
  AIC_1PL = NA, AIC_2PL = NA,
  BIC_1PL = NA, BIC_2PL = NA
)

# Function to filter dataset based on response threshold
filter_items_by_response <- function(response_threshold) {
  # Select items with more than the specified number of responses
  filtered_items <- names(item_response_counts[item_response_counts >= response_threshold])
  
  # Subset dataset
  PreTaskFiltered <- PreTaskWide %>% select(user_id, all_of(filtered_items))
  
  # Convert to matrix format for IRT modeling
  IRT_Data_Filtered <- as.matrix(PreTaskFiltered[-1])  # Remove user_id column
  
  # Handle Missing Data: Replace NA with 0 (assuming missing responses = incorrect)
  IRT_Data_Filtered[is.na(IRT_Data_Filtered)] <- 0
  
  return(IRT_Data_Filtered)
}

# Step 2: Loop through each threshold and compute AIC & BIC
for (i in 1:length(thresholds)) {
  # Filter dataset based on threshold
  IRT_Data_Filtered <- filter_items_by_response(thresholds[i])
  
  # Fit 1PL (Rasch) and 2PL Models
  irt_1pl <- rasch(IRT_Data_Filtered)
  
  # Try fitting 2PL model, handle errors
  irt_2pl <- tryCatch({
    ltm(IRT_Data_Filtered ~ z1)
  }, error = function(e) {
    return(NULL)  # If 2PL fails, return NULL
  })
  
  # Compute AIC and BIC for 1PL
  model_comparison$AIC_1PL[i] <- AIC(irt_1pl)
  model_comparison$BIC_1PL[i] <- BIC(irt_1pl)
  
  # Compute AIC and BIC for 2PL only if it was successfully fitted
  if (!is.null(irt_2pl)) {
    model_comparison$AIC_2PL[i] <- AIC(irt_2pl)
    model_comparison$BIC_2PL[i] <- BIC(irt_2pl)
  } else {
    model_comparison$AIC_2PL[i] <- NA
    model_comparison$BIC_2PL[i] <- NA
  }
}

# Print the final results table
print(model_comparison)
