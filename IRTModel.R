# Load necessary libraries
library(ltm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

response_threshold <- 30

# Load the dataset
PreTask <- read.csv("data/Simulated_Adaptive_Test_Data.csv")



# Remove parent items (since they have no scores)
PreTaskNP <- PreTask %>% filter(item_type != "parent")

# Create item-specific column names (e.g., 733S for standalone, 733C for child)
PreTaskNP <- PreTaskNP %>%
  mutate(item_col = paste0(item_id, ifelse(item_type == "standalone", "S", "C")))

# Extract necessary columns (response count, raw difficulty, skill)
ItemMetaData <- PreTaskNP %>%
  group_by(item_col) %>%
  summarise(
    raw_difficulty = first(raw_difficulty),  # Same for each item_id
    skill = first(skill),                    # Skill category for the item
    Response_Count = sum(!is.na(score))       # Count responses for each item
  )

# Convert to wide format (keeping only score)
PreTaskWide <- PreTaskNP %>%
  select(user_id, item_col, score) %>%
  pivot_wider(names_from = item_col, values_from = score)

# Compute response counts and filter items with ≥ response_threshold
filtered_items <- ItemMetaData %>%
  filter(Response_Count >= response_threshold) %>%
  pull(item_col)

# Subset dataset to keep only selected items
PreTaskFiltered <- PreTaskWide %>% select(user_id, all_of(filtered_items))

# Convert to matrix format for IRT modeling
IRT_Data_Filtered <- as.matrix(PreTaskFiltered[-1])  # Remove user_id column

# Handle missing data: Replace NA with 0 (assuming missing responses = incorrect)
IRT_Data_Filtered[is.na(IRT_Data_Filtered)] <- 0

# Fit the 2PL Model
irt_2pl <- ltm(IRT_Data_Filtered ~ z1)

# Extract Item Parameters
item_params <- as.data.frame(coef(irt_2pl))  # Convert IRT output to a dataframe
colnames(item_params) <- c("Dscrmn", "Dffclt")  # Rename columns

# Add item names
item_params$Item_ID <- rownames(item_params)

# Merge with item metadata (response count, raw difficulty, skill)
item_params_final <- left_join(item_params, ItemMetaData, by = c("Item_ID" = "item_col"))

# Save as CSV
write.csv(item_params_final, "item_params_with_metadata.csv", row.names = FALSE)


# Compute Cronbach's Alpha
alpha_result <- psych::alpha(IRT_Data_Filtered)

# Print Cronbach’s Alpha value
print(paste("Cronbach’s Alpha:", round(alpha_result$total$raw_alpha, 3)))
# Set response threshold (e.g., n >= 30)
# Print first few rows
print(head(item_params_final))


# Filter items with negative discrimination (Dscrmn < 0)
negative_discrim_items <- item_params_final %>% filter(Dscrmn < 0)

# Select Top 10 items with the most negative discrimination
top_10_negative <- negative_discrim_items %>% arrange(Dscrmn) %>% head(10)

# Select negative discrimination items with exactly 349 responses
negative_349_responses <- negative_discrim_items %>% filter(Response_Count == 349)

# Function to generate ICC plots for selected items with fixed xlim
plot_icc <- function(selected_items, title, xlim_range = c(-4, 4)) {
  if (nrow(selected_items) == 0) {
    print(paste("No items found for", title))
    return(NULL)
  }
  
  # Generate ICC curves using IRT parameters
  ability_range <- seq(xlim_range[1], xlim_range[2], length.out = 100)
  icc_data <- data.frame(Ability = rep(ability_range, times = nrow(selected_items)))
  icc_data$Item <- rep(selected_items$Item_ID, each = length(ability_range))
  
  # Compute ICC probability using the 2PL model formula
  icc_data$Probability <- unlist(
    lapply(1:nrow(selected_items), function(i) {
      a <- selected_items$Dscrmn[i]  # Discrimination
      b <- selected_items$Dffclt[i]  # Difficulty
      1 / (1 + exp(-a * (icc_data$Ability[icc_data$Item == selected_items$Item_ID[i]] - b)))
    })
  )
  
  # Generate ICC Plot with xlim fixed
  ggplot(icc_data, aes(x = Ability, y = Probability, color = Item)) +
    geom_line(size = 1) +
    labs(title = title, x = "Ability (Theta)", y = "Probability of Correct Response") +
    theme_minimal() +
    theme(legend.position = "right") +
    coord_cartesian(xlim = xlim_range)  # Set x-axis limits
}

# Generate and display ICC plots with xlim set to [-4, 4]
plot_icc(top_10_negative, "ICC for Top 10 Negative Discrimination Items", xlim_range = c(-0.2, 0.1))
plot_icc(negative_349_responses, "ICC for Negative Discrimination Items with 349 Responses", xlim_range = c(-5, 5))



# Define threshold for extreme difficulty
difficulty_threshold_high <- 5   # Extremely difficult

# Select Most Difficult Items (Dffclt > 5)
most_difficult_items <- item_params %>% 
  filter(Dffclt > difficulty_threshold_high) %>%
  arrange(desc(Dffclt))



# Generate and display ICC plot for extreme difficulty items
plot_icc(most_difficult_items, "ICC for Most Difficult Items (Difficulty > 5)", xlim_range = c(0, 40))


# Compute correlation
difficulty_correlation <- cor(item_params_final$Dffclt, item_params_final$raw_difficulty, use = "complete.obs")
print(paste("Correlation between IRT Difficulty and Raw Difficulty:", round(difficulty_correlation, 3)))

# Scatter Plot: IRT Difficulty vs. Raw Difficulty
ggplot(item_params_final, aes(x = raw_difficulty, y = Dffclt)) +
  geom_point(aes(color = skill), alpha = 0.7, size = 3) +  # Color by skill category
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Regression line
  labs(title = "Comparison of IRT Difficulty vs. Raw Difficulty",
       x = "Raw Difficulty",
       y = "IRT Difficulty (Dffclt)",
       color = "Skill") +
  theme_minimal()

# Flagging items where the two difficulty measures strongly disagree
disagreement_threshold <- 3  # Define threshold for disagreement

flagged_items <- item_params_final %>%
  filter(abs(Dffclt - raw_difficulty) > disagreement_threshold) %>%
  arrange(desc(abs(Dffclt - raw_difficulty)))  # Sort by difference

# Save flagged items as CSV
write.csv(flagged_items, "flagged_items_difficulty_disagreement.csv", row.names = FALSE)

# Print flagged items
print("Items where IRT and Raw Difficulty strongly disagree:")
print(flagged_items)


# Define the IQR outlier removal method
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Filter out extreme values
  df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
}

# Remove outliers based on `Dffclt`
cleaned_item_params <- remove_outliers(item_params_final, "Dffclt")

# Compute new correlation after removing outliers
new_correlation <- cor(cleaned_item_params$Dffclt, cleaned_item_params$raw_difficulty, use = "complete.obs")
print(paste("New Correlation after Outlier Removal:", round(new_correlation, 3)))

# Scatter Plot: IRT Difficulty vs. Raw Difficulty (After Outlier Removal)
ggplot(cleaned_item_params, aes(x = raw_difficulty, y = Dffclt)) +
  geom_point(aes(color = skill), alpha = 0.7, size = 3) +  # Color by skill category
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Regression line
  labs(title = "IRT Difficulty vs. Raw Difficulty (Outliers Removed)",
       x = "Raw Difficulty",
       y = "IRT Difficulty (Dffclt)",
       color = "Skill") +
  theme_minimal()

