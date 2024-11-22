#RISC Video scoring 

library(dplyr)
Summer_2024 <- read.csv("data/Summer_2024.csv")
Fall_2024 <- read.csv("data/Fall_2024.csv")

# Answer key for scoring
answer_key <- data.frame(
  "Material.set" = c(1, 2, 3, 4),  
  "Video.1" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.2" = c("Literal", "Sarcastic", "Sarcastic", "Literal"),
  "Video.3" = c("Sarcastic", "Sarcastic","Literal","Literal"),
  "Video.4" = c("Sarcastic", "Literal", "Literal", "Sarcastic"),
  "Video.5" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.6" = c("Literal", "Sarcastic", "Sarcastic", "Literal"),
  "Video.7" = c("Sarcastic", "Sarcastic","Literal","Literal"),
  "Video.8" = c("Sarcastic", "Literal", "Literal", "Sarcastic"),
  "Video.9" = c("Literal", "Literal", "Sarcastic", "Literal"),
  "Video.10" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.11" = c("Sarcastic", "Sarcastic","Literal","Literal"), 
  "Video.12" = c("Sarcastic","Literal","Literal", "Literal"),
  "Video.13" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.14" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.15" = c("Sarcastic", "Sarcastic","Literal","Literal"),
  "Video.16" = c("Sarcastic","Literal","Literal", "Literal"),
  "Video.17" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.18" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.19" = c("Sarcastic", "Sarcastic","Literal","Literal"),
  "Video.20" = c("Sarcastic","Literal","Literal", "Literal"),
  "Video.21" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.22" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.23" = c("Sarcastic", "Sarcastic","Literal","Literal"),
  "Video.24" = c("Sarcastic","Literal","Literal", "Literal"),
  "Video.25" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.26" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.27" = c("Sarcastic", "Sarcastic","Literal","Literal"),
  "Video.28" = c("Sarcastic","Literal","Literal", "Literal"),
  "Video.29" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.30" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic")
)

# Ensure that the answer_key has columns named correctly for the merge
colnames(answer_key)[-1] <- paste0(colnames(answer_key)[-1], ".y")

# Perform the merge again
Summer_2024 <- Summer_2024 %>%
  left_join(answer_key, by = "Material.set")

# Initialize the RISC.Score column before the loop
Summer_2024$RISC.Score <- 0

# Loop through each video (1 to 30) and update the RISC score based on the comparison
for (i in 1:30) {
  video_column <- paste0("Video.", i)
  answer_column <- paste0("Video.", i, ".y")  # Assuming correct answer columns have '.y'
  
  # Check if the answer_column exists in the data
  if (answer_column %in% colnames(Summer_2024)) {
    
    # Print values to check if comparison is working
    print(paste("Comparing", video_column, "with", answer_column))
    print(head(Summer_2024[, c(video_column, answer_column)]))  # Check the first few rows
    
    # Update the RISC score by comparing responses for each row
    Summer_2024 <- Summer_2024 %>%
      rowwise() %>%
      mutate(
        RISC.Score = RISC.Score + ifelse(get(video_column) == get(answer_column), 1, 0)
      ) %>%
      ungroup()
  } else {
    warning(paste("Column", answer_column, "not found in the dataset"))
  }
}

# View the modified dataset with the new RISC.Score column
head(Summer_2024)



'''Previous Attempt 
# Preprocess the video responses to extract "Literal" or "Sarcastic"
# Assuming Summer_2024 has columns with responses like "Sarcastic positive", so we will extract the first part
Summer_2024_clean <- Summer_2024 %>%
  mutate(across(starts_with("Video"), ~ gsub(" (positive|negative)$", "", .)))

# Merge the Summer_2024 data with the answer key based on Material.set
Summer_2024_scored <- Summer_2024_clean %>%
  left_join(answer_key, by = c("Material.set")) %>%
  rowwise() %>%
  mutate(
    RISC.Score = sum(c_across(starts_with("Video")) == c_across(ends_with(".y")), na.rm = TRUE)
  ) %>%
  ungroup()

# View the result with the new RISC.Score column
head(Summer_2024_scored)
'''
