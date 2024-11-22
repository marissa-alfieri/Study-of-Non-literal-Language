#RISC Video scoring 

library(dplyr)

# Function to process datasets
process_data <- function(data, answer_key) {
  # Perform the merge with the answer key
  data <- data %>%
    left_join(answer_key, by = "Material.set")
  
  # Initialize RISC.Score
  data$RISC.Score <- 0
  
  # Get the list of video columns from the answer key
  video_columns <- colnames(answer_key)[grepl("Video", colnames(answer_key))]
  
  # Loop over the video columns and calculate the score
  for (video_column in video_columns) {
    video_column_x <- paste0(video_column, ".x")  # Data column
    video_column_y <- paste0(video_column, ".y")  # Answer key column
    
    # Ensure both columns exist
    if (video_column_x %in% colnames(data) && video_column_y %in% colnames(data)) {
      # Compare values row-wise and add to RISC.Score
      data$RISC.Score <- data$RISC.Score + (data[[video_column_x]] == data[[video_column_y]])
    } else {
      warning(paste("Columns", video_column_x, "or", video_column_y, "not found in the dataset"))
    }
  }
  
  return(data)
}

# Load your datasets
Summer_2024 <- read.csv("data/Summer_2024.csv")
Fall_2024 <- read.csv("data/Fall_2024.csv")

# Answer key for scoring
answer_key <- data.frame(
  "Material.set" = c(1, 2, 3, 4),  
  "Video.1" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.2" = c("Literal", "Sarcastic", "Sarcastic", "Literal"),
  "Video.3" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.4" = c("Sarcastic", "Literal", "Literal", "Sarcastic"),
  "Video.5" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.6" = c("Literal", "Sarcastic", "Sarcastic", "Literal"),
  "Video.7" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.8" = c("Sarcastic", "Literal", "Literal", "Sarcastic"),
  "Video.9" = c("Literal", "Literal", "Sarcastic", "Literal"),
  "Video.10" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.11" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.12" = c("Sarcastic", "Literal", "Literal", "Literal"),
  "Video.13" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.14" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.15" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.16" = c("Sarcastic", "Literal", "Literal", "Literal"),
  "Video.17" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.18" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.19" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.20" = c("Sarcastic", "Literal", "Literal", "Literal"),
  "Video.21" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.22" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.23" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.24" = c("Sarcastic", "Literal", "Literal", "Literal"),
  "Video.25" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.26" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic"),
  "Video.27" = c("Sarcastic", "Sarcastic", "Literal", "Literal"),
  "Video.28" = c("Sarcastic", "Literal", "Literal", "Literal"),
  "Video.29" = c("Literal", "Literal", "Sarcastic", "Sarcastic"),
  "Video.30" = c("Literal", "Sarcastic", "Sarcastic", "Sarcastic")
)

# Process each dataset
Summer_2024 <- process_data(Summer_2024, answer_key)
Fall_2024 <- process_data(Fall_2024, answer_key)

# Print the results
print(head(Summer_2024[, c("Material.set", "RISC.Score")]))
print(head(Fall_2024[, c("Material.set", "RISC.Score")]))
