# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Step 1: Read the alpha diversity data from alpha-div.tsv
alpha_div <- read.delim("alpha-div.tsv", sep = "\t", header = TRUE)

# Step 2: Prepare data for PCA
# Identify the column that contains sample IDs (adjust if different)
sample_id_col <- "group"  # Assuming the 'group' column contains sample IDs
alpha_div_data <- alpha_div %>% select(where(is.numeric))

# Step 2.1: Remove constant columns
alpha_div_data <- alpha_div_data %>% select(where(~ n_distinct(.) > 1))

# Step 2.2: Add classification column
alpha_div <- alpha_div %>%
  mutate(Group = case_when(
    grepl("^Bph", group) ~ "Baleen",
    grepl("^Bmu", group) ~ "Baleen",
    grepl("^Pma", group) ~ "Toothed",
    TRUE ~ "Unknown"  # Adjust as necessary
  ))

# Step 3: Perform PCA
pca_result <- prcomp(alpha_div_data, center = TRUE, scale. = TRUE)

# Step 4: Interpret PCA results
summary(pca_result)

# Step 5: Extract PCA scores and sample IDs
pca_scores <- as.data.frame(pca_result$x)
pca_scores$SampleID <- alpha_div[[sample_id_col]]

# Step 5.1: Merge PCA scores with classification data
pca_scores <- pca_scores %>% 
  left_join(select(alpha_div, group, Group), by = c("SampleID" = "group"))

# Step 6: Plot PCA results using ggplot2
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(x = "PC1", y = "PC2", title = "PCA Plot of Alpha Diversity") +
  theme(legend.position = "right") +
  scale_color_manual(values = c("Baleen" = "blue", "Toothed" = "red"))

