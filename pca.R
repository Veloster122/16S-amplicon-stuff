# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan) # For PCA

# Read the beta diversity data from the provided file
beta_div <- read.delim("beta-div.tsv", sep = "\t", header = TRUE)

# Prepare data for PCA
# Extract the relevant columns for PCA
beta_div_pca_data <- beta_div %>%
  select(braycurtis, thetayc) # Select columns that are relevant for PCA

# Perform PCA
pca <- prcomp(beta_div_pca_data, center = TRUE, scale. = TRUE)

# Create a data frame with PCA results
pca_df <- as.data.frame(pca$x)
pca_df$group <- beta_div$comparison1 # Assuming 'comparison1' represents the sample IDs for grouping

# Define whale groups based on the sample IDs
# You need to replace the 'comparison1' values with actual grouping logic
# Here, it's assumed 'Bph' and 'Bmu' indicate baleen whales, and others indicate toothed whales
pca_df$whale_group <- ifelse(grepl("^Bph|^Bmu", pca_df$group), "Baleen", "Toothed")

# Plot PCA results
ggplot(pca_df, aes(x = PC1, y = PC2, color = whale_group)) +
  geom_point(size = 3) +
  labs(title = "PCA of Beta Diversity Metrics",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Whale Group") +
  theme_minimal()

