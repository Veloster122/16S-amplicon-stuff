library(ggplot2)
library(dplyr)

# dados do dataset em vetores
alpha_div <- data.frame(
  group = c("Bph055", "Bph050", "Bph049", "Bph048", "Bph044", "BphF003", "Bmu025", 
            "Pma188", "Pma185", "Pma179", "Bph063", "Bph058", "Bph057", "Bmu018", "Bbo036"),
  shannon = c(1.546847, 2.77671, 2.10136, 2.861532, 2.356537, 2.061215, 2.115312, 
              1.841776, 2.728204, 2.578837, 2.703602, 2.838636, 2.553966, 1.913494, 0.866619)
)

# classificação das baleias 
toothed_whale <- filter(alpha_div, grepl("^Pma", group))
baleen_whale <- filter(alpha_div, grepl("^Bph|^Bmu|^Bbo", group))


# combinação dos dados filtrados 
combined_data <- rbind(
  mutate(toothed_whale, type = "Toothed Whale"),
  mutate(baleen_whale, type = "Baleen Whale")
)

# boxplot
ggplot(combined_data, aes(x = type, y = shannon, fill = type)) +
  geom_boxplot() +
  labs(x = "Sample Type", y = "Shannon Diversity", title = "Shannon Diversity Comparison") +
  theme_minimal()

