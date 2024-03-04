library(tidyverse)
library(janitor)
library(readxl)
library(ggrepel)

metadata <- read_excel("Metadata_Table_Samples.xlsx") 


metadata_tidy <- metadata %>% 
  clean_names() %>% 
  select(-identification_of_m_leprae, -rlep_q_pcr) %>% 
  mutate(rlpm_q_pcr = factor(rlpm_q_pcr)) %>% 
  group_by(country, rlpm_q_pcr) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  filter(!is.na(rlpm_q_pcr))

total_counts <- metadata_tidy %>% 
  group_by(country) %>% 
  summarize(total_count = sum(count))

metadata_tidy <- left_join(metadata_tidy, total_counts, by = "country")

lookup_table <- data.frame(
  country = c(
    "Argentina (Chubut province)",
    "Brazil, Manaus",
    "Brazil, Para",
    "Brazil, Recife",
    "Canada (Metlakatla first Nation)",
    "Costa Rica",
    "French Guiana",
    "Mexican/Texas",
    "Mexico",
    "Mexico Sonora",
    "Mexico, Sinaloa",
    "Paraguay",
    "USA"
  ),
  latitude = c(
    -44.0678,
    -3.1190,
    -4.4652,
    -8.0476,
    55.1783,
    9.7489,
    3.9339,
    27.7719, 
    23.6345,
    29.2972,
    25.1721,
    -23.4425,
    30.8528
  ),
  longitude = c(
    -68.6977,
    -60.0217,
    -53.0841,
    -34.8770,
    -131.5417,
    -83.7534,
    -53.1258,
    -99.8386,
    -102.5528,
    -111.0542,
    -107.4795,
    -58.4438,
    -94.6817
  )
)

metadata_tidy <- merge(metadata_tidy, lookup_table, by = "country", all.x = TRUE)


#-----------

# Create pie charts
pie_charts <- ggplot(metadata_tidy, aes(x = "" , y = count/total_count, fill = fct_inorder(rlpm_q_pcr))) +
  geom_col(width = 1, color = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void()

# Add data labels with repelling
pie_charts <- pie_charts +
  geom_label_repel(data = metadata_tidy2,
                   aes(y = pos, label = paste0(round(count/total_count*100, 1), "%")),
                   size = 2.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "rlpm_q_pcr")) +
  facet_wrap(~ country) + 
  theme(axis.text.x = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),  # Legend title appearance
        legend.text = element_text(size = 10),  # Legend text appearance
        strip.text = element_text(size = 12, face = "bold"),
        strip.clip = "off")  # Facet label appearance


# Print the pie charts
print(pie_charts)

#--------------- Testing