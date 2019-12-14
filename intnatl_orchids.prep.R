library(janitor)
library(broom)

# Writing a csv file containing all of the data I have aggregated
dir.create("./data")

# Cleaning and renaming the all_table with janitor
aggregated <- clean_names(all_table)

# Writing aggregated as a csv file
write.csv(aggregated, "data/aggregated.csv", row.names = FALSE)

# Finding the top exporters to a given country and year
top_ten <- aggregate %>%
  filter(importer_country == "France", term == "live", year == 2016) %>%
  group_by(exporter_country) %>%
  summarize(imported_count = sum(importer_reported_quantity), 
            exporter_count = sum(exporter_reported_quantity)) %>%
  mutate(total_count = ifelse(is.na(imported_count), 
                              exporter_count, imported_count)) %>%
  arrange(desc(total_count)) %>%
  head(top_ten, n = 10) %>%
  select(exporter_country, total_count) %>%
  replace_na(list(total_count = "Not recorded")) %>%
  rename(rowname = exporter_country)

gt_table <- gt(top_ten) %>%
  tab_stubhead(label = md("**Exporter**")) %>%
  cols_label(total_count = md("**Quantity**"))

# Can I sum with NAs, so later I can replace them with "Not recorded"?
aggregate %>%
  select(importer_country, importer_reported_quantity, 
         exporter_reported_quantity) %>%
  group_by(importer_country) %>%
  replace_na(list(importer_reported_quantity = 0, 
                  exporter_reported_quantity = 0)) %>%
  summarize(importer_count = sum(importer_reported_quantity), 
            exporter_count = sum(exporter_reported_quantity),
            number = n()) %>%
  mutate(total_count = ifelse(importer_count < exporter_count, 
                              exporter_count, importer_count)) %>%
  arrange(desc(number)) 

top_ten <- 
  aggregate %>%
  filter(importer_country == "France", 
         term == "live", year == 1995) %>%
  group_by(exporter_country) %>%
  replace_na(list(importer_reported_quantity = 0)) %>%
  summarize(importer_count = sum(importer_reported_quantity)) %>%
  arrange(desc(importer_count)) %>%
  head(top_ten, n = 10) %>%
  select(exporter_country, importer_count)

as.character(top_ten$importer_count)
top_ten <- top_ten %>% 
  str_replace_all("0", "Not recorded") %>%
  tbl(top_ten)

library(devtools)
install_github(install_github("bwlewis/rthreejs"))


top_ten <-
  aggregated %>%
  filter(importer_country == "United States", 
         term == "live", year == 2000) %>%
  group_by(exporter_country) %>%
  replace_na(list(importer_reported_quantity = 0)) %>%
  replace_na(list(exporter_reported_quantity = 0)) %>%
  summarize(importer_count = sum(importer_reported_quantity),
            exporter_count = sum(exporter_reported_quantity),
            number_imports = n()) %>%
  arrange(desc(number_imports)) %>%
  head(top_ten, n = 10) %>%
  select(exporter_country, importer_count, exporter_count)

top_ten_graph <- pivot_longer(top_ten, 
                              cols = c(importer_count, exporter_count),
                              values_to = "value")

chunk <- 
  aggregated %>%
  select(genus, species, rationale, redlist_category, year) %>%
  filter(year == 2017) %>%
  filter(redlist_category == "Endangered") %>%
  distinct(rationale) %>%
  as.character()

# Cleaning data for model
model_data <- 
  aggregated %>%
  select(redlist_category, realm, year) %>%
  filter(! redlist_category %in% c("Lower Risk/near threatened", 
                                   "Data Deficient")) %>%
  drop_na()

# Renaming the characters
model_data$redlist_category[model_data$redlist_category=="Least Concern"] <- "1"
model_data$redlist_category[model_data$redlist_category=="Near Threatened"] <- "2"
model_data$redlist_category[model_data$redlist_category=="Vulnerable"] <- "3"
model_data$redlist_category[model_data$redlist_category=="Endangered"] <- "4"
model_data$redlist_category[model_data$redlist_category=="Critically Endangered"] <- "5"

model_data <-
  model_data %>%
  transform(redlist_category = as.numeric(redlist_category)) %>%
  filter(! realm %in% c("Afrotropical|Palearctic", 
                        "Australasian|Indomalayan",
                        "Australasian|Oceanian|Palearctic",
                        "Indomalayan|Palearctic",
                        "Nearctic|Neotropical",
                        "Afrotropical|Nearctic|Palearctic",
                        "Australasian|Indomalayan|Palearctic",
                        "Nearctic|Palearctic"))

model1 <- lm(redlist_category ~ realm, model_data)

model_data$realm <- factor(model_data$realm, 
                           levels = c("Australasian",
                                      "Nearctic",
                                      "Neotropical",
                                      "Oceanian",
                                      "Palearctic",
                                      "Indomalayan",
                                      "Afrotropical"))

realmsplot <- ggplot(model_data, 
                     aes(x = realm, y = redlist_category, color = realm)) + 
  geom_jitter(width = .01, alpha = .05) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        legend.position = "none") + 
  labs(x = "Realm", y = "Least to Highest Concern")

model2data <- 
  aggregated %>%
  select(year, importer_country, exporter_country,
         exporter_reported_quantity) %>%
  drop_na() %>%
  filter(year >= 1995) %>%
  group_by(importer_country, year) %>%
  mutate(sum = sum(exporter_reported_quantity)) %>%
  mutate(number = n()) %>%
  filter(number >= 15) %>%
  ungroup() %>%
  group_by(importer_country) %>%
  nest() %>%
  mutate(models = map(data, ~lm(sum ~ year, data = .x))) %>%
  mutate(coefs = map(models, ~coef(.x))) %>%
  mutate(intercept = map_dbl(coefs, ~purrr::pluck(.x, "(Intercept)"))) %>%
  mutate(year = map_dbl(coefs, ~purrr::pluck(.x, "year"))) %>%
  arrange(desc(year)) %>%
  drop_na()

# Change in US exports over time: countries and numbers
# Change in US imports over time

ggplot(model2data, aes(x = reorder(importer_country, year), 
                       y = year)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
  coord_flip() + 
  labs(x = "Shipped To", y = "Slope")





