library(janitor)

# Writing a csv file containing all of the data I have aggregated
dir.create("./data")

write_csv(all_table, "./data/aggregate.csv")

aggregate <- clean_names(aggregate)

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

