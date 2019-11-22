# Writing the table to a csv file in the intorch (shiny app)
write.csv(aggregated, "./intorch/aggregated.csv", row.names = FALSE)

# Testing
aggregated <- read_csv("intorch/aggregated.csv", col_types = 
                         cols(
                           .default = col_character(),
                           year = col_double(),
                           importer_reported_quantity = col_double(),
                           exporter_reported_quantity = col_double(),
                           assessment_id = col_double(),
                           internal_taxon_id = col_double(),
                           year_published = col_double(),
                           criteria_version = col_double(),
                           year_last_seen = col_double(),
                           possibly_extinct = col_logical(),
                           possibly_extinct_in_the_wild = col_logical(),
                           infra_type = col_logical(),
                           infra_name = col_logical(),
                           infra_authority = col_logical(),
                           subpopulation_name = col_logical()
                         ))
