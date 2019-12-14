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

write.csv(points_plus, "./intorch/points_plus.csv", row.names = FALSE)

points_plus <- read_csv("./intorch/points_plus.csv", col_types = 
                          cols(
                            binomial = col_character(),
                            longitude = col_double(),
                            latitude = col_double(),
                            redlistCategory = col_character(),
                            label = col_character())) 


search_word <- function() {
  
  sample_n(im_index, size = 1) -> random
  
# //// Cap here for the eventReactive
  
# Display this text: 
  name <- as.character(random[1,8])
  
# Image search: if possible do this in output image, 
# although I'm not sure if that will work...
  keyword <- str_c("https://www.google.com/search?site=&tbm=isch&q=", 
                   name,
                   sep = "")
  
  search <- read_html(keyword)
  
  #Grab all <img> tags, get their "src" attribute, a URL to an image
  urls <- search %>%
    html_nodes("img") %>% 
    html_attr("src") 
  #Get urls of pictures
  
  ima <- image_read(urls[1]) %>%
    image_scale(geometry = "100")
  
  ima <- image_border(image_background(ima, "hotpink"), "#000080", "5x5")
  
  ima
  
}

