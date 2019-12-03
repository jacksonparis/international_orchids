library(shiny)
library(tidyverse)
library(markdown)

aggregated <- read_csv("aggregated.csv", col_types = 
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

points_plus <- read_csv("points_plus.csv", col_types = 
                            cols(
                                binomial = col_character(),
                                longitude = col_double(),
                                latitude = col_double(),
                                redlistCategory = col_character(),
                                label = col_character())) 

im_index <- aggregated %>%
    select(genus, species, rationale, habitat,
           threats, range, use_trade) %>%
    mutate(binomial = str_c(genus, species, sep = "-"))

library(rvest)
library(imager)
library(magick)

#Run a search query (returning html content)
search_word <- function() {
    
    sample_n(im_index, size = 1) -> random
    
    name <- as.character(random[1,8])
    
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


# Making the user interface
ui <- navbarPage("Orchids Around the World",
                 tabPanel("Importers",
                          # Putting the reactive input into a sidebar
                          sidebarLayout(
                              sidebarPanel(
                                  # Text
                                  helpText("Check out some numbers on 2017's 
                    top importers of orchids globally. 
                    All trade data is sourced from CITES."),
                                  
                                  # Country input
                                  selectInput("country", "Select a Country", 
                                              choices = c(
                                                  "United States",
                                                  "Japan",
                                                  "Germany",
                                                  "Switzerland",
                                                  "Canada",
                                                  "United Kingdom",
                                                  "Australia",
                                                  "France",
                                                  "Netherlands",
                                                  "Singapore"
                                              )),
                                  
                                  # Year Input
                                  selectInput("year", "Select a Year", c(1995, 1996, 1997, 1998, 1999, 2000,
                                                                         2001, 2002, 2003, 2004, 2005, 2006,
                                                                         2007, 2008, 2009, 2010, 2011, 2012, 
                                                                         2013, 2014, 2015, 2016, 2017))),
                              # Making space for the table output
                              mainPanel(
                                  tableOutput("table"),
                                  plotOutput("plot")
                              )
                          )
                 ), 
                 tabPanel("Map",
                          leafletOutput("map")
),
                 tabPanel("Explore Species",
                          sidebarLayout(
                              sidebarPanel(
                                  helpText("Click to discover something about
                                           a random orchid in the IUCN 
                                           database."),
                                  actionButton("button", "Random Orchid"),
                              ),
                          mainPanel(
                          imageOutput("image"),
                          textOutput("text"))
                          ))
)

# Setting up the server function
server <- function(input, output) {
    # Making the table output
    output$table <- renderTable({
        top_ten <- 
            
            # Starting with the entire dataset
            aggregated %>%
            
            # Filtering by importer country, year, and live plants
            filter(importer_country == as.character(input$country), 
                   term == "live", year == input$year) %>%
            
            # Group by exporter country
            group_by(exporter_country) %>%
            
            # Replacing NA with zero so summing can happen later
            replace_na(list(importer_reported_quantity = 0)) %>%
            
            # Creating new columns, plant and import counts
            summarize(importer_count = sum(importer_reported_quantity),
                      number_imports = n()) %>%
            
            # Arrange and head to filter top 10
            arrange(desc(number_imports)) %>%
            head(top_ten, n = 10) %>%
            
            # Select columns to be in final table display
            select(exporter_country, importer_count, number_imports)
        
        # Renaming the columns
        names(top_ten) <- c("Exporter", 
                            "Number of Live Specimens", 
                            "Number of Import Shipments")
        top_ten
    })
    
    output$plot <- renderPlot({
        top_ten <- 
            aggregated %>%
            filter(importer_country == as.character(input$country), 
                   term == "live", year == input$year) %>%
            group_by(exporter_country) %>%
            replace_na(list(importer_reported_quantity = 0)) %>%
            summarize(importer_count = sum(importer_reported_quantity),
                      number_imports = n()) %>%
            arrange(desc(number_imports)) %>%
            head(top_ten, n = 10) %>%
            select(exporter_country, importer_count, number_imports)

    })
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>%
            setView(lng = 0, lat = 20, zoom = 1.5) %>% 
            addCircleMarkers(data = points_plus, ~longitude, ~latitude,
                             weight = 0.5,
                             col = ~palette(redlistCategory), 
                             fillColor = ~palette(redlistCategory),
                             radius = 3, 
                             fillOpacity = 0.9, 
                             stroke = T, 
                             label = ~points_plus$label
            )
    })
    
    ima <- eventReactive(input$button, {
        search_word() })
    
    output$image <- renderImage({
        
        ima <- image_write(ima(), path = "ima.jpeg", format = "jpeg")
        
            list(src = "ima.jpeg",
                 contentType = "image",
                 width = 110,
                 alt = "..."
            )}, 
            
            deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
