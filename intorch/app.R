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

# Making the user interface
ui <- navbarPage("Orchids Around the World",
                 tabPanel("Importers",
                          # Putting the reactive input into a sidebar
                          sidebarLayout(
                              sidebarPanel(
                                  # Year input
                                  helpText("Check out some numbers on 2017's 
                    top importers of orchids globally. 
                    All trade data is sourced from CITES."),
                                  selectInput("year", "Select a Year", c(1995, 1996, 1997, 1998, 1999, 2000,
                                                                         2001, 2002, 2003, 2004, 2005, 2006,
                                                                         2007, 2008, 2009, 2010, 2011, 2012, 
                                                                         2013, 2014, 2015, 2016, 2017)),
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
                                              ))),
                              # Making space for the table output
                              mainPanel(
                                  tableOutput("table")
                              )
                          )
                 ), 
                 tabPanel("Map",
                          p("This tab will display a map
             of endangered orchid distributions."),
                          verbatimTextOutput("Map"))
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
                            "Importer Reported Quantity", 
                            "Number of Imports")
        top_ten
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
