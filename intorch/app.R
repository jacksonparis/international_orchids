library(shiny)
library(tidyverse)
library(markdown)
library(leaflet)
library(shinythemes)
library(wordcloud2)
library(slam)
library(tm)
library(htmltools)

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

create_wordcloud <- function(data, num_words = 100, background = "white") {
    # Function found on Datacamp:
    # Building Web Application in R with Shiny: Case Studies
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
        corpus <- Corpus(VectorSource(data))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        tdm <- as.matrix(TermDocumentMatrix(corpus))
        data <- sort(rowSums(tdm), decreasing = TRUE)
        data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
        num_words <- 3
    }  
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
        return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
}

library(rvest)
library(imager)
library(magick)

#Run a search query (returning html content)
search_word <- function() {
    
    sample_n(im_index, size = 1) -> random
    
    name <- as.character(random[1,8])
}

# Making the user interface
ui <- navbarPage(theme = shinytheme("cerulean"),
"Orchids Around the World",
                 tabPanel("About",
                     htmlOutput("about1"),
                     actionButton("button", "Random Orchid"),
                     imageOutput("image"),
                     htmlOutput("about2"),
                     p("Link to informative video about the project: 
                           https://www.youtube.com/watch?v=tlUXSMwKek8")
                 ),
                 tabPanel("Importers 1",
                          sidebarLayout(
                              sidebarPanel(
                                  helpText("Ten countries imported the most
                                           orchids worldwide in 2017. This 
                                           interactive allows you to see where
                                           those same countries have imported
                                           orchids from, all the way back 
                                           to 1995."),
                                  
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
                 )),
                tabPanel("Importers 2",
                         sidebarLayout(
                             sidebarPanel(
                                 helpText("The slope is based on a model representing
                                          change in number of live specimens imported 
                                          or exported per year for each country. Greater
                                          slopes correspond to higher levels of activity
                                          over the past fifteen years."),
                                 
                                 selectInput("side", "View Countries as:",
                                             c("importer_country", 
                                               "exporter_country"))
                             ),
                             mainPanel(
                                 plotOutput("plot2")
                             )
                         )),
                 tabPanel("Map",
                          leafletOutput("map", width = "75%", height = "500px"),
                          htmlOutput("text2")),
                tabPanel("Threats",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("category", 
                                             "Select a Redlist category", choices = c(
                                                                                "Critically Endangered",
                                                                                "Endangered",
                                                                                "Vulnerable",
                                                                                "Near Threatened",
                                                                                "Least Concern"))),
                
                             mainPanel(
                                 wordcloud2Output(outputId = "cloud"),
                                 htmlOutput("text3")
                             )
                        
                                     )

            )
             
         )



# Setting up the server function
server <- function(input, output, session) {
    # Making the table output
    output$table <- renderTable({
        top_ten <- 
            aggregated %>%
            filter(importer_country == as.character(input$country), 
                   term == "live", year == input$year) %>%
            group_by(exporter_country) %>%
            replace_na(list(importer_reported_quantity = 0)) %>%
            replace_na(list(exporter_reported_quantity = 0)) %>%
            summarize(importer_count = sum(importer_reported_quantity),
                      exporter_count = sum(exporter_reported_quantity),
                      number_imports = n()) %>%
            arrange(desc(number_imports)) %>%
            head(top_ten, n = 10) %>%
            select(exporter_country, importer_count, exporter_count, 
                   number_imports)

        names(top_ten) <- c("Exporter", 
                            "Importer Reported Specimens",
                            "Exporter Reported Specimens",
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
        
        ggplot(top_ten_graph, aes(x = exporter_country, y = value, 
                                  fill = name)) +
            geom_bar(position = "dodge", stat = "identity") + 
            theme(axis.text.x = element_text(angle = 55, hjust = 1),
                  legend.title = element_blank()) + 
            labs(x = "Exporter Coutnry", 
                 y = "Reported Live Specimens")
 
    })
    
    output$map <- renderLeaflet({
        points_plus$redlistCategory <- 
            factor(points_plus$redlistCategory,
                   levels = c(
                       "Extinct",
                       "Critically Endangered",
                       "Endangered",
                       "Vulnerable",
                       "Near Threatened",
                       "Least Concern"
                   ))
        
        palette <- colorFactor("RdYlGn", points_plus$redlistCategory)
        
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
    
    name <- eventReactive(input$button, {
        search_word() })
    
    output$image <- renderImage({
        
        keyword <- str_c("https://www.google.com/search?site=&tbm=isch&q=",
                         name(),
                         sep = "")
        
        search <- read_html(keyword)
        
        urls <- search %>%
            html_nodes("img") %>%
            html_attr("src")
        
        ima <- image_read(urls[1]) %>%
            image_scale(geometry = "100")
        
        ima <- image_border(image_background(ima, "hotpink"), "#000080", "5x5")
        
        ima <- image_write(ima, path = "ima.jpeg", format = "jpeg")
        
            list(src = "ima.jpeg",
                 contentType = "image",
                 width = 110,
                 alt = "..."
            )}, 
            
            deleteFile = TRUE)
    
    output$text2 <- renderUI({
        HTML("This map was compiled using data from the IUCN Species Redlist
             database. 
             <br/>
             The Redlist is “the world’s most comprehensive information 
             source on the global conservation status of animal, fungus 
             and plant species.”
             <br/>
             Each point represents a wild orchid assessed during a population
             study. Hover over the dots to see the species names, status and 
             year observed.
             <br/>
             <i>https://www.iucnredlist.org</i>"
             )   
    })
    
    output$about1 <- renderUI({
        HTML("<b> So... what are orchids? </b>
             <br/><br/>
             A diverse family of plants, Orchidaceae, with a global distribution 
             -- they are found in the wild on every continent except Antarctica. 
             Their vast array of floral forms has captured the attention
             of insect pollinators and biologists alike for many years. 
             <br/><br/>
             <b> Did you know... </b>
             <br/><br/>
             There are almost 30,000 species of orchids -- and many, many
             more hybrids of these used in ornamental horticulture. Besides 
             the flowers, other parts of certain species are used
             as medicine or food. Vanilla beans are also dried orchid fruits.
             <br/><br/>
             Click the button to generate a picture of a random orchid from 
             the IUCN species database, or scroll down to read more about the
             data.")
    })
    
    output$about2 <- renderUI({
        HTML("<b> About the Data </b>
              <br/><br/>
        CITES is “…an international agreement between governments. 
        Its aim is to ensure that international trade in specimens of 
        wild animals and plants does not threaten their survival.”
        <br/><br/>
        The organization provides a list of species that are to be monitored, 
        each placed in one of three “appendices” in relation 
        to its threat level or protection status.
        <br/><br/>
        As you can see in the table, the importer and exporter 
        accounts often do not match up. CITES acknowledges these 
        inconsistencies in their guide to using trade data:
        <br/><br/>
        “It should be noted that the details of a particular 
        transaction reported by both the exporter/re-exporter and 
        importer frequently fail to show perfect correlation, 
        and therefore will not appear in the same line of the 
        comparative tabulation. This is often for one or more of 
        the following reasons:
        <br/>
        -The source of the items and purpose of the transaction are 
         often reported differently, if at all;
        <br/>
        -Terms and units may also be reported differently for the same 
         items in trade;
        <br/>
        -One of the trading partners may not have submitted a report 
         for the year in question, or may not be a CITES Party;
        <br/>
        -Specimens may be exported at the end of one year but not 
         received by the importer until the following year;
         <br/>
        -Trade may be reported at species level by one country 
         and at a higher taxonomic level by another. This is 
         particularly common in the reporting of artificially 
         propagated plants.”
         <br/><br/>
         CITES does not represent international law —  
         “it provides a framework to be respected by each Party 
         [participating country], which has to adopt its own domestic 
         legislation to ensure that CITES is implemented at the national 
         level.”
        <br/><br/>
             <i>https://www.cites.org/eng</i>
             <br/><br/>
             Project inspired in part by Hinsley et. al 2017, 
             <i>A review of the trade in orchids and its implications for conservation<i/>
             <br/><br/>
             ")
    })
    
    output$cloud <- renderWordcloud2({
        chunk <- 
            aggregated %>%
            select(genus, species, rationale, redlist_category, year) %>%
            filter(year == 2017) %>%
            filter(redlist_category == input$category) %>%
            distinct(rationale) %>%
            as.character() %>%
            htmlEscape() 
        
        create_wordcloud(chunk, num_words = 100)
    })
    
    output$text3 <- renderUI({
        HTML("<br/><br/>
        <p>These are some of the most common words in the <i>rationale</i>
        tabs for IUCN listed species. This tab contains information on 
             why the species was placed in its current Redlist category.</p>
             <br/>
             A sample:
             <br/>
             <i><p>Due to the small number of locations and ongoing habitat decline, 
             we assess this taxon as Endangered. (Habenaria isoantha) <p/>
             </i>")
    })
    
    output$plot2 <- renderPlot({
        model2data <- 
            aggregated %>%
            select(year, importer_country, exporter_country,
                   exporter_reported_quantity) %>%
            drop_na() %>%
            filter(year >= 1995) %>%
            group_by(!! rlang::sym(input$side), year) %>%
            mutate(sum = sum(exporter_reported_quantity)) %>%
            mutate(number = n()) %>%
            filter(number >= 15) %>%
            ungroup() %>%
            group_by(!! rlang::sym(input$side)) %>%
            nest() %>%
            mutate(models = map(data, ~lm(sum ~ year, data = .x))) %>%
            mutate(coefs = map(models, ~coef(.x))) %>%
            mutate(intercept = map_dbl(coefs, ~purrr::pluck(.x, "(Intercept)"))) %>%
            mutate(year = map_dbl(coefs, ~purrr::pluck(.x, "year"))) %>%
            arrange(desc(year)) %>%
            drop_na()
        
        ggplot(model2data, aes(x = reorder(!! rlang::sym(input$side), year), 
                               y = year)) + 
            geom_col() +
            coord_flip() + 
            labs(y = "Slope")
    })
    
}

shinyApp(ui = ui, server = server)
