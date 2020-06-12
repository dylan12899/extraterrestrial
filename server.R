library(shiny)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(shinythemes)

#base of the data set for the project 
aliens <- read_csv("https://www.dropbox.com/s/wg5mcr7c02mp1py/scrubbed.csv?dl=1") %>%
    filter(country == "us") %>%
    drop_na()
colnames(aliens)[9] <- "date_posted"

aliens <- aliens %>% separate(date_posted, sep = "/", into = c("month","day","year"))

alien_state <- as.data.frame(table(aliens$state))

#reading in data for map from json
states <- 
    geojson_read( 
        x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
        , what = "sp"
    )



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    #creating the base map
    m <- leaflet(states) %>%
        setView(-96, 37.8, 4) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
            id = "mapbox.light",
            accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    #Color Pallete for map
    pal <- colorNumeric("Reds", domain = alien_state$Freq)
    
    
    #Creating summary stats for city labels on map
    alien_city <- as.data.frame(table(aliens$city)) %>% filter(Freq > 100)
    colnames(alien_city) <- c("city", "sightings")
    
    cities <- alien_city %>%
        pull(city)
    
    alien_cords <- aliens %>%
        filter(city %in% cities) %>%
        distinct(city, .keep_all = TRUE)
    
    alien_cords <- alien_cords[,c(2,3,12,13)]
    alien_cords <- arrange(alien_cords, city)
    
    city_cords <- merge(alien_city, alien_cords, by = "city")
    
    alien_icon <- makeIcon(iconUrl = "https://purepng.com/public/uploads/medium/purepng.com-ufo-clipartufospace-shipaliencliparticon-11526911775peowi.png",
                           iconWidth = 25, iconHeight = 25)
    
    alien_tables <- aliens %>%
        mutate(shape = case_when(
            shape == "changed" ~ "other",
            shape == "changing" ~ "other",
            shape == "crescent" ~ "other",
            shape == "cone" ~ "other",
            shape == "corss" ~ "other",
            shape == "delta" ~ "other",
            shape == "dome" ~ "other", 
            shape == "flare" ~ "other",
            shape == "hexagon" ~ "other", 
            shape == "pyramid" ~ "other",
            shape == "round" ~ "other",
            TRUE ~ as.character(shape) )) %>%
        filter(city %in% cities) %>%
        group_by(city, shape) %>%
        summarise(shape_count = table(shape)) %>%
        pivot_wider(names_from = shape, values_from = shape_count)
    
    alien_map <- merge(city_cords, alien_tables, by = "city")
     
    output$map <- renderLeaflet({
        m %>% addPolygons(
            fillColor = ~pal(alien_state$Freq),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "1",
            fillOpacity = 0.9,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
            label = paste0(states$name," sightings: ", alien_state$Freq)) %>%
            addMarkers(lat = city_cords$latitude, lng = city_cords$longitude, 
                       label = paste0(str_to_title(city_cords$city),", " , str_to_upper(city_cords$state)),
                       labelOptions = labelOptions(textonly = TRUE),
                        popup = paste0("Number of Total Sightings: ", alien_map$sightings, "<br>","<br>",
                                       "Shapes of sightings seen in ", str_to_title(alien_map$city), ": ", "<br>", "<br>",
                                       "Chevron: ", alien_map$chevron, " | ",
                                       "Cigar: ", alien_map$cigar, " | ",
                                       "Circle: ", alien_map$circle, "<br>",
                                       "Cross: ", alien_map$cross, " | ",
                                       "Cylinder: ", alien_map$cylinder, " | ",
                                       "Diamond: ", alien_map$diamond, "<br>",
                                       "Disk: ", alien_map$disk, " | ", 
                                       "Egg: ", alien_map$egg, " | ",
                                       "Fireball: ", alien_map$fireball, "<br>", 
                                       "Flash: ", alien_map$flash, " | ", 
                                       "Formation: ", alien_map$formation, " | ",
                                       "Light: ", alien_map$light, "<br>",
                                       "Oval: ", alien_map$oval, " | ", 
                                       "Rectangle: ", alien_map$oval, " | ",
                                       "Sphere: ", alien_map$sphere, "<br>",
                                       "Teardrop: ", alien_map$teardrop, " | ",
                                       "Triangle: ", alien_map$triangle, " | ",
                                       "Other: ", alien_map$other, "<br>",
                                       "Unknown: ", alien_map$unknown
                                       
                                       ),
                       icon = alien_icon
                       )
    })
    
    output$day_sum <- renderTable({
        alien_day <- aliens %>%
            filter(month == input$month, day == input$day) 
        adt <- as.data.frame(table(alien_day$state))
        colnames(adt) <- c("State", "Sightings")
        adt
    })
    
    output$pop_city <- renderTable({
        pop_city <- as.data.frame(table(aliens$city)) %>%
            arrange(desc(Freq))
        colnames(pop_city) <- c("City", "Sightings")
        as.data.frame(head(pop_city, n = 5))
    })
    
    output$pop_state <- renderTable({
        pop_state <- as.data.frame(table(aliens$state)) %>%
            arrange(desc(Freq))
        colnames(pop_state) <- c("State", "Sightings")
        as.data.frame(head(pop_state, n = 5))
    })
    
    output$pop_shape <- renderTable({
        pop_shape <- as.data.frame(table(aliens$shape)) %>%
            arrange(desc(Freq))
        colnames(pop_shape) <- c("Shape", "Occourences")
        as.data.frame(head(pop_shape, n = 5))
    })
    
    output$stories_txt <- renderText(as.character(aliens[input$number,8]))
    
    output$map_txt <- renderText({"A map of all UFO sightings across the United States, 
        States are color coded by the number of sightings. Cities with more than 100 UFO spottings are plotted"})
    
    output$date_txt <- renderText({"Select a date and a month to see what UFO's were spotted in which states that day."})
    output$note_txt <- renderText({"Note: There are some days where no observations were made"})
    
    output$sight_txt <- renderText({"The most popular cities, states, and shapes spooted in the entire UFO dataset!"})
    output$city_txt <- renderText({"The top 5 cities with the most UFO sightings"})
    output$state_txt <- renderText({"The top 5 states with the most UFO sightings"})
    output$shape_txt <- renderText({"The 5 most spotted shapes of UFO sightings"})
    
    output$header_txt <- renderText({"Enter a number up to 63553 to see different UFO sighting stories"})
    
})
