library(shiny)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(shinythemes)


#Actual UI


navbarPage(theme = shinythemes::shinytheme("flatly"), 
           "Aliens... they're out there!",
    tabPanel("Map", textOutput("map_txt"),br(), leafletOutput("map")),
    tabPanel("Date Finder", textOutput("date_txt"), br(), textOutput("note_txt"),
             br(), 
             sidebarPanel(
                 sliderInput("month", "Month:",
                             min = 1, max = 12, 
                             value = 1, step = 1),
                 sliderInput("day", "Day:",
                             min = 1, max = 31, 
                             value = 28, step =1)),tableOutput("day_sum")),
    tabPanel("Popular Sights",
             textOutput("sight_txt"), br(),
             textOutput("city_txt"), tableOutput("pop_city"),
             textOutput("state_txt"), tableOutput("pop_state"),
             textOutput("shape_txt"), tableOutput("pop_shape")),
    tabPanel("Stories", textOutput("header_txt"), br(), 
             sidebarPanel(
                 numericInput("number", "Select a Number: ", 
                              min = 1, max = 63553, 
                              value = 93, step = 1)),
             br(),
             textOutput("stories_txt")
             )
)