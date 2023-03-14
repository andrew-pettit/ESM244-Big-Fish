library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(lubridate)
library(leaflet)

gsb_fidelity<-read_csv(here("ESM-244-Final-Project", "GSB_fidelity.csv"))

gsb_island_sums<- gsb_fidelity %>%
  group_by(year_collected) %>%
  count(group_locations)

gsb_top5 <- gsb_fidelity %>%
  filter(marked_individual == "GSB136" | marked_individual == "GSB187" | marked_individual == "GSB023" | marked_individual == "GSB244" | marked_individual == "GSB359")

gsb_fidelity_lubridate <- gsb_fidelity %>%
  drop_na(year_collected) %>%
  mutate(year_recorded = as.Date(as.character(year_collected), format = "%Y")) %>%
  mutate(year_final = year(year_recorded))

reefstatus_vec <- c("Protected" = "#440154FF", "Non-Protected" = "#1FA187FF", "Unknown" = "gray80")

location_vec <- c("Catalina Island" = "#EDD9A3", "Los Angeles" = "#FA7876", "San Diego" = "#C0369D", "Northern Channel Islands" = "#5A2995")

fidelity_vec <- c("Protected" = "#440154FF", "Non-Protected" = "#1FA187FF", "Unknown" = "gray80")


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Spotting Giant Sea Bass", titleWidth = 250),
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                       menuItem("Project Background", tabName = "background", icon = icon("sailboat")),
                                       menuItem("GSB Map", tabName = "GSBmap", icon = icon("map-marker")),
                                       menuItem("Figures", tabName = "figures", icon = icon("fish-fins")))),
                    dashboardBody(
                      tabItems(
                        tabItem("background",
                                h1("Project Background"),
                                box(h6("Data Summary", align = "center"),
                                    p("Data is collected from the Spotting Giant Sea Bass project, a collaborative community science project created in 2016 where divers upload their photos of giant sea bass for researchers to identify individuals via machine learning. Giant sea bass have unique spot patterns - like a fingerprint - allowing for researchers to identity individual giant sea bass through highly accurate pattern recognition software. Once the researcher has identified the spots along the giant sea bass’ side, the software compares spot patterns of previously identified individuals and provides a ranked list of possible matches for the researcher to determine if there is a spot pattern match. If the researcher determines that we have not seen this individual, this giant sea bass is marked as a new individual and given a name. We currently have 340 verified unique individuals out of 545 observations."),
                                    h6("Our Coding Team", align = "center"),
                                    p("Charlie Braman, Lauren Enright, and Andrew Pettit wrangled and designed this Shiny App. Big thank you to Andrew for having such a cool dataset for us to work with.")),
                                box(h6("Matching Example", align = "center"),
                                    imageOutput("match_img"))),
                        
                        
                        tabItem("GSBmap",
                                h1("Hey Cute bass!"),
                                box(
                                  leafletOutput("gsb_map"), 
                                  br(),
                                  "Follow along with some of our most frequently sighted Giant Sea Bass.
                                  Choose one of our unique basses (GSB 187 is our favorite) and see all the locations
                                  that it has been spotted.",
                                  radioButtons(
                                    inputId = "pick_GSB",
                                    label = 'Choose a GSB!',
                                    choices = c("GSB187","GSB136","GSB023","GSB244","GSB359"))),
                                box(h6("GSB187", align = "center"),
                                    imageOutput("gsb187_img"))
                        ),
                        tabItem("figures",
                                h1("Figures"),
                                fluidRow(
                                  box(
                                    title = "Site Fidelity",
                                    solidHeader = TRUE,
                                    br(),
                                    "Choose a range of observations years! This figure will illustrate the number of
                                    Giant Sea Bass encounters for each year, by site status. Site Status included protected
                                    (like an MPA), non-protected, or unknown.",
                                    br(),
                                    br(),
                                    sliderInput("Observation Years",
                                                inputId = 'ob_year',
                                                2004,2022,
                                                value = c(2005,2010),
                                                sep = ""),
                                    plotOutput("fidelity_plot")
                                  ),
                                  box(title = "Reef Status",
                                      solidHeader = TRUE,
                                      br(),
                                      "This figure shows the number of unique individuals in each fidelity level, by site status.
                                      High Fidelity means X amount of returns, medium fidelity means y amounts of returns, and low fidelity means z amount of
                                      returns. Protected sites include marine protected areas (like MPAs).",
                                      br(),
                                      br(),
                                      checkboxGroupInput(inputId = "reef_type",
                                                         label = 'GSB Reef Type!',
                                                         choices = unique(gsb_fidelity$status),
                                                         selected = 'Protected'),
                                      plotOutput("reef_plot"))),
                                fluidRow(
                                  box(
                                    title = 'Sitings By Location',
                                    solidHeader = TRUE,
                                    br(),
                                    "This figure demonstrates the number of Giant Sea Bass (GSB) sightings by location!
                                    Maybe add a sentence about how the earliest sightings were on Catalina Island?",
                                    br(),
                                    br(),
                                    checkboxGroupInput(inputId = "island_id",
                                                       label = 'Pick a location!',
                                                       choices = unique(gsb_fidelity$group_locations),
                                                       selected = 'Northern Channel Islands'),
                                    plotOutput("island_plot")
                                  ) # end box
                                  
                                ) # end fluidRow
                        ) # end tabItem
                      ) #end tabItems
                    ) # end dashboard body
) #end dashboard pages

server<- function(input, output){
  fidelity_reactive <- reactive({ gsb_fidelity_lubridate %>%
      filter(between(year_final, input$ob_year[1],input$ob_year[2]))
  })
  
  map_reactive <- reactive({gsb_top5 %>%
      filter(marked_individual %in% input$pick_GSB)
  }) #end fidelity reactive
  
  map_reactive <- reactive({gsb_top5 %>%
      filter(marked_individual %in% input$pick_GSB)
  })
  
  output$fidelity_plot <- renderPlot(
    ggplot(data = fidelity_reactive())+
      geom_histogram(aes(x=year_collected, fill=status))+
      labs(
        x="Year",
        y="GSB Encounters"
      )  +
      scale_fill_manual("Status", values = fidelity_vec) +
      theme_light()
  ) #end output$fidelity_plot
  
  reef_reactive <- reactive({
    gsb_fidelity %>%
      filter(status %in% input$reef_type)
  }) #end reef_reactive
  
  output$reef_plot <- renderPlot(
    ggplot(data=reef_reactive())+
      geom_bar(aes(x = fidelity, fill = status)) +
      scale_fill_manual("Status", values = reefstatus_vec) +
      labs(
        x = "Fidelity Level",
        y = "Number of Unique Individuals",
        title = "Location Fidelity By Site Status"
      ) +
      theme_light() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = .1, color = "grey"),
            panel.grid.minor.y = element_line(size = .1, color = "grey"))
  ) #end output$reef_plot
  
  island_reactive<- reactive({
    gsb_island_sums %>%
      filter(group_locations %in% input$island_id)
  }) #end island_reactive
  
  output$island_plot <- renderPlot(
    ggplot(data=island_reactive())+
      geom_point(aes(x=year_collected, y=n, color=group_locations))+
      geom_path(aes(x=year_collected, y=n, color=group_locations)) +
      scale_color_manual("Locations", values = location_vec) +
      labs(
        x = "Year",
        y = "Number of Sightings",
        title = "Number of Sightings by Location") +
      theme_light()
  ) # end output$island_plot
  
  output$gsb_map <- renderLeaflet({
    leaflet(map_reactive()) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(-118.737930, 33.569371, zoom = 8) %>%
      #addMarkers(data = map_reactive(), lng = ~latitude, lat=~longitude, popup = marked_individual)
      addMarkers(lng = ~latitude, lat=~longitude, popup = input$gsb_map)
  })
  
  output$match_img <- renderImage({
    
    list(src = "www/Matching_example.png",
         width = "100%",
         height = 300)
    
  }, deleteFile = F)
  
  output$gsb187_img <- renderImage({
    
    list(src = "www/GSB187.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  
  
} 

shinyApp(ui, server)

