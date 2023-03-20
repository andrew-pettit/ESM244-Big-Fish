library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(lubridate)
library(leaflet)
library(scales)

gsb_fidelity<-read_csv(here("ESM-244-Final-Project", "GSB_fidelity.csv"))

gsb_fidelity$fidelity<-factor(gsb_fidelity$fidelity, levels=c("High", "Medium", "Low"))

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
                                h1(strong("Project Background")),
                                fluidRow(
                                box(h3(strong("Data Summary"), align = "center"),
                                    br(),
                                    p("Data is collected from the Spotting Giant Sea Bass project, a collaborative community science project created in 2016 where divers upload their photos of giant sea bass for researchers to identify individuals via machine learning. Giant sea bass have unique spot patterns - like a fingerprint - allowing for researchers to identity individual giant sea bass through highly accurate pattern recognition software. Once the researcher has identified the spots along the giant sea bass’ side, the software compares spot patterns of previously identified individuals and provides a ranked list of possible matches for the researcher to determine if there is a spot pattern match. If the researcher determines that we have not seen this individual, this giant sea bass is marked as a new individual and given a name. We currently have 340 verified unique individuals out of 545 observations."),
                                    br(),
                                    h4(strong("Our Coding Team"), align = "center"),
                                    br(),
                                    p("Charlie Braman, Lauren Enright, and Andrew Pettit wrangled the data and designed this Shiny App. Big thank you to Andrew for having such a cool dataset for us to work with."),
                                    br()),
                                box(h3(strong("Map of all Encounters"), align = "center"),
                                    leafletOutput("gsb_map_all"), 
                                  p("All giant sea bass encounters submitted from 2004 - 2022.")
                                  )
                                ),
                                fluidRow(
                                  box(h3(strong("Matching Example"), align = "center"),
                                    imageOutput("match_img"), 
                                    p("Once the photos and videos are uploaded to the project website (https://spottinggiantseabass.msi.ucsb.edu) with detailed information about the encounter (date, location, behavior, depth, etc.), researchers will then “spot map” the giant sea bass’s spot pattern. To do this, researchers manually mark each visible spot on the side of the fish and then run the algorithm, which is essentially a facial recognition software for giant sea bass. (Fun fact: this algorithm was originally developed by astrophysicists to identify patterns in star constellations and was later used by NASA with the Hubble telescope!) The algorithm provides a ranked selection of possible giant sea bass matches, and the research team decides if the spot pattern matches with a previously identified individual. If there is no match, the giant sea bass is marked as a new individual and the diver who submitted the image will be notified and given the opportunity to give the fish a nickname! This data helps researchers have a better understand of how giant sea bass move throughout their natural range, how large the population is, and how effective marine protected areas are in helping preserve this magnificent species. 
")#end matching example caption
                                    ) #end box
                                ) #end fluid row
                                ),
                      
                        tabItem("GSBmap",
                                h1(strong("Hey Cute Bass!")),
                                box(
                                  leafletOutput("gsb_map"), 
                                  br(),
                                  "Follow along with our most frequently sighted giant sea bass.
                                  Choose one of our unique individuals (GSB 187 is our favorite) and see all the locations
                                  where each individual has been encountered",
                                  radioButtons(
                                    inputId = "pick_GSB",
                                    label = 'Choose a GSB',
                                    choices = c("GSB187","GSB136","GSB023","GSB244","GSB359"))),
                                box(h4("GSB187 at Cathedral Point, Anacapa Island in September, 2018. Photo by Douglas Klug", align = "center"),
                                    imageOutput("gsb187_img"), 
                                    br(),
                                    br(),
                                    p("GSB187 has been observed 11 times in 6 different dive sites all over Anacapa Island! GSB187 was first spotted in June, 2015 at Cathedral Cove at Anacapa Island and was last seen in July, 2021 at the northwest side of Anacapa Island. Having data on where individual giant sea bass were encountered over the years is crucial in better understanding their life history and spatial patterns."))
                        ),
                        tabItem("figures",
                                h1(strong("Figures")),
                                fluidRow(
                                  box(
                                    h3(strong("Annual Observations by Site Status"), align= "center"),
                                    solidHeader = TRUE,
                                    br(),
                                    "Choose a date range to see the number of giant sea bass encounters that were recorded within protected and non-preotected areas. Protected areas are dive sites within a Marine Protected Area (MPA). Non-protected sites are areas that are outside MPAs. Unknown protected status are giant sea bass encounters whre the diver provided too broad of a location to determine the protected status of the where the giant sea bass was encountered (ex. a reported location of “Channel Islands”). 
",
                                    br(),
                                    br(),
                                    sliderInput("Choose Observation Year Range",
                                                inputId = 'ob_year',
                                                2004,2022,
                                                value = c(2016,2022),
                                                sep = ""),
                                    plotOutput("fidelity_plot")
                                  ),
                                  box(
                                    h3(strong("Site Fidelity by Site Status"), align = "center"),
                                      solidHeader = TRUE,
                                      br(),
                                      "Compare the number of unique individuals encountered and site fidelity sites amongst  protected and non-protected sites. 
                                      Site fidelity is defined as the tendency of a giant sea bass individual to return to a previously visited location. 
                                      A giant sea bass with high site fidelity represents and individual that has been encountered in only one site over the course of the study. 
                                      Medium site fidelity is when a giant sea bass has been sighted in up to three locations, and low site fidelity is given to giant sea bass individuals who has been encountered in four or more locations over the course of the study. 
                                      Only giant sea bass individuals who have been encountered at least two times are incorporated in this figure. Protected status refers to encounters in dives sites within an MPA and non-protected status refers to sites outside of MPAs. 
                                      Unknown status is designated to giant sea bass individuals that been encountered in an unspecified location making it impossible to determine the protected staus of the giant sea bass encounter.
                                      Since our project was created in 2016, we do not have much data before then.",
                                      br(),
                                      br(),
                                      checkboxGroupInput(inputId = "reef_type",
                                                         label = 'Select Site Status',
                                                         choices = unique(gsb_fidelity$status),
                                                         selected = 'Protected'),
                                      plotOutput("reef_plot"))),
                                fluidRow(
                                  box(
                                    h3(strong('Annual Encounters By Location'), align = 'center'),
                                    solidHeader = TRUE,
                                    br(),
                                    "Compare the four main areas where divers have encountered giant sea bass over the years. Even though the project was created in 2016, community scientists graciously uploaded giant sea bass encounters dating back to 2004. Our earliest confirmed encounter was at Italian Gardens, Catalina on May 28th, 2004. Los Angeles refers to coastal dive sites from Malibu to Long Beach and San Diego refers to coastal dive sites from San Clemente to the Mexican border. Northern Channel Islands include Santa Cruz, Anacapa, Santa Rosa, and San Miguel islands.",
                                    br(),
                                    br(),
                                    checkboxGroupInput(inputId = "island_id",
                                                       label = 'Pick a Location',
                                                       choices = unique(gsb_fidelity$group_locations),
                                                       selected = 'Northern Channel Islands'),
                                    plotOutput("island_plot")
                                  ), # end box
                                box(h3(strong("Marine Protected Areas in Study Site"), align = "center"),
                                imageOutput("MPA_img"), 
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                p("Map of Marine Protected Areas (MPAs) in the Southern California Bight. 
                                  
                                   Ocean Conservancy. (2012). California Marine Protected Areas: South Coast Maps. California Department of Fish and Wildlife. https://californiampas.org/outreach-toolkit/printed-materials/maps"
                                )
                                ) #end box  
                                ), # end fluidRow
                                
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
      geom_bar(aes(x=year_collected, fill = status))+
      labs(
        x="Year",
        y="GSB Encounters"
      )  +
      scale_fill_manual("Status", values = fidelity_vec) +
      scale_x_continuous(breaks = seq(input$ob_year[1],input$ob_year[2], by = 1)) +
      theme_light() +
      theme(text=element_text(size=18),
            panel.grid.major.x = element_line(size = .1, color = "grey"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(size = .1, color = "grey"),
            panel.grid.minor.y = element_line(size = .1, color = "grey"),
            axis.text.x = element_text(angle = 30, hjust = 1))

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
        y = "Number of Unique Individuals") +
      theme_light() +
      theme( 
            text=element_text(size=18),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = .1, color = "grey"),
            panel.grid.minor.y = element_line(size = .1, color = "grey"))
  ) #end output$reef_plot
  
  island_reactive<- reactive({
    gsb_island_sums %>%
      filter(group_locations %in% input$island_id)
  }) #end island_reactive
  
  output$island_plot <- renderPlot(
    ggplot(data=island_reactive())+
      geom_point(aes(x=year_collected, y=n, color=group_locations), size = 3)+
      geom_path(aes(x=year_collected, y=n, color=group_locations), size = 1.5) +
      scale_color_manual("Locations", values = location_vec) +
      scale_y_continuous(breaks = pretty_breaks()) +
      labs(
        x = "Year",
        y = "Number of Sightings") +
      theme_light()+
      theme(
        text=element_text(size=18)),
  ) # end output$island_plot
  
  output$gsb_map <- renderLeaflet({
    leaflet(map_reactive()) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(-118.737930, 33.569371, zoom = 8) %>%
      #addMarkers(data = map_reactive(), lng = ~latitude, lat=~longitude, popup = marked_individual)
      addMarkers(lng = ~latitude, lat=~longitude, popup = input$gsb_map)
  }) # end reactive leaflet map 
  
output$gsb_map_all <- renderLeaflet({
    leaflet(gsb_fidelity) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(-118.737930, 33.569371, zoom = 8) %>%
      addMarkers(data = gsb_fidelity, lng = ~latitude, lat=~longitude, popup = gsb_fidelity$marked_individual)
  }) # end leaflet map 
  
  output$match_img <- renderImage({
    
    list(src = "www/Matching_example.png",
         width = "100%",
         height = 310)
    
  }, deleteFile = F)
  
  output$gsb187_img <- renderImage({
    
    list(src = "www/GSB187.png",
         width = "100%",
         height = 440)
    
  }, deleteFile = F)
  
output$MPA_img <- renderImage({
    
    list(src = "www/MPA_map.jpeg",
         width = "100%",
         height = 500)
    
  }, deleteFile = F)
  
  
} 

shinyApp(ui, server)

