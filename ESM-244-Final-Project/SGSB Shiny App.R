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


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Spotting Giant Sea Bass", titleWidth = 250),
                    dashboardSidebar(width = 250,
                                     menuItem("GSB Map", tabName = "GSBmap", icon = icon("map-marker")),
                                     menuItem("Figures", tabName = "figures", icon = icon("fish-fins")),
                                     menuItem("Project Background", tabName = "background", icon = icon("sailboat"))),
                    dashboardBody(
                      tabItems(
                        tabItem("GSBmap",
                                h1("Hey Cute bass!"),
                                box(
                                  leafletOutput("gsb_map"), 
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
                                    sliderInput("Observation Years",
                                                inputId = 'ob_year',
                                                2004,2022,
                                                value = c(2005,2010)),
                                    plotOutput("fidelity_plot")
                                  ),
                                  box(title = "Reef Status",
                                      solidHeader = TRUE,
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
                                    checkboxGroupInput(inputId = "island_id",
                                                       label = 'Pick a location!',
                                                       choices = unique(gsb_fidelity$group_locations),
                                                       selected = 'Northern Channel Islands'),
                                    plotOutput("island_plot")
                                  )
                                )),
                        tabItem("background",
                                h1("Project Background"),
                                box(h6("Data Summary", align = "center"),
                                    p("Data is collected from the Spotting Giant Sea Bass project, a collaborative community science project created in 2016 where divers upload their photos of giant sea bass for researchers to identify individuals via machine learning. Giant sea bass have unique spot patterns - like a fingerprint - allowing for researchers to identity individual giant sea bass through highly accurate pattern recognition software. Once the researcher has identified the spots along the giant sea bass’ side, the software compares spot patterns of previously identified individuals and provides a ranked list of possible matches for the researcher to determine if there is a spot pattern match. If the researcher determines that we have not seen this individual, this giant sea bass is marked as a new individual and given a name. We currently have 340 verified unique individuals out of 545 observations.")),
                                box(h6("Matching Example", align = "center"),
                                    imageOutput("match_img"))
                        )
                      )
                    )
)
server<- function(input, output){
  fidelity_reactive <- reactive({ gsb_fidelity %>%
      filter(between(year_collected,input$ob_year[1],input$ob_year[2]))
  })
  #end fidelity reactive
  output$fidelity_plot <- renderPlot(
    ggplot(data = fidelity_reactive())+
      geom_histogram(aes(x=year_collected, fill=status))+
      labs(
        x="Year",
        y="GSB Encounters"
      )
  ) #end output$fidelity_plot
  reef_reactive <- reactive({
    gsb_fidelity %>%
      filter(status %in% input$reef_type)
  }) #end reef_reactive
  output$reef_plot <- renderPlot(
    ggplot(data=reef_reactive())+
      geom_bar(aes(x = fidelity, fill = status)) +
      scale_fill_brewer(palette = "Pastel1", direction = 1) +
      labs(
        x = "Fidelity Level",
        y = "Number of Unique Individuals",
        title = "Location Fidelity By Site Status"
      ) +
      theme_minimal()
  ) #end output$reef_plot
  island_reactive<- reactive({
    gsb_island_sums %>%
      filter(group_locations %in% input$island_id)
  }) #end island_reactive
  output$island_plot <- renderPlot(
    ggplot(data=island_reactive())+
      geom_point(aes(x=year_collected, y=n, color=group_locations))+
      geom_path(aes(x=year_collected, y=n, color=group_locations))
  )
  
  output$gsb_map <- renderLeaflet({
    leaflet(gsb_top5) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(-118.737930, 33.569371, zoom = 8) %>%
      addMarkers(lng = ~latitude, lat=~longitude, popup = ~gsb_top5$marked_individual)
  })
  
  output$match_img <- renderImage({
    
    list(src = "www/Matching_example.png",
         width = "100%",
         height = 220)
    
  }, deleteFile = F)
  
  output$gsb187_img <- renderImage({
    
    list(src = "www/GSB187.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
}
shinyApp(ui, server)