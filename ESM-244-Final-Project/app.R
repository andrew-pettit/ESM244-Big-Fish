library(shiny)
library(tidyverse)
library(bslib)
library(here)

gsb_fidelity<-read_csv(here("ESM-244-Final-Project", "GSB_fidelity.csv"))
gsb_fidelity <- gsb_fidelity %>%
  mutate(group_locations = case_when(location_id %in% c("Catalina Island- Casino Point", "Catalina Island- Eagle Reef", "Catalina Island- Italian Gardens", "Catalina Island- Goat Harbor", "Catalina Island- Rock Quarry",
                                                        "Catalina Island- Twin Rocks", "Catalina Island- frontside", "Catalina Island- Big Fisherman's Cove", "Catalina Island", "Catalina Island- Little Farnsworth", "Catalina Island- Isthmus Cove") ~ "Catalina Island",
                                     location_id %in% c("Anacapa Island","Anacapa Island- Admiral's Reef","Anacapa Island- Brickyard", "Anacapa Island- Canyons","Anacapa Island- Cathedral Cove","Anacapa Island- Cathedral Point",
                                                        "Anacapa Island- East Fish Camp","Anacapa Island- Landing Cove","Anacapa Island- Lighthouse Reef", "Anacapa Island- Northeast Side", "Anacapa Island- Northwest Side","Anacapa Island- Rat Rock", "Anacapa Island- Winfield Scott Wreck",
                                                        "Channel Islands","Santa Barbara Island", "Santa Cruz Island","Santa Cruz Island- Yellow Banks") ~ "Northern Channel Islands",
                                     location_id %in% c("Hermosa Artificial Reef", "Redondo Beach") ~ "Los Angeles",
                                     location_id %in% c("La Jolla Cove", "La Jolla Shores", "San Clemente Island", "San Diego Kelp", "San Diego- HMCS Yukon", "Wheeler North Reef, SONGS site") ~ "San Diego"))
my_theme <- bs_theme(
  bg = "lightblue", #background
  fg = "black", #foreground
  primary = "yellow",
  base_font = font_google("Roboto") #auto grab a google font we lke
)
ui <- fluidPage(theme = my_theme,
                navbarPage(
                  "Spotting Giant Sea Bass",
                  tabPanel("Map",
                           sidebarLayout(
                             sidebarPanel(tags$img(src = 'GSB187.png', height = 300, width = 430),
                                          radioButtons(
                                            inputId = "pick_GSB",
                                            label = 'Choose a GSB!',
                                            choices = c("GSB 187","GSB 226","GSB 168","GSB 307","GSB 346")
                                          )
                             ),
                             mainPanel("Map")
                           )#end Sidebarlayout 1
                  ),#endMaptabPanel
                  tabPanel("Figures",
                           sidebarLayout(
                             sidebarPanel("Widgets",
                                          checkboxGroupInput(
                                            inputId = "artificial",
                                            label = 'Reef Type!',
                                            choices = unique(gsb_fidelity$status)
                                          ),
                                          checkboxGroupInput(
                                            inputId = "location",
                                            label = 'Location!',
                                            choices = unique(gsb_fidelity$group_locations)
                                          ),
                                          sliderInput("Observation Years",
                                                      inputId = 'ob_year',
                                                      2004,2022,
                                                      value = c(2005,2010))
                             ),
                             mainPanel("FIGURES!",
                                       plotOutput("gsb_fidelity_plot"))
                           ) #end sidebarlayoutFigurestab
                  ), #end tabpanel figures tab
                  tabPanel("Project Summary",
                           sidebarLayout(
                             sidebarPanel(
                               tags$img(src = 'Matching_example.png', height = 250, width = 500),
                             ),
                  mainPanel(
                        h6("Data Summary", align = "center"), 
                        p("Data is collected from the Spotting Giant Sea Bass project, a collaborative community science project created in 2016 where divers upload their photos of giant sea bass for researchers to identify individuals via machine learning. Giant sea bass have unique spot patterns - like a fingerprint - allowing for researchers to identity individual giant sea bass through highly accurate pattern recognition software. Once the researcher has identified the spots along the giant sea bass’ side, the software compares spot patterns of previously identified individuals and provides a ranked list of possible matches for the researcher to determine if there is a spot pattern match. If the researcher determines that we have not seen this individual, this giant sea bass is marked as a new individual and given a name. We currently have 340 verified unique individuals out of 545 observations.") 
                  ) #ending mainPanel
                  ) #ending sidebarLayout
                  ) #ending tabPanel
                ) #end navbarPage
) #end fluidPage
server <- function(input, output) {
  gsb_reactive<- reactive({
    x <- gsb_fidelity %>%
      filter("artificial" %in% input$status )
    return(x)
  }) #end gsb_reactive
  output$gsb_fidelity_plot <- renderPlot(
    ggplot(data=gsb_reactive()+
             geom_bar(aes(x = fidelity, fill = status)) +
             scale_fill_brewer(palette = "Pastel1", direction = 1) +
             labs(
               x = "Fidelity Level",
               y = "Number of Unique Individuals",
               title = "Location Fidelity By Site Status"
             ) +
             theme_minimal()
    ))
}

shinyApp(ui = ui, server = server)








