#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(httr)
library(RCurl)
library(scales)
library(readr)
library(plotly)
library(shinytest)
# Avoid plotly issues ----------------------------------------------
pdf(NULL)






header <- dashboardHeader(title = "Pittsburgh Transit Deserts"
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Transit Desert Map", icon = icon("bar-chart"), tabName = "map_tab"),
    menuItem("Summary Charts", icon = icon("pie-chart"), tabName = "plot_tab"),
    menuItem("Data Table", icon = icon("table"), tabName = "table_tab"),
    
    #radio buttons to choose destination
    radioButtons("destination",
                 "Destination",
                 c("Nearest Grocery Store"="grocery",
                   "Nearest Hospital"="hospital"),
                 selected="grocery"),
    #radio buttons to choose departure time
    radioButtons("departure_time",
                 "Departure Time",
                 c("Friday Morning (8am)"="1571400000",
                   "Friday at noon"="1571414400",
                   "Friday Evening (5pm)"="1571432400"),
                 selected="1571400000"),
    #radio button to chose measure of transit difficult
    radioButtons("transit_metric",
                 "Measure of Public Transit Difficulty",
                 c("Trip time (min)"="Total Transit Time",
                   "Number of transfers"="Number of Transfers",
                   "Total walking distance (mi.)"="Total Distance to/from transit stops")),
    #checkbox to select if bus route lines should be shown
    checkboxInput("bus_routes",
                  "Show Bus Routes")
  )
)


# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Map page ----------------------------------------------
  tabItem("map_tab", class="active",
        
          
          # Plot ----------------------------------------------
          #fluidRow(
          box(title = "Pittsburgh Transit Deserts",
              width = 12,
              leafletOutput("map"))
          #)
  ),
  
  #Breakdown plot page
  tabItem("plot_tab", class = "active"#,
          
          # # Plot ----------------------------------------------
          # tabBox(title = "Summary Charts",
          #        width = 12,
          #        tabPanel("Average Difficulty per Locality", plotlyOutput(outputId = "bar_plot")),
          #        tabPanel("Seasonality",plotlyOutput(outputId="seasonality")),
          #        tabPanel("Overall Share",plotOutput(outputId="donut")))
          
  ),
  
  # Table Page ----------------------------------------------
  tabItem("table_tabl", class = "active"#,
    #      box(title = "Data Table", DT::dataTableOutput("datatab")))
)
))

# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)


# Create plots in the server function
server <- function(input, output) {
  
  #load data
  
  load("address_data.RData")
  load("transit_difficulties2.RData")
  
  #turn off scientific notation
  options(scipen=999)
  
  #create a reactive for map marker click
  marker_click<-reactive({
    input$map_marker_click[[1]]
  })

  #create a reactive object for destination choice
  chosen_destination<-reactive({
    paste0(input$destination)
  })
  #create a reactive object for the departure time choice
  chosen_departure_time<-reactive({
    paste0(input$departure_time)
  })
  #create a reactive object out of transit metric choice
  chosen_stat<-reactive({
    paste0(input$transit_metric)
  })
  
  #create a reactive object for adding bus routes
  fixed_route<-reactive({
    input$bus_routes
  })
  

  #create react objects for available and unavailable routes based on radio button selections
  available<-reactive({
    transit_difficulties[[chosen_destination()]][[chosen_departure_time()]][["available"]]%>%
    #transit_difficulties[[chosen_destination()]][[chosen_departure_time()]][["available"]]%>%
      #since walking distance is in meters, make it miles
      mutate(`Total Distance to/from transit stops`=`Total Distance to/from transit stops`/1609.34)%>%
      #add a column for the data in the popup
      mutate(pop_up=paste(sep="<br/>",
                          paste0("<b>Address:</b> ",gsub('\\+',' ',start_address)),
                          paste0("<b>Total Transit Time:</b> ",
                                 round(`Total Transit Time`,1)),
                          paste0("<b>Number of Transfers: </b>",`Number of Transfers`),
                          paste0("<b>Total Distance to/from transit stops: </b>",
                                 round(`Total Distance to/from transit stops`,1)),
                          paste0("<b>Transit Lines Used: </b>", `Transit Lines`)))%>%
      #limit only to stat selected
      dplyr::rename("chosen_stat"=chosen_stat())
  })
  
  #create a list of directions to display below map
  output$directions<-reactive({
    
    if(!is.null(marker_click())){
      temp<-available()%>%
        dplyr::filter(start_address==marker_click())
      paste0(temp$Instructions)
    }
    
  })
  unavailable<-reactive({
    #test<-transit_difficulties[["South+Norwalk+Train+Station,+Norwalk,+CT"]][[]]
    transit_difficulties[[chosen_destination()]][[chosen_departure_time()]][["unavailable"]]%>%
      as.data.frame()%>%
      mutate(pop_up=paste(sep="<br/>",
                          paste0("<b>Start Address:</b> ",start_address),
                          paste0("<b>Soonest Available Departure:</b> ",soonest_departure),
                          paste0("<b>Reason:</b> ",reason)))
  })
  
  #create color palette for chosen series
  pal <-reactive({
    colorNumeric(palette = "RdYlGn",
                 domain = available()$chosen_stat, n = nrow(available()),
                 reverse=TRUE)
  })
  
  
  #initialize map 
  output$map<-renderLeaflet({
    
    #st_transform(crs = "+init=epsg:4326") %>%
    leaflet(
      width = "50%")%>%
      setView(-80.0005025, 40.4483066, 11.25) %>%
      addProviderTiles(provider = "CartoDB.Positron") 
    #        addProviderTiles(provider="CartoDB.Positron")
  })
  
  #create an observer that bus route lines if checked
  # observe({
  #   if(wheels2u()){
  #     leafletProxy({"map"})%>%
  #       clearGroup("wheels2u")%>%
  #       addKML(kml=wheels2u_boundary,
  #              stroke=FALSE,
  #              opacity=.3,
  #              color="blue",
  #              group="wheels2u")
  #   }else{
  #     leafletProxy({"map"})%>%
  #       clearGroup("wheels2u")
  #   }
  # })
  

  #create star icon
  # star<-makeIcon(
  #   iconUrl="www/star_dark.png",
  #   iconWidth = 35, iconHeight=35
  # )
  
  #add the destination to the map upon click
  # observe({
  #   leafletProxy("map")%>%
  #     clearGroup("destination")%>%
  #     addMarkers(data=destination_layer(),
  #                icon=star,
  #                group="destination")
  # })
 
  #create redx icon
  redX <- makeIcon(
    iconUrl = "www/not_found.png",
    iconWidth = 20, iconHeight = 20#,
    # iconAnchorX = 22, iconAnchorY = 94,
  )


  # #add the layer for available routes
  observe({
    leafletProxy("map")%>%
      clearGroup("avail")%>%
      addCircleMarkers(data=available()%>%st_as_sf(crs = "+init=epsg:4326"),popup =~pop_up,
                       layerId = ~`start_address`,
                       stroke=FALSE,
                       radius=10,
                       fillOpacity=0.8,
                       color=~pal()(chosen_stat),
                       group="avail"
                       )
  })
  # #add the layer for unavailable routes
  observe({
    leafletProxy("map")%>%

      clearGroup("unavail")%>%
      addMarkers(data=unavailable()%>%st_as_sf(),popup=~pop_up,
                 icon=redX,
                 group="unavail")
  })
  
  # #add the layer for unavailable routes
  # icons <- awesomeIconList(
  #   times_circle = makeAwesomeIcon(icon = "times-circle", library = "fa", markerColor = "red")
  # )
  
  
  
  # observe({
  #   leafletProxy("map")%>%
  #     clearGroup("unavail")%>%
  #     addCircleMarkers(data=unavail,
  #                       #icon=icons["times_circle"],
  #                       group="unavail")
  # })
  #add legend to the chart
  # observe({
  #   leafletProxy("map")%>%
  #     clearControls()%>%
  #     clearGroup("legend")%>%
  #     addLegend(data=available(),"bottomright",
  #               pal = pal(),
  #               values = ~ chosen_stat,
  #               title = gsub('/','/</br>',chosen_stat()),
  #               opacity = 1,
  #               group="legend")
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)