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
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
# Avoid plotly issues ----------------------------------------------
#pdf(NULL)


rm(list=ls())
#load bus api key
load(file="bus_time_api_key.RData")

################################ SET UP LIVE BUS LOCATION API ############################
############# WAS HOPING TO MAKE THIS CLICK REACTIVE BUT COULDN'T MERGE ROUTES, SO USING DROPDOWN SELECT #####
baseUrl <- "http://realtime.portauthority.org/bustime/api/v3/"
#function to call API (from Geoff's code)
getRealTime <- function(endpoint, params, response) {
  if (missing(params)) {
    url <- paste0(baseUrl, endpoint, "?format=json&key=", bus_time_api_key)
  } else if (typeof(params) == "list") {
    params_text <- paste0(names(params), "=", params, collapse = "&")
    url <- paste0(baseUrl, endpoint, "?format=json&key=", bus_time_api_key, "&", params_text)
  }
  json <- fromJSON(url)$`bustime-response`[[response]]
  if (is.null(json)) {
    return(data.frame())
  } else {
    return(json)
  }
}

# Routes
load.routes <- getRealTime("getroutes", response = "routes")


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
                 selected="hospital"),
    #radio buttons to choose departure time
    radioButtons("departure_time",
                 "Departure Time",
                 c("Friday Morning (8am)"="1571400000",
                   "Friday at noon"="1571414400",
                   "Friday Evening (5pm)"="1571432400"),
                 selected="1571414400"),
    #radio button to chose measure of transit difficult
    radioButtons("transit_metric",
                 "Measure of Public Transit Difficulty",
                 c("Trip time (min)"="Total Transit Time",
                   "Number of transfers"="Number of Transfers",
                   "Total walking distance (mi.)"="Total Distance to/from transit stops")),
    #checkbox to select if bus route lines should be shown
    checkboxInput("bus_routes",
                  "Show Bus Routes")#,

    
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
              HTML("The dots below are 500 randomly selected addresses in Allegheny County. </br> The colors of the dots below indicate the difficulty of getting from that dot to the nearest selected destination using only public transportation. Click the dot to see the nearest location and view public transit journey information."),
              leafletOutput("map"),
              #dropdown to show live bus routes
              selectInput("routeSelect",
                          "Show Live Bus Locations:",
                          choices = c(" ",sort(load.routes$rt)),
                          selected = " "),
              uiOutput(outputId="current_unavailable"),
              uiOutput(outputId="directions"))
          #)
  ),
  
  #Charts plot page
  tabItem("plot_tab", class = "active",
          
          # Plot ----------------------------------------------
          tabBox(title = "Summary Charts",
                 width = 12,
                 height="1000px",
                 tabPanel("Average difficulty per municipality", plotlyOutput(outputId = "plot1")),
                 tabPanel("Addresses served by location",plotOutput(outputId="plot2"))
                 )

  ),
  
  # Table Page ----------------------------------------------
  tabItem("table_tab", class = "active",
          downloadButton("downloadData", "Download"),
          box(title = "Data Table", DT::dataTableOutput("datatab"))
)
))

# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)


# Create plots in the server function
server <- function(input, output) {


  

  
  #create reactive for displaying directions on click
  output$directions<-reactive({
    
    if(!is.null(marker_click())){
      temp<-available()%>%
        dplyr::filter(start_address==marker_click())
      paste0(temp$Instructions)
    }
    
  })
  #load data
  
  load("address_data.RData")
  load("transit_difficulties2.RData")
  
  #load bus routes
  bus_routes<-read_sf("paacroutes1611/PAAC_Routes_1611.shx")%>%
    st_transform(crs = "+init=epsg:4326")
  
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
  
  #create reactive for bus routes
  bus_checked<-reactive({
    paste0(input$bus_routes)
  })

test2<-transit_difficulties[[1]][[1]][[1]]
test<-address_data[["grocery"]]
test4<-test2%>%
  left_join(test%>%
            mutate(address_number=1:500,
                   address_number=as.character(address_number)),by="address_number")

  #create react objects for available and unavailable routes based on radio button selections
  available<-reactive({
    #select shape file data for chosen destination (grocery or hospital)
    destination_data<-address_data[[chosen_destination()]]%>%
      #add row_num to allow merge
      mutate(address_number=1:500,
             address_number=as.character(address_number))
    
    
    #select extracted route information for chosen dataset
    transit_difficulties[[chosen_destination()]][[chosen_departure_time()]][["available"]]%>%
      left_join(destination_data,by="address_number")%>%
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
                          paste0("<b>Closest ",chosen_destination(),":</b> ",Name)
                          ))%>%
      #limit only to stat selected
      dplyr::rename("chosen_stat"=chosen_stat())
  })
  
  ############################################################################################################
  ###############################  CREATE CHARTS      ########################################################
  
  # plot 1: average difficulty by municipality for chosen stat and departure time
  output$plot1<-renderPlotly({
    for_plot1<-available()%>%
      as.data.frame()%>%
      group_by(MUNICIPALI)%>%
      summarise(locations=length(chosen_stat),
        chosen_stat=mean(chosen_stat)
                )%>%
      ungroup()
    
      ggplot(data=for_plot1,aes(x=MUNICIPALI,y=chosen_stat,fill=locations))+
        geom_bar(stat="identity")+
        labs(title="Average Difficulty Per Municipality",
             y=chosen_stat(),
             x="Municipality")+
        theme(axis.text.x=element_text(size=rel(0.5), angle=90))+
        scale_fill_continuous()
  })
  
  #plot 2: number of locations corresponding to chosen destination
  output$plot2<-renderPlot({
    
    palette_choice<-"Paired"
    #summarize by number of locations per destinations
    for_plot2<-available()%>%
      as.data.frame()%>%
      group_by(Name)%>%
      summarise(locations=length(start_address))%>%
      ungroup()
    
    for_plot2%>%
      filter(locations>20)%>%
      mutate(ymax=cumsum(locations),ymin=c(0,head(ymax,n=-1)))%>%
      ggplot(
             aes(fill=Name,ymin=ymin,ymax=ymax,xmin=3,xmax=5,label=Name),colour="white")+
      #make chart rectangular
      geom_rect()+
      #convert rectangular chart to polar (donut)
      coord_polar(theta="y")+
      xlim(c(0,5))+
      #remove background
      theme_void() +
      #remove x and y value labels
      xlab("")+
      ylab("")+
      #remove other aesthetic elements of chart
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      #center title
      theme(plot.title = element_text(hjust = 0.5))+
      #place legend on bottom with no legend title
      theme(legend.title=element_blank())+
      theme(legend.position="bottom")+
      #resize legend items to fit well
      guides(fill=guide_legend(nrow=2,byrow=TRUE,keywidth=.4,keyheight=.2,default.unit="inch"),
             color="none")+
      geom_label(
        aes(x=4,y=(ymax+ymin)/2,label=comma(locations)),
        label.size=.175,
        show.legend=FALSE)+
      labs(title="Addresses served by Destination\n (minimum 20)")+
      scale_fill_brewer(palette=palette_choice)
  })



  ############################################################################################################
  ############################################# CREATE DATA TABLE ###########################################################
  dt_reactive<-reactive({
    
    temp<-available()#%>%
    #getting absurd errors trying to reassign the original name to chosen stat within available(), so had to use this weird construction
    names(temp)[names(temp)=="chosen_stat"]<-chosen_stat()
    #select only relevant transit columns
    temp%>%
      select("Start Address"=start_address,"Destination Name"=Name,
            `Total Transit Time`, `Number of Transfers`,
            `Total Distance to/from transit stops`,`Transit Lines`)%>%
      #round some of these numbers
      mutate(`Total Transit Time`=round(`Total Transit Time`),
             `Total Distance to/from transit stops`=round(`Total Distance to/from transit stops`,2))
  })
  
  #create data table output
  output$datatab<-DT::renderDataTable(
    
    DT::datatable(dt_reactive())


  )
  #for download button
  output$downloadData<-downloadHandler(
    filename="transit_difficulties.csv",
    content=function(file){
      write.csv(dt_reactive(), file, row.names = FALSE)
    }
  )
  
  ############################################################################################################
  
  #create star icon
  star<-makeIcon(
    iconUrl="www/star_dark.png",
    iconWidth = 35, iconHeight=35
  )

  #show destination on click
  observe({
    #test if the marker is clicked
    if(!is.null(marker_click())){
      #start with available
      add_destination<-available()%>%
        #remove existing geometry
        dplyr::select(-starts_with("geometry"))%>%
        #rename grocery/hospital geometry as just geometry
        rename(geometry=paste0(chosen_destination(),"_geometry"))%>%
        filter(start_address==marker_click())
      
      #add destination to leaflet
      leafletProxy({"map"})%>%
        clearGroup("destination")%>%
        addMarkers(data=add_destination%>%st_as_sf(crs = "+init=epsg:4326"),
                   icon=star,group="destination")
    }
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

  
  #create star icon
  bus_icon<-makeIcon(
    iconUrl="www/bus.png",
    iconWidth = 35, iconHeight=35
  )
  
  ###################################### FOR API CALL ######################################
  routesInput <- reactive({
    routes <- filter(load.routes, rt %in% input$routeSelect)
  })
  
  #create observer that checks if a route is selected for live vehicle display and if it is, adds vehicle locations to map
  observe({
    #condition on a route being selected
    if(input$routeSelect==" "){
      #make output$current_unavailable blank
      output$current_unavailable<-reactive({""})
      #clear map 
      leafletProxy({"map"})%>%
        clearGroup("live_vehicles")
    }else{
      #call API for selected route
      vehicles <- getRealTime("getvehicles", list(rt = paste(routesInput()$rt, collapse =",")), "vehicle")
      if(nrow(vehicles>0)){
        #make output$current_unavailable blank
        output$current_unavailable<-reactive({""})
        #add vehicles to map
        leafletProxy({"map"})%>%
          clearGroup("live_vehicles")%>%
          addMarkers(data=vehicles%>%
                       #make lat and lon numeric
                       mutate(lat = as.numeric(lat),
                              lon = as.numeric(lon)),icon=bus_icon,group="live_vehicles")
        }else{
        output$current_unavailable<-reactive({"No Vehicles for this line currently in service"})
        leafletProxy({"map"})%>%
          clearGroup("live_vehicles")
        
      }

      
    }
    
    
    
  })
############################################################################################################
  #create an observer for if bus route lines iS checked
  observe({
    if(bus_checked()){
      leafletProxy({"map"})%>%
        clearGroup("bus_routes")%>%
        addPolylines(data=bus_routes,
                     weight=1,
                     #add label for bus route/line
                     label=~paste(Route_Name,ROUTE),
                     opacity=.5,
                     color="black",
                     group="bus_routes")
    }else{
      leafletProxy({"map"})%>%
        clearGroup("bus_routes")
    }
  })
  


  
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
                       radius=6,
                       fillOpacity=0.5,
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
  

  
  

  #add legend to the chart
  observe({
    leafletProxy("map")%>%
      clearControls()%>%
      clearGroup("legend")%>%
      addLegend(data=available(),"bottomright",
                pal = pal(),
                values = ~ chosen_stat,
                title = gsub('/','/</br>',chosen_stat()),
                opacity = 1,
                group="legend")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)