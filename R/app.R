###########################################################################################
############################### GEODESIGN_CNG_SW_2018 #####################################
###########################################################################################
library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(sp)
library(dplyr)
library(ggvis)
library(ggplot2)
library(RColorBrewer)
library(mongolite) # connecting to mongo lab, a web-based mongodb
library(htmltools) # for point popups
library(httr)
library(rsconnect)
library(stringr)
library(shinyTree) # generating an expandable/collapsed checkboxes (nested checkboxes)
# packages used for simplifying spatial lines dataframes
library(spdplyr) # for manipulating the attribute data inside the spatial data frame
library(rmapshaper) # for manipulating the geometry (polygon, line, marker) part of the GeoJSON data
#### Notice there is a limit of 2,500 calls per day
## calculating shortest distance between any pair of origin and destination through using Google API
library(XML)
library(RCurl)
library(scales) # formatting percentage


load("geodesign_cng_sw_2018.RData")


# Connect to the database
## This part of information is hidden. A user can create a free mongo cloud db (mlab).
options(mongodb = list(
  "host" = "...",
  "username" = "...",
  "password" = "..."
))
databaseName <- "geodesign_cng_sw_2018"

# Global variables that can be put on the x and y axes
xaxis_vars <- c(
  "GROUP and STAGE" = "GROUP_STAGE",
  "AVERAGE AADTT" = "AVERAGE_AADTT",
  "AVERAGE FLEET" = "AVERAGE_FLEET",
  "PATH COVERED" = "PATH_COVERED",
  "KTONS COVERED" = "KTONS_COVERED",
  "TMILES COVERED" = "TMILES_COVERED"
)

yaxis_vars <- c(
  "AVERAGE AADTT" = "AVERAGE_AADTT",
  "AVERAGE FLEET" = "AVERAGE_FLEET",
  "PATH COVERED" = "PATH_COVERED",
  "KTONS COVERED" = "KTONS_COVERED",
  "TMILES COVERED" = "TMILES_COVERED"
)

header <-  dashboardHeader(
  title = tags$div(style="font-size:0",p(span("COL",style="color:white;font-size:20px"),span("LAB",style="color:red;font-size:20px"),span("LOCATION: alt fuel stations",style="color:white;font-size:20px"))),
  titleWidth = 400
)

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  fluidRow(
    tags$head(
      tags$style(
        HTML(
          ".form-group{
          margin-top: 0px;
          margin-bottom: 0px;
          }
          .selectize-control{
          margin-top: 0px;
          margin-bottom: 0px;
          }
          .checkbox{
          margin-top: 0px;
          margin-bottom: 0px;
          padding:0px 0px 0px 0px;
          }"
        )
        )
        ),
    
    tabsetPanel(
      id = "Geodesign", type = "tabs",
      
      ########### Main Tab ###########      
      tabPanel(
        title = strong("Main"), id = "mainTab", value = "mainTab",
        
        column(width=10,
               leafletOutput("map",height = 900)
        ),
        
        column(width=2, height = 1200, style = "padding:0px 5px 0px 5px;",
               wellPanel(
                 div(style="padding:0px 0px 5px 0px;", title="Start with workshop name if applicable, e.g. TRB2018_Group A.",
                     textInput("group",label="Group Name (20 characters max)",value="", placeholder = "Enter your group name...")),
                 div(style="padding:0px 0px 5px 0px;",numericInput("stage",label="Stage",value=NULL)),
                 div(style="padding:0px 0px 5px 0px;",numericInput("iteration",label="Iteration",value=NULL)),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               ),
               wellPanel(
                 checkboxInput("CNGs","Existing CNG Stations (green circles)",FALSE),
                 checkboxInput("Truck","Truck Stops (red circles)",FALSE),
                 checkboxInput("AADT_NHS1","AADTT-Interstate",FALSE),
                 checkboxInput("AADT_NHS3","AADTT-Non-Interstate Strategic Highway Network",FALSE),
                 checkboxInput("AADT_NHS7","AADTT-Other National Highway System",FALSE),
                 checkboxInput("AADT_NHS0","AADTT-Not on National Highway System",FALSE),
                 checkboxInput("ZIP_Fleet","Truck Fleet Data (ZIP-based)"),
                 checkboxInput("Pipelines","Natural Gas Pipelines"),
                 checkboxInput("Centroids","Metro O-D Centroids"),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               ),
               wellPanel(
                 bootstrapPage(
                   div(p(style="padding:0px 0px 0px 0px;margin-bottom:5px",strong("Distance Calculation"))),
                   div(style="display:inline-block",textInput(inputId="from", label=NULL, value = "", width = 90, placeholder="Enter ID")),
                   div(style="display:inline-block","To"),
                   div(style="display:inline-block; padding:0px 15px 0px 0px",textInput(inputId="to", label=NULL, value = "", width = 90, placeholder="Enter ID"))
                 ),
                 bootstrapPage(
                   div(style="display:inline-block; padding:5px 5px 5px 0px",title="Distance calculation.",actionButton("distCal", "Calculate")),
                   div(style="display:inline-block; padding:5px 0px 5px 0px",title="Remove labels.",actionButton("removeLabels","Remove Labels")) #
                 ),
                 verbatimTextOutput(outputId = "distance_result"),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               ),
               wellPanel(
                 div(p(style="padding:0px 0px 0px 0px;margin-bottom:5px",strong("Driving Range"))),
                 div(style = "display:inline-block; padding:0px 0px 5px 0px",textInput("driving_range",label=NULL,value="",placeholder = "Enter driving range...")),
                 div(style="display:inline-block; padding:0px 0px 5px 0px","miles"),
                 div(p(style="padding:3px 0px 0px 0px;margin-bottom:0px",strong("Show Coverage Gaps"))),
                 checkboxInput("gaps_top_quartile","Top Quartile (tons)",FALSE),
                 checkboxInput("gaps_2nd_quartile","2nd Quartile",FALSE),
                 checkboxInput("gaps_3rd_quartile","3rd Quartile",FALSE),
                 checkboxInput("gaps_4th_quartile","4th Quartile",FALSE),
                 div(style = "padding:5px 0px 5px 0px",selectInput("od_pairs", label = "Origin-Destination Path", c(Choose='', OD_paths$PAIR), selectize=FALSE)),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               ),
               wellPanel(
                 # adding a submit button "Evaluate" to get results and generate a table (inlcuding loading and saving data to a romote database)
                 uiOutput("previousgroup_list"),
                 tags$div(style = "padding:0px 0px 5px 0px",title="Adding previously selected truck stops to the map.",
                          actionButton("add_previous", "Add Previous Selections")),
                 tags$div(style = "padding:0px 0px 5px 0px",title="Removing previously selected truck stops to the map.",
                          actionButton("remove_previous", "Remove Previous Selections")),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               ),
               wellPanel(
                 tags$div(style = "padding:5px 0px 5px 0px",title="Performance evaluation for selected truck stops.",
                          actionButton("eval", "Evaluate and Save")),
                 tags$div(style = "padding:0px 0px 5px 0px",title="Refresh the platform",
                          actionButton("refresh", "Refresh")),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               )
        )
      ),
      
      tabPanel(
        title = strong("Selected New CNG Stations"), id = "formTab_selections", value = "formTab_selections",
        br(),
        div(style = 'overflow-x: scroll', DT::dataTableOutput("selectionsTable")),
        downloadButton("downloadGroupSelectionData","Download the table")
      ),
      
      tabPanel(
        title = strong("Group Performance Measures by Stage"), id = "formTab_group", value = "formTab_group",
        br(),
        div(style = 'overflow-x: scroll', DT::dataTableOutput("measuresTable")),
        downloadButton("downloadGroupMeasureData","Download the table")
      ),
      
      tabPanel(
        title = strong("Comparing to Other Groups"), id = "formTab_allgroup", value = "formTab_allgroup",
        column(width = 10, 
               wellPanel(
                 div(style = 'overflow-x: scroll', DT::dataTableOutput("allmeasuresTable")),
                 bootstrapPage(
                   div(style="display:inline-block",title="Update the table to include recent inputs from other groups.",actionButton("update_table", "Update the table")), #
                   div(style="display:inline-block",title="Download the table as a .csv file.",downloadButton("downloadAllGroupMeasureData","Download the table")) #
                 )
               ),
               wellPanel(
                 plotOutput("groupcomparison_plot",height = 600),
                 tags$div(title="Download the plot as a .png file.",
                          downloadButton("downloadPlot","Download the plot"))
               )
        ),
        column(width = 2, style = "padding:0px 5px 0px 5px",
               wellPanel(
                 div(p(style="padding:0px 0px 0px 5px;margin-bottom:0px",strong("Select group(s) and stage(s)"))),
                 tags$div(style="font-size:10pt; padding:0px 0px 0px 0px; margin-bottom:0px; margin-top:0px",
                          class=".jstree-node",
                          shinyTree("groupstage_plot", checkbox = TRUE, theme = "proton")),
                 style = "padding:0px 0px 20px 0px; margin-bottom: 0px; height:800px; overflow-y:scroll"
               ),
               wellPanel(
                 tags$div(style = "padding:5px 0px 5px 0px", title="Update the list of groups and stages.",
                          actionButton("update_plot", "Update the list")),
                 style = "padding:0px 0px 0px 5px; margin-bottom: 20px"
               ),
               wellPanel(
                 selectInput("xvar_plot", label = "X-axis variable", xaxis_vars, selected = NULL), # choices = c("GROUP_STAGE","AVERAGE AADTT","AVERAGE FLEET")
                 selectInput("yvar_plot", label = "Y-axis variable", yaxis_vars, selected = NULL),
                 tags$small(paste0(
                   "Note: A bar chart will be created to ",
                   "compare one performance measure between ",
                   "selected groups when GROUP_STAGE is ",
                   "selected as X-axis variable. Otherwise, ",
                   "a scatter plot will be generated to compare ",
                   "two performance measures between groups."
                 )),
                 style = "padding:0px 5px 0px 5px"
               )
        )
      ),
      
      tabPanel(
        title = strong("Spatial Comparison"), id = "formTab_allgroupselections", value = "formTab_allgroupselections",
        column(width=10,
               leafletOutput("map_compare",height=900)),
        column(width=2, style = "padding:0px 5px 0px 5px",
               wellPanel(
                 checkboxInput("CNGs_compare","Existing CNG Stations (green circles)",FALSE),
                 checkboxInput("Truck_compare","Truck Stops (red circles)",FALSE),
                 checkboxInput("AADT_NHS1_compare","AADTT-Interstate",FALSE),
                 checkboxInput("AADT_NHS3_compare","AADTT-Non-Interstate Strategic Highway Network",FALSE),
                 checkboxInput("AADT_NHS7_compare","AADTT-Other National Highway System",FALSE),
                 checkboxInput("AADT_NHS0_compare","AADTT-Not on National Highway System",FALSE),
                 checkboxInput("ZIP_Fleet_compare","Truck Fleet Data (ZIP-based)"),
                 checkboxInput("Pipelines_compare","Natural Gas Pipelines"),
                 checkboxInput("Centroids_compare","Metro O-D Centroids"),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               ),
               wellPanel(
                 div(p(style="padding:0px 0px 0px 0px;margin-bottom:0px",strong("Select group(s) and stage(s)"))),
                 tags$div(style="font-size:10pt; padding:0px 0px 0px 0px; margin-bottom:0px; margin-top:0px",
                          class=".jstree-node",
                          shinyTree("groupstage_spatial", checkbox = TRUE, theme = "proton")),
                 style = "padding:0px 0px 20px 0px; margin-bottom: 0px; height:800px; overflow-y:scroll"
               ),
               wellPanel(
                 tags$div(style = "padding:5px 0px 15px 0px",title="Update the list of groups and stages.",
                          actionButton("update_spatial", "Update the list")),
                 bootstrapPage(
                   div(style="display:inline-block; padding:0px 5px 15px 0px",title="Visualize selected groups/stages.",actionButton("vis_spatial", "Visualize points")), #
                   div(style="display:inline-block; padding:0px 5px 15px 0px",title="Remove Selected Points on the map.",actionButton("removeSelectedGroups_spatial","Remove points")), #
                   div(style="display:inline-block; padding:0px 5px 15px 0px",title="Download selected data as a .csv file.",downloadButton("downloadSelectedData_spatial", "Download selected data")) #
                 ),
                 style = "padding:0px 5px 0px 5px; margin-bottom: 10px;"
               )
        )     
      ),
      
      tabPanel(
        title = strong("About"), id = "about", value = "about",
        br(),
        tags$iframe(style="height:900px; width:100%",src="About.pdf")
      ),
      
      tabPanel(
        title = strong("Help & Data"), id = "help", value = "help",
        br(),
        tags$iframe(style="height:900px; width:100%",src="Help.pdf")
      )
    )
        )
      )

sidebar <- dashboardSidebar(disable=TRUE)

ui <- dashboardPage(title = "COLLABLOCATION: Geodesign Platform",
                    header,
                    sidebar,
                    body
)

server <- function(input, output, session){
  
  selection <- reactiveValues()
  selection$df <- data.frame()
  
  #### Main Tab ####
  output$map <- renderLeaflet({
    leaflet() %>% 
      # Add base maps
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Positron", group = "Black & White") %>%
      addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png',
               attribution = paste('&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors','&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'),group="Labels Only") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Awesome Dark") %>%
      addGeoJSON(I10,color="cyan",weight=3,opacity=1,fill=FALSE,group="I-10 (CA-TX)",options = pathOptions(clickable=FALSE)) %>%
      addGeoJSON(ALLNHS_1,color="#FEF9E7",weight=1,opacity=0.3,fill=FALSE,group="Annual Average Daily Traffic for Trucks",options = pathOptions(clickable=FALSE)) %>%
      addGeoJSON(ALLNHS_2,color="#FCF3CF",weight=2,opacity=0.5,fill=FALSE,group="Annual Average Daily Traffic for Trucks",options = pathOptions(clickable=FALSE)) %>%
      addGeoJSON(ALLNHS_3,color="#F9E79F",weight=3,opacity=0.8,fill=FALSE,group="Annual Average Daily Traffic for Trucks",options = pathOptions(clickable=FALSE)) %>%
      addGeoJSON(ALLNHS_4,color="#F7DC6F",weight=4,opacity=1,fill=FALSE,group="Annual Average Daily Traffic for Trucks",options = pathOptions(clickable=FALSE)) %>%
      addGeoJSON(ALLNHS_5,color="#F4D03F",weight=5,opacity=2,fill=FALSE,group="Annual Average Daily Traffic for Trucks",options = pathOptions(clickable=FALSE)) %>%
      hideGroup("Annual Average Daily Traffic for Trucks") %>%
      hideGroup("I-10 (CA-TX)") %>%
      
      # adding a layer control to swith between different reference maps
      addLayersControl(baseGroups = c("OpenStreetMap","Black & White","Labels Only","Awesome Dark"),
                       overlayGroups = c("I-10 (CA-TX)","Annual Average Daily Traffic for Trucks"),
                       options = layersControlOptions(collapsed = TRUE,autoZIndex=TRUE)) %>%
      # adding a scalebar
      addScaleBar(position="bottomright",options=scaleBarOptions(maxWidth=100,metric=FALSE,imperial=TRUE,updateWhenIdle=TRUE)) %>%
      setView(-112.058487, 33.462173, 6) # addProviderTiles("Stamen.TonerLite")
  })
  
  # function of adding points on a reference map
  add <- reactiveValues()
  
  observeEvent(add$lat, {
    leafletProxy("map") %>% addMarkers(lat = add$lat, lng = add$lng,layerId=paste("New CNG station with Lat: ",add$lat," and Lon: ",add$lng,sep="")) # layerId has to be a vector and each marker has a unique layerId
  })
  
  observeEvent(input$map_click, {
    coords <- input$map_click
    if ( (!is.null(input$add) && (!is.null(coords))) ) {
      if (input$add >= 1) {
        add[["lat"]] <- coords$lat
        add[["lng"]] <- coords$lng        
      }
      else {
        print("Please click 'Add' button to draw points!")
      }
    }
  })
  
  # displaying additional data layers
  observe({
    if (input$CNGs) {
      leafletProxy("map",data=CNG_Stations) %>% 
        addCircles(lng=~Longitude,lat=~Latitude,color="#33FD28",opacity=10,radius=100,fill=TRUE,fillColor="#33FD28",fillOpacity=10,group="CNG Stations",
                   layerId=~ID_2,label=~paste("ID: ",ID_2,"; ",Groups_Wit,"; ",NG_Vehicle),labelOptions=labelOptions(direction="top",offset=c(0,-45)))}
    else {
      leafletProxy("map",data=CNG_Stations) %>% clearGroup("CNG Stations")
    }
  })
  
  observe({
    if (input$Truck) {
      leafletProxy("map",data=Truck_Stops) %>% 
        addCircles(lng=~LONGITUDE,lat=~LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                   layerId=~ID_2,label=~paste0("ID: ",ID_2),labelOptions=labelOptions(direction="top",
                                                                                      offset=c(0,-45)))}
    else {
      leafletProxy("map",data=Truck_Stops) %>% clearGroup("Truck Stops") 
      selection$df <- data.frame(id=factor(),group=factor(),lat=numeric(),lon=numeric())
    }
  })
  
  observe({
    if (input$Centroids) {
      leafletProxy("map",data=Metro_centroids) %>% 
        addCircleMarkers(lng=~NEAR_X,lat=~NEAR_Y,color="#FF52F6",opacity=8,radius=3,fill=TRUE,fillColor="#490C46",fillOpacity=8,group="Metro Centroids",
                         options=pathOptions(clickable=FALSE))}
    else {
      leafletProxy("map",data=Metro_centroids) %>% clearGroup("Metro Centroids") 
    }
  })
  
  # six AADT layers share the same color palette
  colorpal <- colorFactor(c("#7FB3D5", "#5499C7", "#2471A3", "#1A5276", "#154360"),
                          Annual_Average_Daily_Traffic_NHS1$Jenks)
  
  # nested if/else statement for each layer is used to deal with the issue of one single legend shared by several AADT layers
  observe({
    if (input$AADT_NHS1) {
      if (!input$AADT_NHS3&!input$AADT_NHS7&!input$AADT_NHS0){
        leafletProxy("map") %>% 
          addGeoJSON(NHS1_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          addGeoJSON(NHS1_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS1",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS3&!input$AADT_NHS7&!input$AADT_NHS0){
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS1") %>% 
          removeControl(layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS1")
      }
    }
  })
  
  observe({
    if (input$AADT_NHS3) {
      if (!input$AADT_NHS1&!input$AADT_NHS7&!input$AADT_NHS0){
        leafletProxy("map") %>% 
          addGeoJSON(NHS3_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          addGeoJSON(NHS3_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS3",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS1&!input$AADT_NHS7&!input$AADT_NHS0){
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS3") %>% 
          removeControl(layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS3")
      }
    }
  })
  
  observe({
    if (input$AADT_NHS7) {
      if (!input$AADT_NHS1&!input$AADT_NHS3&!input$AADT_NHS0){
        leafletProxy("map") %>% 
          addGeoJSON(NHS7_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          addGeoJSON(NHS7_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS7",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS1&!input$AADT_NHS3&!input$AADT_NHS0){
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS7") %>% 
          removeControl(layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS7")
      }
    }
  })
  
  observe({
    if (input$AADT_NHS0) {
      if (!input$AADT_NHS1&!input$AADT_NHS3&!input$AADT_NHS7){
        leafletProxy("map") %>% 
          addGeoJSON(NHS0_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          addGeoJSON(NHS0_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS0",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS1&!input$AADT_NHS3&!input$AADT_NHS7){
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS0") %>% 
          removeControl(layerId="AADT_Legend")
      }
      else {
        leafletProxy("map") %>% 
          clearGroup("Traffic Flow_NHS0")
      }
    }
  })
  
  # adding fleet data shapefiles 
  # color palette for fleet data layer
  colorpal_fleet <- colorFactor(c("#E6B0AA","#D98880", "#CD6155", "#C0392B", "#A93226", "#922B21"),ZIP_Fleet$Jenks)
  
  observe({
    if (input$ZIP_Fleet) {
      leafletProxy("map") %>% 
        addPolygons(data=ZIP_Fleet,weight=0.8,color="#BDC3C7",opacity=0.5,fillColor = ~colorpal_fleet(Jenks),fillOpacity = 0.5,
                    label=~str_c("Fleets: ",FLEET,", ",STATE," ",ZIP_CODE),
                    options = pathOptions(clickable=FALSE),
                    labelOptions=labelOptions('auto'),
                    highlightOptions=highlightOptions(color="cyan",opacity=1,weight=2,fillOpacity=1,bringToFront=TRUE,sendToBack=TRUE),
                    group="Fleet") %>%
        addLegend("bottomleft",pal=colorpal_fleet,value=ZIP_Fleet$Jenks,opacity=3,title="Truck Fleet Data (ZIP-based)",layerId="Fleet_Legend")
    }
    else {
      leafletProxy("map") %>% 
        clearGroup("Fleet") %>% 
        removeControl(layerId="Fleet_Legend")
    }
  })
  
  observe({
    if (input$Pipelines) {
      leafletProxy("map") %>% 
        addGeoJSON(Pipeline,weight=1,color="purple",fill=FALSE,opacity=1,
                   options = pathOptions(clickable=FALSE),
                   group="NG_Pipelines")
    }
    else {
      leafletProxy("map") %>% 
        clearGroup("NG_Pipelines") 
    }
  })
  
  # Output the data
  ## REMEMBER: An observer is like a reactive expression in that it can read reactive values and call reactive expressions, 
  ## and will automatically re-execute when those dependencies change. But unlike reactive expressions, 
  ## it doesn't yield a result and can't be used as an input to other reactive expressions. 
  ## Thus, observers are only useful for their side effects (for example, performing I/O).
  ## Reference link: https://shiny.rstudio.com/reference/shiny/latest/observe.html
  ## That said, data generated within an observer cannot be used for another reactive expression
  ## so reactive should be used.
  
  measure <- reactiveValues()
  measure$df <- data.frame()
  
  remove <- reactiveValues()
  
  observe({
    if (nchar(paste0("",input$group)) >20 ) {
      alert("Group name longer than 20 characters. Please rename it.")
    }
  })
  
  observeEvent(input$map_shape_click, {
    records <- input$map_shape_click
    if(as.numeric(unlist(records$id))>360) {
      if(input$group!="" & !is.na(input$stage) & !is.na(input$iteration)) {
        records_v2 <- as.data.frame(t(records))
        
        records_v2 <- records_v2 %>% select(id,group,lat,lng)
        # Insert the data into the mongo collection as a data.frame
        records_v2$id <- as.factor(unlist(records_v2$id))
        records_v2$group <- as.factor(unlist(records_v2$group))
        records_v2$lat <- as.numeric(unlist(records_v2$lat))
        records_v2$lng <- as.numeric(unlist(records_v2$lng))
        records_v2 <- as.data.frame(records_v2)
        
        leafletProxy("map") %>% #layerId=as.character(unlist(records_v2$id,
          addMarkers(lng=records$lng,lat=records$lat,group="Truck Stops",icon=stationIcon,layerId=records$id,label=paste0("ID: ",records$id),labelOptions=labelOptions(direction="top",
                                                                                                                                                                       offset=c(0,-45))) %>%
          removeShape(layerId=records$id)
        if(nrow(selection$df)>0) {
          selection$df <- as.data.frame(selection$df)
          selection$df <- rbind(selection$df,records_v2)
          
          records_v2 <- records_v2 %>% mutate(GROUP_STAGE=paste("Temp_",input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
          db_selections_tempgroups <- mongo(collection = "tempgroups_selections",
                                            url = sprintf(
                                              "mongodb://%s:%s@%s/%s",
                                              options()$mongodb$username,
                                              options()$mongodb$password,
                                              options()$mongodb$host,
                                              databaseName))
          records_v2$GROUP_STAGE <- as.factor(records_v2$GROUP_STAGE)
          db_selections_tempgroups$insert(records_v2)
        } 
        else {
          selection$df <- records_v2
          selection$df <- as.data.frame(selection$df)
          
          records_v2 <- records_v2 %>% mutate(GROUP_STAGE=paste("Temp_",input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
          db_selections_tempgroups <- mongo(collection = "tempgroups_selections",
                                            url = sprintf(
                                              "mongodb://%s:%s@%s/%s",
                                              options()$mongodb$username,
                                              options()$mongodb$password,
                                              options()$mongodb$host,
                                              databaseName))
          # Insert the data into the mongo collection as a data.frame
          records_v2$GROUP_STAGE <- as.factor(records_v2$GROUP_STAGE)
          db_selections_tempgroups$insert(records_v2)
        }
      } else {
        alert("Pleaes enter group name, stage number and iteration number.")
      }
    } else {
      alert("Please select an existing truck stop as a potential CNG station!")
    }
  })
  
  # function of removing selected points on a reference map
  observeEvent(input$map_marker_click, {
    if(input$group!="" & !is.na(input$stage) & !is.na(input$iteration)) {
      records <- input$map_marker_click
      records_v2 <- as.data.frame(t(records))
      records_v2 <- records_v2 %>% select(id,group,lat,lng)
      # Insert the data into the mongo collection as a data.frame
      records_v2$id <- as.factor(unlist(records_v2$id))
      records_v2$group <- as.factor(unlist(records_v2$group))
      records_v2$lat <- as.numeric(unlist(records_v2$lat))
      records_v2$lng <- as.numeric(unlist(records_v2$lng))
      records_v2 <- as.data.frame(records_v2)

      leafletProxy("map") %>% 
        removeMarker(layerId=records$id) %>% #paste("New CNG station with Lat: ",records$lat," and Lon: ",records$lng,sep="")
        addCircles(lng=records$lng,lat=records$lat,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",layerId=records$id,label=paste0("ID: ",records$id),labelOptions=labelOptions(direction="top",
                                                                                                                                                                                                                             offset=c(0,-45)))
      selection$df <- as.data.frame(selection$df)
      selection$df <- rbind(selection$df,records_v2) # allow to add duplicate records in the dataframe and then identify them later and delete
      
      records_v2 <- records_v2 %>% mutate(GROUP_STAGE=paste("Temp_",input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
      db_selections_tempgroups <- mongo(collection = "tempgroups_selections",
                                        url = sprintf(
                                          "mongodb://%s:%s@%s/%s",
                                          options()$mongodb$username,
                                          options()$mongodb$password,
                                          options()$mongodb$host,
                                          databaseName))
      records_v2$GROUP_STAGE <- as.factor(records_v2$GROUP_STAGE)
      db_selections_tempgroups$insert(records_v2)
    } else {
      alert("Pleaes enter group name, stage number and iteration number.")
    }
  })
  
  # update selection$df 
  selections <- reactive({
    if(nrow(selection$df)>0) {
      sessiondata <- selection$df
      
      colnames(sessiondata) <- c("ID","GROUP","LATITUDE","LONGITUDE") # "NONCE",
      sessiondata$ID <- as.factor(unlist(sessiondata$ID))
      sessiondata$GROUP <- as.factor(unlist(sessiondata$GROUP))
      sessiondata$LATITUDE <- as.numeric(unlist(sessiondata$LATITUDE))
      sessiondata$LONGITUDE <- as.numeric(unlist(sessiondata$LONGITUDE))
      sessiondata <- sessiondata %>% group_by(ID,GROUP) %>%
        summarise(AVE_LATITUDE=mean(LATITUDE),AVE_LONGITUDE=mean(LONGITUDE),COUNT=n()) # AVE_NONCE=mean(NONCE),
      sessiondata <- sessiondata %>% filter(COUNT %% 2 == 1) # eliminating points that want to be removed 
      sessiondata <- sessiondata %>% select(ID,GROUP,AVE_LATITUDE,AVE_LONGITUDE) # AVE_NONCE,
      colnames(sessiondata) <- c("ID_2","GROUP","LATITUDE","LONGITUDE") # remember: ID_2 is used "NONCE",
      sessiondata
    } else {
      sessiondata <- data.frame(ID_2=factor(),GROUP=character(),LATITUDE=numeric(),LONGITUDE=numeric())
      sessiondata
    }
  })
  
  group_selections <- reactive({
    return(paste(input$group,"_Selections",sep=""))
  })
  
  group_measures <- reactive({
    return(paste(input$group,"_Measures",sep=""))
  })
  
  selections2 <- reactive({
    sessiondata <- merge(selections(),Truck_Stops,all.x=TRUE) ##REMEMBER: ID_2 IS USED FOR MERGING
    sessiondata <- sessiondata %>% select(ID_2,NAME,ADDRESS,GROUP,AADTT,FLEET,LATITUDE,LONGITUDE)
    colnames(sessiondata) <- c("ID","NAME","ADDRESS","GROUP","AADTT","FLEET","LATITUDE","LONGITUDE") # remember: change back to ID 
    sessiondata <- sessiondata %>% mutate(GROUP_STAGE=paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
    sessiondata
  })
  
  measures <- reactive({
    sessiondata <- selections2()
    sessiondata <- sessiondata %>% group_by(GROUP_STAGE) %>% summarise(Stations=n(),TOTAL_AADTT=sum(AADTT),AVG_AADTT=as.integer(round(mean(AADTT),digits = 0)),TOTAL_FLEET=sum(FLEET),AVG_FLEET=as.integer(round(mean(FLEET),digits = 0))) # colSums(df)
    colnames(sessiondata) <- c("GROUP_STAGE","NUMBER_OF_STATIONS","TOTAL_AADTT","AVERAGE_AADTT","TOTAL_FLEET","AVERAGE_FLEET")
    sessiondata
  })
  
  # predefine from_point2 and to_point2 dataframes that will be used globally
  # remember, reactive values are similar to lists
  from_point2 <- reactiveValues()
  from_point2$df <- data.frame()
  
  to_point2 <- reactiveValues()
  to_point2$df <- data.frame()
  
  # clicking calculate button
  observeEvent(input$distCal, {
    if(input$from!="" & input$to!="") {
      from_point <- as.data.frame(as.factor(unlist(input$from)))
      to_point <- as.data.frame(as.factor(unlist(input$to)))
      colnames(from_point) <- c("ID_2")
      colnames(to_point) <- c("ID_2")
      from_point1 <- merge(from_point,locations,all.x=TRUE)
      to_point1 <- merge(to_point,locations,all.x=TRUE)

      xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',paste0(from_point1$LATITUDE,",",from_point1$LONGITUDE),'&destinations=',paste0(to_point1$LATITUDE,",",to_point1$LONGITUDE),'&mode=driving&sensor=false&traffic_mode=best_guess&departure_time=now') # &departure_time=now
      xmlfile <- xmlParse(getURL(xml.url))
      dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
      distance <- as.numeric(sub(" m","",dist))
      # FROM METER TO MILE
      mi=distance*0.000621371
      mi=round(mi,digits = 2) #rounding the number
      dura <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[1]])$value)
      duration <- as.numeric(sub(" s","",dura))
      # FROM METER TO MILE
      time=duration/3600
      hour=trunc(time)
      minute=round((duration %% 3600)/60,digits = 0)
      
      output$distance_result <- renderText({
        paste0(mi," miles")
      })
      
      updateTextInput(session,inputId="from", label=NULL, value = "")
      updateTextInput(session,inputId="to", label=NULL, value = "")
      
      if(as.numeric(as.character(from_point$ID_2)) < 361) {
        from_point2$df <- merge(from_point,CNG_Stations,all.x=TRUE)
        
        leafletProxy("map",data=from_point2$df) %>% 
          addCircleMarkers(lng=~Longitude,lat=~Latitude,color="cyan",radius=5,group="Selected Points",
                           label=~paste("ID: ",ID_2,"; ",Groups_Wit,"; ",NG_Vehicle),labelOptions=labelOptions(noHide = T,direction="top",offset=c(0,-45), style=list(
                             'border' = '1px solid #666266','padding'='0px 0px 0px 5px','text-aligh'='center'
                           )),options=pathOptions(clickable=F))
      } else {
        from_point2$df <- merge(from_point,Truck_Stops,all.x=TRUE)
        leafletProxy("map",data=from_point2$df) %>% 
          addCircleMarkers(lng=~LONGITUDE,lat=~LATITUDE,color="cyan",radius=5,fill=TRUE,fillColor="cyan",fillOpacity=10,group="Selected Points",
                           label=~paste0("ID: ",ID_2),labelOptions=labelOptions(noHide = T,direction="top",offset=c(0,-45), style=list(
                             'border' = '1px solid #666266','padding'='0px 0px 0px 5px','text-aligh'='center'
                           )),options=pathOptions(clickable=F))
      }
      
      if(as.numeric(as.character(to_point$ID_2)) < 361) {
        to_point2$df <- merge(to_point,CNG_Stations,"ID_2",all.x=TRUE)
        leafletProxy("map",data=to_point2$df) %>% 
          addCircleMarkers(lng=~Longitude,lat=~Latitude,color="cyan",radius=5,group="Selected Points",
                           label=~paste0("ID: ",ID_2,"; ",Groups_Wit,"; ",NG_Vehicle),labelOptions=labelOptions(noHide = T,direction="top",offset=c(0,-45), style=list(
                             'border' = '1px solid #666266','padding'='0px 0px 0px 5px','text-aligh'='center'
                           )),options=pathOptions(clickable=F))
      } else {
        to_point2$df <- merge(to_point,Truck_Stops,"ID_2",all.x=TRUE)
        leafletProxy("map",data=to_point2$df) %>% 
          addCircleMarkers(lng=~LONGITUDE,lat=~LATITUDE,color="cyan",radius=5,fill=TRUE,fillColor="cyan",fillOpacity=10,group="Selected Points",
                           label=~paste0("ID: ",ID_2),labelOptions=labelOptions(noHide = T,direction="top",offset=c(0,-45), style=list(
                             'border' = '1px solid #666266','padding'='0px 0px 0px 5px','text-aligh'='center'
                           )),options=pathOptions(clickable=F))
      }
      
    } else {
      alert("Please enter IDs of start location and destination!")
    }
  })
  
  # clicking remove labels button
  observeEvent(input$removeLabels, {
    if(!is.null(from_point2$df$ID_2) && (!is.null(to_point2$df$ID_2))) {
      leafletProxy("map") %>%
        clearGroup(group="Selected Points")
    } else {
      alert("Please calculate distance first!")
    }
  })
  
  # clicking evaluate button
  df <- observeEvent(input$eval, {
    if(nrow(selections2())>0) {
      if((input$group!="") & !is.na(input$stage) & !is.na(input$iteration)) {
        if(!(selections2()$GROUP_STAGE %in% downloadAllGroupMeasures()$GROUP_STAGE)) {
          # Connect to the database
          db_selections <- mongo(collection = group_selections(),
                                 url = sprintf(
                                   "mongodb://%s:%s@%s/%s",
                                   options()$mongodb$username,
                                   options()$mongodb$password,
                                   options()$mongodb$host,
                                   databaseName))
          db_measures <- mongo(collection = group_measures(),
                               url = sprintf(
                                 "mongodb://%s:%s@%s/%s",
                                 options()$mongodb$username,
                                 options()$mongodb$password,
                                 options()$mongodb$host,
                                 databaseName))
          db_measures_allgroups <- mongo(collection = "allgroups_measures",
                                         url = sprintf(
                                           "mongodb://%s:%s@%s/%s",
                                           options()$mongodb$username,
                                           options()$mongodb$password,
                                           options()$mongodb$host,
                                           databaseName))
          db_measures_allgroups_plot <- mongo(collection = "allgroups_measures_plot",
                                              url = sprintf(
                                                "mongodb://%s:%s@%s/%s",
                                                options()$mongodb$username,
                                                options()$mongodb$password,
                                                options()$mongodb$host,
                                                databaseName))
          db_selections_allgroups <- mongo(collection = "allgroups_selections",
                                           url = sprintf(
                                             "mongodb://%s:%s@%s/%s",
                                             options()$mongodb$username,
                                             options()$mongodb$password,
                                             options()$mongodb$host,
                                             databaseName))
          # a table (details of uncovered segments) in DB is used to read uncovered segments and show/hide the layer
          # Cannot be just stored in memory because of show/hide feature and observe event of evaluation function
          db_uncovered_segments_allgroups <- mongo(collection = "allgroups_uncovered_segments",
                                                   url = sprintf(
                                                     "mongodb://%s:%s@%s/%s",
                                                     options()$mongodb$username,
                                                     options()$mongodb$password,
                                                     options()$mongodb$host,
                                                     databaseName))
          # a table (general info) in DB is used to store summarized info of OD paths including uncovered segments
          db_uncovered_odpairs_allgroups <- mongo(collection = "allgroups_uncovered_odpairs",
                                                  url = sprintf(
                                                    "mongodb://%s:%s@%s/%s",
                                                    options()$mongodb$username,
                                                    options()$mongodb$password,
                                                    options()$mongodb$host,
                                                    databaseName))
          
          # evaluate the driving range
          if(input$driving_range!=""){
            # Insert the data into the mongo collection as a data.frame
            data_selection <- selections2()
            data_measure <- measures()
            
            range <- as.data.frame(as.factor(unlist(input$driving_range)))
            colnames(range) <- c("Distance")
            range$Distance <- as.numeric(as.character(range$Distance))
            od_table_range <- data.frame(lat_ori=numeric(),lon_ori=numeric(),lat_des=numeric(),lon_des=numeric(),sum_len=numeric(),od_path=numeric())
            
            for(path in 1:323) { 
              od_table_temp <- od_table_coverage_segments %>% filter(OD_PATH==path_number[path])
              od_table_temp_selected <- merge(od_table_temp,data_selection,by.x="ID_2",by.y="ID")
              od_table_temp_stations <- od_table_temp %>% filter(SELECTION==1)
              od_table_temp_stations <- merge(od_table_temp_stations,locations,"ID_2")
              od_table_temp_stations <- od_table_temp_stations %>% select(2:5,1,6:9,12,13)
              
              colnames(od_table_temp_stations)[10:11] <- c("LAT_END","LON_END")
              od_table_temp_selected <- od_table_temp_selected %>% select(2:5,1,6:9,17:18)
              colnames(od_table_temp_selected)[10:11] <- c("LAT_END","LON_END")
              if(nrow(od_table_temp_selected)>0) {
                od_table_temp_selected[,6] <- 1
              } else {od_table_temp_selected <- od_table_temp_selected}
              od_table_temp_others <- od_table_temp %>% filter(SELECTION==0)
              od_table_temp_others <- od_table_temp_others %>% filter(!(LABEL %in% od_table_temp_selected$LABEL))
              od_table_temp_dest <- od_table_temp %>% filter(ID_2=='0')
              od_table_temp_v2 <- rbind(od_table_temp_stations,od_table_temp_selected,od_table_temp_others,od_table_temp_dest)
              od_table_temp_v2 <- od_table_temp_v2 %>% arrange(LABEL)
              
              if(od_table_temp_v2[1,5]=='0') {
                od_table_temp_v2 <- od_table_temp_v2 %>% arrange(desc(LABEL))
                j <- 1
              } else {
                j <- 1
              }
              
              od_table_range_path <- data.frame(lat_ori=numeric(),lon_ori=numeric(),lat_des=numeric(),lon_des=numeric(),sum_len=numeric(),od_path=numeric())
              
              for(i in 1:nrow(od_table_temp_v2)) {
                if(od_table_temp_v2[i,6]==1) {
                  sum_len <- sum(od_table_temp_v2[j:i,4])
                  if(j==1) {
                    od_table_calc <- data.frame(lat_ori=od_table_temp_v2[j,8],lon_ori=od_table_temp_v2[j,9],lat_des=od_table_temp_v2[i,10],lon_des=od_table_temp_v2[i,11],sum_len,od_path=path_number[path])
                    od_table_range_path <- rbind(od_table_range_path,od_table_calc)
                    j <- i+1
                  } else {
                    od_table_calc <- data.frame(lat_ori=od_table_temp_v2[j-1,10],lon_ori=od_table_temp_v2[j-1,11],lat_des=od_table_temp_v2[i,10],lon_des=od_table_temp_v2[i,11],sum_len,od_path=path_number[path])
                    od_table_range_path <- rbind(od_table_range_path,od_table_calc)
                    j <- i+1
                  }
                }
              }
              
              od_table_range_path[1,5] <- od_table_range_path[1,5]*2
              od_table_range_path[nrow(od_table_range_path),5] <- od_table_range_path[nrow(od_table_range_path),5]*2
              
              od_table_range <- rbind(od_table_range,od_table_range_path)
            }
            
            od_table_range_over <- od_table_range %>% filter(sum_len > range$Distance)
            
            # create a new table for visualizing uncovered road segments
            # cannot correctly show uncovered segments so a for loop is used
            od_table_range_over <- od_table_range_over %>% 
              mutate(GROUP_STAGE=paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
            
            # visualize locations not covered
            # calculate number of paths covered and volume of freight covered
            od_table_range_over_general <- od_table_range_over %>% group_by(od_path) %>% summarise(count=n()) %>% mutate(uncovered=1)
            od_table_range_over_general_v2 <- merge(od_table_range_over_general,od_table_coverage_final,by.x="od_path",by.y="OD_PATH",all.x=TRUE)
            
            od_table_range_over_general_v2 <- od_table_range_over_general_v2 %>% 
              mutate(GROUP_STAGE=paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
            
            path_covered <- 323-nrow(od_table_range_over_general)
            ktons_covered <- sum(od_table_coverage_final$TOTAL_KTONS_2015)-sum(od_table_range_over_general_v2$TOTAL_KTONS_2015)
            tkm_covered <- sum(od_table_coverage_final$TOTAL_TON_MILE_2015)-sum(od_table_range_over_general_v2$TOTAL_TON_MILE_2015)
            ktons_covered_pt <- percent(ktons_covered / sum(od_table_coverage_final$TOTAL_KTONS_2015))
            tkm_covered_pt <- percent(tkm_covered / sum(od_table_coverage_final$TOTAL_TON_MILE_2015))
            
            path_covered_v2 <- paste0("Driving range: ",input$driving_range,"  ",path_covered," out of 323")
            ktons_covered_v2 <- paste0(ktons_covered," (",ktons_covered_pt,")")
            tkm_covered_v2 <- paste0(tkm_covered," (",tkm_covered_pt,")")
            
            # add driving range result to the table
            data_measure <- data_measure %>% mutate(paste0("Driving range: ",input$driving_range,"   ",path_covered," out of 323"),
                                                    paste0(ktons_covered," (",ktons_covered_pt,")"),
                                                    paste0(tkm_covered," (",tkm_covered_pt,")"))
            colnames(data_measure)[7:9] <- c("PATH_COVERED","KTONS_COVERED","TMILES_COVERED")
            
            # add driving range result to the plot
            data_measure_plot <- data_measure %>% select (1:6) %>% 
              mutate(path_covered,ktons_covered,tkm_covered)
            colnames(data_measure_plot)[7:9] <- c("PATH_COVERED","KTONS_COVERED","TMILES_COVERED")
            
            db_selections$insert(data_selection)
            db_measures$insert(data_measure)
            db_measures_allgroups$insert(data_measure)
            db_measures_allgroups_plot$insert(data_measure_plot)
            db_selections_allgroups$insert(data_selection)
            db_uncovered_segments_allgroups$insert(od_table_range_over)
            db_uncovered_odpairs_allgroups$insert(od_table_range_over_general_v2)
            
            alert("Selected truck stops are saved!
Please check 'Selected New CNG Stations','Group Performance Measures by Stage', 'Comparing to Other Groups', and 'Spatial Comparison' tabs for viewing results.")
          } else{
            alert("Please enter driving range.")
          }
        } else {
          alert("WARNING!
The combination of group name and stage is the same as a previous one. Selections cannot be saved. Please use another combination of group name and stage.")
        }
        } else {
          alert("Pleaes enter group name, stage number and iteration number.")
      }
    } else {
      alert("The input table is empty. Please select potential stations first.")
    }
    })
  
  downloadUncoveredSegments <- reactive({
    input$eval
    db_uncovered_segments_allgroups <- mongo(collection = "allgroups_uncovered_segments",
                                             url = sprintf(
                                               "mongodb://%s:%s@%s/%s",
                                               options()$mongodb$username,
                                               options()$mongodb$password,
                                               options()$mongodb$host,
                                               databaseName))
    data <- db_uncovered_segments_allgroups$find()
    data$lat_ori <- as.numeric(unlist(data$lat_ori))
    data$lon_ori <- as.numeric(unlist(data$lon_ori))
    data$lat_des <- as.numeric(unlist(data$lat_des))
    data$lon_des <- as.numeric(unlist(data$lon_des))
    data$sum_len <- as.numeric(unlist(data$sum_len))
    data$od_path <- as.numeric(unlist(data$od_path))
    data$GROUP_STAGE <- as.factor(unlist(data$GROUP_STAGE))
    data
  })
  
  # checkbox to show/hide coverage gaps (top quartile by tons)
  observe({
    if (input$gaps_top_quartile) {
      if((input$group!="") & !is.na(input$stage) & !is.na(input$iteration)) {
        group_existing <- downloadGroupMeasures()
        group_existing_temp <- group_existing %>%
          filter(as.factor(unlist(GROUP_STAGE))==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
        if(nrow(group_existing_temp) > 0) {
          data <- downloadUncoveredSegments()
          data_temp <- data %>% 
            filter(GROUP_STAGE==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep="") & 
                     od_path %in% od_table_coverage_final_top_quartile$OD_PATH)
          if (nrow(data_temp) > 0) {
            # visualize uncovered segments
            for(gap in 1:nrow(data_temp)) {
              leafletProxy("map") %>% 
                addPolylines(data=data_temp,lng=as.numeric(data_temp[gap,c(2,4)]),lat=as.numeric(data_temp[gap,c(1,3)]),group="Uncovered segments_top quartile",
                             color="cyan",weight=2,opacity=1,fill=FALSE,options = pathOptions(clickable=FALSE))
            }
            # visualize uncovered points
            locations_uncovered_ori <- data_temp %>% select(lat_ori,lon_ori)
            colnames(locations_uncovered_ori) <- c("lat","lon")
            locations_uncovered_des <- data_temp %>% select(lat_des,lon_des)
            colnames(locations_uncovered_des) <- c("lat","lon")
            locations_uncovered <- rbind(locations_uncovered_ori,locations_uncovered_des)
            locations_uncovered_2 <- locations_uncovered %>% group_by(lat,lon) %>% summarise(count=n())
            leafletProxy("map") %>% 
              addCircleMarkers(data=locations_uncovered_2,lng=~lon,lat=~lat,color="cyan",radius=5,fill=TRUE,fillColor="cyan",fillOpacity=10,group="Uncovered segments_top quartile",
                               options = pathOptions(clickable=FALSE))
          } else {
            alert("No coverage gaps (top quartile).")
          }
        } else{
          leafletProxy("map") %>%
            clearGroup("Uncovered segments_top quartile")
        }
      } else {
        alert("Pleaes enter group name, stage number and iteration number.")
      }
    }
    else {
      leafletProxy("map") %>%
        clearGroup("Uncovered segments_top quartile") 
    }
  })
  
  # checkbox to show/hide coverage gaps (2nd quartile by tons)
  observe({
    if (input$gaps_2nd_quartile) {
      if((input$group!="") & !is.na(input$stage) & !is.na(input$iteration)) {
        # check if the entered group/stage/iteration is saved already
        group_existing <- downloadGroupMeasures()
        group_existing_temp <- group_existing %>%
          filter(as.factor(unlist(GROUP_STAGE))==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
        if(nrow(group_existing_temp) > 0) {
          data <- downloadUncoveredSegments()
          data_temp <- data %>% 
            filter(GROUP_STAGE==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep="") & 
                     od_path %in% od_table_coverage_final_2nd_quartile$OD_PATH)          
          if (nrow(data_temp) > 0) {
            # visualize uncovered segments
            for(gap in 1:nrow(data_temp)) {
              leafletProxy("map") %>% 
                addPolylines(data=data_temp,lng=as.numeric(data_temp[gap,c(2,4)]),lat=as.numeric(data_temp[gap,c(1,3)]),group="Uncovered segments_2nd quartile",
                             color="cyan",weight=2,opacity=1,fill=FALSE,options = pathOptions(clickable=FALSE))
            }
            # visualize uncovered points
            locations_uncovered_ori <- data_temp %>% select(lat_ori,lon_ori)
            colnames(locations_uncovered_ori) <- c("lat","lon")
            locations_uncovered_des <- data_temp %>% select(lat_des,lon_des)
            colnames(locations_uncovered_des) <- c("lat","lon")
            locations_uncovered <- rbind(locations_uncovered_ori,locations_uncovered_des)
            locations_uncovered_2 <- locations_uncovered %>% group_by(lat,lon) %>% summarise(count=n())
            leafletProxy("map") %>% 
              addCircleMarkers(data=locations_uncovered_2,lng=~lon,lat=~lat,color="cyan",radius=5,fill=TRUE,fillColor="cyan",fillOpacity=10,group="Uncovered segments_2nd quartile",
                               options = pathOptions(clickable=FALSE))
          } else {
            alert("No coverage gaps (2nd quartile).")
          }
        } else{
          leafletProxy("map") %>%
            clearGroup("Uncovered segments_2nd quartile")
        }
      }
      else {
        alert("Pleaes enter group name, stage number and iteration number.")
      }
    }
    else {
      leafletProxy("map") %>%
        clearGroup("Uncovered segments_2nd quartile") 
    }
  })
  
  # checkbox to show/hide coverage gaps (3rd quartile by tons)
  observe({
    if (input$gaps_3rd_quartile) {
      if((input$group!="") & !is.na(input$stage) & !is.na(input$iteration)) {
        # check if the entered group/stage/iteration is saved already
        group_existing <- downloadGroupMeasures()
        group_existing_temp <- group_existing %>%
          filter(as.factor(unlist(GROUP_STAGE))==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
        if(nrow(group_existing_temp) > 0) {
          data <- downloadUncoveredSegments()
          data_temp <- data %>% 
            filter(GROUP_STAGE==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep="") & 
                     od_path %in% od_table_coverage_final_3rd_quartile$OD_PATH)
          if (nrow(data_temp) > 0) {
            # visualize uncovered segments
            for(gap in 1:nrow(data_temp)) {
              leafletProxy("map") %>% 
                addPolylines(data=data_temp,lng=as.numeric(data_temp[gap,c(2,4)]),lat=as.numeric(data_temp[gap,c(1,3)]),group="Uncovered segments_3rd quartile",
                             color="cyan",weight=2,opacity=1,fill=FALSE,options = pathOptions(clickable=FALSE))
            }
            # visualize uncovered points
            locations_uncovered_ori <- data_temp %>% select(lat_ori,lon_ori)
            colnames(locations_uncovered_ori) <- c("lat","lon")
            locations_uncovered_des <- data_temp %>% select(lat_des,lon_des)
            colnames(locations_uncovered_des) <- c("lat","lon")
            locations_uncovered <- rbind(locations_uncovered_ori,locations_uncovered_des)
            locations_uncovered_2 <- locations_uncovered %>% group_by(lat,lon) %>% summarise(count=n())
            leafletProxy("map") %>% 
              addCircleMarkers(data=locations_uncovered_2,lng=~lon,lat=~lat,color="cyan",radius=5,fill=TRUE,fillColor="cyan",fillOpacity=10,group="Uncovered segments_3rd quartile",
                               options = pathOptions(clickable=FALSE))
          } else {
            alert("No coverage gaps (3rd quartile).")
          }
        } else{
          leafletProxy("map") %>%
            clearGroup("Uncovered segments_3rd quartile")
        }
      } else {
        alert("Pleaes enter group name, stage number and iteration number.")
      }
    }
    else {
      leafletProxy("map") %>%
        clearGroup("Uncovered segments_3rd quartile") 
    }
  })
  
  # checkbox to show/hide coverage gaps (4th quartile by tons)
  observe({
    if (input$gaps_4th_quartile) {
      if((input$group!="") & !is.na(input$stage) & !is.na(input$iteration)) {
        group_existing <- downloadGroupMeasures()
        group_existing_temp <- group_existing %>%
          filter(as.factor(unlist(GROUP_STAGE))==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep=""))
        if(nrow(group_existing_temp) > 0) {
          data <- downloadUncoveredSegments()
          data_temp <- data %>% 
            filter(GROUP_STAGE==paste(input$group,"_Stage ",input$stage,".",input$iteration,sep="") & 
                     od_path %in% od_table_coverage_final_4th_quartile$OD_PATH)
          if (nrow(data_temp) > 0) {
            # visualize uncovered segments
            for(gap in 1:nrow(data_temp)) {
              leafletProxy("map") %>% 
                addPolylines(data=data_temp,lng=as.numeric(data_temp[gap,c(2,4)]),lat=as.numeric(data_temp[gap,c(1,3)]),group="Uncovered segments_4th quartile",
                             color="cyan",weight=2,opacity=1,fill=FALSE,options = pathOptions(clickable=FALSE))
            }
            # visualize uncovered points
            locations_uncovered_ori <- data_temp %>% select(lat_ori,lon_ori)
            colnames(locations_uncovered_ori) <- c("lat","lon")
            locations_uncovered_des <- data_temp %>% select(lat_des,lon_des)
            colnames(locations_uncovered_des) <- c("lat","lon")
            locations_uncovered <- rbind(locations_uncovered_ori,locations_uncovered_des)
            locations_uncovered_2 <- locations_uncovered %>% group_by(lat,lon) %>% summarise(count=n())
            leafletProxy("map") %>% 
              addCircleMarkers(data=locations_uncovered_2,lng=~lon,lat=~lat,color="cyan",radius=5,fill=TRUE,fillColor="cyan",fillOpacity=10,group="Uncovered segments_4th quartile",
                               options = pathOptions(clickable=FALSE))
          } else {
            alert("No coverage gaps (4th quartile).")
          }
        } else{
          leafletProxy("map") %>%
            clearGroup("Uncovered segments_4th quartile")
        }
      } else {
        alert("Pleaes enter group name, stage number and iteration number.")
      }
    }
    else {
      leafletProxy("map") %>%
        clearGroup("Uncovered segments_4th quartile") 
    }
  })
  
  observe({
    od_pair <- OD_paths %>% filter(PAIR == input$od_pairs)
    leafletProxy("map") %>% 
      clearGroup("OD Path") 
    
    leafletProxy("map") %>% 
      addPolylines(data=od_pair,color="cyan",weight=2,opacity=1,fill=FALSE,group="OD Path",options = pathOptions(clickable=FALSE))
  })
  
  downloadTempGroupSelections <- reactive({
    # Update the responses whenever a new button action (e.g. eval, update, etc.) is made
    input$map_shape_click
    input$map_marker_click
    input$previous_groups_list
    db_selections_tempgroups <- mongo(collection = "tempgroups_selections",
                                      url = sprintf(
                                        "mongodb://%s:%s@%s/%s",
                                        options()$mongodb$username,
                                        options()$mongodb$password,
                                        options()$mongodb$host,
                                        databaseName))
    data <- db_selections_tempgroups$find()
    data$id <- as.factor(unlist(data$id))
    data$group <- as.factor(unlist(data$group))
    data$lat <- as.numeric(unlist(data$lat))
    data$lng <- as.numeric(unlist(data$lng))
    data$GROUP_STAGE <- as.factor(unlist(data$GROUP_STAGE))
    data <- data %>% group_by(id,group,GROUP_STAGE) %>%
      summarise(LATITUDE=mean(lat),LONGITUDE=mean(lng),COUNT=n()) # NONCE=mean(.nonce),
    data <- data %>% filter(COUNT %% 2 == 1) # eliminating points that want to be removed 
    data <- data %>% select(id,group,LATITUDE,LONGITUDE,GROUP_STAGE) # NONCE,
    data
  })
  
  # create a list of previous groups/stages
  output$previousgroup_list <- renderUI({
    selectizeInput(inputId = "previousgroups",label = "Retrieve previous selection(s):", # used to have inputId with two underscores previous_groups_list but didn't work, and then removed underscores and ran no problem
                   choices = list(
                     Completed = unique(sort(downloadAllGroupSelections()$GROUP_STAGE)),
                     Temporary = unique(sort(downloadTempGroupSelections()$GROUP_STAGE))), multiple = TRUE, options = list(placeholder = "Select one or more scenarios")) #, options = list(maxOptions = 5)
  })
  
  # add a layer of previous group/stage
  observeEvent(input$add_previous, {
    if(!is.null(input$previousgroups)) {
      data_temp <- downloadTempGroupSelections()
      data_completed <- downloadAllGroupSelections()
      if(nrow(data_temp)>0 & nrow(data_completed)>0) {
        data2_temp <- data_temp %>% filter(GROUP_STAGE %in% input$previousgroups) 
        data2_temp <- data2_temp %>% select(id,group,LATITUDE,LONGITUDE,GROUP_STAGE)
        data2_completed <- data_completed %>% filter(GROUP_STAGE %in% input$previousgroups) 
        data2_completed <- data2_completed %>% select(ID,GROUP,LATITUDE,LONGITUDE,GROUP_STAGE)
        data2_completed$ID <- as.factor(unlist(data2_completed$ID))
        data2_completed$GROUP <- as.factor(unlist(data2_completed$GROUP))
        data2_completed$LATITUDE <- as.numeric(unlist(data2_completed$LATITUDE))
        data2_completed$LONGITUDE <- as.numeric(unlist(data2_completed$LONGITUDE))
        data2_completed$GROUP_STAGE <- as.factor(unlist(data2_completed$GROUP_STAGE))
        colnames(data2_completed)[1:2] <- c("id","group")
        data2_temp <- as.data.frame(data2_temp)
        data2_completed <- as.data.frame(data2_completed)
        data2_all <- rbind(data2_temp,data2_completed)
        data2_all <- data2_all %>% group_by(id,group) %>% summarise(lat=mean(LATITUDE),lng=mean(LONGITUDE))
        
        Truck_Stops2 <- Truck_Stops %>% filter(!(ID_2 %in% data2_all$id))
        if (input$Truck) {
          leafletProxy("map") %>%
            clearGroup("Truck Stops") %>%
            addCircles(lng=Truck_Stops2$LONGITUDE,lat=Truck_Stops2$LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                       layerId=Truck_Stops2$ID_2,label=paste0("ID: ",Truck_Stops2$ID_2),labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
            addMarkers(lng=data2_all$lng,lat=data2_all$lat,group="Truck Stops",icon=stationIcon,layerId=data2_all$id,label=paste0("ID: ",data2_all$id),
                       labelOptions=labelOptions(direction="top",offset=c(0,-45)))
        } else {
          leafletProxy("map") %>%
            addCircles(lng=Truck_Stops2$LONGITUDE,lat=Truck_Stops2$LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                       layerId=Truck_Stops2$ID_2,label=paste0("ID: ",Truck_Stops2$ID_2),labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
            addMarkers(lng=data2_all$lng,lat=data2_all$lat,group="Truck Stops",icon=stationIcon,layerId=data2_all$id,label=paste0("ID: ",data2_all$id),
                       labelOptions=labelOptions(direction="top",offset=c(0,-45)))
        }
        
        selection$df <- data2_all
        
      } else if(nrow(data_temp)>0 & nrow(data_completed)==0) {
        data2_temp <- data_temp %>% filter(GROUP_STAGE %in% input$previousgroups) 
        data2_temp <- data2_temp %>% select(id,group,LATITUDE,LONGITUDE,GROUP_STAGE)
        data2_temp <- as.data.frame(data2_temp)
        data2_all <- data2_temp %>% group_by(id,group) %>% summarise(lat=mean(LATITUDE),lng=mean(LONGITUDE))
        
        Truck_Stops2 <- Truck_Stops %>% filter(!(ID_2 %in% data2_all$id))
        if (input$Truck) {
          leafletProxy("map") %>%
            clearGroup("Truck Stops") %>%
            addCircles(lng=Truck_Stops2$LONGITUDE,lat=Truck_Stops2$LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                       layerId=Truck_Stops2$ID_2,label=paste0("ID: ",Truck_Stops2$ID_2),labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
            addMarkers(lng=data2_all$lng,lat=data2_all$lat,group="Truck Stops",icon=stationIcon,layerId=data2_all$id,label=paste0("ID: ",data2_all$id),
                       labelOptions=labelOptions(direction="top",offset=c(0,-45)))
        } else {
          leafletProxy("map") %>%
            addCircles(lng=Truck_Stops2$LONGITUDE,lat=Truck_Stops2$LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                       layerId=Truck_Stops2$ID_2,label=paste0("ID: ",Truck_Stops2$ID_2),labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
            addMarkers(lng=data2_all$lng,lat=data2_all$lat,group="Truck Stops",icon=stationIcon,layerId=data2_all$id,label=paste0("ID: ",data2_all$id),
                       labelOptions=labelOptions(direction="top",offset=c(0,-45)))
        }
        
        selection$df <- data2_all
        
      } else if(nrow(data_temp)==0 & nrow(data_completed)>0) {
        data2_completed <- data_completed %>% filter(GROUP_STAGE %in% input$previousgroups) 
        data2_completed <- data2_completed %>% select(ID,GROUP,LATITUDE,LONGITUDE,GROUP_STAGE)
        data2_completed$ID <- as.factor(unlist(data2_completed$ID))
        data2_completed$GROUP <- as.factor(unlist(data2_completed$GROUP))
        data2_completed$LATITUDE <- as.numeric(unlist(data2_completed$LATITUDE))
        data2_completed$LONGITUDE <- as.numeric(unlist(data2_completed$LONGITUDE))
        data2_completed$GROUP_STAGE <- as.factor(unlist(data2_completed$GROUP_STAGE))
        colnames(data2_completed)[1:2] <- c("id","group")
        data2_completed <- as.data.frame(data2_completed)
        data2_all <- data2_completed %>% group_by(id,group) %>% summarise(lat=mean(LATITUDE),lng=mean(LONGITUDE))
        
        Truck_Stops2 <- Truck_Stops %>% filter(!(ID_2 %in% data2_all$id))
        if (input$Truck) {
          leafletProxy("map") %>%
            clearGroup("Truck Stops") %>%
            addCircles(lng=Truck_Stops2$LONGITUDE,lat=Truck_Stops2$LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                       layerId=Truck_Stops2$ID_2,label=paste0("ID: ",Truck_Stops2$ID_2),labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
            addMarkers(lng=data2_all$lng,lat=data2_all$lat,group="Truck Stops",icon=stationIcon,layerId=data2_all$id,label=paste0("ID: ",data2_all$id),
                       labelOptions=labelOptions(direction="top",offset=c(0,-45)))
        } else {
          leafletProxy("map") %>%
            addCircles(lng=Truck_Stops2$LONGITUDE,lat=Truck_Stops2$LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops",
                       layerId=Truck_Stops2$ID_2,label=paste0("ID: ",Truck_Stops2$ID_2),labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
            addMarkers(lng=data2_all$lng,lat=data2_all$lat,group="Truck Stops",icon=stationIcon,layerId=data2_all$id,label=paste0("ID: ",data2_all$id),
                       labelOptions=labelOptions(direction="top",offset=c(0,-45)))
        }
        
        selection$df <- data2_all
        
      } else {
        alert("No records are retrieved.")
      } 
    } else {
      alert("Pleaes select one or more scenarios.")
    }
  })
  
  observeEvent(input$remove_previous, {
    leafletProxy("map",data=Truck_Stops) %>%
      clearGroup("Truck Stops")
    
    # reset the dataframe
    selection$df <- data.frame(id=factor(),group=factor(),lat=numeric(),lon=numeric())
  })
  
  observeEvent(input$refresh, {
    js$refresh()
  })
  
  
  ################################################################
  #### Selected New CNG Stations Tab ####
  output$selectionsTable <- DT::renderDataTable({
    downloadGroupSelections()
  })
  
  downloadGroupSelections <- reactive({
    input$eval
    db_selections <- mongo(collection = group_selections(),
                           url = sprintf(
                             "mongodb://%s:%s@%s/%s",
                             options()$mongodb$username,
                             options()$mongodb$password,
                             options()$mongodb$host,
                             databaseName))
    data <- db_selections$find()
    data
  })
  
  output$downloadGroupSelectionData <- downloadHandler(
    filename = function() {
      paste(input$group,"_Selections.csv",sep="")
    },
    content = function(file) {
      write.csv(downloadGroupSelections(),file)
    }
  )
  
  
  ################################################################
  #### Group Performance Measures by Stage Tab ####
  output$measuresTable <- DT::renderDataTable({
    downloadGroupMeasures()
  })
  
  downloadGroupMeasures <- reactive({
    input$eval
    db_measures <- mongo(collection = group_measures(),
                         url = sprintf(
                           "mongodb://%s:%s@%s/%s",
                           options()$mongodb$username,
                           options()$mongodb$password,
                           options()$mongodb$host,
                           databaseName))
    data <- db_measures$find()
    data
  })
  
  output$downloadGroupMeasureData <- downloadHandler(
    filename = function() {
      paste(input$group,"_Measures.csv",sep="")
    },
    content = function(file) {
      write.csv(downloadGroupMeasures(),file)
    }
  )
  
  
  ################################################################
  #### Comparing to Other Groups Tab ####
  output$allmeasuresTable <- DT::renderDataTable({
    input$eval
    data <- downloadAllGroupMeasures()
    
    ## Scenario: GROUP_STAGE does not include word 'Stage'
    # find the position of the last _ 
    underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
    GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
    STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
    data <- as.data.frame(cbind(data,GROUP,STAGE))
    data$GROUP <- as.character(data$GROUP)
    data$STAGE <- as.character(data$STAGE)
    data <- data %>% mutate(ITERATION=paste0(GROUP,"_",STAGE))
    data2 <- data %>% filter(ITERATION %in% get_selected(input$groupstage_plot,format="names"))
    # If no data are in view, return all
    if (nrow(data2) == 0)
      return(downloadAllGroupMeasures())
    data2
  })
  
  downloadAllGroupMeasures <- reactive({
    input$eval
    input$update_table
    input$update_plot
    input$update_spatial
    db_measures_allgroups <- mongo(collection = "allgroups_measures",
                                   url = sprintf(
                                     "mongodb://%s:%s@%s/%s",
                                     options()$mongodb$username,
                                     options()$mongodb$password,
                                     options()$mongodb$host,
                                     databaseName))
    data <- db_measures_allgroups$find()
    data
  })
  
  downloadAllGroupMeasures_plot <- reactive({
    input$eval
    input$update_table
    input$update_plot
    db_measures_allgroups_plot <- mongo(collection = "allgroups_measures_plot",
                                        url = sprintf(
                                          "mongodb://%s:%s@%s/%s",
                                          options()$mongodb$username,
                                          options()$mongodb$password,
                                          options()$mongodb$host,
                                          databaseName))
    data <- db_measures_allgroups_plot$find()
    data
  })
  
  output$downloadAllGroupMeasureData <- downloadHandler(
    filename = function() {
      paste("Group_Measures.csv",sep="")
    },
    content = function(file) {
      data <- downloadAllGroupMeasures()
      ## Scenario: GROUP_STAGE does not include word 'Stage'
      # find the position of the last _ 
      underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
      GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
      STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
      data <- as.data.frame(cbind(data,GROUP,STAGE))
      data$GROUP <- as.character(data$GROUP)
      data$STAGE <- as.character(data$STAGE)
      data <- data %>% mutate(ITERATION=paste0(GROUP,"_",STAGE))
      data2 <- data %>% filter(ITERATION %in% get_selected(input$groupstage_plot,format="names"))
      if (nrow(data2) == 0) {
        write.csv(data,file)
      } else {
        write.csv(data2,file)
      }
    }
  )
  
  # create a checkbox group input list based on the data read from database
  output$groupstage_plot <- renderTree({
    data <- downloadAllGroupMeasures()
    
    ## Scenario: GROUP_STAGE does not include word 'Stage'
    # find the position of the last _ 
    underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
    GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
    STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
    data <- as.data.frame(cbind(data,GROUP,STAGE))
    data$GROUP <- as.character(data$GROUP)
    data$STAGE <- as.character(data$STAGE)
    data <- data %>% mutate(ITERATION=paste0(data$GROUP,"_",data$STAGE))
    data <- data %>% arrange(GROUP)
    group <- as.data.frame(data %>% group_by(GROUP) %>% summarise(COUNT=n()))
    
    # If no data are in view, don't plot
    if (nrow(data) == 0) {
      return(list())
    } else {
      # create a nested list with two levels
      list <- list()
      n <- 1
      for(i in 1:nrow(group)){ 
        nested_list <- list()
        for(j in 1:group[i,2]) {
          nested_list[[data[n,12]]] <- data[n,12]
          n <- n+1
        }
        name <- group[i,1]
        list[[name]] <- nested_list
      }
      # Here, a list will be created everytime when to update the database table
      list
    }
  })
  
  # create bar charts and scatter plots by selecting different x and y variables
  output$groupcomparison_plot <- renderPlot({
    data <- downloadAllGroupMeasures_plot()
    ## Scenario: GROUP_STAGE does not include word 'Stage'
    # find the position of the last _ 
    underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
    GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
    STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
    data <- as.data.frame(cbind(data,GROUP,STAGE))
    data$GROUP <- as.character(data$GROUP)
    data$STAGE <- as.character(data$STAGE)
    data <- data %>% mutate(ITERATION=paste0(GROUP,"_",STAGE))
    data2 <- data %>% filter(ITERATION %in% get_selected(input$groupstage_plot,format="names"))
    
    data2$GROUP_STAGE <- as.factor(data2$GROUP_STAGE)
    
    # If no data are in view, don't plot
    if (nrow(data2) == 0)
      return(NULL)
    
    # Lables for axes
    xvar_name <- names(xaxis_vars)[xaxis_vars == input$xvar_plot]
    yvar_name <- names(yaxis_vars)[yaxis_vars == input$yvar_plot]
    
    # but since the inputs are strings, we need to do a little more work.
    xvar <- as.symbol(input$xvar_plot)
    yvar <- as.symbol(input$yvar_plot)
    
    # render a barplot
    if(input$xvar_plot == "GROUP_STAGE") {
      p <- ggplot(data2,aes_string(x=input$xvar_plot,y=input$yvar_plot)) + 
        geom_bar(stat = "identity",width=0.1,aes(fill=GROUP_STAGE)) + # ,fill=GROUP_STAGE stat="identity" refers y to a column in a df, not the number of cases in each group (count)
        labs(x="GROUPS & STAGES", y=yvar_name) +
        ggtitle("Group comparison (one measure)") +
        scale_fill_hue("Groups and Stages") +
        theme(legend.text=element_text(size=14),legend.title=element_text(size=18,margin=margin(0,0,20,0)),axis.text=element_text(size=14),
              axis.title=element_text(size=18),plot.title = element_text(size=22,face="bold",margin=margin(0,0,20,0)),
              axis.title.x=element_text(margin=margin(20,0,0,0)),
              axis.title.y=element_text(margin=margin(0,20,0,0))) # panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),
      print(p) # ggplot function has to include print() in the end.
      
    }
    # render a scatterplot
    else {
      p <- ggplot(data2,aes_string(x=input$xvar_plot,y=input$yvar_plot)) + 
        geom_point(size=8,shape=19,aes(color=GROUP_STAGE)) + #,color=factor(GROUP_STAGE),label=factor(GROUP_STAGE)
        labs(x=xvar_name,y=yvar_name) +
        ggtitle("Group comparison (two measures)") +
        scale_color_hue("Groups and Stages") +
        geom_text(aes(label=GROUP_STAGE),check_overlap=FALSE,hjust=0.5,vjust=-1.25,size=5,color="black") +   
        xlim(0.95*min(data2[,input$xvar_plot]),0.05*min(data2[,input$xvar_plot])+max(data2[,input$xvar_plot])) +
        ylim(min(data2[,input$yvar_plot]),0.05*min(data2[,input$yvar_plot])+max(data2[,input$yvar_plot])) +
        theme(legend.text=element_text(size=14),legend.title=element_text(size=18,margin=margin(0,0,20,0)),axis.text=element_text(size=14),
              axis.title=element_text(size=18),plot.title = element_text(size=22,face="bold",margin=margin(0,0,20,0)),
              axis.title.x=element_text(margin=margin(20,0,0,0)),
              axis.title.y=element_text(margin=margin(0,20,0,0))) # panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),
      print(p)
    }  })
  
  
  # plot function that is used for "download" process
  plotInput <- function() {
    data <- downloadAllGroupMeasures_plot()
    ## Scenario: GROUP_STAGE does not include word 'Stage'
    # find the position of the last _ 
    underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
    GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
    STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
    data <- as.data.frame(cbind(data,GROUP,STAGE))
    data$GROUP <- as.character(data$GROUP)
    data$STAGE <- as.character(data$STAGE)
    data <- data %>% mutate(ITERATION=paste0(GROUP,"_",STAGE))
    
    data2 <- data %>% filter(ITERATION %in% get_selected(input$groupstage_plot,format="names"))
    data2$GROUP_STAGE <- as.factor(data2$GROUP_STAGE)
    # If no data are in view, don't plot
    if (nrow(data2) == 0)
      return(NULL)
    # Lables for axes
    xvar_name <- names(xaxis_vars)[xaxis_vars == input$xvar_plot]
    yvar_name <- names(yaxis_vars)[yaxis_vars == input$yvar_plot]
    # but since the inputs are strings, we need to do a little more work.
    xvar <- as.symbol(input$xvar_plot)
    yvar <- as.symbol(input$yvar_plot)
    # render a barplot
    if(input$xvar_plot == "GROUP_STAGE") {
      p <- ggplot(data2,aes_string(x=input$xvar_plot,y=input$yvar_plot)) + 
        geom_bar(stat = "identity",width=0.1,aes(fill=GROUP_STAGE)) + # ,fill=GROUP_STAGE stat="identity" refers y to a column in a df, not the number of cases in each group (count)
        labs(x="GROUPS & STAGES", y=yvar_name) +
        ggtitle("Group comparison (one measure)") +
        scale_fill_hue("Groups and Stages") +
        theme(legend.text=element_text(size=14),legend.title=element_text(size=18,margin=margin(0,0,20,0)),axis.text=element_text(size=14),
              axis.title=element_text(size=18),plot.title = element_text(size=22,face="bold",margin=margin(0,0,20,0)),
              axis.title.x=element_text(margin=margin(20,0,0,0)),
              axis.title.y=element_text(margin=margin(0,20,0,0)))
      print(p) # ggplot function has to include print() in the end.
      
    }
    # render a scatterplot
    else {
      p <- ggplot(data2,aes_string(x=input$xvar_plot,y=input$yvar_plot)) + 
        geom_point(size=8,shape=19,aes(color=GROUP_STAGE)) + #,color=factor(GROUP_STAGE),label=factor(GROUP_STAGE)
        labs(x=xvar_name,y=yvar_name) +
        ggtitle("Group comparison (two measures)") +
        scale_color_hue("Groups and Stages") +
        geom_text(aes(label=GROUP_STAGE),check_overlap=FALSE,hjust=0.5,vjust=-1.25,size=5,color="black") + 
        xlim(0.95*min(data2[,input$xvar_plot]),0.05*min(data2[,input$xvar_plot])+max(data2[,input$xvar_plot])) +
        ylim(min(data2[,input$yvar_plot]),0.05*min(data2[,input$yvar_plot])+max(data2[,input$yvar_plot])) +
        theme(legend.text=element_text(size=14),legend.title=element_text(size=18,margin=margin(0,0,20,0)),axis.text=element_text(size=14),
              axis.title=element_text(size=18),plot.title = element_text(size=22,face="bold",margin=margin(0,0,20,0)),
              axis.title.x=element_text(margin=margin(20,0,0,0)),
              axis.title.y=element_text(margin=margin(0,20,0,0)))
      print(p)
    }
  }
  
  # download the plot as a .png file
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Plot for group comparison.png",sep="")
    },
    content = function(file) {
      png(file,width = 1024, height = 850, units = "px")
      plotInput()
      dev.off()
    }
  )

  
  #################################################################
  #### Spatial Comparison Tab ####
  downloadAllGroupSelections <- reactive({
    # Update the responses whenever a new button action (e.g. eval, update, etc.) is made
    input$eval
    input$update_spatial
    db_selections_allgroups <- mongo(collection = "allgroups_selections",
                                     url = sprintf(
                                       "mongodb://%s:%s@%s/%s",
                                       options()$mongodb$username,
                                       options()$mongodb$password,
                                       options()$mongodb$host,
                                       databaseName))
    data <- db_selections_allgroups$find()
    data
  })
  
  # adding a basemap under "Spatial Comparison" tab
  output$map_compare <- renderLeaflet({
    leaflet() %>% 
      # Add base maps
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Positron", group = "Black & White") %>%
      addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png',
               attribution = paste('&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors','&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'),group="Labels Only") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Awesome Dark") %>%
      # adding a layer control to swith between different reference maps
      addGeoJSON(I10,color="cyan",weight=3,opacity=1,fill=FALSE,group="I-10 (CA-TX)",pathOptions(clickable=FALSE)) %>%
      hideGroup("I-10 (CA-TX)") %>%
      # adding a layer control to swith between different reference maps
      addLayersControl(baseGroups = c("OpenStreetMap","Black & White","Labels Only","Awesome Dark"),
                       overlayGroups = c("I-10 (CA-TX)"),
                       options = layersControlOptions(collapsed = TRUE,autoZIndex=TRUE)) %>%
      # adding a scalebar
      addScaleBar(position="bottomright",options=scaleBarOptions(maxWidth=100,metric=FALSE,imperial=TRUE,updateWhenIdle=TRUE)) %>%
      setView(-112.058487, 33.462173, 6) # addProviderTiles("Stamen.TonerLite")
  })
  
  # displaying additional data layers
  observe({
    if (input$CNGs_compare) {
      leafletProxy("map_compare",data=CNG_Stations) %>% 
        addCircles(lng=~Longitude,lat=~Latitude,color="#33FD28",opacity=10,radius=100,fill=TRUE,fillColor="#33FD28",fillOpacity=10,group="CNG Stations_compare",
                   layerId=~ID_2,label=~paste("ID: ",ID_2,"; ",Groups_Wit,"; ",NG_Vehicle),options = pathOptions(clickable=FALSE),labelOptions=labelOptions(direction="top",offset=c(0,-45)))}
    else {
      leafletProxy("map_compare",data=CNG_Stations) %>% clearGroup("CNG Stations_compare")
    }
  })
  
  observe({
    if (input$Truck_compare) {
      leafletProxy("map_compare",data=Truck_Stops) %>% 
        addCircles(lng=~LONGITUDE,lat=~LATITUDE,color="red",opacity=8,radius=100,fill=TRUE,fillColor="red",fillOpacity=8,group="Truck Stops_compare",
                   layerId=~ID_2,label=~paste0("ID: ",ID_2),options = pathOptions(clickable=FALSE),labelOptions=labelOptions(direction="top",
                                                                                                                             offset=c(0,-45)))}
    else {
      leafletProxy("map_compare",data=Truck_Stops) %>% clearGroup("Truck Stops_compare") 
      #selection$df <- data.frame(id=factor(),group=factor(),lat=numeric(),lon=numeric())
    }
  })
  
  observe({
    if (input$Centroids_compare) {
      leafletProxy("map_compare",data=Metro_centroids) %>% 
        addCircleMarkers(lng=~snap_x,lat=~snap_y,color="#FF52F6",opacity=8,radius=3,fill=TRUE,fillColor="#490C46",fillOpacity=8,group="Metro Centroids_compare",
                         options=pathOptions(clickable=FALSE))}
    else {
      leafletProxy("map_compare",data=Metro_centroids) %>% clearGroup("Metro Centroids_compare") 
    }
  })
  
  # nested if/else statement for each layer is used to deal with the issue of one single legend shared by several AADT layers
  observe({
    if (input$AADT_NHS1_compare) {
      if (!input$AADT_NHS3_compare&!input$AADT_NHS7_compare&!input$AADT_NHS0_compare){
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS1_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS1_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS1_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS1_compare",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS3_compare&!input$AADT_NHS7_compare&!input$AADT_NHS0_compare){
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS1_compare") %>% 
          removeControl(layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS1_compare")
      }
    }
  })
  
  observe({
    if (input$AADT_NHS3_compare) {
      if (!input$AADT_NHS1_compare&!input$AADT_NHS7_compare&!input$AADT_NHS0_compare){
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS3_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS3_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS3_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS3_compare",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS1_compare&!input$AADT_NHS7_compare&!input$AADT_NHS0_compare){
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS3_compare") %>% 
          removeControl(layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS3_compare")
      }
    }
  })
  
  observe({
    if (input$AADT_NHS7_compare) {
      if (!input$AADT_NHS1_compare&!input$AADT_NHS3_compare&!input$AADT_NHS0_compare){
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS7_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS7_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS7_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS7_compare",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS1_compare&!input$AADT_NHS3_compare&!input$AADT_NHS0_compare){
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS7_compare") %>% 
          removeControl(layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS7_compare")
      }
    }
  })
  
  observe({
    if (input$AADT_NHS0_compare) {
      if (!input$AADT_NHS1_compare&!input$AADT_NHS3_compare&!input$AADT_NHS7_compare){
        #leafletProxy("map",data=Annual_Average_Daily_Traffic_NHS0) %>% 
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS0_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addLegend("bottomright",pal=colorpal,values=Annual_Average_Daily_Traffic_NHS1$Jenks,opacity=3,title="Annual Average Daily Traffic for Trucks (AADTT)",layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          addGeoJSON(NHS0_1,color="#7FB3D5",weight=2,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_2,color="#5499C7",weight=4,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_3,color="#2471A3",weight=6,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_4,color="#1A5276",weight=8,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE)) %>%
          addGeoJSON(NHS0_5,color="#154360",weight=10,opacity=1,fill=FALSE,group="Traffic Flow_NHS0_compare",options = pathOptions(clickable=FALSE))
      }
    }
    else {
      if (!input$AADT_NHS1_compare&!input$AADT_NHS3_compare&!input$AADT_NHS7_compare){
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS0_compare") %>% 
          removeControl(layerId="AADT_Legend_compare")
      }
      else {
        leafletProxy("map_compare") %>% 
          clearGroup("Traffic Flow_NHS0_compare")
      }
    }
  })
  
  observe({
    if (input$ZIP_Fleet_compare) {
      leafletProxy("map_compare") %>% 
        addPolygons(data=ZIP_Fleet,weight=0.8,color="#BDC3C7",opacity=0.5,fillColor = ~colorpal_fleet(Jenks),fillOpacity = 0.5,
                    label=~str_c("Fleets: ",FLEET,", ",STATE," ",ZIP_CODE),
                    options = pathOptions(clickable=FALSE),
                    labelOptions=labelOptions('auto'),
                    highlightOptions=highlightOptions(color="cyan",opacity=1,weight=2,fillOpacity=1,bringToFront=TRUE,sendToBack=TRUE),
                    group="Fleet_compare") %>%
        addLegend("bottomleft",pal=colorpal_fleet,value=ZIP_Fleet$Jenks,opacity=3,title="Truck Fleet Data (ZIP-based)",layerId="Fleet_Legend_compare")
    }
    else {
      leafletProxy("map_compare") %>% 
        clearGroup("Fleet_compare") %>% 
        removeControl(layerId="Fleet_Legend_compare")
    }
  })
  
  observe({
    if (input$Pipelines_compare) {
      leafletProxy("map_compare") %>% 
        addGeoJSON(Pipeline,weight=1,color="purple",fill=FALSE,opacity=1,
                   options = pathOptions(clickable=FALSE),
                   group="NG_Pipelines_compare")
    }
    else {
      leafletProxy("map_compare") %>% 
        clearGroup("NG_Pipelines_compare") 
    }
  })
  
  # create a checkbox group input list based on the data read from database
  output$groupstage_spatial <- renderTree({
    data <- downloadAllGroupMeasures()
    
    ## Scenario: GROUP_STAGE does not include word 'Stage'
    # find the position of the last _ 
    underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
    GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
    STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
    data <- as.data.frame(cbind(data,GROUP,STAGE))
    data$GROUP <- as.character(data$GROUP)
    data$STAGE <- as.character(data$STAGE)
    data <- data %>% mutate(ITERATION=paste0(GROUP,"_",STAGE))
    data <- data %>% arrange(GROUP)
    group <- as.data.frame(data %>% group_by(GROUP) %>% summarise(COUNT=n()))
    
    # If no data are in view, don't plot
    if (nrow(data) == 0) {
      return(list())
    } else {
      # create a nested list with two levels
      list <- list()
      n <- 1
      for(i in 1:nrow(group)){ 
        nested_list <- list()
        for(j in 1:group[i,2]) {
          nested_list[[data[n,12]]] <- data[n,12]
          n <- n+1
        }
        name <- group[i,1]
        list[[name]] <- nested_list
      }
      # Here, a list will be created everytime when to update the database table
      list
    }
  })
  
  # clicking visualize button
  observeEvent(input$vis_spatial, {
    data <- downloadAllGroupSelections()
    ## Scenario: GROUP_STAGE does not include word 'Stage'
    # find the position of the last _ 
    underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
    # already have GROUP column in the dataset so use DESIGN_GROUP
    DESIGN_GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
    STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
    data <- as.data.frame(cbind(data,DESIGN_GROUP,STAGE))
    data$DESIGN_GROUP <- as.character(data$DESIGN_GROUP)
    data$STAGE <- as.character(data$STAGE)
    data <- data %>% mutate(ITERATION=paste0(DESIGN_GROUP,"_",STAGE))
    data2 <- data %>% filter(ITERATION %in% get_selected(input$groupstage_spatial,format="names"))
    data3 <- data2 %>% select(GROUP_STAGE,ID,NAME,LONGITUDE,LATITUDE) %>% group_by(ID,NAME) %>% 
      summarise(COUNT=n(),meanLONGITUDE=mean(LONGITUDE),meanLATITUDE=mean(LATITUDE),G_S=paste(GROUP_STAGE,collapse=",")) 
    data3 <- data3 %>% filter(COUNT>1)
    colorpal_comparison <- colorFactor(rainbow(NROW(unique(data2$GROUP_STAGE))),data2$GROUP_STAGE)
    if(nrow(data3) > 0) {
      leafletProxy("map_compare") %>% 
        addCircleMarkers(data=data2,lng=~LONGITUDE,lat=~LATITUDE,color=~colorpal_comparison(GROUP_STAGE),opacity=0.7,radius=6,
                         fill=TRUE,fillColor=~colorpal_comparison(GROUP_STAGE),fillOpacity=0.7,group="Group Comparison",
                         label=~str_c("Name: ",NAME,"; Lat/Lon: ",round(LATITUDE,digits=2)," ",round(LONGITUDE,digits=2),"; Group/stage: ",GROUP_STAGE),
                         labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
        addCircleMarkers(data=data3,lng=~meanLONGITUDE,lat=~meanLATITUDE,color="gray",opacity=1,radius=12,
                         fill=TRUE,fillColor="gray",fillOpacity=1,group="Group Comparison",
                         label=~str_c("Name: ",NAME,"; Groups/Stages: ",G_S),
                         labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
        addLegend("bottomleft",pal=colorpal_comparison,value=data2$GROUP_STAGE,opacity=3,title="Selected Group(s)/Stage(s)",layerId="Comparison_Legend")
    } else{
      leafletProxy("map_compare") %>% 
        addCircleMarkers(data=data2,lng=~LONGITUDE,lat=~LATITUDE,color=~colorpal_comparison(GROUP_STAGE),opacity=0.7,radius=6,
                         fill=TRUE,fillColor=~colorpal_comparison(GROUP_STAGE),fillOpacity=0.7,group="Group Comparison",
                         label=~str_c("Name: ",NAME,"; Group/Stage: ",GROUP_STAGE),
                         labelOptions=labelOptions(direction="top",offset=c(0,-45))) %>%
        addLegend("bottomleft",pal=colorpal_comparison,value=data2$GROUP_STAGE,opacity=3,title="Selected Group(s)/Stage(s)",layerId="Comparison_Legend")
    }
  })
  
  observeEvent(input$removeSelectedGroups_spatial, {
    leafletProxy("map_compare") %>%
      clearGroup("Group Comparison") %>%
      removeControl(layerId="Comparison_Legend")
  })
  
  output$downloadSelectedData_spatial <- downloadHandler(
    filename = function() {
      paste("spatial comparison.csv",sep="")
    },
    content = function(file) {
      data <- downloadAllGroupSelections()
      ## Scenario: GROUP_STAGE does not include word 'Stage'
      # find the position of the last _ 
      underscore <- regexpr("\\_[^\\_]*$",data$GROUP_STAGE) # return the specific position, integer: the position of the last underscore
      # already have GROUP column in the dataset so use DESIGN_GROUP
      DESIGN_GROUP <- substr(data$GROUP_STAGE, 1, underscore-1) # extract substrings in a character vector: first character to the stopping character
      STAGE <- substr(data$GROUP_STAGE, underscore+7, nchar(as.character(data$GROUP_STAGE))) # extract substrings in a character vector: stopping character to the last one
      data <- as.data.frame(cbind(data,DESIGN_GROUP,STAGE))
      data$DESIGN_GROUP <- as.character(data$DESIGN_GROUP)
      data$STAGE <- as.character(data$STAGE)
      data <- data %>% mutate(ITERATION=paste0(DESIGN_GROUP,"_",STAGE))
      data2 <- data %>% filter(ITERATION %in% get_selected(input$groupstage_spatial,format="names"))
      write.csv(data2,file)
    }
  )
  }

shinyApp(ui, server)
