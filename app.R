## app.R ##
#Loading libraries
#---------------------------------------------------------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(geojsonsf)
library(shinycssloaders)

#Setup, load files, data etc.
#---------------------------------------------------------------------------------------------------------------------------------------------------------
#upload India state boundary layers
india_state <-  geojsonsf::geojson_sf("./data/india_state_v3.geojson")
india_district <-  geojsonsf::geojson_sf("./data/india_district_v2.geojson")

#load sample data for testing
data <- read.csv("./data/sample_data.csv")
values <- reactiveValues(selected_indicator = NA)

#UI part
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Build sidebar and load tabs
sidebar <- dashboardSidebar(sidebarMenu(
  sidebarSearchForm(
    textId = "searchText",
    buttonId = "searchButton",
    label = "Search..."
  ),
  selectInput(
    'indicator',
    label = 'Select an Indicator',
    choices = c('indicator_1', 'indicator_2', 'indicator_3')
  ),
  menuItem("State", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("District",icon = icon("th"),tabName = "widgets",badgeLabel = "new",badgeColor = "green")
))

#Build main body
body <- dashboardBody(
  tags$style(type = "text/css", "#mymap {height: calc(80vh - 20px) !important;}"),
  tags$style(type = "text/css", "#mychart {height: calc(80vh - 20px) !important;}"),
  tags$style(type = "text/css", "#mymap1 {height: calc(80vh - 20px) !important;}"),
  tags$style(type = "text/css", "#mychart1 {height: calc(80vh - 20px) !important;}"),
  
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              #Stat Boxes for quick info
              valueBox(10 * 2, "Statistics #1", icon = icon("credit-card"), color = "purple"),
              valueBox(10 * 2, "Statistics #2", icon = icon("credit-card"), color = "yellow"),
              valueBox(10 * 2, "Statistics #3", icon = icon("credit-card")),
              #Leaflet
              box(width = 8,leafletOutput("mymap")%>% withSpinner(color="#0dc5c1")),
              #Chart
              box(width = 4,plotlyOutput("mychart")%>% withSpinner(color="#0dc5c1"))
            )),
    
    # Second tab content
    tabItem(tabName = "widgets",
            fluidRow(
              #Stat Boxes for quick info
              valueBox(10 * 2, "Statistics #1", icon = icon("credit-card"), color = "purple"),
              valueBox(10 * 2, "Statistics #2", icon = icon("credit-card"), color = "yellow"),
              valueBox(10 * 2, "Statistics #3", icon = icon("credit-card")),
              #Leaflet
              box(width = 8,leafletOutput("mymap1")%>% withSpinner(color="#0dc5c1")),
              #Chart
              box(width = 4,plotlyOutput("mychart1")%>% withSpinner(color="#0dc5c1"))
              #h2("Dashboard tab content")
            ))
  ))

# Put them together into a dashboardPage
ui <- dashboardPage(dashboardHeader(title = "Simple tabs"),
                    sidebar,
                    body)


#Server part
#---------------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$indicator, {
    values$selected_indicator <- input$indicator
  })
  
  pal <- colorNumeric('Reds', NULL)
  
  #Build state map leaflet  
  output$mymap <- renderLeaflet({
    leaflet(india_state,options = leafletOptions(minZoom = 5, maxZoom = 6)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        #overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset Zoom",
        onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
      addPolygons(
        label = ~ paste(india_state$State, ":", data[[values$selected_indicator]]),
        stroke = FALSE,
        smoothFactor = 0,
        fillOpacity = 1,
        fillColor = ~ pal(data[[values$selected_indicator]])
      ) %>%
      addLegend(
        position = 'bottomright',
        pal = pal,
        values = ~ data[[values$selected_indicator]],
        title = 'Legend'
      )
  })
  #Build state chart 
  output$mychart <- renderPlotly({
    plot_ly(
      data = data,
      y = india_state$State,
      x =  ~ data[[values$selected_indicator]],
      type = "bar",
      orientation = 'h',
      text =  ~ data[[values$selected_indicator]]
    ) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  #Build district map leaflet
  output$mymap1 <- renderLeaflet({
    leaflet(india_district,options = leafletOptions(minZoom = 5, maxZoom = 6)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        #overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Reset Zoom",
        onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
      addPolygons(
        label = ~ paste(india_district$State, ":", data[[values$selected_indicator]]),
        stroke = FALSE,
        smoothFactor = 0,
        fillOpacity = 1,
        fillColor = ~ pal(data[[values$selected_indicator]])
      ) %>%
      addLegend(
        position = 'bottomright',
        pal = pal,
        values = ~ data[[values$selected_indicator]],
        title = 'Legend'
      )
  })
  #Build district chart
  output$mychart1 <- renderPlotly({
    plot_ly(
      data = data,
      y = india_state$State,
      x =  ~ data[[values$selected_indicator]],
      type = "bar",
      orientation = 'h',
      text =  ~ data[[values$selected_indicator]]
    ) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
}


shinyApp(ui, server)