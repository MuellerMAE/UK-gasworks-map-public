##### Code by Marie Mueller

library(shiny)
library(data.table)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(stringr)
library(rsconnect)

########################################################
# DATA: BROWNFIELD
########################################################

# bfss <- fread("data/brownfield-land.csv")
# 
# bfss_clean <- bfss %>%
#   mutate(
#     point = na_if(point, ""),
#     point = ifelse(point == "POINT", NA, point)
#   ) %>%
#   filter(!is.na(point))
# 
# bfss_sf <- st_as_sf(
#   bfss_clean,
#   wkt = "point",
#   crs = 4326
# )
# 
# names(bfss_sf) <- gsub("-", "", names(bfss_sf))
# 
# # bfss_filtered <- bfss_sf %>%
# #   filter(hazardoussubstances == "yes")
# 
# bfss_filtered <- bfss_sf %>%
#   filter(
#     str_detect(coalesce(siteaddress, ""), regex("gas\\s*works", ignore_case = TRUE)) |
#       str_detect(coalesce(notes, ""), regex("gas\\s*works", ignore_case = TRUE))
#   )

########################################################
# DATA: GASWORKS
########################################################

gasworks <- fread("data/gasworks sites v2.csv")

gasworks_sf <- st_as_sf(
  gasworks,
  coords = c("LONG", "LAT"),
  crs = 4326
)

gasworks_sf$uid <- paste0(
  "gas_",
  seq_len(nrow(gasworks_sf))
)

########################################################
# MARKER ICONS
########################################################

black_pin <- icons(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-black.png",
  iconWidth = 25,
  iconHeight = 41,
  iconAnchorX = 12,
  iconAnchorY = 41
)

blue_pin <- icons(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
  iconWidth = 25,
  iconHeight = 41,
  iconAnchorX = 12,
  iconAnchorY = 41
)

########################################################
# UI
########################################################

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .container-fluid {
        padding-top: 25px;
        padding-left: 30px;
        padding-right: 30px;
        padding-bottom: 25px;
      }
    "))
  ),
  
  h2("UK Gasworks"),
  p(
    "This map is work in progress and does not include all UK gasworks sites."
    ),
  
  fluidRow(
    column(
      8,
      leafletOutput(
        "map",
        height = 500
      )
    ),
    
    column(
      4,
      div(
        style = "
      height: 500px;
      overflow-y: auto;
      padding-right: 10px;
    ",
        uiOutput("info")
      )
    )
  )
)

########################################################
# SERVER
########################################################

server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  last_info <- reactiveVal(NULL)     # controls info panel
  current_zoom <- reactiveVal(NULL)
  
  ########################################################
  # SAVE CURRENT ZOOM
  ########################################################
  
  observeEvent(
    input$map_zoom,
    {
      current_zoom(input$map_zoom)
    }
  )
  
  ########################################################
  # MAP
  ########################################################
  
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = TRUE
      )
    ) %>%
      
      addTiles(
        options = tileOptions(
          opacity = 0.5
        )
      ) %>%
      
      # addCircleMarkers(
      #   data = bfss_filtered,
      #   radius = 3,
      #   color = "red",
      #   fillColor = "red",
      #   fillOpacity = 1
      # ) %>%
      
      addMarkers(
        data = gasworks_sf,
        layerId = ~uid,
        group = "gasworks",
        icon = black_pin,
        label = ~NAME
      ) %>%
      
      onRender("
        function(el, x) {
          var map = this;
          map.zoomControl.setPosition('bottomright');
        }
      ")
  })
  
  ########################################################
  # MARKER CLICK
  ########################################################
  
  observeEvent(
    input$map_marker_click,
    {
      req(
        input$map_marker_click$group == "gasworks"
      )
      
      id <- as.character(input$map_marker_click$id)
      
      if (identical(selected(), id)) {
        # deselect marker only
        selected(NULL)
      } else {
        selected(id)
        last_info(id)
      }
      
      coords <- st_coordinates(
        gasworks_sf[
          gasworks_sf$uid == id,
        ]
      )
      
      zoom_level <- current_zoom()
      
      if (is.null(zoom_level)) {
        zoom_level <- 6
      }
      
      leafletProxy("map") %>%
        setView(
          lng = coords[1],
          lat = coords[2],
          zoom = max(
            zoom_level,
            7.5
          )
        )
    }
  )
  
  ########################################################
  # UPDATE MARKERS AFTER SELECTION
  ########################################################
  
  observeEvent(selected(), {
    
    sel <- selected()
    
    proxy <- leafletProxy("map")
    
    proxy %>%
      clearGroup("gasworks")
    
    # Add all markers back as red
    proxy %>%
      addMarkers(
        data = gasworks_sf,
        layerId = ~uid,
        group = "gasworks",
        icon = black_pin,
        label = ~NAME
      )
    
    # Replace selected marker with black
    if (!is.null(sel)) {
      proxy %>%
        addMarkers(
          data = gasworks_sf[gasworks_sf$uid == sel, ],
          layerId = ~uid,
          group = "gasworks",
          icon = black_pin,
          label = ~NAME
        )
    }
  })
  
  ########################################################
  # DETAILS PANEL
  ########################################################
  
  output$info <- renderUI({
    if (is.null(last_info())) {
      return(
        tagList(
          h3("How to use the map"),
          p(
            "Click a black pin to display information about the site."
          )
        )
      )
    }
    
    row <- gasworks_sf[gasworks_sf$uid == last_info(), ]
   
    row <- as.data.frame(row)
    
    make_field <- function(label, value) {
      
      if (
        is.null(value) ||
        is.na(value) ||
        value == ""
      ) {
        return(NULL)
      }
      
      if (
        grepl(
          "^https?://",
          value
        )
      ) {
        
        links <- str_split(
          value,
          ";"
        )[[1]]
        
        value <- tagList(
          lapply(
            links,
            function(link) {
              tags$div(
                style = "margin-bottom: 8px;",
                tags$a(
                  href = link,
                  link,
                  target = "_blank"
                )
              )
            }
          )
        )
      }
      
      tags$p(
        tags$b(
          paste0(label, ": ")
        ),
        value
      )
    }
    
    tagList(
      h3(row$NAME),
      
      # Photo
      if (
        "Photo" %in% names(row) &&
        !is.na(row$PHOTO) &&
        row$PHOTO != ""
      ) {
        tags$img(
          src = row$PHOTO,
          width = "100%",
          style = "margin-bottom:15px;"
        )
      },
      
      make_field(
        "Owner/developer",
        row$OWNER
      ),
      
      make_field(
        "Local authority",
        row$LA
      ),
      
      make_field(
        "Planning status",
        row$PLANNINGSTATUS
      ),
      
      make_field(
        "Number of housing units",
        row$NOHOUSINGUNITS
      ),
      
      make_field(
        "Info",
        row$INFO
      ),
      
      make_field(
        "Chemicals on site",
        row$CHEMICALS
      ),

      make_field(
        "Health impacts",
        row$HEALTHIMPACTS
      ),
      
      make_field(
        "Media coverage",
        row$PRESS
      )
    )
  })
}

########################################################
# RUN APP
########################################################

shinyApp(
  ui,
  server
)

########################################################
# FOR GITHUB
########################################################

# rsconnect::writeManifest(
#   appDir = getwd(),
#   appPrimaryDoc = "UK gasworks map.R",
#   appMode = "shiny"
# )
