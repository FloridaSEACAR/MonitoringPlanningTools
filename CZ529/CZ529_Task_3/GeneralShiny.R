# CZ529 Task 2a
# Shiny app for all  managed areas and parameters


library(shiny)    # for shiny apps
library(plotly)
library(leaflet)  # renderLeaflet function
library(leaflet.extras)
library(tidyverse)
library(magrittr)
library(sf) # can use this to write gpx file
library(here) # where am I?
library(terra) # for raster (GeoTiff) reading 
library(spsurvey) # for grts

# FWC - Oyster Beds in Florida
reef_crosswalk_final = read.csv(here("./code/data/reef_crosswalk_final.csv"))

#all the data needed for a shiny UI

# shiny_store_shell_height = read_rds(here("./code/data/EBAP_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/EBAP_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/EBAP_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/EBAP_depth_map.tif")) # anything above sea-level is transparent
# Managed_Area_Title = shiny_store_shell_height$Managed_Area
# 

# shiny_store_shell_height = read_rds(here("./code/data/GRMAP_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GRMAP_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GRMAP_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GRMAP_depth_map.tif"))# anything above sea-level is transparent
# Managed_Area_Title = shiny_store_shell_height$Managed_Area

# # 
shiny_store_shell_height = read_rds(here("./code/data/ABAP_Shell_Height_shiny_store.RDS"))
shiny_store_percent_live = read_rds(here("./code/data/ABAP_Percent_Live_shiny_store.RDS"))
shiny_store_density = read_rds(here("./code/data/ABAP_Density_shiny_store.RDS"))
belowsealevel = terra::rast(here("./code/data/ABAP_depth_map.tif"))
Managed_Area_Title = shiny_store_shell_height$Managed_Area

# shiny_store_shell_height = read_rds(here("./code/data/ANERR_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/ANERR_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/ANERR_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/ANERR_depth_map.tif"))
# Managed_Area_Title = shiny_store_shell_height$Managed_Area


# 
# shiny_store_shell_height = read_rds(here("./code/data/IRVBFPAP_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/IRVBFPAP_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/IRVBFPAP_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/IRVBFPAP_depth_map.tif"))
# Managed_Area_Title = shiny_store_shell_height$Managed_Area

# 
# shiny_store_shell_height = read_rds(here("./code/data/LBAP_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/LBAP_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/LBAP_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/LBAP_depth_map.tif"))
# Managed_Area_Title = shiny_store_shell_height$Managed_Area

# # 
# shiny_store_shell_height = read_rds(here("./code/data/PISAP_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/PISAP_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/PISAP_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/PISAP_depth_map.tif"))
# Managed_Area_Title = shiny_store_shell_height$Managed_Area

# 
# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRGR_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRGR_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRGR_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRGR_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Guana River"

# 
# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRSR_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRSR_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRSR_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRSR_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Salt Run"

# 
# # 
# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRTR_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRTR_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRTR_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRTR_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Tolomato River"

# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRSA_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRSA_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRSA_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRSA_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve St Augustine"

# # 
# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRPF_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRPF_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRPF_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRPF_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Pellicer Flats"

# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRBB_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRBB_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRBB_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRBB_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Butler Beach"
# 

# shiny_store_shell_height = read_rds(here("./code/data/Guana_Tolomato_Matanzas_National_Estuarine_Research_Reserve_Butler_Beach_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/Guana_Tolomato_Matanzas_National_Estuarine_Research_Reserve_Butler_Beach_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/Guana_Tolomato_Matanzas_National_Estuarine_Research_Reserve_Butler_Beach_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/Guana_Tolomato_Matanzas_National_Estuarine_Research_Reserve_Butler_Beach_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Butler Beach"


# shiny_store_shell_height = read_rds(here("./code/data/GTMNERRFM_Shell_Height_shiny_store.RDS"))
# shiny_store_percent_live = read_rds(here("./code/data/GTMNERRFM_Percent_Live_shiny_store.RDS"))
# shiny_store_density = read_rds(here("./code/data/GTMNERRFM_Density_shiny_store.RDS"))
# belowsealevel = terra::rast(here("./code/data/GTMNERRFM_depth_map.tif"))
# Managed_Area_Title = "Guana Tolomato Matanzas National Estuarine Research Reserve Fort Matanzas"


mag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
    font-family: Arial, Helvetica, sans-serif;
    clear: none;
  }"))

plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        #legend.text.align = 0,
        theme(legend.text = element_text(hjust=0)),
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = -45, hjust = 0))


normalize = function(x){ (x - min(x))/(max(x)-min(x))}


Oyster_Beds_in_Managed_Area_depths = terra::extract(belowsealevel, shiny_store_shell_height$Oyster_Beds_in_Managed_Area, fun = "mean", na.rm = TRUE) %>% 
  setNames(c("ID","depth")) %>% mutate(depth = round(ifelse(is.nan(depth),0,depth),3))
Shapearea = round(shiny_store_shell_height$Oyster_Beds_in_Managed_Area$Shapearea, 3)

# this one is for labels
shiny_store_shell_height$Oyster_Beds_in_Managed_Area = cbind(shiny_store_shell_height$Oyster_Beds_in_Managed_Area,Oyster_Beds_in_Managed_Area_depths)

# this one is for site selection
shiny_store_shell_height$oyster.managed_area_parameter_positions_sd = cbind(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd,Oyster_Beds_in_Managed_Area_depths)
shiny_store_shell_height$oyster.managed_area_parameter_positions_sd = cbind(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd, Shapearea)

min_depth = min(shiny_store_shell_height$Oyster_Beds_in_Managed_Area$depth, na.rm = TRUE)
max_depth = max(shiny_store_shell_height$Oyster_Beds_in_Managed_Area$depth, na.rm = TRUE)

min_shape_area = round(min(shiny_store_shell_height$Oyster_Beds_in_Managed_Area$Shapearea, na.rm = TRUE),3)
max_shape_area = round(max(shiny_store_shell_height$Oyster_Beds_in_Managed_Area$Shapearea, na.rm = TRUE),3)

OysterReefLabels = function(OBMA ){
  return( OBMA %>% st_drop_geometry( )%>% dplyr::select(OBJECTID, SOURCEDATE, OYSTER, COMMENTS,Shapearea, depth) %>% 
            mutate( label = paste("<b>OBJECTID</b> &nbsp", OBJECTID,"<br><b>SOURCEDATE</b> &nbsp",SOURCEDATE
                                  ,"<br><b>OYSTER</b> &nbsp",OYSTER,"<br><b>COMMENTS</b> &nbsp",COMMENTS,"<br><b>Shapearea</b> &nbsp" ,Shapearea, "<br><b>depth</b> &nbsp" ,depth)) %>% 
            dplyr::select(label))
}

ORL = lapply(OysterReefLabels(shiny_store_shell_height$Oyster_Beds_in_Managed_Area)$label, htmltools::HTML) # Oyster Reef Labels



ui <- fluidPage(
  titlePanel(Managed_Area_Title),
  tags$div(
    includeMarkdown("Readme.md"),
    style = "
      border: 1px solid #ccc;
      padding: 15px;
      height: 200px;
      overflow-y: auto;
      background-color: #fafafa;
      font-family: Helvetica, Arial, sans-serif;
    "
  ),
  
  
  
  fluidRow(
    # First Column (width = 3 out of 12) with 3 stacked rows
    column(3,
           tags$div(
             style = "margin-top: 20px;",
             wellPanel(
               HTML("<a href='https://data.florida-seacar.org/programs/details/5059' target='_blank'>FWC - Oyster Beds in Florida</a>"),
               style = "
                 border: 1px solid #ccc;
                 padding: 6px;
                 background-color: #f9f9f9;
                 font-family: Helvetica, Arial, sans-serif;
                 white-space: normal;
                 overflow-y: auto;
                  height: 30px;
           ")
           ),
           
           div(
             style = "margin-bottom: 20px;",
             wellPanel(
               checkboxGroupInput(
                 inputId = "show_layers",
                 label = HTML("Select Layers to Show:<br>"),
                 choiceNames = list(
                   HTML("<span style='color:white;'>◉</span> Bathymetry"),
                   HTML("<span style='color:red;'>◉</span> Prior sample positions"),
                   HTML("<span style='color:black;'>◉</span> FWC oyster polygons"),
                   HTML("<span style='color:blue;'>◉</span> Shell height uncertainty on FWC oyster positions"),
                   HTML("<span style='color:purple;'>◉</span> Percent live uncertainty on FWC oyster positions"),
                   HTML("<span style='color:green;'>◉</span> Density uncertainty on FWC oyster positions"),
                   HTML("<span style='color:orange;'>◉</span> GRTS sample sites")
                 ),
                 choiceValues = c(
                   "Bathymetry",
                   "Prior sample positions",
                   "FWC oyster polygons",
                   "Shell height uncertainty on FWC oyster positions",
                   "Percent live uncertainty on FWC oyster positions",
                   "Density uncertainty on FWC oyster positions",
                   "GRTS sample sites"
                 ),
                 selected = "Prior sample positions"
               )
             )
           )
           , 
 
           
           div(
             wellPanel(
               
               actionButton("show_depth_dialog", "Choose depths to include"),
             )
           ),
           div(
             wellPanel(
               
               actionButton("show_area_dialog", "Choose reef areas to include"),
             )
           ),
           div(
             wellPanel(
               
               fileInput("file", "Choose CSV File for excluded sample points",
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               checkboxInput("header", "Header", TRUE),
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                            selected = ","),
               radioButtons("quote", "Quote",
                            choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                            selected = '"')
             )
           ),
           div(
             wellPanel(
               
               actionButton("show_dialog", "Enter Number of GRTS Samples"),
             )
           ),
           div(
             wellPanel(
               
               selectInput(
                 inputId = "file_format",
                 label = "Choose file format for GRTS sample sites:",
                 choices = c("CSV", "KML", "GPX"),
                 selected = "CSV"
               ),
               downloadButton("download_spatial", "Download File")
             )
           )
           
           
    ),
    column(2,
           div(
             style = "margin-top: 20px;",
             wellPanel(
               plotlyOutput("interactivePlotShellHeightSampleSize")
             )
           ),
           div(
             style = "margin-bottom: 2px;",
             wellPanel(
               plotlyOutput("interactivePlotPercentLiveSampleSize")
             )
           ),
           div(
             style = "margin-bottom: 2px;",
             wellPanel(
               plotlyOutput("interactivePlotDensitySampleSize")
             )
           )
           
    ),
    column(2,
           
           div(
             style = "margin-top: 20px;",
             wellPanel(
               plotlyOutput("interactivePlotShellHeightNumberSampleSites")
               
             )
           ),
           div(
             style = "margin-bottom: 2px;",
             wellPanel(
               plotlyOutput("interactivePlotPercentLiveNumberSampleSites")
             )
           )  ,
           div(
             style = "margin-bottom: 2px;",
             wellPanel(
               plotlyOutput("interactivePlotDensityNumberSampleSites")
             )
             
           )
    ),
    #  4th column for map
    column(5,
           div(
             style = "margin-top: 20px;",
             leafletOutput("myMap", height = 1300)
           )
    )
  )
)



server <- function(input, output, session) {
  number_val <- reactiveVal(NULL) # for GRTS Sample Size
  depth_range = reactiveValues(range = NULL)
  shape_area_range = reactiveValues(range = NULL)
  grts_positions <- reactiveValues(df = data.frame(X = numeric(), Y = numeric())) # will add OBJECTID
  exclude_these_points <- reactiveValues(data = NULL)
  number_excluded <- reactiveVal(0)
  number_legacy <- reactiveVal(nrow(shiny_store_shell_height$managed_area_parameter_sample_URI_positions))
  number_candidate <- reactiveVal(nrow(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd))
  
  output$myMap <- renderLeaflet({
    # CartoDB.PositronNoLabels
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(opacity = 0.5))    %>%
      #addProviderTiles(providers$USGS.USTopo, options = providerTileOptions(opacity = 0.5))    %>%
      addMapPane("background", zIndex = 400) %>%  addMapPane("foreground", zIndex = 500)  %>% 
      addScaleBar("bottomright") %>% 
      addControl(
        html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
        position = "bottomright",
        className = "map-title"
      ) %>% leaflet.extras::addFullscreenControl() # not sure this works as intended.
    
    if("Bathymetry"  %in% input$show_layers) {
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(belowsealevel),
                          na.color = "transparent") 
      
      m <-  m  %>% addRasterImage(belowsealevel, colors = pal, opacity = 1,options = pathOptions(pane = "background")) %>%  
        addLegend(pal = pal, values = values(belowsealevel), title = "Depth(m)", opacity = 1)
    }
    
    if ("FWC oyster polygons" %in% input$show_layers) {
      
      m <- m %>% addPolygons(data=shiny_store_shell_height$Oyster_Beds_in_Managed_Area
                             , color="black"
                             , weight = 1
                             , label = ORL
                             , labelOptions = labelOptions(interactive = TRUE)
                             , options = pathOptions(pane = "foreground")
      ) 
    }
    
    
    if ("Prior sample positions" %in% input$show_layers) {
      m <- m %>%
        addCircleMarkers(
          lng = shiny_store_shell_height$managed_area_parameter_sample_URI_positions$mean_lon,
          lat = shiny_store_shell_height$managed_area_parameter_sample_URI_positions$mean_lat,
          color = "red",
          radius = 5,
          label = lapply(shiny_store_shell_height$shiny_labels, htmltools::HTML),
          labelOptions = labelOptions(interactive = TRUE),
          options = pathOptions(pane = "foreground")
        )
    }
    
    
    if ("Shell height uncertainty on FWC oyster positions" %in% input$show_layers) {
      
      m <- m %>% 
        addCircleMarkers(
          lng = shiny_store_shell_height$oyster.managed_area_parameter_positions_sd$X,
          lat = shiny_store_shell_height$oyster.managed_area_parameter_positions_sd$Y,
          radius = 10*normalize(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd$sd), 
          color = "blue",
          label = "FWC Oyster Positions",
          labelOptions = labelOptions(interactive = TRUE),
          options = pathOptions(pane = "foreground")
        )
    }
    
    if ("Percent live uncertainty on FWC oyster positions" %in% input$show_layers) {
      
      m <- m %>% 
        addCircleMarkers(
          lng = shiny_store_percent_live$oyster.managed_area_parameter_positions_sd$X,
          lat = shiny_store_percent_live$oyster.managed_area_parameter_positions_sd$Y,
          radius = 10*normalize(shiny_store_percent_live$oyster.managed_area_parameter_positions_sd$sd), 
          color = "purple",
          label = "FWC Oyster Positions",
          labelOptions = labelOptions(interactive = TRUE),
          options = pathOptions(pane = "foreground")
        )
    }
    
    if ("Density uncertainty on FWC oyster positions" %in% input$show_layers) {
      
      m <- m %>% 
        addCircleMarkers(
          lng = shiny_store_density$oyster.managed_area_parameter_positions_sd$X,
          lat = shiny_store_density$oyster.managed_area_parameter_positions_sd$Y,
          radius = 10*normalize(shiny_store_density$oyster.managed_area_parameter_positions_sd$sd), 
          color = "green",
          label = "FWC Oyster Positions",
          labelOptions = labelOptions(interactive = TRUE),
          options = pathOptions(pane = "foreground")
        )
    }
    # if there are no GRTS sample sites yet, don't show anything
    if ("GRTS sample sites" %in% input$show_layers) {
      
      m <- m %>%
        addCircleMarkers(
          lng = grts_positions$df$X,
          lat = grts_positions$df$Y,
          radius = 5,
          color = "darkorange",
          label = "GRTS Position",
          labelOptions = labelOptions(interactive = TRUE),
          options = pathOptions(pane = "foreground")
        )
      
    }
    
    
    
    m <- m %>% addPolygons(data=shiny_store_shell_height$Managed_Area_sf, color="black", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0.1, options = pathOptions(pane = "background"))
    
  })
  output$interactivePlotShellHeightSampleSize <- renderPlotly({
    k = ifelse(length(unique(shiny_store_shell_height$sd_by_sample_size$sample_size)) <= 7, 1, 5)
    ggfig11 <- shiny_store_shell_height$sd_by_sample_size %>%
      ggplot(aes(x = sample_size, y = sd)) +    geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = k), se = FALSE) +
      ylab("sd") + xlab("sample size") + ggtitle("Shell Height") + plot_theme # theme(legend.position = "none") 
    
    gp <- ggplotly(ggfig11)
    
  })
  
  output$interactivePlotShellHeightNumberSampleSites <- renderPlotly({
    ggfig12 <- shiny_store_shell_height$sd_by_number_of_sites_no_posteriors %>%
      ggplot(aes(x = number_of_sites, y = sd)) +
      geom_point() +
      #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 5), se = FALSE) +
      geom_smooth( se = FALSE) +
      ylab("sd") + xlab("number of sites") + ggtitle("Shell Height") +  plot_theme# theme(legend.position = "none")
    
    gp <- ggplotly(ggfig12) 
    
  })
  
  output$interactivePlotPercentLiveSampleSize <- renderPlotly({
    k = ifelse(length(unique(shiny_store_percent_live$sd_by_sample_size$sample_size)) <= 7, 1, 5)
    ggfig21 <- shiny_store_percent_live$sd_by_sample_size %>%
      ggplot(aes(x = sample_size, y = sd)) + geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = k), se = FALSE) +
      ylab("sd") + xlab("sample size") + ggtitle("Percent Live") +  plot_theme #theme(legend.position = "none") 
    
    gp <- ggplotly(ggfig21)
    
  })
  
  output$interactivePlotPercentLiveNumberSampleSites <- renderPlotly({
    ggfig22 <- shiny_store_percent_live$sd_by_number_of_sites_no_posteriors %>%
      ggplot(aes(x = number_of_sites, y = sd)) +
      geom_point() +
      #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 5), se = FALSE) +
      geom_smooth( se = FALSE) +
      ylab("sd") + xlab("number of sites") + ggtitle("Percent Live") + plot_theme # theme(legend.position = "none")
    
    gp <- ggplotly(ggfig22) 
    
  })
  
  
  output$interactivePlotDensitySampleSize <- renderPlotly({
    k = ifelse(length(unique(shiny_store_density$sd_by_sample_size$sample_size)) <= 7, 1, 5)
    ggfig31 <- shiny_store_density$sd_by_sample_size %>%
      mutate(sd = 1000*sd) %>%
      ggplot(aes(x = sample_size, y = sd)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = k),se = FALSE) +
      ylab("sd") + xlab("sample size") + ggtitle("Density") +  plot_theme #theme(legend.position = "none") 
    
    gp <- ggplotly(ggfig31)
    
  })
  output$interactivePlotDensityNumberSampleSites <- renderPlotly({
    
    
    ggfig32 <- shiny_store_density$sd_by_number_of_sites_no_posteriors %>%
      mutate(sd = 1000*sd) %>%
      ggplot(aes(x = number_of_sites, y = sd)) +
      geom_point() +
      #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 2), se = FALSE) +
      geom_smooth( se = FALSE) +
      ylab("sd") + xlab("number of sites") + ggtitle("Density") +  plot_theme # theme(legend.position = "none")
    
    gp <- ggplotly(ggfig32) 
    
  })
  
  
  output$download_spatial <- downloadHandler(
    filename = function() {
      ext <- tolower(input$file_format)
      paste0("GRTS_Sample_Sites.", ifelse(ext == "csv", "csv", tolower(ext)))
    },
    content = function(file) {
      fmt <- input$file_format
      df = st_as_sf(grts_positions$df, coords = c("X","Y"), crs = st_crs("WGS84"))
      if (fmt == "CSV") {
        coords <- st_coordinates(df)
        df_out <- bind_cols(st_drop_geometry(df), 
                            tibble(lon = coords[,1], lat = coords[,2]))
        write.csv(df_out, file, row.names = FALSE)
        
      } else if (fmt == "KML") {
        st_write(df, file, driver = "KML", delete_dsn = TRUE,append=FALSE)
        
      } else if (fmt == "GPX") {
        write_sf(df, file, driver = "GPX", delete_dsn = TRUE,
                 dataset_options = "GPX_USE_EXTENSIONS=YES")
        
      }
    })
  
  observeEvent(input$file, {
    req(input$file)
    
    # Read the uploaded CSV file and store it
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    exclude_these_points$data <- df  # Store in reactiveValues
    number_excluded(nrow(df))
  })
  
  observeEvent(input$show_depth_dialog, {
    
    showModal(modalDialog(
      title = "Enter depth range to include for candidate sites",
      #numericInput("user_number", "Number:", value = NA),
      sliderInput("user_depth_number", label = "Depth range (m):", min = min_depth, max = max_depth, value = c(min_depth,max_depth)),
      
  
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_depth_number", "Submit")
      )
      
      
    ))
   
  })
  observeEvent(input$submit_depth_number, {
    print(input$user_depth_number) # logical - was a number entered?
    removeModal() # removes the dialog box
    depth_range$range = input$user_depth_number
   
    })
    
 
  observeEvent(input$show_area_dialog, {
    
    showModal(modalDialog(
      title = "Enter area range to include for candidate sites",
      #numericInput("user_number", "Number:", value = NA),
      sliderInput("user_area_number", label = "Area range (m2):", min = min_shape_area, max = max_shape_area, value = c(min_shape_area,max_shape_area)),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_area_number", "Submit")
      )
    ))
    
  })
  observeEvent(input$submit_area_number, {
    print(input$user_area_number) # logical - was a number entered?
    removeModal() # removes the dialog box
    shape_area_range$range = input$user_area_number
    # figure out how many sites are left
    
  })
  
  
  
  observeEvent(input$show_dialog, {
    
    showModal(modalDialog(
      title = "Please Enter GRTS Sample Size",
      #numericInput("user_number", "Number:", value = NA),
      #sliderInput("user_number", label = "Number:", min = 1, max = max(0,min(50,number_candidate()-number_legacy()-number_excluded())), value = 1),
      # allows legacy sites to be chosen.
      sliderInput("user_number", label = "Number:", min = 1, max = max(0,min(50,number_candidate()-number_excluded())), value = 1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_number", "Submit")
      )
    ))
    
  })
  
  observeEvent(input$submit_number, {
    number_val(input$user_number) # logical - was a number entered?
    removeModal() # removes the dialog box
    sample_size = number_val()
    print(sample_size)
    # grts
   
    # add some general measure of uncertainty for all three parameters combined
    # add a small amount in case there are zeros; grts fails
    shiny_store_shell_height$oyster.managed_area_parameter_positions_sd %<>%  mutate(uncertainty = 0.1 + (
       normalize(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd$sd)
       + normalize(shiny_store_percent_live$oyster.managed_area_parameter_positions_sd$sd)
       + normalize(shiny_store_density$oyster.managed_area_parameter_positions_sd$sd))/3
    )
    
  
      
   
    if( !is.null(exclude_these_points$data)){ # 
      names(exclude_these_points$data) = c("OBJECTID","X","Y") # whatever the names the user used, use X and Y
      excluded = inner_join(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd, exclude_these_points$data,by = join_by(OBJECTID), keep = FALSE)
      
      
      if(!is.null(excluded)){ # no overlap with oyster reefs in this manage area
        excluded <- excluded[,c(1:4)]
        names(excluded) = c("OBJECTID","X","Y","sd")
        
        candidate_sites = st_as_sf(dplyr::setdiff(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd
                                                  ,excluded), coords = c(2:3), crs =  st_crs("WGS84"))
      }
      else{
        candidate_sites = st_as_sf(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd, coords = c(2:3), crs =  st_crs("WGS84"))
        
      }
    }
    else{
      candidate_sites = st_as_sf(shiny_store_shell_height$oyster.managed_area_parameter_positions_sd, coords = c(2:3), crs =  st_crs("WGS84"))
    }
    
 
    
    legacy_sites = st_as_sf(shiny_store_shell_height$managed_area_parameter_sample_URI_positions, coords = c(2:3), crs =  st_crs("WGS84")) %>% mutate(uncertainty = 1)


    if(!is.null(depth_range$range)) { 
      candidate_sites <- candidate_sites %>% filter( depth_range$range[1] <= depth & depth <= depth_range$range[2])
      }
    
    if(!is.null(shape_area_range$range)) { 
      candidate_sites <- candidate_sites %>% filter( shape_area_range$range[1] <= Shapearea & Shapearea <= shape_area_range$range[2])
      
    }
    
    

    #grts_positions$df = inner_join(shiny_store_shell_height$managed_area_oyster_objectID_points,
    #                              as.data.frame(st_coordinates(grts(candidate_sites,n_base = sample_size + nrow(legacy_sites), projcrs_check = FALSE, aux_var = "uncertainty", legacy_sites = legacy_sites)$sites_base)))
   
     # does not use legacy sites
    grts_positions$df = inner_join(shiny_store_shell_height$managed_area_oyster_objectID_points,
                                                               as.data.frame(st_coordinates(grts(candidate_sites,n_base = sample_size, projcrs_check = FALSE, aux_var = "uncertainty")$sites_base)))
                                   
     grts_positions$df = grts_positions$df[sample(1:nrow(grts_positions$df)),] # randomize the output
    

    
  })
  
}


shinyApp(ui, server)

