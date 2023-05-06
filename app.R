# load in packages
library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)
library(ggplot2)
library(shinyjs)
library(lubridate)
library(dplyr)
library(shinythemes)
library(rsconnect)
library(ggpmisc)


# data read in and organization
# read discharge data
dischargeDat <- read_csv("SiteDischarge_Summary.csv")
dischargeDat <- dischargeDat %>% 
  rename(`MS1` = "site1DischargeMean",
         `MS2` = "site2DischargeMean",
         `MS3` = "site3DischargeMean") %>%
  pivot_longer(cols = c(`MS1`, `MS2`, `MS3`),
               names_to = "Site", values_to = "meanDischarge") %>%
  select(date_time, Site, meanDischarge)

# read drip data
dripDat <- read_csv("merged_drip_mm.csv")
dripDat <- dripDat %>% 
  rename(`MS1` = "Drips_mm_site1",
         `MS2` = "Drips_mm_site2",
         `MS3` = "Drips_mm_site3",
         `date_time` = "Datetime") %>%
  pivot_longer(cols = c(`MS1`, `MS2`, `MS3`),
               names_to = "Site", values_to = "meanDrip") %>%
  select(date_time, Site, meanDrip)

# read in conductance data
condDat <- read_csv("cond_summary.csv")
condDat <- condDat %>%
  rename(`MS1` = "cond_site1_avg",
         `MS2` = "cond_site2_avg",
         `MS3` = "cond_site3_avg",
         `avgStreamCond` = "cond_stream_avg") %>%
  pivot_longer(cols = c(`MS1`, `MS2`, `MS3`),
               names_to = "Site", values_to = "avgCond") %>%
  select(date_time, Site, avgCond, avgStreamCond)

# read in precipitation data
precipDat <- read_csv("DublinPrecip_hourly.csv") 
precipDat <- precipDat %>%
  mutate(site1 = HourlyPrecipitation,
         site2 = HourlyPrecipitation,
         site3 = HourlyPrecipitation) %>%
  rename(`MS1` = "site1",
         `MS2` = "site2",
         `MS3` = "site3") %>%
  pivot_longer(cols = c(`MS1`, `MS2`, `MS3`),
               names_to = "Site", values_to = "hourlyPrecip") %>%
  select(date_time, Site, hourlyPrecip)

# read in temp data
tempDat <- read_csv("temp_summary.csv")
tempDat <- tempDat %>% 
  rename(`MS1` = "temp_cel_ms1",
         `MS2` = "temp_cel_ms2",
         `MS3` = "temp_cel_ms3",
         `Surface` = "temp_cel") %>%
  pivot_longer(cols = c(`MS1`, `MS2`, `MS3`, `Surface`),
               names_to = "Site", values_to = "temp") %>%
  select(date_time, Site, temp)


# merge datasets - FULL DATASET
disdripDat <- full_join(dischargeDat, unique(dripDat), by = c("date_time", "Site"))
condprecipDat <- full_join(precipDat, unique(condDat), by = c("date_time", "Site"))
caveData <- full_join(disdripDat, unique(condprecipDat))
caveData <- full_join(caveData, unique(tempDat), by = c("date_time", "Site"))

# mutate cave and precip data to add month, day, and year for season filter
caveData <- caveData %>%
  mutate(monthDay = as.numeric(format(date_time, "%m%d")))

precipDat <- precipDat %>%
  mutate(monthDay = as.numeric(format(date_time, "%m%d")))

#### UI ####
ui <- fluidPage(
  tags$head(tags$style(HTML('*{font-family: times;}'))),
  useShinyjs(),
  theme = shinytheme("darkly"),
  navbarPage("James Cave Data Collection",
    
    # information tab
    tabPanel("Information",
             titlePanel(div(strong("James Cave Background Information"),align = "center", style = "font-family: 'times'; font-size:30px;")),
             hr(),
             br(),
             sidebarLayout(
               sidebarPanel(h3(strong("Location"), style = "font-family: 'times'; font-si35pt"),
                            p("The data used was collected from 3 sites located
                              within James Cave in Pulaski, VA.,", style = "font-family:
                              'times'; font-si16pt"),
                            p("The land use of the area above the cave is pasture
                              land, and its geological formation is conococheague
                              limestone.", style = "font-family: 'times'; font-si16pt"),
                            br(),
                            h3(strong("References"), style = "font-family: 'times'; font-si35pt"),
                            h4(strong("Papers"), style = "font-family: 'times'; font-si12pt"),
                            p(" Groce-Wright NC*, Benton JR*, Hammond NW*, Schreiber ME. 2022.  A decade of cave drip hydrographs shows spatial and temporal variability in epikarst storage and recharge to Appalachian karst systems. Hydrology 9, 131. DOI: 10.3390/hydrology9080131", style = "font-family: 'times'; font-si16pt"),
                            p("Eagle S, Orndorff W, Schwartz B, Doctor D, Gerst J*, Schreiber ME.  2016.  Analysis of hydrologic and geochemical time series data at James Cave, Virginia: Implications for epikarst influence on recharge in Appalachian karst aquifers. Geological Society of America Special Paper 516. ", style = "font-family: 'times'; font-si16pt"),
                            p("Schreiber M, Schwartz B, Orndorff W, Doctor D, Eagle S*, Gerst J*.  2015. Instrumenting caves to collect hydrologic and geochemical data:  Example from James Cave, Virginia, in Younos T and Parece T (eds).  Advances in Watershed Science and Assessment, Handbook of Environmental Chemistry 33: 205-231 Springer. ", style = "font-family: 'times'; font-si16pt"),
                            h4(strong("Datasets"), style = "font-family: 'times'; font-si12pt"),
                            p("Groce-Wright, N.C.*; Eagle, S.D; Gerst, J.D.; Orndorff, W.; Malabad, T; Ficco, K; Schwartz, B; Junod, M**; Schreiber, ME. 2021. James Cave Epikarst Monitoring Drip Data: 2007-2018. University Libraries, Virginia Tech. Dataset. https://doi.org/10.7294/14992041.", style = "font-family: 'times'; font-si16pt"),
                            p("Eagle SD*, Schreiber ME.  2013. James Cave Epikarst Monitoring Drip Data, Virginia Tech VTechWorks http://hdl.handle.net/10919/22024.", style = "font-family: 'times'; font-si16pt")),
               mainPanel(h3(strong("Data Collection"), style = "font-family: 'times'; font-si35pt"),
                         p("The data was collected every ten minutes from a tarp
                           hanging below speleothems which funnel the water into
                           the recorder.", style = "font-family: 'times'; font-si16pt"),
                         p("Data on the drip rate, drip amount, and specific
                           conductance of the water was recorded at all 3 sites.",
                           style = "font-family: 'times'; font-si16pt"),
                         p("The first data was collected in September 2007, and
                           data collection ended August 2019.", style = "font-family:
                           'times'; font-si16pt"),
                         br(),
                         h3(strong("Description of Variables:"), style = "font-family: 'times'; font-si35pt"),
                         p(strong("meanDischarge"), "- hourly drip rate of the water, measured in milliliters per minute", style = "font-family: 'times'; font-si16pt"),
                         p(strong("avgCond"), "- hourly conductance or the amount of ions present in the water, measured in microsiemens per centimeter (µs/cm)", style = "font-family: 'times'; font-si16pt"),
                         p(strong("temp"), "- temperature of the sites in the cave, measured in °C", style = "font-family: 'times'; font-si16pt"),
                         p(strong("hourlyPrecip"), "- hourly precipitation in the area near the cave, measured in mm", style = "font-family: 'times'; font-si16pt"),
                         p(strong("date_time"), "- date and time by hour", style = "font-family: 'times'; font-si16pt"),
                         p(strong("Site"), "- labels the variables by which site they represent, ex. 'MS1', 'MS2', 'MS3'", style = "font-family: 'times'; font-si16pt"),
                         tags$img(src = "SiteLocations.png", height = "400px",
                                  width = "550px", alt = "Something went wrong",
                                  deleteFile= FALSE),
                         br(),
                         br(),
                         tags$img(src = "PulLoc.png", height = "150px",
                                  width = "275px", alt = "Something went wrong",
                                  deleteFile= FALSE),
                         tags$img(src = "VALoc.jpg", height = "150px",
                                  width = "275px", alt = "Something went wrong",
                                  deleteFile= FALSE),
                         p("(Eagle et al., 2016)", style = "font-family: 'times'; font-si16pt")
               ))),
    
    
    # page and inputs
    tabPanel("Time Series",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("siteInput", "Site to graph:",
                                    choices = c("MS1", "MS2", "MS3", "Surface"),
                                    selected = "MS1"),
                 div(dateRangeInput("dateInput", "Dates", start = "2008-02-22", end = "2018-08-08", min = "2008-02-22",
                                    max = "2018-08-08")),
                 checkboxGroupInput("selectY", "Select data to show:",
                                    choices = c("Discharge", "Specific Conductance", "Temperature",
                                                "Precipitation"),
                                    selected = c("Discharge", "Specific Conductance", "Temperature",
                                                 "Precipitation")),
                 selectInput("seasonInput", "Select a season to view data", choices = c("None","Winter", "Spring", "Summer", "Fall")),
                 actionButton("resetDates", "Reset Dates"),
                 p("In order to zoom in on a plot, click and drag over an area of the plot then double-click inside that area of the plot. 
                   To zoom back out, just double-click anywhere inside of the plot.",
                   style = "font-family: 'times'; font-si20pt")
               ),
               
               mainPanel(
                 titlePanel(div(strong("James Cave Data Graphs"), align = "center", style = "font-family: 'times'; font-size:30px;")),
                 hr(),
                 conditionalPanel(condition = "($.inArray('Discharge', input.selectY) != -1)",
                                  plotOutput("dischargePlot", height = "300px", dblclick = "dischargePlot_dblclick", 
                                             brush = brushOpts(id = "dischargePlot_brush",  resetOnNew = TRUE))),
                 conditionalPanel(condition = "($.inArray('Specific Conductance', input.selectY) != -1)",
                                  plotOutput("condPlot", height = "300px", dblclick = "condPlot_dblclick", 
                                             brush = brushOpts(id = "condPlot_brush", resetOnNew = TRUE))),
                 conditionalPanel(condition = "($.inArray('Temperature', input.selectY) != -1)",
                                  plotOutput("tempPlot", height = "300px", dblclick = "tempPlot_dblclick", 
                                             brush = brushOpts(id = "tempPlot_brush", resetOnNew = TRUE))),
                 conditionalPanel(condition = "($.inArray('Precipitation', input.selectY) != -1)",
                                  plotOutput("precipPlot", height = "300px", dblclick = "precipPlot_dblclick", 
                                             brush = brushOpts(id = "precipPlot_brush", resetOnNew = TRUE)))
               ),
             )),
    tabPanel("Bivariate",
             sidebarLayout(
               sidebarPanel(
                 div(dateRangeInput("dateInputbi", "Dates", start = "2008-02-22", end = "2018-08-08", min = "2008-02-22",
                                    max = "2018-08-08")),
                 selectInput("seasonInputbi", "Select a season to view data", choices = c("None","Winter", "Spring", "Summer", "Fall")),
                 selectInput("selectYbi", "Select to choose your Y variable", choices = c("meanDischarge", "hourlyPrecip", "avgCond", "temp")),
                 selectInput("selectXbi", "Select to choose your X variable", choices = c("meanDischarge","hourlyPrecip", "avgCond", "temp")),
                 checkboxInput("showBestFit", "Show Line of Best Fit", value = FALSE),
                 actionButton("resetDatesbi", "Reset Dates"), 
                 p("In order to zoom in on a plot, click and drag over an area of the plot. 
                   To zoom back out, just double-click anywhere inside of the plot.",
                   style = "font-family: 'times'; font-si20pt")
               ),
               
               mainPanel(
                 titlePanel(div(strong("Bivariate Plot"), align = "center", style = "font-family: 'times'; font-size:30px;")),
                 hr(),
                 plotOutput("bivariatePlot", height = "750px", width = "750px", 
                            dblclick = "biPlot_dblclick", brush = brushOpts(id = "biPlot_brush", resetOnNew = TRUE))
               ),
             ))))




#### SERVER ####     
server <- function(input, output) {
  
  #Used to help brush line plots
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #used to help brushing on bivariate plot
  zoomed_data <- reactiveVal()
  
  
  
  
  
  # reactive data  
  # discharge, drip, and conductance data
  dataReactive <- reactive({
    
    if(input$seasonInput == "None"){
      filter(caveData, Site %in% input$siteInput,
             date_time >= input$dateInput[1], 
             date_time <=input$dateInput[2])
    }
    else if(input$seasonInput == "Winter"){
      filter(caveData, Site %in% input$siteInput,
             monthDay <= 321 | (monthDay >= 1221 & monthDay <= 1231))
    }
    else if(input$seasonInput == "Spring"){
      filter(caveData, Site %in% input$siteInput, 
             between(monthDay, 321, 620))
    }
    else if(input$seasonInput == "Summer"){
      filter(caveData, Site %in% input$siteInput, 
             between(monthDay, 620, 922))
    }
    else if(input$seasonInput == "Fall"){
      filter(caveData, Site %in% input$siteInput, 
             between(monthDay, 922, 1220))
      
    }
  })
  
  # precipitation reactive data (without site selection)
  precipReactive <- reactive({
    
    if(input$seasonInput == "None"){
      filter(precipDat,
             date_time >= input$dateInput[1], 
             date_time <=input$dateInput[2])
    }
    else if(input$seasonInput == "Winter"){
      filter(precipDat,
             monthDay <= 321 | (monthDay >= 1221 & monthDay <= 1231))
    }
    else if(input$seasonInput == "Spring"){
      filter(precipDat,
             between(monthDay, 321, 620))
    }
    else if(input$seasonInput == "Summer"){
      filter(precipDat,
             between(monthDay, 620, 922))
    }
    else if(input$seasonInput == "Fall"){
      filter(precipDat, 
             between(monthDay, 922, 1220))
      
    }
    
    
  })
  
  #Reactive for Bivariate
  
  biReactive <- reactive({
    
    if(input$seasonInputbi == "None"){
      filter(caveData,
             date_time >= input$dateInputbi[1], 
             date_time <=input$dateInputbi[2])
    }
    else if(input$seasonInputbi == "Winter"){
      filter(caveData,
             monthDay <= 321 | (monthDay >= 1221 & monthDay <= 1231))
    }
    else if(input$seasonInputbi == "Spring"){
      filter(caveData,
             between(monthDay, 321, 620))
    }
    else if(input$seasonInputbi == "Summer"){
      filter(caveData,
             between(monthDay, 620, 922))
    }
    else if(input$seasonInputbi == "Fall"){
      filter(caveData, 
             between(monthDay, 922, 1220))
      
    }
    
    
  })
  
  
  # plot outputs
  
  #Drip Discharge Plot
  
  output$dischargePlot <- renderPlot({
    
    if ("Discharge" %in% input$selectY) {
      if (!is.null(ranges$x)) {
        ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
      }
      dischargePlot <- ggplot(dataReactive(), aes(x=date_time, y = meanDischarge, color = Site)) +
        geom_line() + 
        theme_classic() +
        scale_color_manual(values = c("#353436", "#1b98e0", "#5FB404", "#cc5500")) +
        labs(x = "", y = "Drip Discharge (mL/min)") +
        theme(text = element_text(size = 15, family = "Times")) +
        coord_cartesian(xlim = ranges$x, ylim = discharge_ranges$y, expand = FALSE)
      return(dischargePlot)
    } else {
      return(ggplot() + theme_void())
    }
  })
  
  #Conductance Plot
  
  output$condPlot <- renderPlot({
    
    if ("Specific Conductance" %in% input$selectY) {
      if (!is.null(ranges$x)) {
        ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
      }
      condPlot <- ggplot(dataReactive(), aes(x=date_time, y = avgCond, color = Site)) +
        geom_line() + 
        theme_classic() +
        scale_color_manual(values = c("#353436", "#1b98e0", "#5FB404", "#cc5500")) +
        labs(x = "", y = "Specific Conductance (μS/cm)") +
        theme(text = element_text(size = 15, family = "Times")) +
        coord_cartesian(xlim = ranges$x, ylim = cond_ranges$y, expand = FALSE)
      return(condPlot)
    } else {
      return(ggplot() + theme_void())
    }
  })
  
  #Temperature Plot
  
  output$tempPlot <- renderPlot({
    
    if ("Temperature" %in% input$selectY) {
      if (!is.null(ranges$x)) {
        ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
      }
      tempPlot <- ggplot(dataReactive(), aes(x=date_time, y = temp, color = Site)) +
        geom_line() + 
        theme_classic() +
        scale_color_manual(values = c("#353436", "#1b98e0", "#5FB404", "#cc5500")) +
        labs(x = "", y = "Temperature (°C)") +
        theme(text=element_text(size=15,  family="Times")) +
        coord_cartesian(xlim = ranges$x, ylim = cond_ranges$y, expand = FALSE)
      return(tempPlot)
    } else {
      return(ggplot() + theme_void())
    }
  })
  
  #Precipitation Plot
  
  output$precipPlot <- renderPlot({
    
    if ("Precipitation" %in% input$selectY) {
      if (!is.null(ranges$x)) {
        ranges$x <- as.POSIXct(ranges$x, origin = "1970-01-01")
      }
      precipPlot <- ggplot(precipReactive(), aes(x = date_time, y = hourlyPrecip, color = "Precip")) +
        geom_line() +
        theme_classic() +
        scale_color_manual(values = c("#353436", "#1b98e0", "#5FB404", "#cc5500")) +
        labs(x = "", y = "Hourly Precipitation (mm)") +
        theme(text = element_text(size = 15, family = "Times")) +
        coord_cartesian(xlim = ranges$x, ylim = precip_ranges$y, expand = FALSE)
      return(precipPlot)
    } else {
      return(ggplot() + theme_void())
    }
  }) 
  
  
  # Create a reactive expression for the title
  title <- reactive({
    paste("The Relationship Between", input$selectXbi, "and", input$selectYbi)
  })
  
  
  #  bivariate plot output
  output$bivariatePlot <- renderPlot({
    data <- if (is.null(zoomed_data())) biReactive() else zoomed_data()
    
    data <- data %>%
      filter(Site != "Surface")
    
    bivariate_plot <- ggplot(data, aes(x= get(paste(input$selectXbi)), y = get(paste(input$selectYbi)), color = Site)) +
      geom_point() + 
      theme_classic() +
      labs(x = input$selectXbi, y = input$selectYbi) +
      theme(text = element_text(size = 20, family = "Times")) +
      scale_color_manual(values = c("#353436", "#1b98e0", "#5FB404")) +
      ggtitle(title())
    if (input$showBestFit) {
      bivariate_plot <- bivariate_plot +
        stat_poly_line() + 
        stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                       after_stat(rr.label), sep = "*\", \"*")))
    }
    
    return(bivariate_plot)
  })
  
  
  
  
  
  # Reactive values for each plot's y-axis range
  discharge_ranges <- reactiveValues(y = NULL)
  drip_ranges <- reactiveValues(y = NULL)
  cond_ranges <- reactiveValues(y = NULL)
  precip_ranges <- reactiveValues(y = NULL)
  temp_ranges <- reactiveValues(y = NULL)
  
  # Update discharge_ranges in the brush event observer
  observeEvent(input$dischargePlot_dblclick, {
    brush <- input$dischargePlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      discharge_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      discharge_ranges$y <- NULL
    }
  })
  
  # Update drip_ranges in the brush event observer
  observeEvent(input$dripPlot_dblclick, {
    brush <- input$dripPlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      drip_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      drip_ranges$y <- NULL
    }
  })
  
  # Update cond_ranges in the brush event observer
  observeEvent(input$condPlot_dblclick, {
    brush <- input$condPlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      cond_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      cond_ranges$y <- NULL
    }
  })
  
  # Update temp_ranges in the brush event observer
  observeEvent(input$tempPlot_dblclick, {
    brush <- input$tempPlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      cond_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      cond_ranges$y <- NULL
    }
  })
  
  
  # Update precip_ranges in the brush event observer
  observeEvent(input$precipPlot_dblclick, {
    brush <- input$precipPlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      precip_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      precip_ranges$y <- NULL
    }
  })
  
  # Update bivariate plot 
  observeEvent(c(input$biPlot_dblclick, input$biPlot_brush), {
    # Check if the brush is empty (i.e., no selection)
    if (is.null(input$biPlot_brush)) {
      # If the brush is empty, reset the data to the original dataset
      zoomed_data(NULL)
    } else {
      # If the brush is not empty, subset the data based on the brushed area
      brushed_data <- biReactive() %>%
        dplyr::filter(get(input$selectXbi) >= input$biPlot_brush$xmin,
                      get(input$selectXbi) <= input$biPlot_brush$xmax,
                      get(input$selectYbi) >= input$biPlot_brush$ymin,
                      get(input$selectYbi) <= input$biPlot_brush$ymax)
      
      # Update the zoomed_data reactive value with the new subset
      zoomed_data(brushed_data)
    }
  })
  
  
  
  # hide date box and reset button based on season chosen
  observeEvent(input$seasonInput, {
    if(input$seasonInput == "None"){
      shinyjs::show("dateInput") +
        shinyjs::show("resetDates")
    }else{
      shinyjs::hide("dateInput") +
        shinyjs::hide("resetDates")
    }
  })
  
  observeEvent(input$seasonInputbi, {
    if(input$seasonInputbi == "None"){
      shinyjs::show("dateInputbi") +
        shinyjs::show("resetDatesbi")
    }else{
      shinyjs::hide("dateInputbi") +
        shinyjs::hide("resetDatesbi")
    }
  })
  
  
  
  
  
  
  
  # reset date input
  observeEvent(input$resetDates, reset("dateInput"))
  observeEvent(input$resetDatesbi, reset("dateInputbi"))
  
}

shinyApp(ui=ui, server=server)