library(shinycssloaders)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(FITfileR)
library(imputeTS)
library(slider)
library(changepoint)
library(highcharter)
library(leaflet)
library(shinythemes)

#increase max file size allowed 
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

ui <- fluidPage(
    theme = shinytheme("slate"),
    
    # Application title
    titlePanel("MyRide Interval Analyzer"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
                     fileInput(inputId = "file_upload",
                               label = h4("Upload a FIT file to get started"),
                               multiple = FALSE,
                               accept = c(".fit"),
                               placeholder = ".fit format only"),
                     radioButtons(inputId = "measure_select",
                                  label = h4("Choose what you want to see"),
                                  choices = c("heart_rate", "power", "cadence", "speed"),
                                  selected = "power"),
                     sliderInput(inputId = "slider_select",
                                 label = h4("If the blue line isn't lining up with your intervals, adjust the slider (Click 'About & Help' for more info)"),
                                 min = 0,
                                 max = 35,
                                 value = 15,
                                 step = 1),
                     shiny::actionButton(inputId = "about", label = "About & Help", width = "100%")
        ),
        
        # main page plots
        mainPanel(width = 10,
                  fluidPage(
                      tags$head(tags$style(HTML("a {color: steelblue}"))),
                      fluidRow(column(12, highchartOutput("changepoint_plot") %>% shinycssloaders::withSpinner(type = 1))),
                      fluidRow(column(12, highchartOutput("overlay_plot") %>% shinycssloaders::withSpinner(type = 1))),
                      fluidRow(column(12, highchartOutput("histogram") %>% shinycssloaders::withSpinner(type = 1))),
                      fluidRow(column(12, leafletOutput("map") %>% shinycssloaders::withSpinner(type = 1)))
                  )
        )
    )
)

server <- function(input, output) {
    
    source("util.R")
    
    # get values based on user upload
    parsed_fit_file <- reactive({
        
        validate(need(input$file_upload != "", "Upload a .fit file to get started"))
        
        infile <- input$file_upload
        
        parse_fit_file(infile$datapath)
        
    })
    
    selected_value <- reactive({
        get_specific_value(parsed_fit_file(), input$measure_select)
    })
    
    changes <- reactive({
        get_changepoints(selected_value(), sensitivity = input$slider_select)
    })
    
    color <- reactive({
        
        colors <- tibble(value = c("cadence", "heart_rate", "power", "speed"),
                         color = c("#66D9EF", "#F92672", '#A6E22E', '#AE81FF')
        ) %>%
            filter(value == input$measure_select) %>%
            pull(color)
        
    })
    
    normalized_power <- reactive({
        get_normalized_power(
            get_specific_value(parsed_fit_file(), "power"))
    })
    
    ######################################
    
    # main plots
    output$changepoint_plot <- renderHighchart({
        
        validate(
            need(sum(selected_value()$metric_avg, na.rm = T) > 0,
                 paste("There's no", input$measure_select, "data to plot :( Please select another option on the sidebar", sep = " "))
        )
        
        normalized_power_label <- if (input$measure_select == "power") { normalized_power() } else { "Not Applicable" }
        
        subtitle <- if (input$measure_select == "power") {
            paste("Overall Avg:", round(mean(changes()$metric_avg), 2), " | Normalized:", round(mean(normalized_power_label), 2))
        } else { paste("Overall Avg:", round(mean(changes()$metric_avg), 2)) }
        
        hchart(changes(), "line", hcaes(x = datetime_to_timestamp(time), y = round(metric_avg, 0)), color = color(), showInLegend = F, name = input$measure_select) %>%
            hc_add_series(changes(), "line", hcaes(x = datetime_to_timestamp(time), y = round(mean)), lineWidth = 5,
                          color = "#1E90FF", showInLegend = T, name = "Region Average") %>%
            hc_add_theme(hc_theme_monokai()) %>%
            hc_tooltip(shared = T) %>%
            hc_title(text = paste("Changepoint Analysis:", input$measure_select),
                     align = "left") %>%
            hc_subtitle(text = subtitle,
                        align = "left") %>%
            hc_xAxis(title = list(text = "time"),
                     type = "datetime"
            ) %>%
            hc_chart(zoomType = "x")
    })
    output$overlay_plot <- renderHighchart({
        
        validate(need(input$file_upload != "", " "))
        
        my_vars <- c("time", "cadence", "heart_rate", "power", "speed")
        
        cols <- c("#66D9EF", "#F92672", '#A6E22E', '#AE81FF')
        
        parsed_fit_file() %>%
            mutate(timestamp = ymd_hms(timestamp) - hours(5),
                   time = floor_date(timestamp, "1 second")) %>%
            arrange(time) %>%
            select(one_of(my_vars)) %>%
            pivot_longer(cols = c("cadence", "heart_rate", "power", "speed")) %>%
            hchart(., "line", hcaes(x = datetime_to_timestamp(time), y = round(value, 0), group = name), showInLegend = T) %>%
            hc_colors(cols) %>%
            hc_add_theme(hc_theme_monokai()) %>%
            hc_tooltip(shared = T) %>%
            hc_title(text = "Data Overlay",
                     align = "left") %>%
            hc_xAxis(title = list(text = "time"),
                     type = "datetime"
            ) %>%
            hc_yAxis(title = list(text = " ")) %>%
            hc_chart(zoomType = "x")
    })
    output$histogram <- renderHighchart({
        
        validate(need(input$file_upload != "", " "))
        
        validate(
            need(sum(selected_value()$metric_avg, na.rm = T) > 0,
                 paste("There's no", input$measure_select, "data to plot :( Please select another option on the sidebar", sep = " "))
        )
        
        hchart(selected_value()$metric_avg, color = color()) %>%
            hc_add_theme(hc_theme_monokai()) %>%
            hc_title(text = input$measure_select,
                     align = "left") %>%
            hc_xAxis(title = list(text = input$measure_select)) %>%
            hc_legend(enabled = FALSE)
        
    })
    output$map <- renderLeaflet({
        
        validate(need(input$file_upload != "", " "))
        
        validate(
            need(sum(parsed_fit_file()$position_lat, na.rm = T) > 0,
                 "")
        )
        
        coords <- parsed_fit_file() %>%
            select(starts_with("position"))
        
        leaflet(coords) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            setView(lng = mean(coords$position_long, na.rm = T),
                    lat = mean(coords$position_lat, na.rm = T),
                    zoom = 11) %>%
            leaflet::addPolylines(~position_long,
                                  ~ position_lat,
                                  weight = 4,
                                  color = "#A6E22E",
                                  stroke = TRUE,
                                  fillOpacity = 0)
        
    })
    
    ###################
    
    # About & Help modal
    observeEvent(input$about, {
        show_alert(
            title = "MyRide",
            type = "info",
            text = tags$span(
                tags$h3("About",
                        style = "color: steelblue;"),
                "The app will display power, heart rate, cadence, or speed of your ride.",
                "If any intervals are detected, this app will also display the", tags$em("average"), "power, heart rate, cadence, or speed in these intervals.",
                tags$br(),
                tags$em("note that large files take longer to process."),
                
                tags$h3("Help",
                        style = "color: red;"),
                tags$b("What is this app for? Why should I use this instead of something like Strava, Training Peaks, or Garmin Connect?"),
                tags$br(),
                "The free versions of these apps aren't very helpful for analyzing intervals. 
                While they can tell you your overall average power, heart rate or cadence for a ride it's harder if not impossible to get at 
                the average during an interval or hard effort, which is probably what you care about more so than the overall average",
                tags$br(),
                tags$br(),
                tags$b("How do I use the chart? What are the green and blue lines telling me?"),
                tags$br(),
                "Let's say you want to analyze your power during a recent training ride. 
                You upload a file, and select 'Power and a chart appears. 
                The Green Line is showing you your average power at each minute of your ride.
                The Blue Line is detects changes in your effort and tells you the average power within each detected change point. 
                It is a result of a",
                tags$a(href = "http://members.cbio.mines-paristech.fr/~thocking/change-tutorial/RK-CptWorkshop.html", "changepoint detection algorithm"),
                "the sensitivity of which you can control with the slider in the sidebar.",
                tags$br(),
                tags$br(),
                tags$b("Ok but what if the Blue Line isn't lining up with my intervals?"),
                tags$br(),
                "The Blue Line tries to detect changes automatically, but can sometimes miss them. 
                The good news is you can adjust the number of changepoints to search for with the slider - the lower the number the less sensitive 
                and the higher the number the more sensitive it becomes. The goal is to strike a balance, where the blue line changes in step with 
                your your intervals but doesn't change too much to the point where it reacts to every small fluctuation within an interval.
                Usually setting it somewhere between 10 and 20 should do the trick."
            ),
            html = FALSE,
            width = "60%")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
