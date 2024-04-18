library(shiny)
#
ui <- tagList(
  tags$head(tags$script(type = "text/javascript", src = "code.js")),
  navbarPage(
    theme = shinytheme("cosmo"), "Passaic River Nitrate Study", # footer = "Created by David Rosenman",

    tabPanel(
      "USGS Nitrate Data",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Nitrate Data from USGS 01389005:"),
          h4("Passaic River below Pompton River at Two Bridges NJ"),
          conditionalPanel(
            "input.main_panel == 'Dataset'",
            downloadButton("download_dataset", label = "Download Dataset")
          ),
          conditionalPanel(
            "input.main_panel == 'Scatter Plot'",
            dateRangeInput(
              inputId = "scatter_date_range", label = "Date Range", start = "2017-01-01", end = "2019-11-21",
              min = "2009-07-29", max = "2019-11-21"
            ),
            sliderInput("point_size", "Point Size", min = 0.1, max = 2, value = 1, step = 0.1),
            checkboxInput(inputId = "add_smoothing_line", label = "Add Spline Curve"),
            checkboxInput(inputId = "add_best_fit_line", label = "Add Regression Line"),
            uiOutput("usgs_plot_download_button")
          ),
          conditionalPanel(
            "input.main_panel == 'Annual Range Plot' || input.main_panel == 'Hourly Range Plot' || input.main_panel == 'Monthly Comparison Range Plot' || input.main_panel == 'Monthly Trend Range Plot'",
            selectInput(inputId = "location", "Select Location", choices = c("Passaic", "Pompton"), selected = "Passaic"),
            pickerInput(
              inputId = "range_years", "Select Year(s)",
              choices = nitrate_data_years,
              multiple = TRUE, selected = nitrate_data_years,
              options = list(`actions-box` = TRUE)
            )
          ),
          conditionalPanel(
            "input.main_panel == 'Monthly Comparison Range Plot'",
            splitLayout(
              downloadButton("download_monthly_range_data", label = "Download Summary Table"),
              downloadButton("download_usgs_monthly", "Download Plot")
            )
          ),
          conditionalPanel(
            "input.main_panel == 'Hourly Range Plot'",
            splitLayout(
              downloadButton("download_hourly_range_data", label = "Download Summary Table"),
              downloadButton("download_usgs_hourly", label = "Download Plot")
            )
          ),
          conditionalPanel(
            "input.main_panel == 'Annual Range Plot'",
            splitLayout(downloadButton("download_annual_range_data", label = "Download Summary Table"), uiOutput("usgs_annual_plot_download_button"))
          ),
          conditionalPanel(
            "input.main_panel == 'Monthly Trend Range Plot'",
            selectInput(inputId = "month", "Select Month", choices = c(
              "January" = "1", "February" = "2",
              "March" = "3", "April" = "4", "May" = "5", "June" = "6", "July" = "7",
              "August" = "8", "September" = "9", "October" = "10", "November" = "11", "December" = "12"
            )),
            splitLayout(
              downloadButton("download_monthly_trend_range_data", label = "Download Summary Table"),
              downloadButton("download_usgs_monthly_trend", label = "Download Plot")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "main_panel",
            tabPanel(
              "Scatter Plot",
              withSpinner(plotOutput("display_scatter_plot", height = 900))
            ),
            tabPanel(
              "Annual Range Plot",
              withSpinner(plotOutput("display_annual_range_plot")),
              dataTableOutput("display_annual_range_data")
            ),
            tabPanel(
              "Monthly Comparison Range Plot",
              withSpinner(plotOutput("display_monthly_range_plot")),
              withSpinner(dataTableOutput("display_monthly_range_data"))
            ),
            tabPanel(
              "Monthly Trend Range Plot",
              withSpinner(plotOutput("display_monthly_trend_range_plot")),
              withSpinner(dataTableOutput("display_monthly_trend_range_data"))
            ),
            tabPanel(
              "Hourly Range Plot",
              withSpinner(plotOutput("display_hourly_range_plot")),
              withSpinner(dataTableOutput("display_hourly_range_data"))
            ),
            tabPanel(
              "Dataset",
              withSpinner(DT::dataTableOutput("nitrate_dataset"))
            )
          )
        )
      )
    ),
    tabPanel(
      "Discharge Data",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            "input.discharge_main == 'Single Location Info'",
            dateRangeInput("discharge_date_range", "Select Date Range", #start = "2000-07-31", 
                           start = '2002-01-01',
                           end = "2018-04-30", 
                           min = '2002-01-01',
                           #min = "2000-07-31", 
                           max = "2018-04-30"),
            selectInput("discharge_plot_type", "Plot Type", choices = c("Location Boxplot", "Yearly Location Boxplot", "Scatter Plot")),
            uiOutput("discharge_selection"),
            # selectInput("discharge_locations", "Select Location", choices = c(discharge_locations), selected = "Berkeley Hts Wpcp"),
            selectInput("discharge_parameter", "Select Parameter", choices = discharge_params, selected = "Nitrogen, Nitrate Total (as N)"),
            selectInput("discharge_quantity", "Quantity/Concentration", choices = c(
              "Average Quantity" = "Quantity.Avg",
              "Max Quantity" = "Quantity.Max",
              "Average Concentration" = "Concentration.Avg",
              "Max Concentration" = "Concentration.Max",
              "Min Concentration" = "Concentration.Min"
            ), selected = "Concentration.Avg"),
            downloadButton("download_discharge_plot", "Download Plot"),
            h4("Data Represented From Only Facilities That Report")
          ),
          conditionalPanel(
            "input.discharge_main == 'Relative Loading'",
            selectInput("discharge_comparison_average_by", "Average By...", choices = c("Month", "Year")),
            uiOutput("discharge_input"),
            downloadButton("download_relative_loading", "Download Plot"),
            h4("Data Represented From Only Facilities That Report")
          )
        ),
        mainPanel(tabsetPanel(
          id = "discharge_main",
          tabPanel(
            "Single Location Info",
            withSpinner(plotOutput("discharge_plot")),
            leafletOutput("discharge_map", height = 800)
          ),
          tabPanel(
            "Relative Loading",
            withSpinner(plotOutput("discharge_comparison_boxplot", height = 800))
          )
        ))
      )
    ),
    tabPanel(
      "TMDL Model",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(inputId = "tmdl_date_range", label = "Date Range", start = "1999-10-01", end = "2003-12-01", min = "1999-10-01", max = "2003-12-01"),
          uiOutput("tmdl_station_menu"),
          selectInput(inputId = "tmdl_time_series_parameter", "Time Series Parameter", choices = sort(old_tmdl_df_names[-c(1, 2, 46, 47)]), selected = "NO3 mg/l"),
          selectInput(inputId = "tmdl_data_type", "Data Type", choices = c("Calibration" = "existing", "TMDL" = "tmdl")),
          selectInput(inputId = "tmdl_stat", "Plot Daily...", choices = c("max and min", "mean", "median", "max", "min")),
          selectInput(inputId = "tmdl_cross_y", "Cross Plot Y Parameter", choices = tmdl_cross_parameters, selected = "NO3 mg/l"),
          selectInput(inputId = "tmdl_cross_x", "Cross Plot X Parameter", choices = tmdl_cross_parameters, selected = "NH4 mg/l"),
          downloadButton("download_tmdl_plots", "Download Plots")
        ),
        mainPanel(
          fluidRow(
            column(6, withSpinner(plotOutput("tmdl_time_series_plot"))),
            column(6, withSpinner(plotOutput("tmdl_parameter_flow_plot")))
          ),
          fluidRow(
            column(
              6, h4("(Critical locations and model segments: upstream to downstream ordering)"),
              tableOutput("tmdl_location_info")
            ),
            column(6, withSpinner(plotOutput("tmdl_cross_plot")))
          )
        )
      ), fluidRow(column(12, leafletOutput("model_node_map", height = 900)))
    ),
    tabPanel(
      "USGS Flow",
      fluidPage(
        fluidRow(
          column(3), column(
            6,
            withSpinner(plotOutput(outputId = "frequency_plot"))
          ),
          column(3)
        )
      ),
      fluidRow(
        column(3),
        column(
          6,
          withSpinner(DT::dataTableOutput("frequency_summary"))
        ),
        column(3)
      )
    ),
    tabPanel(
      "2018 ISCO Sampling",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          conditionalPanel(
            "input.sampling_main == 'Spatial Analysis' || input.sampling_main == 'Event Time Series'",
            selectInput("sample_event", "Select Event", c(" ", "Event 1 - 06/12 - 06/13" = "1", "Event 2 - 6/26 - 6/28" = "2", "Event 3 - 7/10 - 7/12" = "3", "Event 4 - 8/7 - 8/9" = "4"), selected = "1")
          ),
          conditionalPanel(
            "input.sampling_main == 'Spatial Analysis'",
            selectInput("sample_color_scheme",
              label = "Color Palette",
              choices = color_pal_choices
            ),
            selectInput("sample_slider_type", "Filter Event...", choices = c("Uniformly" = "single", "One Slider Per Station" = "six")),
            selectInput("sample_parameter", "Select Parameter", sample_data_parameters, selected = "NO3+NO2 (mg/L)")
          ),
          conditionalPanel(
            "input.sampling_main == 'Event Time Series'",
            pickerInput(
              inputId = "sample_time_series_parameter", "Select Parameter(s)",
              choices = sample_data_parameters,
              multiple = TRUE, selected = sample_data_parameters,
              options = list(`actions-box` = TRUE)
            )
          ),
          conditionalPanel(
            "input.sampling_main == 'Overall Time Series'",
            selectInput("overall_sample_time_series_parameter", "Select Parameter", sample_data_parameters, selected = "NO3+NO2 (mg/L)")
          ),
          conditionalPanel(
            "input.sampling_main == 'Event Time Series'",
            pickerInput(
              inputId = "sample_stations", "Select Stations(s)",
              choices = sample_data_stations,
              selected = sample_data_stations,
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
            downloadButton("download_sample_time_series_plot", "Download Plots")
          ),
          conditionalPanel(
            "input.sampling_main == 'Spatial Analysis' && input.sample_slider_type == 'single'",
            sliderInput("sample_interval", "Select Interval", min = 1L, max = 24L, step = 1L, value = 1L)
          ),
          conditionalPanel(
            "input.sampling_main == 'Spatial Analysis' && input.sample_slider_type == 'six'",
            sliderInput(inputId = "station_1", label = "Passaic River at Eagle Rock (PA-5)", min = 1L, max = 24L, step = 1L, value = 1L),
            sliderInput(inputId = "station_2", label = "Passaic River at Pine Brook (WI-3)", min = 1L, max = 24L, step = 1L, value = 1L),
            sliderInput(inputId = "station_3", label = "Passaic River at Two Bridges (PA-7)", min = 1L, max = 24L, step = 1L, value = 1L),
            sliderInput(inputId = "station_4", label = "Pompton River at Two Bridges (POM-3)", min = 1L, max = 24L, step = 1L, value = 1L),
            sliderInput(inputId = "station_5", label = "Rockaway River on Bloomfield Ave. Montville (RO-2)", min = 1L, max = 24L, step = 1L, value = 1L),
            sliderInput(inputId = "station_6", label = "Whippany River at Pine Brook (PA-6)", min = 1L, max = 24L, step = 1L, value = 1L)
          ), # ,
          # conditionalPanel("insput.sampling_main == 'Time Series'",
          #   selectInput("time_series_type", "Display Setting", choices = c("Single Plot", "One Plot Per Station")))
          conditionalPanel(
            "input.sampling_main == 'Spatial Analysis' || input.sampling_main == 'Time Series'",
            withSpinner(tableOutput("event_details_df2"))
          ),
          conditionalPanel(
            "input.sampling_main == 'Dataset'",
            downloadButton("download_sampling_data", "Download Data")
          ),
          conditionalPanel(
            "input.sampling_main == 'Overall Time Series'",
            pickerInput(
              inputId = "overall_sample_stations", "Select Stations(s)",
              choices = c("Caldwell STP Discharge", "Hanover SA Discharge", "Livingston Discharge",
                          "Molitor Discharge", "Parsippany Discharge",
                          "Passaic River at Eagle Rock (PA-5)" = "Passaic River at Eagle Rock",
                          "Passaic River at Pine Brook (PA-6)" = "Passaic River at Pine Brook",
                          "Passaic River at Two Bridges (PA-7)" = "Passaic River at Two Bridges",
                          "Pompton River at Two Bridges (POM-3)" = "Pompton River at Two Bridges",
                          "Rockaway River on Bloomfield Ave. Montville (RO-2)" = "Rockaway River on Bloomfield Ave. Montville",
                          "Rockaway Valley Discharge", "Two Bridges Discharge", "Wayne Twp Discharge",
                          "Whippany River at Pine Brook (WI-3)" = "Whippany River at Pine Brook")
                          ,
              multiple = TRUE, selected = c("Caldwell STP Discharge", "Hanover SA Discharge", "Livingston Discharge",
                                            "Molitor Discharge", "Parsippany Discharge",
                                            "Passaic River at Eagle Rock (PA-5)" = "Passaic River at Eagle Rock",
                                            "Passaic River at Pine Brook (PA-6)" = "Passaic River at Pine Brook",
                                            "Passaic River at Two Bridges (PA-7)" = "Passaic River at Two Bridges",
                                            "Pompton River at Two Bridges (POM-3)" = "Pompton River at Two Bridges",
                                            "Rockaway River on Bloomfield Ave. Montville (RO-2)" = "Rockaway River on Bloomfield Ave. Montville",
                                            "Rockaway Valley Discharge", "Two Bridges Discharge", "Wayne Twp Discharge",
                                            "Whippany River at Pine Brook (WI-3)" = "Whippany River at Pine Brook")
                                            ,
              options = list(`actions-box` = TRUE)
            )
          ),
          conditionalPanel(
            "input.sampling_main == 'Continuous Data'",
            selectInput("continuous_sampling_station", "Station",
              choices = c("BFBM000288" = "1", "01379580" = "2", "01381900" = "3")
            ),
            selectInput("continuous_x_parameter", "x-Parameter",
              choices = sort(c(
                "Datetime", "Temperature", "Cond", "Sal", "DOsat",
                "DO", "pH", "Turb"
              )),
              selected = "Datetime"
            ),
            selectInput("continuous_y_parameter", "y-Parameter",
              choices = sort(c(
                "Datetime", "Temperature", "Cond", "Sal", "DOsat",
                "DO", "pH", "Turb"
              )),
              selected = "DO"
            )
          ),
          conditionalPanel("input.sampling_main == 'Passaic at Two Bridges Nitrate'",
                           "(USGS - ISCO) Is Difference Between USGS Nitrate measurements and ISCO Sampling measurements"),
          conditionalPanel("input.sampling_main == 'Summary Statistics'",
                           downloadButton("download_sample_data_summary", "Download Data"))
        ),
        mainPanel(
          tabsetPanel(
            id = "sampling_main",
            tabPanel(
              "Spatial Analysis",
              withSpinner(leafletOutput("sample_map", height = 750, width = 1500)),
              # withSpinner(tableOutput("event_details_df"))
              withSpinner(dataTableOutput("spatial_analysis_df"))
            ),
            tabPanel(
              "Event Time Series",
              uiOutput("time_series_plot_grid")
            ),
            tabPanel(
              "Overall Time Series",
              withSpinner(plotOutput("overall_time_series_plot", height = 700))
            ),
            tabPanel(
              "Dataset",
              DT::dataTableOutput("sampling_dataset")
            ),
            tabPanel(
              "Passaic at Two Bridges Nitrate",
              withSpinner(plotOutput("sample_nitrate_1")),
              withSpinner(plotOutput("sample_nitrate_2")),
              withSpinner(plotOutput("sample_nitrate_3")),
              withSpinner(plotOutput("sample_nitrate_4")),
              withSpinner(plotOutput("sample_nitrate_cross_1")),
              withSpinner(plotOutput("sample_nitrate_cross_2")),
              withSpinner(plotOutput("sample_nitrate_cross_3")),
              withSpinner(plotOutput("sample_nitrate_cross_4")),
              withSpinner(dataTableOutput("sample_nitrate_df"))
            ),
            tabPanel(
              "Summary Statistics",
              dataTableOutput("summary_stats")
            )
            # tabPanel("Nitrate Loads",
            #   plotOutput("nitrate_discharger_loads_1"),
            #   plotOutput("nitrate_discharger_loads_2"))
          )
        )
      ),
      tags$script(HTML("var header = $('.navbar > .container');
                       header.append('<div style=\"float:right\"><h3>Company name text here</h3></div>');
                       console.log(header)"))
    )
  )
)


server <- function(input, output, session) {
  # the reactive value nitrate_df() returns a version of nitrate_data that filters by the users selected scatter_date_range. 
  nitrate_df <- reactive({
    req(input$scatter_date_range)
    read_nitrate_data(input$scatter_date_range)
  })

  rain_df <- reactive({
    req(input$scatter_date_range)
    read_rain_data(input$scatter_date_range)
  })

  gage_df <- reactive({
    req(input$scatter_date_range)
    read_gage_data(input$scatter_date_range)
  })


  # reactive scatter_plot value: returns the scatter plot based on the date_time range selected by the user, using nitrate_df.
  scatter_plot <- reactive({
    req(nitrate_df(), rain_df(), gage_df(), input$point_size)
    nitrate_plot <- plot_nitrate_data(
      nitrate_df = nitrate_df(), rain_df = rain_df(), add_spline_curve = input$add_smoothing_line, add_best_fit_line = input$add_best_fit_line,
      point_size = input$point_size
    )
    nitrate_pompton_plot <- plot_nitrate_pompton_data(nitrate_df = nitrate_df(), add_spline_curve = input$add_smoothing_line, add_best_fit_line = input$add_best_fit_line, point_size = input$point_size)
    gage_plot <- plot_gage_data(gage_df = gage_df(), point_size = input$point_size)
    nitrate_plot + nitrate_pompton_plot + gage_plot + plot_layout(guides = "collect", ncol = 1, nrow = 3)
  })

  output$usgs_plot_download_button <- renderUI({
    if (!is.null(scatter_plot())) {
      downloadButton(outputId = "download_usgs_scatter", "Download Plots")
    }
  })
  output$download_usgs_scatter <- downloadHandler(
    filename = function() {
      "usgs_passaic_scatter_plots.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = scatter_plot(), base_height = 8, base_width = 10)
    }
  )

  output$usgs_annual_plot_download_button <- renderUI({
    if (!is.null(annual_range_plot())) {
      downloadButton(outputId = "download_usgs_annual", "Download Plots")
    }
  })
  output$download_usgs_annual <- downloadHandler(
    filename = function() {
      "usgs_annual_range_plot.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = annual_range_plot(), base_height = 8, base_width = 10)
    }
  )

  output$download_usgs_monthly_trend <- downloadHandler(
    filename = function() {
      "usgs_monthly_trend_plot.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = monthly_trend_range_plot(), base_height = 8, base_width = 10)
    }
  )

  output$download_usgs_monthly <- downloadHandler(
    filename = function() {
      "usgs_monthly_plot.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = monthly_range_plot(), base_height = 8, base_width = 10)
    }
  )

  output$download_usgs_hourly <- downloadHandler(
    filename = function() {
      "usgs_hourly_plot.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = hourly_range_plot(), base_height = 8, base_width = 10)
    }
  )
  # SCATTER PLOT OUTPUT
  output$display_scatter_plot <- renderPlot({
    scatter_plot()
  })

  #
  range_plot_data <- reactive({
    req(input$range_years, input$location)
    read_range_plot_data(input$location, input$range_years)
  })

  month_plot_data <- reactive({
    req(input$range_years, input$month, input$location)
    read_monthly_trend_range_plot_data(input$location, input$month, input$range_years)
    
  })
  
  annual_range_plot <- reactive({
    req(range_plot_data())
    plot_annual_range_plot(range_plot_data())
  })

  monthly_range_plot <- reactive({
    req(range_plot_data())
    plot_monthly_range_plot(range_plot_data())
  })

  monthly_range_data <- reactive({
    req(range_plot_data())
    read_monthly_range_plot_data_summary(range_plot_data())
      
  })

  output$download_monthly_range_data <- downloadHandler(
    filename = function() {
      "monthly_range_data.xlsx"
    },
    content = function(file) {
      write.xlsx(monthly_range_data(), file)
    }
  )

  output$download_dataset <- downloadHandler(
    filename = function() {
      "nitrate_dataset.xlsx"
    },
    content = function(file) {
      write.xlsx(nitrate_data, file)
    }
  )




  annual_range_data <- reactive({
    req(range_plot_data()) 
    read_annual_range_plot_data_summary(range_plot_data())
  })

  output$download_annual_range_data <- downloadHandler(
    filename = function() {
      "annual_range_data.xlsx"
    },
    content = function(file) {
      write.xlsx(annual_range_data(), file)
    }
  )


  hourly_range_data <- reactive({
    req(range_plot_data())
    read_hourly_range_plot_data_summary(range_plot_data())
    
  })

  output$download_hourly_range_data <- downloadHandler(
    filename = function() {
      "hourly_range_data.xlsx"
    },
    content = function(file) {
      write.xlsx(hourly_range_data(), file)
    }
  )

  hourly_range_plot <- reactive({
    req(range_plot_data())
    plot_hourly_range_plot(range_plot_data())
  })

  monthly_trend_range_data <- reactive({
    req(month_plot_data())
    read_monthly_trend_range_plot_data_summary(month_plot_data())
    
  })
  output$download_monthly_trend_range_data <- downloadHandler(
    filename = function() {
      "monthly_trend_range_data.xlsx"
    },
    content = function(file) {
      write.xlsx(monthly_trend_range_data(), file)
    }
  )


  monthly_trend_range_plot <- reactive({
    ggplot(data = month_plot_data(), aes(x = factor(Year), y = nitrate)) +
      geom_boxplot() +
      theme(legend.position = "none") +
      xlab("Year") +
      ylab("Nitrate, mg/L") +
      ggtitle(month.name[as.integer(input$month)]) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  })
  output$display_monthly_trend_range_data <- renderDataTable(datatable(monthly_trend_range_data(), rownames = FALSE, options = list("pageLength" = 10, lengthChange = FALSE)) %>% formatRound(c("Mean", "SD"), 2))

  output$display_monthly_trend_range_plot <- renderPlot(monthly_trend_range_plot())

  output$display_annual_range_plot <- renderPlot(annual_range_plot())

  output$display_annual_range_data <- renderDataTable(datatable(annual_range_data(), rownames = FALSE, options = list("pageLength" = length(nitrate_data_years), lengthChange = FALSE)) %>% formatRound(c("Mean", "SD"), 2))
  output$display_monthly_range_plot <- renderPlot(monthly_range_plot())
  output$display_monthly_range_data <- renderDataTable(datatable(monthly_range_data(), rownames = FALSE, options = list(
    "pageLength" = 12, lengthChange = FALSE
  )) %>% formatRound(c("Mean", "SD"), 2))
  output$display_hourly_range_plot <- renderPlot(hourly_range_plot())
  output$display_hourly_range_data <- renderDataTable(datatable(hourly_range_data(), rownames = FALSE, options = list(
    "pageLength" = 24, lengthChange = FALSE
  )) %>% formatRound(c("Mean", "SD"), 2))
  output$nitrate_dataset <- DT::renderDataTable(nitrate_data_display, filter = "top", server = TRUE, options = list(columnDefs = list(list(
    targets = c(8), searchable = FALSE
  )), rownames = FALSE))
  #-----------------------------------------------------------------PASSAIC TMDL MODEL
  tmdl_time_series_df <- reactive({
    req(input$tmdl_station, input$tmdl_date_range, input$tmdl_time_series_parameter, input$tmdl_data_type, input$tmdl_stat)
    read_tmdl_time_series_data(input$tmdl_station, parameter = input$tmdl_time_series_parameter, data_type = input$tmdl_data_type, stat = input$tmdl_stat, date_range = input$tmdl_date_range)
  })



  tmdl_time_series_p <- reactive({
    req(tmdl_time_series_df())
    plot_tmdl_time_series_data(tmdl_time_series_df())
  })




  tmdl_parameter_flow_df <- reactive({
    req(input$tmdl_station, input$tmdl_date_range, input$tmdl_time_series_parameter, input$tmdl_data_type, input$tmdl_stat)
    read_tmdl_parameter_flow_data(station = input$tmdl_station, parameter = input$tmdl_time_series_parameter, data_type = input$tmdl_data_type, stat = input$tmdl_stat, date_range = input$tmdl_date_range)
  })

  tmdl_parameter_flow_p <- reactive({
    req(tmdl_parameter_flow_df())
    plot_tmdl_parameter_flow_data(tmdl_parameter_flow_df())
  })

  tmdl_cross_df <- reactive({
    req(input$tmdl_station, input$tmdl_date_range, input$tmdl_cross_x, input$tmdl_cross_y, input$tmdl_data_type, input$tmdl_stat)
    read_tmdl_cross_data(station = input$tmdl_station, parameter_x = input$tmdl_cross_x, parameter_y = input$tmdl_cross_y, data_type = input$tmdl_data_type, stat = input$tmdl_stat, date_range = input$tmdl_date_range)
  })

  tmdl_cross_p <- reactive({
    req(tmdl_cross_df())
    plot_tmdl_cross_data(tmdl_cross_df())
  })

  output$tmdl_cross_plot <- renderPlot({
    tmdl_cross_p()
  })

  output$tmdl_parameter_flow_plot <- renderPlot({
    tmdl_parameter_flow_p()
  })
  output$tmdl_time_series_plot <- renderPlot({
    tmdl_time_series_p()
  })
  output$tmdl_location_info <- renderTable({
    tmdl_location_info
  })

  tmdl_plots_title <- reactive({
    if (input$tmdl_data_type == "existing") {
      type <- "Calibration"
    } else {
      type <- "TMDL"
    }
    ggdraw() +
      draw_label(paste0("Model ", type, " Segment Node: ", input$tmdl_station),
        fontface = "bold"
      )
  })


  output$download_tmdl_plots <- downloadHandler(
    filename = function() {
      "tmdl_model_plots.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = cowplot::plot_grid(tmdl_plots_title(), plot_grid(tmdl_time_series_p(), tmdl_parameter_flow_p(), tmdl_cross_p(), ncol = 2), ncol = 1, rel_heights = c(0.1, 1)), base_height = 8, base_width = 12)
    }
  )

  output$model_node_map <- renderLeaflet({
    nodes_map <- leaflet(data = model_nodes) %>%
      setView(lng = -74.33933, lat = 40.84472, zoom = 12) %>%
      addProviderTiles(group = "ESRI (default)", providers$Esri) %>%
      addProviderTiles(group = "ESRI NatGeoWorldMap", providers$Esri.NatGeoWorldMap) %>%
      addProviderTiles(group = "ESRI WorldImagery", providers$Esri.WorldImagery) %>%
      addProviderTiles(group = "OpenStreetMap", providers$OpenStreetMap) %>%
      addCircleMarkers(
        lng = model_nodes@coords[, 1], lat = model_nodes@coords[, 2], label = model_nodes_labels,
        layerId = model_nodes_labels, popup = model_nodes_labels, radius = 2.5, stroke = TRUE
      ) %>%
      addLayersControl(
        baseGroups = c("ESRI (default)", "ESRI NatGeoWorldMap", "ESRI WorldImagery", "OpenStreetMap"),
        options = layersControlOptions(collapsed = F)
      )
  })
  model_marker_click <- reactive({
    if (!is.null(input$model_node_map_marker_click)) {
      input$model_node_map_marker_click$id
    } else {
      NULL
    }
  })

  output$tmdl_station_menu <- renderUI({
    if (is.null(model_marker_click())) {
      selectInput("tmdl_station", "Select Model Segment Node", choices = tmdl_stations)
    } else {
      selectInput("tmdl_station", "Select Model Segment Node", choices = tmdl_stations, selected = model_marker_click())
    }
  })


  # DISCHARGE TAB -----------------------------------------------------------
  discharge_df <- reactive({
    if (!is.null(input$discharge_locations)) {
      discharge_data %>%
        select_("Date", "Parameter", "Value" = as.name(input$discharge_quantity), "Quantity.Units", "Concentration.Units", "Location") %>%
        filter(Location == input$discharge_locations, Parameter == input$discharge_parameter, !is.na(Value), Date >= ymd(input$discharge_date_range[[1]]), Date <= ymd(input$discharge_date_range[[2]])) %>%
        mutate(Year = factor(lubridate::year(Date)))
    } else {
      NULL
    }
  })

  discharge_p <- reactive({
    if (is.null(discharge_df())) {
      ggplot()
    } else {
      if (input$discharge_quantity %in% c("Quantity.Avg", "Quantity.Max")) {
        units <- discharge_df() %>%
          filter(!is.na(Quantity.Units)) %>%
          pull(Quantity.Units)
        units <- tolower(units[1])
      } else {
        units <- discharge_df() %>%
          filter(!is.na(Concentration.Units)) %>%
          pull(Concentration.Units)
        units <- tolower(units[1])
      }

      if (input$discharge_plot_type == "Location Boxplot") {
        ggplot(discharge_df(), aes(x = Location, y = Value)) +
          geom_boxplot() +
          ylab(paste0(input$discharge_parameter, " ( ", units, ")")) +
          xlab("")
      } else if (input$discharge_plot_type == "Yearly Location Boxplot") {
        ggplot(discharge_df(), aes(x = Year, y = Value)) +
          geom_boxplot() +
          ylab(paste0(input$discharge_parameter, " ( ", units, " )")) +
          xlab(toTitleCase(input$discharge_locations))
      } else if (input$discharge_plot_type == "Scatter Plot") {
        ggplot(discharge_df(), aes(x = Date, y = Value)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          xlab("Date") +
          ylab(paste0(input$discharge_parameter, " ( ", units, ")")) +
          background_grid(major = "xy", minor = "xy")
      } else {
        ggplot()
      }
    }
  })

  output$download_discharge_plot <- downloadHandler(
    filename = function() {
      "discharge_plot.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = discharge_p(), base_height = 8, base_width = 10)
    }
  )
  output$discharge_plot <- renderPlot({
    discharge_p()
  })

  output$discharge_map <- renderLeaflet({
    leaflet(data = discharge_map) %>%
      addProviderTiles(group = "ESRI (default)", providers$Esri) %>%
      addProviderTiles(group = "ESRI NatGeoWorldMap", providers$Esri.NatGeoWorldMap) %>%
      addProviderTiles(group = "ESRI WorldImagery", providers$Esri.WorldImagery) %>%
      addProviderTiles(group = "OpenStreetMap", providers$OpenStreetMap) %>%
      setView(lng = -74.36348, lat = 40.90165, zoom = 12) %>%
      addCircleMarkers(lng = discharge_map@coords[, 1], lat = discharge_map@coords[, 2], popup = discharge_map@data$FAC_LABEL, label = discharge_map@data$FAC_LABEL, layerId = discharge_map@data$FAC_LABEL) %>%
      addLayersControl(
        baseGroups = c("ESRI (default)", "ESRI NatGeoWorldMap", "ESRI WorldImagery", "OpenStreetMap"),
        options = layersControlOptions(collapsed = F)
      )
  })

  discharge_marker_click <- reactive({
    if (!is.null(input$discharge_map_marker_click)) {
      input$discharge_map_marker_click$id
    } else {
      NULL
    }
  })

  output$discharge_selection <- renderUI({
    if (is.null(discharge_marker_click())) {
      selectInput("discharge_locations", "Select Location", choices = c(discharge_locations), selected = "Berkeley Hts Wpcp")
    } else {
      selectInput("discharge_locations", "Select Location", choices = c(discharge_locations), selected = discharge_marker_click())
    }
  })

  #---------------STATION

  discharge_comparison_boxplot_df <- reactive({
    req(input$discharge_comparison_average_by, input$discharge_comparison_average_by_specific)
    read_discharge_comparison_box_plot_data(input$discharge_comparison_average_by, input$discharge_comparison_average_by_specific)
    
  })



  discharge_comparison_boxplot_p <- reactive({
    labels <- c(Year = "Yearly Average Nitrogen Quantity (kg/day)", Month = "")
    ggplot(discharge_comparison_boxplot_df(), aes(x = factor(Location, levels = discharge_levels), y = Quantity.Avg)) +
      geom_boxplot(aes(color = Section)) +
      coord_flip() +
      xlab("Location") +
      ylab(paste0(input$discharge_comparison_average_by, "ly Average Nitrogen Quantity (kg/day)")) +
      scale_y_continuous(breaks = seq(from = 0, to = 600, by = 50))
  })

  output$discharge_comparison_boxplot <- renderPlot({
    discharge_comparison_boxplot_p()
  })


  output$download_relative_loading <- downloadHandler(
    filename = function() {
      "discharge_relative_loading_boxplot.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = discharge_comparison_boxplot_p(), base_height = 10, base_width = 12)
    }
  )
  output$discharge_input <- renderUI({
    if (input$discharge_comparison_average_by == "Month") {
      pickerInput(inputId = "discharge_comparison_average_by_specific",label = "Select Month(s)",
                  choices = c(
                              "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4,
                              "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9,
                              "Oct" = 10, "Nov" = 11, "Dec" = 12
                  ),
                  selected = c(
                               "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4,
                               "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9,
                               "Oct" = 10, "Nov" = 11, "Dec" = 12
                  ),
                  multiple= TRUE,
                  options = list(`actions-box` = TRUE))
      # selectInput("discharge_comparison_average_by_specific", "Select Month(s)", choices = c("All",
      #   "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4,
      #   "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9,
      #   "Oct" = 10, "Nov" = 11, "Dec" = 12
      # ), multiple = TRUE)
    } else {
      pickerInput(inputId = "discharge_comparison_average_by_specific",label = "Select Years(s)",
                  choices = 2002:2018,
                  selected = 2002:2018,
                  multiple= TRUE,
                  options = list(`actions-box` = TRUE))
      # selectInput("discharge_comparison_average_by_specific", "Select Year(s)", choices = c("All", 2000:2018), multiple = TRUE)
    }
  })

  #usgs flow frequency plot
  output$frequency_plot <- renderPlot({
    plot_usgs_flow_frequency_data()
  })

  output$frequency_summary <- DT::renderDataTable(read_usgs_flow_frequency_summary(), filter = "top", options = list(lengthMenu = list(c(10,30, 50, -1), 
                                                                                                                                       c('10', '30', '50', 'All'))))

  sample_data_df <- reactive({
    if (!is.null(input$sample_slider_type)) {
      if (input$sample_slider_type == "single") {
        sample_data %>%
          filter(Event == input$sample_event, Interval == input$sample_interval, Parameter == input$sample_parameter) %>%
          left_join(sample_sp %>% select(`Site ID` = SITE_NO, lng = Long, lat = Lat))
      } else if (input$sample_slider_type == "six") {
        sample_data %>%
          filter(
            Event == input$sample_event, Parameter == input$sample_parameter,
            (`Station Name` == "Passaic River at Eagle Rock" & Interval == input$station_1) | (`Station Name` == "Passaic River at Pine Brook" & Interval == input$station_2) | (`Station Name` == "Passaic River at Two Bridges" & Interval == input$station_3) | (`Station Name` == "Pompton River at Two Bridges" & Interval == input$station_4) | (`Station Name` == "Rockaway River on Bloomfield Ave. Montville" & Interval == input$station_5) | (`Station Name` == "Whippany River at Pine Brook" & Interval == input$station_6)
          ) %>%
          left_join(sample_sp %>% select(`Site ID` = SITE_NO, lng = Long, lat = Lat))
      }
    } else {
      NULL
    }
  })

  sample_time_series_df <- reactive({
    req(input$sample_event, input$sample_stations, input$sample_time_series_parameter)
    read_isco_sample_time_series_data(sample_event = input$sample_event, sample_stations = input$sample_stations, sample_parameters = input$sample_time_series_parameter)
  })

  overall_sample_time_series_df <- reactive({
    req(input$overall_sample_stations, input$overall_sample_time_series_parameter)
    read_isco_overall_sample_time_series_data(input$overall_sample_time_series_parameter, input$overall_sample_stations)
    
  })

  overall_time_series_p <- reactive({
    req(overall_sample_time_series_df())
    overall_sample_parameter_plot2(parameter = input$overall_sample_time_series_parameter, df = overall_sample_time_series_df())
    
  })
  time_series_p <- reactive({
    req(sample_time_series_df(), input$sample_time_series_parameter)
    plots <- purrr::map(input$sample_time_series_parameter, ~ sample_parameter_plot(., sample_time_series_df()))
    plot_grid(plotlist = plots, ncol = 1)
    
  })

  output$time_series_plot <- renderPlot({
      time_series_p()
    
  })

  output$overall_time_series_plot <- renderPlot({
    #if (!is.null(overall_time_series_p())) {
      overall_time_series_p()
    #}
  })

  time_series_save_parameters <- reactive({
    nparams <- length(input$sample_time_series_parameter)
    if (nparams > 1) {
      c(8, 10)
    } else {
      c(12, 20)
    }
  })

  output$download_sample_time_series_plot <- downloadHandler(
    filename = function() {
      "sample_time_series.pdf"
    },
    content = function(file) {
      cowplot::save_plot(file, plot = time_series_p(), base_height = time_series_save_parameters()[[2]], base_width = time_series_save_parameters()[[1]])
    }
  )
  output$discharge_plot <- renderPlot({
    discharge_p()
  })

  output$time_series_plot_grid <- renderUI({
    if ("All" %in% input$sample_time_series_parameter) {
      n_params <- 7
    } else {
      n_params <- length(input$sample_time_series_parameter)
    }
    if (n_params <= 2) {
      ht <- 400
    } else if (n_params > 2) {
      ht <- (as.integer(n_params / 2) + n_params %% 2) * 400
    }
    withSpinner(plotOutput("time_series_plot", height = ht))
  })



  output$sample_map <- renderLeaflet({
    map_isco_sampling()
    
    
  })

  sample_discharger_df <- reactive({
    req(input$sample_event, input$sample_parameter)
    read_isco_sample_discharger_data(sample_event = input$sample_event, sample_parameter = input$sample_parameter)
    
    
  })

  observe({
    sample_df <- sample_data_df() %>%
      filter(!is.na(Result)) %>%
      mutate(`Station Name` = str_c(`Station Name`, ", ", `TMDL ID`))
    # pal_type <- c("reg" = FALSE, "rev" = TRUE)
    pal <- colorNumeric(palette = input$sample_color_scheme, domain = sample_df$Result)

    if (!is.null(input$sample_parameter) && !is.null(input$sample_event) && input$sample_event != " " && nrow(sample_df) > 0) {
      if (input$sample_parameter %in% c("Chl a (ug/L)", "NH3 (mg/L)", "TSS  (mg/L)") || input$sample_event %in% c(3, 4)) {
        leafletProxy(mapId = "sample_map") %>%
          clearControls() %>%
          clearGroup(group = "rectangles") %>%
          addCircleMarkers(
            radius = 10, fillOpacity = 1, opacity = 1, stroke = TRUE, lng = sample_df$lng, lat = sample_df$lat,
            popup = paste0(sample_df$Parameter, ": ", sample_df$Result), label = sample_df$`Station Name`, color = "black", weight = 2, fillColor = pal(sample_df$Result)
          ) %>%
          # addLabelOnlyMarkers(data = sample_highest, lng = ~lng, lat = ~lat, label = ~TMDL,
          #                     labelOptions = labelOptions(noHide = T, direction = "left", textsize = "8px")) %>%
          # addLabelOnlyMarkers(data = sample_high, lng = ~lng, lat = ~lat, label = ~TMDL,
          #                     labelOptions = labelOptions(noHide = T, direction = "right", textsize = "8px")) %>%
          # addLabelOnlyMarkers(data = sample_low, lng = ~lng, lat = ~lat, label = ~TMDL,
          #                     labelOptions = labelOptions(noHide = T, direction = "right", textsize = "8px")) %>%
          addLegend("bottomright",
            pal = pal, values = sample_df$Result,
            title = sample_df$Parameter[1], opacity = 1
          )
      } else {
        discharge_df <- sample_discharger_df()
        pal_dischargers <- colorNumeric(palette = "OrRd", domain = discharge_df$Result)

        leafletProxy(mapId = "sample_map") %>%
          clearControls() %>%
          addCircleMarkers(
            radius = 10, fillOpacity = 1, opacity = 1, stroke = TRUE, lng = sample_df$lng, lat = sample_df$lat,
            popup = paste0(sample_df$Parameter, ": ", sample_df$Result), label = sample_df$`Station Name`, color = "black", weight = 2, fillColor = pal(sample_df$Result)
          ) %>%
          addRectangles(
            fillOpacity = 1, opacity = 1, stroke = TRUE, lng1 = discharge_df$lon, lat1 = discharge_df$lat, lng2 = discharge_df$lon + .0025, lat2 = discharge_df$lat - .0025,
            popup = paste0(discharge_df$Parameter, ": ", discharge_df$Result),
            label = discharge_df$`Station Name`, color = "black", weight = 2,
            fillColor = pal_dischargers(discharge_df$Result), group = "rectangles"
          ) %>%
          addLegend("bottomright",
            pal = pal, values = sample_df$Result,
            title = str_c("Ambient ", sample_df$Parameter[1]), opacity = 1
          ) %>%
          addLegend("bottomleft",
            pal = pal_dischargers, values = discharge_df$Result,
            title = str_c("Discharger ", sample_df$Parameter[1]), opacity = 1
          )
      }
    } else {
      leafletProxy(mapId = "sample_map") %>%
        clearControls() %>%
        clearMarkers()
    }
  })

  output$event_details_df <- renderTable({
    event_details[[as.integer(input$sample_event)]]
  })

  output$event_details_df2 <- renderTable({
    event_details[[as.integer(input$sample_event)]]
  })

  output$spatial_analysis_df <- renderDataTable({
    sample_data_df() %>%
      select("Station Name", `TMDL ID`, Parameter, Result, Date, Hour, Event, Interval)
  })

  output$download_sampling_data <- downloadHandler(
    filename = function() {
      "sample_data.xlsx"
    },
    content = function(file) {
      write.xlsx(sample_data, file)
    }
  )

  output$sampling_dataset <- DT::renderDataTable(datatable(
    sample_data %>%
      select(Datetime, "Site ID", "Station Name", `TMDL ID`, Parameter, Result, Date, Hour, Event, Interval) %>%
      mutate_at(c("Parameter", "Station Name", "Site ID", "Event"), as.factor),
    filter = "top",
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(
        targets = c(1), searchable = FALSE
      ))
    )
  ))

  output$sample_nitrate_1 <- renderPlot({
    isco_nitrate_at_two_bridges_plots[[1]]
  })
  output$sample_nitrate_2 <- renderPlot({
    isco_nitrate_at_two_bridges_plots[[2]]
  })

  output$sample_nitrate_3 <- renderPlot({
    isco_nitrate_at_two_bridges_plots[[3]]
  })

  output$sample_nitrate_4 <- renderPlot({
    isco_nitrate_at_two_bridges_plots[[4]]
  })

  output$sample_nitrate_cross_1 <- renderPlot({
    plot_isco_nitrate_sample_at_two_bridges_crossplot(1)
  })

  output$sample_nitrate_cross_2 <- renderPlot({
    plot_isco_nitrate_sample_at_two_bridges_crossplot(2)
  })

  output$sample_nitrate_cross_3 <- renderPlot({
    plot_isco_nitrate_sample_at_two_bridges_crossplot(3)
  })

  output$sample_nitrate_cross_4 <- renderPlot({
    plot_isco_nitrate_sample_at_two_bridges_crossplot(4)
  })

  output$sample_nitrate_df <- renderDataTable({
    sample_nitrate_data %>%
      select(Date, Hour, Event, ISCO, USGS)
  })
  
  
  output$download_sample_data_summary <- downloadHandler(
    filename = function() {
      "sample_data_summary_statistics.xlsx"
    },
    content = function(file) {
      openxlsx::write.xlsx(sample_data_summary(), file = file)
    }
  )
  
  output$summary_stats <- renderDataTable({
    sample_data_summary()
  })

  # output$nitrate_discharger_loads_1 <- renderPlot({
  #   ggplot(data = sample_discharger_flow %>% filter(Date == mdy("06/14/2018"))) +
  #     geom_bar(aes(x = `Station Name`, y = Perc), fill = "red", stat = "identity") +
  #     ggtitle("Nitrate Load, 06/13-06/14") +
  #     ylab("%") +
  #     xlab("Discharger")
  # })
  # 
  # output$nitrate_discharger_loads_2 <- renderPlot({
  #   ggplot(data = sample_discharger_flow %>% filter(Date == mdy("06/28/2018"))) +
  #     geom_bar(aes(x = `Station Name`, y = Perc), fill = "red", stat = "identity") +
  #     ggtitle("Nitrate Load, 06/27-06/28") +
  #     ylab("%") +
  #     xlab("Discharger")
  # })

  # output$continuous_sampling_plot <- renderPlot({
  #   continuous_sampling_plot(station = as.integer(input$continuous_sampling_station), x = input$continuous_x_parameter,
  #                            y = input$continuous_y_parameter)
  # })
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}


shinyApp(ui, server)
