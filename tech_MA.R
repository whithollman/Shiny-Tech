#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(tseries)
library(scales)
library(janitor)
library(ggthemes)
library(purrr)
library(bslib)
library(shinythemes)
library(forecast)
library(here)

Sys.setenv('_R_USE_PIPEBIND_ ' = TRUE)

data_url <- "https://www.kaggle.com/datasets/shivamb/company-acquisitions-7-top-companies"

tech_ma <- read.csv(url(data_url, method = "libcurl"),
                    stringsAsFactors = FALSE)
names(tech_ma)



ui <- fluidPage(


  headerPanel(strong("Tech Company Merger & Aquisitions")),
    theme = shinytheme("sandstone"),

        div(navlistPanel(
            "Length of Acquisition Time",
            h3(strong("Company Acquisitions 1957-2021")),

            tabPanel(strong("Clickable Plot"), plotOutput("plot",
                                click = "plot_click", width = 500),
                     tableOutput("plotdata"),
                p(
                   "This plot shows that Disney is the slowest to finalize their
              acquisitions, and that IBM is the fastest to close.
              Click on the black bar inside of each company plot box, this will
              bring up the data for that company. The 'aqqu_time' column
              is the average aquistion time for that chosen company.
              The difference in days between these two companyies is typically
              31 days for IBM and Disney roughly 800 days."
                ),
            ),
            tabPanel(strong("Data Summary"),
                     h3("Summary of the original dataset 'tech_ma.csv'"),
                     verbatimTextOutput("summary")
            ),
            tabPanel(strong("Data Table"),
                     h3("Original 'tech_ma' data table"),
                     dataTableOutput("datatable", width = 800)
            ),

            "Forecast Analysis",
            tabPanel(strong("Forecast Plot"), plotOutput("forecastPlot"),

                     h3(strong("Seasonality of Top Six Tech Companies")),
                     selectInput("average_business", label = "Choose a column",
                                 choices = unique(sub_dta$average_business)),

                     h3(strong("Predictive Forecast")),
                     selectInput("column_choice", "Choose a Column",
                                  choices = colnames(WFinalCSV)),
                                 verbatimTextOutput("selected_column")

            ), #end

            tabPanel(strong("Forecast Data Analysis"),
                     selectInput("model_choice", "Choose Model:",
                                 choices = colnames(WMltCSV)),
                                 selected = "WMltCSV",
                                 verbatimTextOutput("model_table"),
                     p(
                       "The WMlt model appears to be
                       better suited to the data compared to the WAdd model.
                       It has lower error measures and lower information
                       criteria, indicating a better fit to the data."
                     )
          ) # end Tab panel

        ) #end nav

      ) #end div panel

    ) # end fluid page



# Define server logic required to create plots
server <- function(input, output, session) {

    #ggplot dataset
    my_tech <- here("Data/my_tech.csv")


    filteredData <- reactive({
      my_tech %>%
        filter(
          acqu_time_min >= input$acqu_time[1],
          acqu_time_max <= input$acqu_time[2],
          acquisition_year_min >= input$acquisition_year[1],
          acquisition_year_min <= input$acquisition_year[2]
        )
    })

    output$plot <- renderPlot({
      filtered_data <- input$filteredData
      acqu_time <- input$acqu_time
      acquisition_year <- input$acquisition_year
      parent_company <- input$parent_company


    #ggplot showing which company completes their deal quickest
        median_acqu_time <- my_tech %>%
        group_by(parent_company) %>%
        summarise(median_acqu_time = median(acqu_time, na.rm = TRUE)) %>%
        arrange(desc(median_acqu_time))

      my_tech$parent_company <- factor(my_tech$parent_company,
                                      levels = median_acqu_time$parent_company)

      ggplot(my_tech, mapping = aes(acqu_time, parent_company)) +
        geom_boxplot(fill = '#00bbf9') +
        theme_classic() +
        ggtitle("Company Acquisitions Descending Order
                (Short to Extended Period of Time)") +
        theme(plot.title = element_text(face = "bold")) +
        labs(
          x = "Acquisition Period(in Days)",
          y = "Parent Company"
        ) +
        theme(axis.title.x = element_text(face = "bold")) +
        theme(axis.title.y = element_text(face = "bold")) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5,
                                         hjust = 0.5, size = 10))

    }, width = 600) #end of first analysis


    # Time series code
    #time series dataset
    sub_dta <- here("Data/sub_dta.csv")

    newData <- ts(sub_dta$average_business, start = c(1987, 1),
                        end = c(2021, 4),
                        frequency = 4)

    filterD <- reactive({
      sub_dta %>%
        filter(average_business == input$average_business)
    })


    output$forecastPlot <- renderPlot({
      req(input$average_business)
      filter_D <- filterD()
      average_business <- input$average_business

        #forecasting code
          TData <- window(newData, end = c(2020, 4))
          VData <- window(newData, start = c(2021, 1))
          WAdd <- ets(TData, model = "AAA", restrict = FALSE)
          WMlt <- ets(TData, model = "AAM", restrict = FALSE)
          summary(WAdd) #*
          summary(WMlt) #*

          nV <- length(VData)
          fAdd <- forecast(WAdd, h=nV)
          fMlt <- forecast(WMlt, h=nV)
          accuracy(fAdd, VData)
          accuracy(fMlt, VData)


          WFinal <- ets(newData, model = "AAA", restrict = FALSE)
          forecast(WFinal, h=4)


        forecastPlot <- print(autoplot(WFinal) +
            autolayer(fAdd, series = "Additive Forecast") +
            autolayer(fMlt, series = "Multiplicative Forecast") +
            xlab("Year") + ylab("Acquisition Year") +
            guides(color=guide_legend(title="Forecasts")) +
            theme(legend.position = "bottom"))


    }) #end output plot

    #Forecast Plot Analysis Summary
    WFinalCSV <- here("Data/WFinalCSV.csv")
    data <- WFinalCSV

    #fMltCSV <- read.csv("fMltCSV.csv")
    #fAddCSV <- read.csv("fAddCSV.csv")

    WMltCSV <- here("Data/WMltCSV.csv")
    #WAddCSV <- read.csv("/Volumes/palang/MA_Shiny/WAddCSV.csv")
    model_data <- WMltCSV

    selected_column <- reactive({
      column_name <- input$column_choice
      selected <- data[[column_name]]
      selected
    })

    model_table <- reactive ({
      model <- input$model_choice
      select <- model_data[[model]]
      select
    })


    #UI Outtputs
    output$plotdata <- renderTable({
      req(input$plot_click)
      nearPoints(my_tech, input$plot_click)
    }) #end^

    output$summary <- renderPrint({
      summary(my_tech, width = 600)
    }) #end^

    output$datatable <- renderDataTable({
      datatable(my_tech, options = list(pageLength = 10))
    }) #end^

    output$selected_column <- renderPrint({
      selected_column()
    })

    output$model_table <- renderPrint({
      model_table()
    })


} #end server


# Run the application
shinyApp(ui = ui, server = server)
