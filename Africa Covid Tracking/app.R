library(shiny)
library(shinydashboard)
library(tidycovid19)
library(dplyr)
library(ggplot2)  
library(ggrepel)
library(DT)
library(scales)

ui <- dashboardPage(
    dashboardHeader(title = "Covid Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Raw Data", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("World Map", tabName = "world", icon = icon("globe")),
            menuItem("Africa Map", tabName = "africa_map", icon = icon("flag")),
            menuItem('Disease Progression', tabName = 'progress',icon=icon('chevron-right')),
            menuItem('About', tabName = 'about',icon=icon('book'))
        )
    ),
    dashboardBody(
        tabItems(
            # Raw data content
            tabItem(tabName = "dashboard",
                fluidRow(
                    h2("Covid-19 In Africa And The Middle East"),
                    br(nrows=1)
                ),
                fluidRow(
                    # display total cases
                    valueBoxOutput("new_cases"),
                    valueBoxOutput("confirmed"),
                    valueBoxOutput("deaths"),
                    valueBoxOutput("recoveries"),
                    
                    column(12, div(dataTableOutput("dataTable")))
                )
            ),
            
            # World map content
            tabItem(tabName = "world",
                    fluidPage(
                        valueBoxOutput("w_confirmed"),
                        valueBoxOutput("w_deaths"),
                        valueBoxOutput("w_recoveries"),
                        
                        plotOutput("worldplot",width = "100%", height = "550px")
                    )
            ),
            # Africa map content
            tabItem(tabName = "africa_map",
                    fluidPage(
                        valueBoxOutput("a_confirmed"),
                        valueBoxOutput("a_deaths"),
                        valueBoxOutput("a_recoveries"),
                        
                        plotOutput("africa",width = "100%", height = "550px")
                    )
            ),
            # Disease progression content
            tabItem(tabName = "progress",
                    fluidPage(
                        plotOutput("progression",width = "100%", height = "550px")
                    )
            ),
            
            # about content
            tabItem(tabName = "about",
                    fluidRow(
                        h3('The dashboard:'),
                        p('This Coronavirus dashboard provides an overview of the 2019 Novel Coronavirus COVID-19 (2019-nCoV) epidemic 
                        in Africa and The Middle East.')
                    ),
                    fluidRow(
                        h3('Data Sources'),
                        p('This display is based on data from the following sources:'),
                        tags$ul(
                            tags$li(tags$a(href="https://github.com/CSSEGISandData/COVID-19", 
                                           "Johns Hopkins University CSSE team on the spread of the SARS-CoV-2 virus")), 
                            tags$li(tags$a(href="https://www.acaps.org/covid19-government-measures-dataset", 
                                           "ACAPS governmental measures database")), 
                            tags$li(tags$a(href="https://data.worldbank.org/", 
                                           "World Bank."))
                        )  
                          
                    ),
                    fluidRow(
                        h3('Development details'),
                        p('This dashboard has been developed with R using', tags$a(href='https://shiny.rstudio.com/','Shiny Dashboard')),
                        p('It is mainly based on the package', tags$a(href='https://github.com/joachim-gassen/tidycovid19','tidycovid19'),
                          'created by',tags$a(href='https://www.wiwi.hu-berlin.de/de/professuren/bwl/rwuwp/staff/gassen','Joachim Gassen.')),
                        p('Other packages used include'),
                        tags$ul(
                            tags$li(tags$a(href="https://rstudio.github.io/DT/", 
                                           "DT"), " -rendering tables"), 
                            tags$li(tags$a(href="https://dplyr.tidyverse.org/", 
                                           "dplyr")," -data manipulation"), 
                            tags$li(tags$a(href="https://ggplot2.tidyverse.org/", 
                                           "ggplo2")," -data visualization ")
                        )
                        
                    ),
                    fluidRow(
                      h3('Creators'),
                      p('This Dashboard has been created by the collaborative efforts of Amayo Mordecai and Andove Brandel.'),
                      p('We are medical students at', tags$a(href ='https://www.uonbi.ac.ke/','The University of Nairobi'), 'and the goal behind this is to provide an easy access to COVID-19 related data for Africa and the Middle East as there are few sources offereing this service currently.')
                    )
            )
        )
    )
)

# helper function for the progress bar
compute_data <- function(updateProgress = NULL) {
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    for (i in 1:10) {
        Sys.sleep(0.25)
        
        # Compute new row of data
        new_row <- data.frame(x = rnorm(1), y = rnorm(1))
        
        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
            text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
            updateProgress(detail = text)
        }
        
        # Add the new row of data
        dat <- rbind(dat, new_row)
    }
    
    dat
}

server <- function(input, output) { 
    
    # fetch and filter the data
    merged_dta <- download_merged_data(cached = TRUE,silent = TRUE)
    filtered_dta <-  merged_dta[grepl("Africa", merged_dta$region), mult="last"]
    
    sumary_dta <- filtered_dta %>% 
        select(country, confirmed, deaths, recovered, timestamp) %>% 
        group_by(country) %>% 
        summarise(Date = as.Date(last(timestamp),format = "%m/%d/%y"), Cases = last(confirmed), Deaths=last(deaths), Recoveries=last(recovered), New_Cases = diff(tail(confirmed, n=2L)))
    
    world_dta <- merged_dta %>% 
        select(country, confirmed, deaths, recovered,) %>% 
        group_by(country) %>% 
        summarise(Cases = last(confirmed), Deaths=last(deaths), Recoveries=last(recovered))
    
    #render the data in atable
    output$dataTable <- renderDT(
        sumary_dta, # data
        class = "display nowrap compact", # style
        options = list(lengthMenu = c(5, 30, 50), pageLength = 25)
    )
    
    # render the total cases values 
    output$new_cases <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$New_Cases))), "New Cases", icon = icon("plus"),
            color = "purple"
        )
    })
    output$confirmed <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$Cases))), "Total Cases", icon = icon("hospital"),
            color = "yellow"
        )
    })
    output$deaths <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$Deaths))), "Total Deaths", icon = icon("skull"),
            color = "red"
        )
    })
    output$recoveries <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$Recoveries))), "Total Recoveries", icon = icon("smile"),
            color = "green"
        )
    })
    
    output$w_confirmed <- renderValueBox({
        valueBox(
            paste0(comma(sum(world_dta$Cases))), "Total Cases", icon = icon("hospital"),
            color = "yellow", width = 6
        )
    })
    output$w_deaths <- renderValueBox({
        valueBox(
            paste0(comma(sum(world_dta$Deaths))), "Total Deaths", icon = icon("skull"),
            color = "red",width = 6
        )
    })
    output$w_recoveries <- renderValueBox({
        valueBox(
            paste0(comma(sum(world_dta$Recoveries))), "Recoveries", icon = icon("plus"),
            color = "purple", width = 6
        )
    })
    output$a_confirmed <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$Cases))), "Total Cases", icon = icon("hospital"),
            color = "yellow"
        )
    })
    output$a_deaths <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$Deaths))), "Total Deaths", icon = icon("skull"),
            color = "red"
        )
    })
    output$a_recoveries <- renderValueBox({
        valueBox(
            paste0(comma(sum(sumary_dta$Recoveries))), "Recoveries", icon = icon("plus"),
            color = "purple"
        )
    })
    
    #plot the world data
    output$worldplot <- renderPlot({
        #Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, detail = detail)
        }
        
        # Compute the new data, and pass in the updateProgress function so
        # that it can update the progress indicator.
        compute_data(updateProgress)
        map_covid19(merged_dta, type = "active", per_capita = TRUE, 
                    cumulative = TRUE)
    })
    
    #plot africa's data
    output$africa <- renderPlot({
        #Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, detail = detail)
        }
        
        # Compute the new data, and pass in the updateProgress function so
        # that it can update the progress indicator.
        compute_data(updateProgress)
        map_covid19(merged_dta, type = "confirmed", per_capita = TRUE, 
                    region = "Africa")
    })
    
    #plot disease progression
    output$progression <- renderPlot({
        #Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, detail = detail)
        }
        
        # Compute the new data, and pass in the updateProgress function so
        # that it can update the progress indicator.
        compute_data(updateProgress)
        plot_covid19_stripes(
            filtered_dta,type = "confirmed",min_cases = 300,
            sort_countries = "start"
        )
    })
    }

shinyApp(ui, server)