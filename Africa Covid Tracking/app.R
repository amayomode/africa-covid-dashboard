library(shiny)
library(shinydashboard)
library(tidycovid19)
library(dplyr)
library(ggplot2)  
library(ggrepel)
library(DT)
library(plotly)
library(scales)
country_names<- read.csv('country.csv')

ui <- dashboardPage(
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Raw Data", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Disease Trends", tabName = "trends", icon = icon("chevron-right")),
            menuItem("Maps", tabName = "map", icon = icon("map")),
            menuItem("Control Measures", tabName = "control", icon = icon("table")),
            menuItem('About', tabName = 'about',icon=icon('book'))
        )
    ),
    dashboardBody(
        # css
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # Raw data content
            tabItem(tabName = "dashboard",
                fluidRow(
                    box(title = "Covid-19 In Africa And The Middle East",
                        solidHeader = TRUE, width = 12, status = "primary",
                        # display total cases
                        valueBoxOutput("new_cases"),
                        valueBoxOutput("confirmed"),
                        valueBoxOutput("deaths"),
                        valueBoxOutput("recoveries"),
                    ),

                ),
                fluidRow(
                      box(title="Raw Data",status = "primary",solidHeader = TRUE ,width = 12, style = "overflow-y: scroll;overflow-x: scroll;",
                           dataTableOutput("dataTable"),
                      )
                )
            ),
        
            # Disease Trend content
            tabItem(tabName = "trends",
                    fluidRow(
                        box(title='General Disease Trend',width = 12,solidHeader = TRUE,status='primary',style = "overflow-y: scroll;overflow-x: scroll;",
                            plotlyOutput("general_trend", width = "100%", height = "550px"),
                        )
                    ),
                    fluidRow(
                        box(title='Country Specific Trend',width = 12,solidHeader = TRUE,status='success',style = "overflow-y: scroll;overflow-x: scroll;",
                            selectInput("country",
                                        "Country:",
                                        c(unique(as.character(country_names$x)))),
                            plotlyOutput("country_trend",width = "100%", height = "550px"),
                        )
                    ),
                    fluidRow(
                        box(title='Stripe Plot',width = 12,solidHeader = TRUE,status='info',style = "overflow-y: scroll;overflow-x: scroll;",
                            plotOutput("progression",width = "100%", height = "550px")
                        )
                    )
            ),
            
            # map content
            tabItem(tabName = "map",
                    fluidRow(
                        box(title = "Covid-19 Maps",
                            solidHeader = TRUE, width = 12, status = "primary",
                            valueBoxOutput("a_confirmed"),
                            valueBoxOutput("a_deaths"),
                            valueBoxOutput("a_recoveries"),
                        ),
                    ),
                    fluidRow(
                        box(title = "Africa",
                            solidHeader = TRUE, width = 12, status = "primary",
                            plotOutput("africa",width = "500px", height = "550px"), align="center",style = "overflow-y: scroll;overflow-x: scroll;"
                        )
                    ),
                    fluidRow(
                        box(title = "Middle East",
                            solidHeader = TRUE, width = 12, status = "info",
                            plotOutput("mdd_east",width = "500px", height = "550px"), align="center",style = "overflow-y: scroll;overflow-x: scroll;"
                        )
                    )
            ),
            
            #control masures tab
            tabItem(tabName = "control",
                    fluidRow(
                        box(title='Social Distancing',width = 12,solidHeader = TRUE,status='primary',style = "overflow-y: scroll;overflow-x: scroll;",
                            plotOutput("social_distancing",width = "100%", height = "550px")
                        )
                    ),
                    fluidRow(
                        box(title='Lockdown',width = 12,solidHeader = TRUE,status='info',style = "overflow-y: scroll;overflow-x: scroll;",
                            plotOutput("lock_down",width = "100%", height = "550px")
                        )
                    ),fluidRow(
                        box(title='Movement Restriction',width = 12,solidHeader = TRUE,status='success',style = "overflow-y: scroll;overflow-x: scroll;",
                            plotOutput("mvt_restriction",width = "100%", height = "550px")
                        )
                    ),fluidRow(
                        box(title='Public Health',width = 12,solidHeader = TRUE,status='warning',style = "overflow-y: scroll;overflow-x: scroll;",
                            plotOutput("pub_health",width = "100%", height = "550px")
                        )
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
                      p('We are medical students at', tags$a(href ='https://www.uonbi.ac.ke/','The University of Nairobi'), 'and the goal behind this project is to enable an easy access to COVID-19 related data for Africa and the Middle East as there are few sources offereing this service currently.'),
                      p('For any question or feedback, you can either open an', tags$a(href ='https://github.com/amayomode/africa-covid-dashboard','issue'), 'or contact us on', tags$a(href='https://twitter.com/amayo_mordecai','Twitter.'))
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
        select(country, confirmed, deaths, recovered) %>% 
        group_by(country) %>% 
        summarise(New_Cases =diff(tail(confirmed,2)), 
                  Cases = tail(confirmed,1), 
                  Deaths=tail(deaths,1), 
                  Recoveries=tail(recovered,1))
    
    #trend data
    plot_data <- filtered_dta %>% select(country,date, confirmed,deaths,recovered)%>%
        group_by(date) %>% 
        summarise(cases=sum(confirmed), 
                  deaths=sum(deaths),
                  recovered=sum(recovered))
    
    #Outputs
    
    # 1. Raw data
    
    # 1.1 render the data in a table
    last_date <-  as.Date(tail(filtered_dta$date,1),format = "%m/%d/%y")
    output$dataTable <- renderDT(
        sumary_dta, # data
        class = "display nowrap compact", # style
        rownames = FALSE,
        caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: left;',
            paste("Last updated", last_date)
        ),
        colnames = c('Country','New Cases','Confirmed Cases', 'Total Deaths', 'Recoveries'),
        options = list(pageLength = 25)
    )
    
    # 1.2 render the total cases values in value boxes 
    output$new_cases <- renderValueBox({
        
        #Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Fetching data", value = 0)
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
    
    # 2 Disease trends
    
    #2.1Trend graphs
    
    # 2.1.1 general trend
    output$general_trend <- renderPlotly({
        
        #Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Generating plots", value = 0)
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
        
        #plot
        plot_ly(plot_data, x = ~date) %>%
            add_trace(y = ~deaths, name = "Deaths", visible = T,type='scatter',mode='lines',color=I("#A93226")) %>%
            add_trace(y = ~cases, name = "Cases", visible = F, type='scatter',mode='lines',color=I("#D4AC0D")) %>%
            add_trace(y = ~recovered, name = "Recoveries", visible = F, type='scatter',mode='lines',color=I("#145A32")) %>%
            layout(
                yaxis = list(title = "Numbers"),
                updatemenus = list(
                    list(
                        y = 0.3,
                        buttons = list(
                            list(method = "restyle",
                                 args = list("visible", list(TRUE, FALSE, FALSE)),
                                 label = "Deaths"),
                            list(method = "restyle",
                                 args = list("visible", list(FALSE, TRUE, FALSE)),
                                 label = "Cases"),
                            list(method = "restyle",
                                 args = list("visible", list(FALSE, FALSE, TRUE)),
                                 label = "Recoveries")))
                )
            )
    })
    
    # 2.1.2 country specific trend
    output$country_trend <- renderPlotly({
        filtered_dta %>% filter(country == input$country)%>%
            plot_ly(x = ~date) %>%
            add_trace(y = ~deaths, name = "Deaths", visible = T,type='scatter',mode='lines',color=I("#A93226")) %>%
            add_trace(y = ~confirmed, name = "Cases", visible = F, type='scatter',mode='lines',color=I("#D4AC0D")) %>%
            add_trace(y = ~recovered, name = "Recoveries", visible = F, type='scatter',mode='lines',color=I("#145A32")) %>%
            layout(
                yaxis = list(title = "Numbers"),
                updatemenus = list(
                    list(
                         y = 0.3,
                         buttons = list(
                             list(method = "restyle",
                                  args = list("visible", list(TRUE, FALSE, FALSE)),
                                  label = "Deaths"),
                             list(method = "restyle",
                                  args = list("visible", list(FALSE, TRUE, FALSE)),
                                  label = "Cases"),
                             list(method = "restyle",
                                  args = list("visible", list(FALSE, FALSE, TRUE)),
                                  label = "Recoveries")))
                )
            )
    })
    
    #2.1.4 disease progression
    output$progression <- renderPlot({
        plot_covid19_stripes(
            filtered_dta,type = "confirmed",min_cases = 300,
            sort_countries = "start"
        )
    })
    
    #3.maps 
    
    # 3.1 Total cases in their value boxes
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
    
    # 3.2 africa's map
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
        
        #plot
        map_covid19(merged_dta, type = "confirmed", per_capita = TRUE, 
                    region = "Africa")
    })
    
    # 3.3 middle east's map
    output$mdd_east <- renderPlot({
        map_covid19(merged_dta, type = "confirmed", per_capita = TRUE, 
                    region = list(x = c(35, 100), y = c(-9, 70)))
    })
    
    # 4 contol measures
    
    # 4.1 scoial distancing measures
    output$social_distancing <- renderPlot({
        filtered_dta %>%
            group_by(iso3c) %>%
            summarise(
                confirmed_cases = max(confirmed, na.rm = TRUE),
                soc_dist_measures = max(soc_dist)
            ) %>%
            filter(confirmed_cases >= 1000) %>%
            ggplot(df, mapping = aes(x = confirmed_cases,
                                     y = soc_dist_measures)) +
            geom_point() +
            geom_label_repel(aes(label = iso3c)) +
            scale_x_continuous(trans='log10', labels = comma)+
            xlab('Confirmed Cases')+
            ylab('Social Distancing Measures')
    })
    
    # 4.2 movement restriction measures
    output$mvt_restriction <- renderPlot({
        filtered_dta %>%
            group_by(iso3c) %>%
            summarise(
                confirmed_cases = max(confirmed, na.rm = TRUE),
                restriction = max(mov_rest)
            ) %>%
            filter(confirmed_cases >= 1000) %>%
            ggplot(df, mapping = aes(x = confirmed_cases,
                                     y = restriction)) +
            geom_point() +
            geom_label_repel(aes(label = iso3c)) +
            scale_x_continuous(trans='log10', labels = comma)+
            xlab('Confirmed Cases')+
            ylab('Movement Restriction')
    })
    
    # 4.3 lock down measures
    output$lock_down <- renderPlot({
        filtered_dta %>%
            group_by(iso3c) %>%
            summarise(
                confirmed_cases = max(confirmed, na.rm = TRUE),
                lock_down = max(soc_dist)
            ) %>%
            filter(confirmed_cases >= 1000) %>%
            ggplot(df, mapping = aes(x = confirmed_cases,
                                     y = lock_down)) +
            geom_point() +
            geom_label_repel(aes(label = iso3c)) +
            scale_x_continuous(trans='log10', labels = comma)+
            xlab('Confirmed Cases')+
            ylab('Lock Down Measures')
    })
    
    # 4.4 public health policies
    output$pub_health <- renderPlot({
        filtered_dta %>%
            group_by(iso3c) %>%
            summarise(
                confirmed_cases = max(confirmed, na.rm = TRUE),
                pub_health = max(soc_dist)
            ) %>%
            filter(confirmed_cases >= 1000) %>%
            ggplot(df, mapping = aes(x = confirmed_cases,
                                     y = pub_health)) +
            geom_point() +
            geom_label_repel(aes(label = iso3c)) +
            scale_x_continuous(trans='log10', labels = comma)+
            xlab('Confirmed Cases')+
            ylab('Public Health Policies')
    })
    
    
    }

shinyApp(ui, server)