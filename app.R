## app.R ##
library(shiny)
library(dplyr)
library(shinydashboard)
library(rvest)
library(leaflet)
library(sf)
library(plotly)


get.data <- function(){
    
    # country_info <- read_excel("country_info.xlsx")
    url <- "https://www.worldometers.info/coronavirus/?mc_cid=d17a0161cb&mc_eid=ea1b6e3dc9#countries"
    
    covid_df <- url %>%
        html() %>%
        html_nodes(xpath='//*[@id="main_table_countries_today"]') %>%
        html_table()
    covid_df <- covid_df[[1]]
    
    # covid_df <- covid_df %>% left_join(country_info, by = c("Country,Other" = "country_english"))
    
    covid_df$TotalCases <- gsub(",", "", covid_df$TotalCases)
    covid_df$TotalDeaths <- gsub(",", "", covid_df$TotalDeaths)
    covid_df$TotalRecovered <- gsub(",", "", covid_df$TotalRecovered)
    covid_df$ActiveCases <- gsub(",", "", covid_df$ActiveCases)
    covid_df$`Serious,Critical` <- gsub(",", "", covid_df$`Serious,Critical`)
    
    covid_df$TotalCases <- as.numeric(covid_df$TotalCases)
    covid_df$TotalDeaths <- as.numeric(covid_df$TotalDeaths)
    covid_df$TotalRecovered <- as.numeric(covid_df$TotalRecovered)
    covid_df$ActiveCases <- as.numeric(covid_df$ActiveCases)
    covid_df$`Serious,Critical` <- as.numeric(covid_df$`Serious,Critical`)
    
    return(covid_df)
    
}


log_it <- function(x) 10^(ceiling(log10(x)))

# read world
world_df <- st_read("ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp", as_tibble = TRUE)
world_df <- world_df %>%
    select(NAME_EN, geometry)

covid_df <- get.data()


covid_df$`Country,Other`[covid_df$`Country,Other` == "USA"] <- "United States of America"
covid_df$`Country,Other`[covid_df$`Country,Other` == "UK"] <- "United Kingdom"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Macedonia"] <- "Republic of Macedonia"
covid_df$`Country,Other`[covid_df$`Country,Other` == "S. Korea"] <- "South Korea"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Czechia"] <- "Czech Republic"
covid_df$`Country,Other`[covid_df$`Country,Other` == "UAE"] <- "United Arab Emirates"
covid_df$`Country,Other`[covid_df$`Country,Other` == "North Macedonia"] <- "Republic of Macedonia"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Faeroe Islands"] <- "Faroe Islands"
covid_df$`Country,Other`[covid_df$`Country,Other` == "DRC"] <- "Democratic Republic of the Congo"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Macao"] <- "Macau"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Bahamas"] <- "The Bahamas"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Eswatini"] <- "eSwatini"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Cabo Verde"] <- "Cape Verde"
covid_df$`Country,Other`[covid_df$`Country,Other` == "St. Barth"] <- "Saint-Barthélemy"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Congo"] <- "Republic of the Congo"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Gambia"] <- "The Gambia"
covid_df$`Country,Other`[covid_df$`Country,Other` == "CAR"] <- "Central African Republic"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Turks and Caicos"] <- "Turks and Caicos Islands"
covid_df$`Country,Other`[covid_df$`Country,Other` == "St. Vincent Grenadines"] <- "Saint Vincent and the Grenadines"
covid_df$`Country,Other`[covid_df$`Country,Other` == "Timor-Leste"] <- "East Timor"
covid_df$`Country,Other`[covid_df$`Country,Other` == "China"] <- "People's Republic of China"




######################################

joined <- world_df %>% left_join(covid_df, by = c("NAME_EN" = "Country,Other"))

# joined <- joined %>% 
#     mutate(
#         country_dari = case_when(
#             NAME_EN == "South Sudan" ~ "سودان جنوبی",
#             NAME_EN == "Turkmenistan" ~ "ترکمنستان",
#             NAME_EN == "Tajikistan" ~ "تاجکستان",
#             NAME_EN == "Yemen" ~ "یمن",
#             NAME_EN == "Somaliland" ~ "سومالیلند",
#             NAME_EN == "Malawi" ~ "ملاوی",
#             NAME_EN == "Lesotho" ~ "لیسوتو",
#             NAME_EN == "Western Sahara" ~ "سهارای جنوبی",
#             NAME_EN == "Burundi" ~ "بروندی",
#             NAME_EN == "Kosovo" ~ "کوزوفو",
#             NAME_EN == "North Korea" ~ "کوریای شمالی",
#             NAME_EN == "Sierra Leone" ~ "سیری لیون",
#             is.na(country_dari) ~ NAME_EN,
#             TRUE ~ country_dari
#         )
#     )


pal <- colorNumeric(
    palette = "Blues",
    domain = c(0,log(log_it(joined$ActiveCases))))

joined$TotalRecovered[is.na(joined$TotalRecovered)] <- 0
joined$TotalCases[is.na(joined$TotalCases)] <- 0
joined$TotalDeaths[is.na(joined$TotalDeaths)] <- 0


popup_ht <- base::sprintf(
    '
  <strong><span style="font-size: 12px; color: #58585A; font-family:Arial;">%s</span>
  <br>
  <span style="font-size: 12px;  "> Active cases: %s </strong></span>
  <br>
  <span style="font-size: 12px;">
  <strong>Death: %g </strong></span>
  <br>
  <span style="font-size: 12px; color: green;">
  <strong>Recovered: %g </strong></span>',
    joined$NAME_EN,joined$ActiveCases, joined$TotalDeaths, joined$TotalRecovered)%>%   
    lapply(htmltools::HTML)



total <- covid_df %>% filter(`Country,Other` == "Total:")


source("time_sereis.R")

# for global time series graph

# sub3 <- joined_data %>% ungroup() %>% select(-country)  %>% group_by(Date) %>% 
#   summarise(
#     cases = sum(cases, na.rm = F),
#     active_cases = sum(active_cases, na.rm = T),
#     recov = sum(recov, na.rm = T),
#     deaths = sum(deaths, na.rm = T)
#   )


#################### U I ########################

ui <- dashboardPage(
      dashboardHeader(title = "Covid-19 Live Tracker"),
      dashboardSidebar(collapsed = T,
                       sidebarMenu(
                           menuItem("Map", tabName = "dashboard", icon = icon("map")),
                           menuItem("Worldometers Data", icon = icon("th"), tabName = "wdata"),
                           menuItem("CSSE Time Series", icon = icon("th"), tabName = "gdata"),
                           
                           hr(),
                           p("Data Sources:", style = "margin-left: 10px;"),
                           p("Map data: Worldmeters.info", style = "margin-left: 10px;"),
                           p("(Frequent updates)", style = "margin-left: 10px;" ),
                           
                           p("Time series Data: CSSE", style = "margin-left: 10px; margin-top:10px;"),
                           p("(Updates once in 24 hours)", style = "margin-left: 10px;"),
                           a(icon("github"), "Scraping R code", href="https://github.com/nzfarhad/scrape_covid_19", target ="blank",  style = "margin-left:10px; margin-top:30px;"),
                         
                           p("Sayed Farhad Nabizada - Apr 04 2020", style = "margin-left: 10px; margin-top:10px; font-size:10px;")
                           
                           
                       )
                       
                       ),
    
      dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            tabItem(tabName = "dashboard",
                    
                    fluidRow(
                        box( width = 8,
                             leafletOutput("mymap", width = "100%", height = 600) ),
                        
                        
                        
                        box(
                            # title = "Time series",
                            span(textOutput("level"), style = "color:#26486e; font-size:22px; font-weight: bold;"),
                            span(textOutput("total_case"), style = "color:#26486e; font-size:16px; font-weight: bold;"),
                            span(textOutput("active"), style = "color:#26486e; font-size:16px;font-weight: bold;"),
                            span(textOutput("recov"), style = "color:#26486e; font-size:16px; font-weight: bold;"),
                            span(textOutput("death"), style = "color:#26486e; font-size:16px; font-weight: bold;"),
                            span(textOutput("tests"), style = "color:#26486e; font-size:16px; font-weight: bold;"),
                            span(textOutput("death_per_mil"), style = "color:#26486e; font-size:16px; font-weight: bold;"),
                            span(textOutput("death_rate"), style = "color:#26486e; font-size:16px; font-weight: bold;"),
                            plotlyOutput("mytime"),
                            width = 4
                            
                        ),
                        
                    )
                    
            ), # abitem dashboard
            
            tabItem(tabName = "wdata",
                    
                    DT::dataTableOutput("wmdata"),
                    downloadButton("downloadData", "Download")
                    
            ),
            tabItem(tabName = "gdata",
                    DT::dataTableOutput("CSSE_data"),
                    downloadButton("downloadData_csse", "Download")
                    
            )
            
        ) # Tab items
        
    ) # dashboardBody
)


##################### S E R V E R ##################

server <- function(input, output) {
    
    output$level = renderText({
        paste0("World")
    })
    output$total_case = renderText({
        paste0("Total Cases: ",total$TotalCases)
    })
    output$active = renderText({
        paste0("Active Cases: ",total$ActiveCases)
    })
    output$recov = renderText({
        paste0("Recovered: ",total$TotalRecovered)
    })
    output$tests = renderText({
        paste0("Tests: ",total$TotalTests)
    })
    output$death = renderText({
        paste0("Death: ",total$TotalDeaths)
    })
    output$death_per_mil = renderText({
        paste0("Deaths / 1 Million Population: ",total$`Deaths/1M pop`)
    })
    output$death_rate = renderText({
        paste0("Mortality Rate: ", round(total$TotalDeaths/ total$TotalCases * 100, 2), "%")
    })
    
    
    # Map output
    RV <- reactiveValues(Clicks=list())
    
    output$mymap <- renderLeaflet({
        leaflet(joined) %>% 
            addPolygons(
                color = "#58585A",
                fillColor = "#FFFFFF",
                weight = 1,
                smoothFactor = 0.5,
                opacity = 0.1,
                fillOpacity = 0.4,
                options = list(zIndex = 99)
            ) %>% addPolygons(data=joined,
                              color = "white",
                              fillColor = ~pal((log((joined$ActiveCases)+1))),
                              weight = 0.5,
                              smoothFactor = 1,
                              opacity = 1.0,
                              fillOpacity = 1,
                              label = popup_ht,
                              layerId = ~NAME_EN,
                              options = list(zIndex = 200),
                              highlightOptions = highlightOptions( fillColor="#75A1D0",
                                                                   color="#75A1D0",
                                                                   weight = 1.4,
                                                                   bringToFront = F
                              ),
                              labelOptions = labelOptions(
                                  style = list("font-weight" = "normal",
                                               "padding" = "3px 8px",
                                               "font-family" = "Arial",
                                               "border-color" = "#D1D3D4"
                                  ),
                                  offset = c(8,0),
                                  direction = "right",
                                  opacity=0.9),
                              
                              popupOptions = popupOptions(
                                  minWidth = 30,
                                  style = list("font-weight" = "normal",
                                               "padding" = "3px 8px",
                                               "font-family" = "Arial",
                                               "border-color" = "#D1D3D4",
                                               "text-align" = "right"
                                  ),
                                  offset = c(8,0),
                                  direction = "right",
                                  opacity=0.9),
                              
                              
            ) %>% setView(lat = 30, lng = 15 ,zoom = 2)

    }) # End plot 
    
    # Get CSSE time series data 
    csse_data <- csse_data()
    
    # Global time series graph
    # output$mytime <- renderPlotly({
    #     
    #     fig <- plot_ly(sub3, x = ~Date, y = ~cases, name = 'Total Cases', type = 'scatter', mode = 'lines',
    #                    line = list(color = 'orange', width = 4)) 
    #     
    #     fig <- fig %>% add_trace(y = ~active_cases, name = 'Active Cases', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    #     fig <- fig %>% add_trace(y = ~recov, name = 'Recovered', line = list(color = 'green', width = 4))
    #     fig <- fig %>% add_trace(y = ~deaths, name = 'Deaths', line = list(color =  'rgb(205, 12, 24)', width = 4))
    #     fig <- fig %>% layout(title = "Time Series", legend = list(x = 0.02, y = 1) )
    #     fig
    #     
    # })
    
    observeEvent({input$mymap_shape_click}, {
        
        #create object for clicked polygon
        click <- input$mymap_shape_click
        RV$Clicks <- c(RV$Clicks,click$id) 
        # output$tt <- renderText(click$id)
        
        sub <- reactive({
          csse_data %>%
            filter(country == click$id)
        })
        
        # txt outputs
        output$level = renderText({
            paste0(click$id)
        })
        output$total_case = renderText({
            paste0("Total Cases: ",covid_df$TotalCases[covid_df$`Country,Other` == click$id])
        })
        output$active = renderText({
            paste0("Active Cases: ",covid_df$ActiveCases[covid_df$`Country,Other` == click$id])
        })
        output$recov = renderText({
            paste0("Recovered: ",covid_df$TotalRecovered[covid_df$`Country,Other` == click$id])
        })
        output$tests = renderText({
            paste0("Tests: ",covid_df$TotalTests[covid_df$`Country,Other` == click$id])
        })
        output$death = renderText({
            paste0("Deaths: ",covid_df$TotalDeaths[covid_df$`Country,Other` == click$id])
        })
        output$death_per_mil = renderText({
            paste0("Deaths / 1 Million Population: ",covid_df$`Deaths/1M pop`[covid_df$`Country,Other` == click$id])
        })
        output$death_rate = renderText({
            paste0("Mortality Rate: ", round(covid_df$TotalDeaths[covid_df$`Country,Other` == click$id] / covid_df$TotalCases[covid_df$`Country,Other` == click$id] * 100, 2), "%")
        })
        
        output$mytime <- renderPlotly({
            
            fig <- plot_ly(sub(), x = ~Date, y = ~cases, name = 'Total Cases', type = 'scatter', mode = 'lines',
                           line = list(color = 'orange', width = 4)) 
            fig <- fig %>% add_trace(y = ~active_cases, name = 'Active Cases', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
            fig <- fig %>% add_trace(y = ~recov, name = 'Recovered', line = list(color = 'green', width = 4))
            fig <- fig %>% add_trace(y = ~deaths, name = 'Deaths', line = list(color =  'rgb(205, 12, 24)', width = 4))
            fig <- fig %>% layout(title = "Time Series", legend = list(x = 0.02, y = 1) )
            fig
            
        })
        
    })
    
    # data table woldmeters
    output$wmdata <- DT::renderDataTable({
        DT::datatable(covid_df, options = list(orderClasses = TRUE))
        
    })
    # dowload data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("worlmeters_covid19_data", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(covid_df, file, row.names = FALSE)
        }
    )
    
    # data table csse
    output$CSSE_data <- DT::renderDataTable({
        DT::datatable(joined_data, options = list(orderClasses = TRUE))
        
    })
    # dowload data
    output$downloadData_csse <- downloadHandler(
        filename = function() {
            paste("CSSE_covid19_data", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(joined_data, file, row.names = FALSE)
        }
    )
    
}

shinyApp(ui, server)