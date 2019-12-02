#' Shiny app for displaying incidence and mortality of Prostate Cancer in Sweden over time
#'  data obtained from social styrelsen:
#'   https://sdb.socialstyrelsen.se/if_can/val.aspx
#'   https://sdb.socialstyrelsen.se/if_dor/val.aspx
#' Designed by Alessio Crippa 191129

# libraries needed
library(shiny)
library(tidyverse)
library(plotly)
library(ggthemes)
library(DT)
theme_set(theme_bw())

# load data from incidence-mortality-PC.R
load("www/data_pc.Rdata")

ui <- navbarPage(
    "Incidence and mortality of Prostate Cancer in Sweden over time",
    
    # First panel: time serie plots
    tabPanel("Time serie",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("rate", "Rate:", 
                                 choices = list("Incidence & mortality" = "all", "Incidence" = "incidence",
                                                "Mortality" = "mortality"), selected = "all"),
                     selectInput("region", "Region:", unique(indices$Region), selected = "Sweden", multiple = T),
                     selectInput("age_cat", "Category of age:", 
                                 choices = unique(indices$`Age category`), selected = "Totalt", multiple = T),
                     sliderInput("year_range", "Years range", sep = "",
                                 min = 1970, max = 2018, value = c(2000, 2018)),
                     width = 3
                 ),
                 mainPanel(
                     plotlyOutput("p_rate"),
                     dataTableOutput("tab_rate")
                 )
             )
    ),
    
    # Second panel: map plots
    tabPanel("Map",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("age_cat_map", "Category of age:", 
                                 choices = unique(indices$`Age category`), selected = "Totalt", multiple = F),
                     sliderInput("year", "Years range", sep = "",
                                 min = 1970, max = 2018, value = 2000, 
                                 animate = animationOptions(interval = 3000, loop = TRUE)),
                     p("See the gif", tags$a(href="https://alecri.github.io/downloads/map_rate.gif", "here")),
                     width = 3
                 ),
                 mainPanel(
                     fluidRow(
                         splitLayout(cellWidths = c("50%", "50%"), 
                                     plotlyOutput("p_map_inc", height = 600), 
                                     plotlyOutput("p_map_mort", height = 600))
                     ),
                     #plotlyOutput("p_map"),
                     dataTableOutput("tab_rate_map")
                 )
             )
    )
    
)

server <- function(input, output) {
    
    # First panel: reactive data and output 
    dat <- reactive({
        dat <- filter(indices, 
                      Region %in% input$region, 
                      `Age category` %in% input$age_cat,
                      Year <= input$year_range[2] &  Year >= input$year_range[1]
        )
        if (input$rate != "all") dat <- filter(dat, Rate == input$rate)
        dat
    })
    
    output$p_rate <- renderPlotly({
        p <- ggplot(dat(), aes(Year, value, linetype = Rate, col = Region)) +
            geom_point() +
            geom_line() +
            labs(y = "Rate (x 100 000)") +
            facet_wrap(`Age category` ~ ., scale = "free")
        ggplotly(p)
    })
    
    output$tab_rate <- renderDataTable({
        dat() %>% 
            pivot_wider(names_from = "Rate", values_from = value)
    })
    
    
    # Second panel: reactive data and output 
    dat_map <- reactive({
        dat_map <- filter(map_indices,
                          `Age category` == input$age_cat_map,
                          Year  == input$year
        )
        dat_map
    })
    
    output$p_map_inc <- renderPlotly({
        ggplotly(
            filter(dat_map(), Rate == "Incidence") %>%
                ggplot(aes(ggplot_long, ggplot_lat, group = Region, fill = value)) +
                geom_polygon() +
                coord_equal() +
                theme_map() +
                theme(legend.position = c(.9, .2)) +
                labs(title = "Incidence rate (x 100 000)") +
                scale_fill_gradientn(colours = heat.colors(n = 7, rev = T))
        )
    })
    output$p_map_mort <- renderPlotly({
        ggplotly(
            filter(dat_map(), Rate == "Mortality") %>%
                ggplot(aes(ggplot_long, ggplot_lat, group = Region, fill = value)) +
                geom_polygon() +
                coord_equal() +
                theme_map() +
                theme(legend.position = c(.9, .2)) +
                labs(title = "Mortality rate (x 100 000)") +
                scale_fill_gradientn(colours = heat.colors(n = 7, rev = T))
        )
    })
    
    output$tab_rate_map <- renderDataTable({
        dat_map() %>% 
            select(Region, Year, `Age category`, Rate, value) %>% 
            distinct() %>% 
            pivot_wider(id_cols = c("Region", "Year", "Age category"), names_from = "Rate", 
                        values_from = value)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
