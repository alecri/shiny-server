#' Shiny app for displaying incidence and mortality of Prostate Cancer in Sweden over time
#'  data obtained from social styrelsen:
#'   https://sdb.socialstyrelsen.se/if_can/val.aspx
#'   https://sdb.socialstyrelsen.se/if_dor/val.aspx
#' Designed by Alessio Crippa 191129

# style for avoiding flickering
tags$style(type="text/css", ".recalculating {opacity: 1.0;}")

# libraries needed
#pacman::p_load(shiny, tidyverse, swemaps, plotly, ggthemes, data.table, DT)
pacman::p_load(tidyverse, plotly, swemaps, ggthemes, gridExtra)
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
                selectInput("region", "Region:", unique(indices$Region), selected = "Riket", multiple = T),
                selectInput("age_cat", "Category of age:", 
                            choices = unique(indices$age_cat), selected = "Totalt", multiple = T),
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
                                 choices = unique(indices$age_cat), selected = "Totalt", multiple = F),
                     sliderInput("year", "Years range", sep = "",
                                 min = 1970, max = 2018, value = 2000, animate = T),
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
                      age_cat %in% input$age_cat,
                      År %inrange% input$year_range
        )
        if (input$rate != "all") dat <- filter(dat, rate == input$rate)
        dat
    })
    
    output$p_rate <- renderPlotly({
        p <- ggplot(dat(), aes(År, value, linetype = rate, col = Region)) +
            geom_point() +
            geom_line() +
            labs(x = "Year", y = "Rate (x 100 000)") +
            facet_wrap(age_cat ~ ., scale = "free")
        ggplotly(p)
    })
    
    output$tab_rate <- renderDataTable({
        dat() %>% 
            pivot_wider(names_from = "rate", values_from = value)
    })
    
    
    # Second panel: reactive data and output 
    dat_map <- reactive({
        dat_map <- filter(map_indices,
                      age_cat == input$age_cat_map,
                      År  == input$year
        )
        dat_map
    })

    output$p_map_inc <- renderPlotly({
        ggplotly(
            filter(dat_map(), rate == "incidence") %>%
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
            filter(dat_map(), rate == "mortality") %>%
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
            select(Region, År, age_cat, rate, value) %>% 
            distinct() %>% 
            pivot_wider(id_cols = c("Region", "År", "age_cat"), names_from = "rate", 
                        values_from = value)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
