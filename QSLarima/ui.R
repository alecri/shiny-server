library(shiny)
library(dygraphs)
library(plotly)
library(timevis)

shinyUI(
  navbarPage(
    "Quit Smoking Line",
    
    tabPanel(
      "Home",
      h2("Intervention time series analysis of the smoking cessation 
                quitline: a 16-years retrospective study in Sweden", 
         align = "center"),
      br(),
      h3("Xingwu Zhou", tags$sup(1), ", Alessio Crippa ", tags$sup(1), ", 
         Rosaria Galanti, ", tags$sup(2), " and Nicola Orsini", tags$sup(1),
         align = "center"),
      br(),
      h4(tags$sup(1), "Biostatistics Team, Department of Public Health Sciences, Karolinska Institutet"),
      h4(tags$sup(2), "Centre of Epidemiology and Community Medicine"),
      br(),
      h3("Abstract", align = "center"),
      br(),
      p(
        strong("Background:"), "Knowledge about the series of phone calls received by a smoking cessation 
        quit-line in response to different interventions aiming at reducing tobacco 
        smoking is currently lacking."
      ),
      p(
        strong("Aim:"), "Aim of this study is to examine the possible effect of four types of policies 
        on the calling rates to the Swedish smoking cessation quitline: a campaign on 
        passive smoking (Jan 2001); 
        placing larger warnings on cigarette packs (Sept 2002); 
        banning smoking from restaurants (Jun 2005); 
        and a 10% tax increase (Jan 2012)."
      ),
      p(
        strong("Methods:"), "We used 16-years of monthly data collected between January 1999 to 
        December 2014 (192 months) counting a total of 162,978 phone calls. 
        Upon definition of four pre-post intervention intervals, we used intervention 
        time series ARIMA (Auto-Regressive Integrated Moving Average) models where 
        the outcome was defined as calling rates expressed per 100,000 smokers. 
        Rate ratio (RR) at 6 months after intervention together with a 95% confidence 
        interval (CI) were derived from the model."
      ),
      p(
        strong("Results:"), "The campaign on passive smoking on Jan 2001 was associated with a 85% higher calling rate 
        (95% CI=1.13-3.04). Larger warnings on cigarette packs in Sept 2002 
        conferred a 53% increment in the calling rate (95% CI=1.20-1.94). 
        Smoking-free restaurants was associated with a significant 11% (95% CI=1.00-1.1.23) 
        higher calling rate. The 10% tobacco tax increase in Jan 2012 had no 
        significant effect on the calling rate (RR=0.98, 95% CI=0.82-1.15)."
      ),
      p(
        strong("Conclusions:"), "Within an overall decreasing trend in the population of smokers in Sweden, 
        we were able to detect differential effects of smoking policies on the calling 
        rates to the quitline, the most effective being the campaign on passive smoking 
        and the larger warnings signs on the cigarette packs."
      )
    ),
    
    tabPanel(
      "Time-serie",
      h4("Four public health interventions on the number of phone calls received 
        by the Swedish quitline"),
      timevisOutput("timeline"),
      br(),
      uiOutput("title_dy_ts"),
      checkboxInput("log", "Display counts", FALSE),
      #dygraphOutput("dy_ts"),
      plotlyOutput("pl_ts"),
      br()
  ),
  
  tabPanel("1st Interv",
           h2("Effect of a campaign on passive smoking (Jan 2001)"),
           br(),
           plotlyOutput("pl_1Int"),
           br(),
           h3("Rate Ratio"),
           # selectInput("tInt1", "Select month after intervantion:", 
           #             choices = 1:c(interval(data_milestone$start[1], data_milestone$wupp[1]) %/% months(1)),
           #             selected = 6),
           # uiOutput("Effect1"),
           plotlyOutput("RR1"),
           dataTableOutput("tableRR1")
           ),
  tabPanel("2nd Interv",
           h2("Effect of larger warnings on cigarette packs (Sept 2002)"),
           br(),
           plotlyOutput("pl_2Int"),
           br(),
           h3("Rate Ratio"),
           plotlyOutput("RR2"),
           dataTableOutput("tableRR2")
           ),
  tabPanel("3rd Interv",
           h2("Effect of banning smoking in restaurants (Jun 2005)"),
           br(),
           plotlyOutput("pl_3Int"),
           br(),
           h3("Rate Ratio"),
           plotlyOutput("RR3"),
           dataTableOutput("tableRR3")
           ),
  tabPanel("4th Interv",
           h2("Effect of a 10% tobacco tax increase (Jan 2012)"),
           br(),
           plotlyOutput("pl_4Int"),
           br(),
           h3("Rate Ratio"),
           plotlyOutput("RR4"),
           dataTableOutput("tableRR4")
           )
  # ,
  # tabPanel("Report",
  #          includeMarkdown("QTL_Monthly_Log_V4.md")
  #          )
  )
)