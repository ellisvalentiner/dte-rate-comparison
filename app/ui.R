library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  navbarPage(
    # App title
    title = "DTE Rate Comparison",
    theme = shinytheme(theme = "lumen"),
    
    # Main tab
    tabPanel(
      title = "Rate Comparison",
      icon = icon("balance-scale"),
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          titlePanel("Upload Energy Usage Data"),
          p(
            "This tool will show you whether you would have saved money",
            "by enrolling in the DTE Dynamic Peak Pricing rate program,",
            "compared to the standard rate."
          ),
          fileInput(
            inputId = "file",
            label = h4("Choose CSV File"),
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"),
          ),
          titlePanel("Instructions"),
          p(
            "DTE provides online access to energy usage data for",
            "residential customers who have an advanced metering",
            "infrastructure (AMI) meter."
          ),
          tags$ol(
            tags$li(
              "Login to your DTE account at",
              a(href = "https://newlook.dteenergy.com", "https://newlook.dteenergy.com")
            ),
            tags$li(
              "Click on \"Energy Usage Data\" in the navigation sidebar on the left."
            ),
            tags$li(
              "Select the \"Electric (Hourly)\" radio button for \"Type of Service\" and",
              "the date range you are interested in. It is recommended to select a wide",
              "date range."
            ),
            tags$li(
              "Download your electric usage data in CSV format by clicking the \"Download Report\" button."
            ),
            tags$li(
              "Upload the electricity usage CSV file by clicking the \"Browse\" button on this page."
            )
          ),
          # titlePanel("About this project"),
          # p(
          #   "This is a simple tool to compare the cost between DTE's",
          #   "standard electric rate and the Dynamic Peak Pricing rate."
          # ),
          # p("This project was built using the",
          #   strong(
          #     a(href = "https://shiny.rstudio.com", "shiny")
          #   ),
          #   "framework in R."),
          width = 4
        ),
        mainPanel(
          titlePanel("Rate Comparison"),
          h4(htmlOutput("placeholder")),
          h4(htmlOutput("recommendation")),
          
          column(
            plotOutput(outputId = "usage_bars",
                       width = "303px"),
            align = "center",
            width = 12
          ),
          h4(htmlOutput("hourly_text")),
          column(
            plotOutput(outputId = "hourly_usage",
                       width = "660px"),
            align = "center",
            width = 12
          )
        )
      )
    ),
    navbarMenu(
      title = "More",
      icon = icon("bars"),
      tabPanel(
        title = "About This Project",
        icon = icon("info-circle"),
        fluid = TRUE,
        column(
          width = 6,
          offset = 3,
          
          h2("About This Project"),
          h4(
            "This tool shows you whether you would have saved money by enrolling in",
            "the DTE Dynamic Peak Pricing rate program compared to the standard rate."
          ),
          h4(
            "This project is written in",
            a(src="https://cran.r-project.org", strong("R")),
            "and uses the",
            a(src="https://shiny.rstudio.com", strong("Shiny")),
            "package.",
            "The application is deployed using",
            a(src = "https://www.shinyapps.io", strong("shinyapps.io")),
            "and the",
            a(src = "https://github.com/ellisvalentiner/dte-rate-comparison", strong("code")),
            "is available on GitHub."
          )
        )
      ),
      tabPanel(
        title = "Frequently Asked Questions",
        icon = icon("question-circle"),
        fluid = TRUE,
        fluidRow(
          column(
            width = 6,
            offset = 3,
            
            h2("Frequently Asked Questions"),
            
            h3("What is this project?"),
            h4(
              "This is a simple tool to compare the cost between DTE's standard electric",
              "rate and the Dynamic Peak Pricing rate."
            ),
            
            h3("What is ", strong("Dynamic Peak Pricing"), "?"),
            h4(
              strong("Dynamic Peak Pricing"),
              "is a time-of-use rate plan offered by",
              strong("DTE Energy"),
              "to residential customers.",
              "Instead of being charged the standard rates, residential customers are",
              "charged different amounts depending on when electricity is used."
            ),
            h4(
              "The",
              strong("off-peak"),
              "price is 4.8¢/kWh and applies between weekdays between 11 PM and 7 AM,",
              "weekends and designiated holidays.",
              "The",
              strong("mid-peak"),
              "price is 9.2¢/kWh on weekdays from 7 AM to 3 PM and 7PM to 11 PM, and the",
              strong("on-peak"),
              "price is 16.6¢/kWh on weekdays from 3 PM to 7 PM.",
              "Also there is a",
              strong("critical peak"),
              "with a price of 95¢/kWh on weekdays during the",
              strong("on-peak"),
              "time period that can occur up to 14 times per calendar year."
            ),
            h4("For more information visit DTE's website."),
            column(
              img(src = "https://newlook.dteenergy.com/wps/wcm/connect/507a10d2-7e9f-47bc-a423-b1d87effac80/1/PeakPricingChart.png?MOD=AJPERES&CACHEID=507a10d2-7e9f-47bc-a423-b1d87effac80/1",
                  width = "460px"),
              p("Source: DTE Energy"),
              align = "center",
              width = 12
            ),
            
            h3("What are", strong("time-of-use"), "electricity rates?"),
            h4(
              "Time-of-use rate plans charge customers different amounts for",
              "electricity depending on when it is used.",
              "It costs the utility company more to produce and distribute",
              "electricity at certain times of day.",
              "During the day utility companies can some generate electricity",
              "from solar to meet demand."
            ),
            
            h3("Why are these cost estimates different from my bill?"),
            h4(
              "This site calculates the costs for electricity usage only and",
              "doesn't include delivery charges, other fees, or credits that",
              "may appear on your bill."
            )
          )
        )
      )
    )
  )
))
