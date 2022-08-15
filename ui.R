
source("global.R")

options(spinner.color="#7abbe6", spinner.color.background="#ffffff", spinner.size=2)

ui <-
  function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
      url <- oauth2.0_authorize_url(api, app, scope = scope)
      redirect <- sprintf("location.replace(\"%s\");", url)
      tags$script(HTML(redirect))
    } else {
      shinyUI(
        fluidPage(
          titlePanel("YNAB spending tracker for groups of budget categories"),
          tags$head(tags$style(
            type="text/css",
            "#image img {max-width: 100%; width: auto; height: auto}",
            # HTML('#privacy{border-color:#7abbe6;border-radius:10px;border-width:medium;text-color:#7abbe6}')
            "#plot_lines_title{font-size: 24px}",
            "#plot_bars_title{font-size: 24px}",
            "#plot_lines_subtitle{font-size: 20px}",
            "#plot_bars_subtitle{font-size: 20px}"
          )),
          br(),
          sidebarLayout(
            position = "left",
            sidebarPanel(
              p("YNAB Grouped Budget Tracker is a R Shiny application that enables users to quickly check their progress towards self-defined groups of spending goals for a given month."),
              p("To get started, select one more more budget categories from the list below. If you encounter any issues with this application, feel free to report them by opening an issue on the Github repository ",a("here", href="https://github.com/eliason-j/ynab-group-tracker/issues", target="_blank"),"."),
              p(a("Privacy Policy", href = "https://gist.github.com/eliason-j/5de2fa2f1b8e38e4cbb90a678ebde951", target="_blank")),
              br(),
              uiOutput('year_values'),
              selectInput(
                "month", "Select month to inspect:",
                stats::setNames(1:12, month.name),
                selected = lubridate::month(lubridate::today())
              ),
              uiOutput('category_values')
            ),
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Lines",br(),
                  textOutput("plot_lines_title"),
                  textOutput("plot_lines_subtitle"),
                  shinycssloaders::withSpinner(plotOutput("plot_lines", height = "500px"), type = 2),
                  br(), br(),
                  p("The solid line displays your net spending for the selected categories over the course of the selected month. The dotted line represents 'on-track' spending towards the combined budgeted amounts for those categories.", style = "font-size:smaller;font-style:italic")
                ),
                tabPanel(
                  "Bars",br(),
                  textOutput("plot_bars_title"),
                  textOutput("plot_bars_subtitle"),
                  shinycssloaders::withSpinner(plotOutput("plot_bars", height = "600px"), type = 2),
                  br(), br(),
                  p("The gray bar displays your budget goal, the blue bars represent 'on-track' spending towards the budget goal, and the orange bars display net spending to date.", style = "font-size:smaller;font-style:italic")
                )
              )
            )
          )
        )
      )
    }
  }
