library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(duckdb)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(shinythemes)

drv = duckdb("project.db")
con = dbConnect(drv)

# Get tables necessary
rosterfact = dbGetQuery(con, "SELECT * FROM rosterfact")
dimteam = dbGetQuery(con, "SELECT * FROM dimteam")

# Close connect
dbDisconnect(con)


create_card = function(title, plotName) {
  return (
    card(
      card_header(title),
      plotlyOutput(plotName)
    )
  )
}


ui = page_navbar(
  title = "NCAA CBB Analysis",
  useShinyjs(),
  nav_panel(
    title = "Roster Sizes",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        pickerInput("season_filter", "Season:",
                    sort(unique(rosterfact$Season, na.rm = T)),
                    selected = sort(unique(rosterfact$Season, na.rm = T)),
                    options = pickerOptions(
                      actionsBox = T
                    ),
                    multiple = T
        ),
        pickerInput("conf_filter", "Conference:",
                    sort(unique(dimteam$Conference)),
                    selected = sort(unique(dimteam$Conference))[1]
        ),
        actionBttn(inputId = "filternil", label = "After NIL/Portal Only", style="gradient", size="sm"),
      ),
      layout_column_wrap(
        width = 1/2,
        create_card("National Roster Sizes", "rostersizemain"),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          create_card("By Conference", "rostersizeconf"),
          create_card("By Team", "rostersizeteam")
        )
      )
    )
  ),
  nav_panel(
    title = "Roster Retention",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        pickerInput("scope", "Scope:", c("National", "Conference", "Team"), selected = "National"),
        conditionalPanel("input.scope == 'Conference'",
                         pickerInput("conf", "Conference:", sort(unique(dimteam$Conference)),
                                     sort(unique(dimteam$Conference))[1])
        ),
        conditionalPanel("input.scope == 'Team'",
                         pickerInput("team", "Team:", sort(unique(dimteam$Name)),
                                     sort(unique(dimteam$Name))[1],
                                     options = pickerOptions(liveSearch = T),
                                     choicesOpt = list(content = stringr::str_trunc(sort(unique(dimteam$Name)), width = 20))
                         )
        ),
        HTML("<p><b>Retention:</b><br/>The % of players from the current roster who were also on the previous year's roster.</p>"),
        HTML("<p><b>Pre-NIL:</b> 2014-2020<br/><b>Post-NIL:</b> 2021-2024"),
        width="20%"
      ),
      layout_column_wrap(
        width=1,
        heights_equal="row",
        layout_column_wrap(
          width=1/2,
          height=100,
          heights_equal="row",
          card(
            card_header("Pre-NIL"),
            textOutput("prenil")
          ),
          card(
            card_header("Post-NIL"),
            textOutput("postnil")
          ),
        ),
        create_card("By Season", "retention")
      )
    )
  ),
  nav_panel(
    title = "Player Tenures",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        pickerInput("tenurescope", "Scope:", c("National", "Conference", "Team"), selected = "National"),
        conditionalPanel("input.tenurescope == 'Conference'",
                         pickerInput("conftenure", "Conference:", sort(unique(dimteam$Conference)),
                                     sort(unique(dimteam$Conference))[1])
        ),
        conditionalPanel("input.tenurescope == 'Team'",
                         pickerInput("teamtenure", "Team:", sort(unique(dimteam$Name)),
                                     sort(unique(dimteam$Name))[1],
                                     options = pickerOptions(liveSearch = T),
                                     choicesOpt = list(content = stringr::str_trunc(sort(unique(dimteam$Name)), width = 20))
                         )
        ),
        HTML("<p><b>Tenure:</b><br/>The # of seasons that a player stays with one team.</p>"),
        HTML("<p><b>Pre-NIL:</b> 2014-2020<br/><b>Post-NIL:</b> 2021-2024"),
        width="20%"
      ),
      layout_column_wrap(
        width=NULL,
        style = css(grid_template_columns = "3fr 1fr"),
        card(
          card_header("Player Tenure"),
          plotOutput("tenures")
        ),
        layout_column_wrap(
          width=1,
          heights_equal="row",
          card(
            card_header("Average Tenure"),
            textOutput("tenureavg")
          ),
          card(
            card_header("Median Tenure"),
            textOutput("tenuremed")
          ),
        )
      )
    )
  ),
  nav_panel(
    title = "Experience",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        pickerInput("expseason", "Season:",
                    sort(unique(rosterfact$Season, na.rm = T)),
                    selected = sort(unique(rosterfact$Season, na.rm = T)),
                    options = pickerOptions(
                      actionsBox = T
                    ),
                    multiple = T
        ),
        pickerInput("expconf", "Conference:", c("All", sort(unique(dimteam$Conference))),
                    "All"),
        conditionalPanel("input.exptab == 'Seasons Played'",
          HTML("<p><b>Experience:</b><br/>The # of seasons that a player has been on a roster in 
               the Power 5 basketball conferences, or the WCC/Mountain West.
               Experience includes the current season, so a freshman's experience would be 1.</p>")
        ),
        conditionalPanel("input.exptab == 'Minutes'",
                         HTML("<p><b>Minutes Returning:</b><br/>The % of the previous season's total minutes played that has returned for the current season.</p>")
        ),
        conditionalPanel("input.exptab == 'Scoring'",
                         HTML("<p><b>Scoring Returning:</b><br/>The % of the previous season's total points scored that has returned for the current season.</p>")
        ),
        HTML("<br/><p>Double click on any team in the legend to see just their data.</p>"),
        width="20%"
      ),
      tabsetPanel(
        id="exptab",
        tabPanel(
          "Seasons Played",
          HTML("<br/>"),
          plotlyOutput("sznexp", height = "100%")
        ),
        tabPanel(
          "Minutes",
          HTML("<br/>"),
          plotlyOutput("minsexp")
        ),
        tabPanel(
          "Scoring",
          HTML("<br/>"),
          plotlyOutput("ptsexp")
        )
      )
    )
  ),
  tags$head(tags$style(
    "#prenil, #postnil, #tenureavg, #tenuremed{
      font-size: 40px;}
      .card-body{
        justify-content:center; align-items: center;
      }
      #sznexp, #minsexp, #ptsexp{
        height: 75vh !important;
      }
      "
  )),
  footer = HTML('<p style="text-align: right; margin-right: 3%; font-size: 12pt;">
                <a class="link-offset-2 link-offset-3-hover link-underline link-underline-opacity-0 link-underline-opacity-75-hover" href="https://www.matthew-mcclain.com" target="_blank">Matthew McClain</a> &emsp;
                <a class="link-offset-2 link-offset-3-hover link-underline link-underline-opacity-0 link-underline-opacity-75-hover" href="https://github.com/mcclainclain" target="_blank">GitHub</a> &emsp; 
                <a class="link-offset-2 link-offset-3-hover link-underline link-underline-opacity-0 link-underline-opacity-75-hover" href="https://github.com/mcclainclain/uw-data-contest-2024" target="_blank">Source</a></p>')
)

