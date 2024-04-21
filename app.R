#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

theme_reach <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0, 
                                       lineheight = 0.9),
      
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(1.5), family = '' , 
                                       face = 'bold', hjust = -0.05, 
                                       vjust = 1.5, colour = '#3B3B3B'),
      axis.text =         element_text(),
      
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )+
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14)
    )
}



#regular libraries
library(tidyverse) 
library(teamcolors) 
library(ggimage)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(knitr)
library(stringr)
library(shiny)
library(DT)
library(fmsb)

CollegeWARJoin <- read_rds("data/CollegeWARJoin.rds") 

CWARname <- CollegeWARJoin %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)


CollegeWRWARFill <- read_rds("data/CollegeWRWARFill.rds")

CWRWARname <- CollegeWRWARFill %>% 
  group_by(player_id) %>% 
  mutate(CareerGrade = mean(grades_offense, na.rm = T)) %>% 
  filter(CareerGrade >= 50, routes >= 20, position == "WR",
         season >= 2010)  %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

CTEWARname <- CollegeWRWARFill %>% 
  group_by(player_id) %>% 
  mutate(CareerGrade = mean(grades_offense, na.rm = T)) %>% 
  filter(CareerGrade >= 50, routes >= 20, position == "TE",
         season >= 2010)  %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

IWARData <- read_rds("data/IndivWARData.rds") 

IWARname <- IWARData %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

WRWARFill <- read_rds("data/WRWARFill.rds") %>% 
mutate(ID = paste0(player, " - ", player_id))

IWRWARname <- WRWARFill %>% 
  group_by(player_id) %>% 
  mutate(CareerGrade = mean(grades_offense, na.rm = T)) %>% 
  filter(CareerGrade >= 50, routes >= 20, position == "WR",
         season >= 2010)  %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

ITEWARname <- WRWARFill %>% 
  group_by(player_id) %>% 
  mutate(CareerGrade = mean(grades_offense, na.rm = T)) %>% 
  filter(CareerGrade >= 50, routes >= 20, position == "TE",
         season >= 2010)  %>% 
  group_by(ID) %>% 
  slice_head(n = 1) %>% 
  select(ID)

WinLoss <- read_rds("data/WinLoss.rds")

WinLossWeek <- read_rds("data/WinLossweek.rds")

WARData <- read_rds("data/WARData.rds") %>% 
    arrange(team_name) %>% 
  group_by(position) %>% 
  mutate(WarPerc = percent_rank(PosWAR)) %>% 
  group_by(season, position) %>% 
  arrange(desc(PosWAR)) %>% 
  mutate(SznRank = seq(1, n(), 1)) %>% 
  group_by() %>% 
  mutate(TopBottom10 = as.factor(case_when(WarPerc >= 0.9 ~ 1,
                                 WarPerc <= 0.1 ~ 0)),
         TopBottom5Szn = as.factor(case_when(SznRank <= 5 ~ 1,
                                 SznRank >= 28 ~ 0)))


CWOE <- read_rds("data/CWOE.rds") %>% 
    filter(posteam != "CLE*") %>% 
    mutate(TotalWAR = RosterWAR + QBWAR)

plot_title <- data.frame(
    Index = c("RosterWAR",
              "TotalWAR",
              "PosteamADJValue",
              "QWOE",
              "CWOE",
              "PosteamADJValue",
              "PythoWins",
              "Wins"),
    Title = c("Non-QB Roster WAR",
              "Total Roster WAR (Including QB)",
              "Expected Analytical Updating Win/Loss Record",
              "QB Wins Over Expected",
              "Coach Wins Over Expected",
              "Wins vs Opponent and WP Adjusted Analytical Wins",
              "Wins vs Pythagoren Wins",
              "Regular Wins"))


WatchData <- read_rds("data/WatchData.rds") %>%
    arrange(home_team) %>% 
    mutate(WatchIndex = round(WatchIndex, 2)) %>% 
    group_by() %>% 
    arrange(desc(WatchIndex)) %>% 
    arrange(desc(CloseTime)) %>% 
    mutate(Count = n(),
           value = as.integer(seq(1, n(), 1)))



QBData <- WatchData %>% 
    arrange(player.x) %>% 
    group_by(player.x) %>% 
    mutate(Count = n()) %>% 
    filter(Count >= 10)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("WAR and Watch Index"),
    
    fluidRow(
        tabsetPanel( 
          tabPanel("Intro Page",
                   fluidRow(
                     column(width = 10, offset = 1,
                            helpText(HTML("This shiny app is designed for showing how players and teams are doing from a big picture perspective. It includes Wins Above Replacement and Team Strength Values. Additionally, it includes my Watch Index metric for deciding which games to rewatch. </br>
                        </br>
                        
                                   To check out my other shiny apps, follow one of the following links.</br>
                                   </br>

        <a href='https://seththedatascientist.shinyapps.io/QB_Bayesian_Updating/'>Bayesian Updating of composite metrics for Quarterback play for NFL and College QBs</a></br>

        This shiny app displays both college and pro QBs in two composite metrics that show not only their relative playstyles, but also how those values change over their careers. These values have a high correlation from college to pro for predicting playstyle once in the NFL</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/General_Manager_Toolbox/'>General Manager's Toolbox: A collection of tools to help analyze an NFL Team's Offseason moves.</a></br>

        This shiny app goes over a handful of useful data points that I have found very helpful for analyzing a team's offseason moves, including draft trade calculators (with some linear programming to try and ensure extra value by comparing the Jimmy Johnson trade chart to the Wins Above Replacement values), created metrics to analyze draft prospects in further detail, and team breakdowns of their effective cap and team structure over the coming years.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Offense_And_Defense_Metrics/'>Collection of Offense & Defense efficiency and playstyle metrics for the NFL</a></br>

        This shiny app includes a number of metrics used to understand Offense and Defense in further detail including down conversion values of how often you are allowing a first down or touchdown based on what down it currently is, explosive play rates, big throws and misses by quarterbacks, and more. Most metrics include a feature to isolate a playcaller's history of that metric across all teams they were the playcaller for.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Season_EPA_Tracker/'>Timeline of play measuring efficiency metrics, team season-long rankings, and team tier plots</a></br>

        This shiny app includes many iterations of breaking down expected points added (EPA) adjusted based on opponent strength and situation. Season long graphs to see individual team or starting quarterback trends, team plots for offense and defense including splits for passing and rushing, and a metric for team strength based on the relative margin of domination during the game as well as opponent strength.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/WAR_positiongroup/'>Position Wins Above Replacement graphs by team and Watch Index</a></br>

        This shiny app shows Wins Above Replacement (WAR) values and plots for both college and pro broken down into many useful facets like by position group, team, and individual player. Includes receiver custom metrics plotted to compare players both within college and pro, as well as a customizable Watch Index which assigns a values based on relative values of excitement and closeness.</br>
        </br>
                  
                                       To check some of my other work, check out my <a href='https://twitter.com/SethDataScience'>Twitter</a>, <a href='https://www.linkedin.com/in/sethlanza/'>Linkedin</a>, or <a href='https://sites.google.com/view/seth-lanza-portfolio'>Portfolio</a>")),
                     )
                   ),
                   fluidRow(
                     column(width = 10, offset = 1,
                            textOutput("lastDeploymentTime")
                     )
                   )
          ),
            tabPanel("Regular Season Wins Above Replacement Values",
                     fluidRow(
                         column(width = 4,
                               sliderInput(inputId = "seasons1",
                                           label = "Team 1 Season Selection",
                                           min = min(WARData$season),
                                           max = max(WARData$season),
                                           value = c(2018,2023),
                                           step = 1,
                                           round = T,
                                           sep = ""),
                               
                               selectizeInput(inputId = "team1",
                                              label = "Team 1 Namecode",
                                              choices = sort(unique(WARData$team_name)),
                                              selected = "BUF"),
                               
                               checkboxGroupInput(inputId = "posgroup1",
                                              label = "Position Group",
                                              choices = c("QB", "OC", "G", "OT", "WR", "TE", "HB", "DI", "ED", "LB", "CB", "S"),
                                              selected  = c("OC", "G", "OT", "WR", "TE", "HB"),
                                              inline = T)
                               ),
                               
        column(width = 4,
               sliderInput(inputId = "seasons2",
                           label = "Team 2 Season Selection",
                           min = min(WARData$season),
                           max = max(WARData$season),
                           value = c(2018,2023),
                           step = 1,
                           round = T,
                           sep = ""),
               
               selectizeInput(inputId = "team2",
                              label = "Team 2 Namecode",
                              choices = sort(unique(WARData$team_name)),
                              selected = "NE"),
               
               checkboxGroupInput(inputId = "posgroup2",
                                  label = "Position Group",
                                  choices = c("QB", "OC", "G", "OT", "WR", "TE", "HB", "DI", "ED", "LB", "CB", "S"),
                                  selected = c("DI", "ED", "LB", "CB", "S"),
                                  inline = T),
        )
        
        ),
        fluidRow(
                   column(
                       plotOutput("Team1Plot", height = 700),
                       width = 10, offset = 1)
               ), 
        
        fluidRow(
            column(
                plotOutput("Team2Plot", height = 700),
                width = 10, offset = 1)
            ),
        fluidRow(column(width = 10, offset = 1,
                        helpText(HTML("The below WAR Table allows you to search by WAR values for an individual player/season/position/etc. The WARTiles are the percentiles of WAR values per position.")))),
        fluidRow(
          column(
            dataTableOutput("WARtable"),
            width = 10, offset = 1
          )
        ),
        fluidRow(
          column(width = 3, offset = 1,
          selectizeInput(inputId = "Player",
                         label = "Player Selection", 
                         choices = IWARname,
                         selected = "Stefon Diggs - 9579"))),
        fluidRow(
          column(
            plotOutput("PlayerWARPlot", height = 700),
            width = 10, offset = 1)
        )
        ),
        
        tabPanel("College WAR Values",
                 fluidRow(column(width = 10, offset = 1,
                                 helpText(HTML("The below WAR Table allows you to search College Players by WAR values for an individual player/season/position/etc. The WARTiles are the percentiles of WAR values per position. <br>
                Transfer and Seasons are based on the data available. So if the player's team in their last year was different than their first year in the dataset they are labelled transfer, and number of seasons is on years of data available as well. There will be players who transferred from the FCS level or players who didn't have enough snaps early in their career to meet the treshold and therefore those years won't show up properly.")))),
                 fluidRow(
                   column(
                     dataTableOutput("CWARtable"),
                     width = 10, offset = 1
                   )
                 ),
                 fluidRow(
                   column(width = 3, offset = 1,
                          selectizeInput(inputId = "CPlayer",
                                         label = "Player Selection", 
                                         choices = CWARname,
                                         selected = "Quenton Nelson - 12954"))),
                 fluidRow(
                   column(
                     plotOutput("CPlayerWARPlot", height = 700),
                     width = 10, offset = 1)
                 )
        ),
        tabPanel("Reciever Clustering Plot",
                 fluidRow(column(width = 10, offset = 1,
                                 helpText(HTML("Select a player to see their Radar plot over their NFL/College Career respectively. The Acronymns starting at the top and going counter-clockwise are Routes Run, Targets/Route Run, Yards/Route Run, Touchdowns/Route Run, Average Depth of Target, Yards After Catch/Reception.")))),
                 fluidRow(
                   column(width = 5, offset = 1,
                          selectizeInput(inputId = "NFLPlayer",
                                         label = "NFL Player Selection", 
                                         choices = IWRWARname,
                                         selected = "Calvin Johnson - 3618")),
                 column(width = 5, offset = 1,
                          selectizeInput(inputId = "CollegePlayer",
                                         label = "College Player Selection", 
                                         choices = CWRWARname,
                                         selected = "Cedrick Wilson - 48170"))),
                 fluidRow(
                   column(
                     plotOutput("NFLPlayerClusterPlot", height = 500),
                     width = 5, offset = 1),
                   column(
                     plotOutput("CollegePlayerClusterPlot", height = 500),
                     width = 5)
                 ),
                 fluidRow(
                   column(width = 5, offset = 1,
                          selectizeInput(inputId = "NFLPlayerTE",
                                         label = "NFL Player Selection", 
                                         choices = ITEWARname,
                                         selected = "Rob Gronkowski - 5567")),
                   column(width = 5, offset = 1,
                          selectizeInput(inputId = "CollegePlayerTE",
                                         label = "College Player Selection", 
                                         choices = CTEWARname,
                                         selected = "Kyle Pitts - 83964"))),
                 fluidRow(
                   column(
                     plotOutput("NFLPlayerClusterPlotTE", height = 500),
                     width = 5, offset = 1),
                   column(
                     plotOutput("CollegePlayerClusterPlotTE", height = 500),
                     width = 5)
                 ),
                 fluidRow(
                   column(width = 6, offset = 1,
                          helpText(HTML("These are the metrics I find valuable for evaluating college receivers going into the NFL.")),
                   )
                 ),
                 fluidRow(
                   column(
                     dataTableOutput("WRMetrics"),
                     width = 6, offset = 1
                   )
                 )
        ),
        
        tabPanel("Positional and Team Selection Plots",
                    fluidRow(
                        column(width = 3,
                               
                               selectizeInput(inputId = "seasons3",
                                              label = "Positional Season Selection",
                                              choices = sort(unique(WARData$season)),
                                              selected = 2023),
                               
                               selectizeInput(inputId = "posgroup3",
                                              label = "Position Group",
                                              choices = c(sort(unique(WARData$position)),
                                                          "OFF (w/o QB)" = "Off (w/o QB)",
                                                          "OFF" = "Off",
                                                          "OL" = "OL",
                                                          "SKILL" = "Skill",
                                                          "DEF" = "Def",
                                                          "DL" = "DL",
                                                          "DB" = "DB"))
                        ),
                    ),
                 
                 fluidRow(
                   column(
                     plotOutput("Team3Plot", height = 700),
                     width = 10, offset = 1)
                 ),
                 fluidRow(
                   column(width = 3,  
                               selectizeInput(inputId = "seasons4",
                                              label = "Team Season Selection",
                                              choices = sort(unique(CWOE$season)),
                                              selected = 2023),
                               
                               selectizeInput(inputId = "Altposgroup",
                                              label = "Team Based Values",
                                              choices = c("Non-QB Roster WAR" = "RosterWAR",
                                                          "Total Roster WAR (Including QB)" = "TotalWAR",
                                                          "Expected Analytical Updating Win/Loss Record" = "PosteamADJValue",
                                                          "QB Wins Over Expected" = "QWOE",
                                                          "Coach Wins Over Expected" = "CWOE"))
                        )
                    ),
                    fluidRow(
                        column(
                            plotOutput("Team4Plot", height = 700),
                            width = 10, offset = 1)
                    ),
                 fluidRow(
                   column(width = 4,
                          sliderInput(inputId = "seasonlong",
                                      label = "Non-QB Roster Season Selection",
                                      min = min(WARData$season),
                                      max = max(WARData$season),
                                      value = c(2018,2023),
                                      step = 1,
                                      round = T,
                                      sep = ""),
                          
                          
                          selectizeInput(inputId = "teamlong",
                                         label = "Non-QB Roster Season Selection",
                                         choices = sort(unique(WARData$team_name)),
                                         selected = "BUF"),
                   )
                 ),
                 fluidRow(
                   column(
                     plotOutput("TeamLongPlot", height = 700),
                     width = 10, offset = 1)
                 )
        ),
        tabPanel("Season Analytical Win-Loss Records",
                 fluidRow(
                   column(width = 3,
                          
                          selectizeInput(inputId = "seasons6",
                                         label = "Season Selection",
                                         choices = sort(unique(WinLoss$season)),
                                         selected = 2023)
                          ),
                   column(width = 2,
                          selectizeInput(inputId = "filter",
                                         label = "Conference",
                                         choices = c("All" = "All", "NFC only" = "AFC", "AFC only" = "NFC"),
                                         selected = "All")
                          ),
                   column(width = 3,
                          selectizeInput(inputId = "team6",
                                         label = "Team Selection",
                                         choices = sort(unique(WinLoss$posteam)),
                                         selected = "BUF")
                          ),
                   
                   
                   column(width = 3,  
                          selectizeInput(inputId = "WinType",
                                         label = "Type of Win/Loss Record",
                                         choices = c("Opponent & WP Adjusted Analytical Wins" = "PosteamADJValue",
                                                     "Pythagoreon Wins" = "PythoWins",
                                                     "Regular Wins" = "Wins"))
                   )
                   
                 ),
                 fluidRow(
                   column(
                     plotOutput("Team5Plot", height = 700),
                     width = 10, offset = 1)
                 ),
                 
                 fluidRow(
                   column(
                     plotOutput("Team6Plot", height = 700),
                     width = 10, offset = 1)
                 )
                 
        ),
        
        tabPanel("Watch Index",
                 fluidRow(
                     
                     column(width = 3,
                            sliderInput(inputId = "seasons5",
                                        label = "Season Selection",
                                        min = min(WatchData$season),
                                        max = max(WatchData$season),
                                        value = c(2018,2023),
                                        step = 1,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 3,
                            selectizeInput(inputId = "Playoff",
                                           label = "Playoffs Options",
                                           choices = c("Regular & Postseason" = "Yes",
                                                       "Regular Season Only" = "No",
                                                       "Postseason Only" = "Only"),
                                           selected = "Regular & Postseason")
                            
                     ),
                     
                     column(width = 3,
                            selectizeInput(inputId = "QBCutoff",
                                           label = "QB Duel WAR Percentile Cutoff",
                                           selected = 0.625,
                                           choices = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))
                            
                     ),
                     
                     column(width = 3,
                            selectizeInput(inputId = "EPACutoff",
                                           label = "EPA Duel Percentile Cutoff",
                                           selected = 0.75,
                                           choices = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1))
                            
                     )),
                 
                 
                 
                 checkboxInput(inputId = "TeamSelect",
                                label = "Team or QB Selection"),
                 conditionalPanel(
                     condition = "input.TeamSelect == true",
                     
                     
                 fluidRow(
                     
                     column(width = 3,
                            
                            
                            selectizeInput(inputId = "team5",
                                           label = "Team 1 Selection",
                                           choices = c("All", sort(unique(WatchData$home_team))),
                                           selected = "All")
                            
                     ),
                     
                     column(width = 3,
                            
                            selectizeInput(inputId = "QB3",
                                           label = "Quarterback 1 Selection",
                                           choices = c("All", sort(unique(QBData$player.x))),
                                           selected = "All")
                     ),
                     
                     column(width = 3,
                            
                            selectizeInput(inputId = "team6",
                                           label = "Team 2 Selection",
                                           choices = c("All", sort(unique(WatchData$home_team))),
                                           selected = "All")
                     ),
                     
                     column(width = 3,
                            
                            selectizeInput(inputId = "QB4",
                                           label = "Quarterback 2 Selection",
                                           choices = c("All", sort(unique(QBData$player.x))),
                                           selected = "All")
                     ),
                     
                     )
                 ),
                 
                 
                 checkboxInput(inputId = "Filter",
                               label = "Adjust Filter Weights"),
                 conditionalPanel(
                     condition = "input.Filter == true",
                 fluidRow(
                     column(width = 2,
                            sliderInput(inputId = "PRCloseTime",
                                        label = "WP Close Time Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRAvgTimeWP",
                                        label = "WP Avg Time Close Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRChangeWP",
                                        label = "WP Favorite Changes Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRAvgChanges",
                                        label = "Avg WP Favorite Changes Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRCloseEnd",
                                        label = "WP Close At End Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "Closeness",
                                        label = "Closeness Composite Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     )
                 ),
                 
                 
                 fluidRow(
                     column(width = 2,
                            sliderInput(inputId = "PRBigPlays",
                                        label = "Total Big Plays Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRAvgBigPlays",
                                        label = "Avg Big Plays Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRTotalScore",
                                        label = "Total Score Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PRAvgScore",
                                        label = "Avg Score Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "PREPA",
                                        label = "Opp/WP Adj. EPA Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     ),
                     
                     column(width = 2,
                            sliderInput(inputId = "Excitement",
                                        label = "Excitement Composite Weight",
                                        min = 0,
                                        max = 2,
                                        value = 1,
                                        step = .125,
                                        round = T,
                                        sep = "")
                     )
                 )
                     ),
                     fluidRow(
                         column(width = 10, offset = 1,
                                helpText(HTML("Please give a moment to load. 
                                This will create a table based on the filters
                                              above based on a Watch Index Metric. <br/>
                                The Watch Index Metric is an average of the the Excitement and Closeness
                                Composites with a 2/3 favor towards Excitement. <br/>
                                Wacky is a metric based on Turnovers, Incompletions, Passed Defensed, etc. <br/>
                                Penalties is a ranking based on Total Yards on Penalties"))
                                         )
                         ),
                 
                 
                 fluidRow( 
                     column(
                       
                        dataTableOutput("RandGame"), width = 10,
                        offset = 1
                     )
                    ),
                 
                 fluidRow(
                   column(width = 4, offset = 4,
                          
                          actionButton("THEBUTTON", "Generate Random Game")
                   )
                 ),
                 # Show a plot of the generated distribution
                 fluidRow(
                     column(
                        dataTableOutput("table1"), width = 10,
                        offset = 1
                     )
                 )
        )
                    
        
        
        
    )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  getLastDeploymentTime <- function() {
    timestamp <- tryCatch(
      readLines("deployment_timestamp.txt"),
      error = function(e) NA
    )
    if (!is.na(timestamp)) {
      as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
      NA
    }
  }
  
  # Display the last deployment time
  output$lastDeploymentTime <- renderText({
    lastDeploymentTime <- getLastDeploymentTime()
    if (!is.na(lastDeploymentTime)) {
      paste("Last Deployment Time: ", format(lastDeploymentTime, "%Y-%m-%d %H:%M:%S"))
    } else {
      "Deployment time not available."
    }
  })
  
  js <- c(
    "table.on('draw.dt', function(){",
    "  var PageInfo = table.page.info();",
    "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
    "    cell.innerHTML = i + 1 + PageInfo.start;",
    "  });",
    "})")
  
  
  
  
    
    output$Team1Plot <- renderPlot({
        
        WARData %>% 
            filter(between(season,input$seasons1[1],input$seasons1[2]), 
                   team_name == input$team1,
                   position %in% input$posgroup1) %>% 
            ggplot(aes(x = season, y = PosWAR))+
            geom_text(aes(label = position, color = position),
                      nudge_y = 0.065)+
            geom_line(aes(color = position))+
            geom_point(aes(color = position, fill = position, shape = TopBottom10, size = TopBottom5Szn))+
            geom_hline(yintercept = 0, linetype = "dashed")+
            facet_wrap(~SideofBall)+
            theme_reach()+
            scale_y_continuous(breaks = seq(-0.5,5,0.25))+
            scale_x_continuous(breaks = seq(2010,2030,1))+
            labs(title = paste0(
                input$seasons1[1],
                "-", 
                input$seasons1[2],
                " ",
                input$team1,
                " ", "Season(s)"),
                 subtitle = "Sum of Wins Above Replacement by position group, min 150 snaps, Shape is Top/Bottom 10% since 2010, Size is Top/Bottom 5 in a Season",
                 y = "WAR",
                 x = "Season",
                 caption = "@SethDataScience"
            )
    })
    
    output$Team2Plot <- renderPlot({
        
        WARData %>% 
            filter(between(season,input$seasons2[1],input$seasons2[2]), 
                   team_name == input$team2,
                   position %in% input$posgroup2) %>% 
            ggplot(aes(x = season, y = PosWAR))+
            geom_text(aes(label = position, color = position),
                      nudge_y = 0.065)+
            geom_line(aes(color = position))+geom_point(aes(color = position, fill = position, shape = TopBottom10, size = TopBottom5Szn))+
            geom_hline(yintercept = 0, linetype = "dashed")+
            facet_wrap(~SideofBall)+
            theme_reach()+
            scale_y_continuous(breaks = seq(-0.5,5,0.25))+
            scale_x_continuous(breaks = seq(2010,2030,1))+
            labs(title = paste0(
                input$seasons2[1],
                "-", 
                input$seasons2[2],
                " ",
                input$team2,
                " ", "Season(s)"),
                subtitle = "Sum of Wins Above Replacement by position group, min 150 snaps, Shape is Top/Bottom 10% since 2010, Size is Top/Bottom 5 in a Season",
                y = "WAR",
                x = "Season",
                caption = "@SethDataScience"
            )
    })
    
    
    
    output$WARtable = DT::renderDataTable({
      
      WARtable <- IWARData %>% 
        mutate(WAR = round(WAR, 3),
               WARTile = round(WARTile, 3)*100) %>%
        arrange(desc(WAR)) %>% 
        mutate(Rank = as.integer(seq(1, n(), 1)),
               player_id = as.integer(player_id),
               Snaps = as.integer(Snaps),
               season = as.factor(season),
               player = as.factor(player),
               team_name = as.factor(team_name),
               position = as.factor(position),
               OffDef = as.factor(OffDef)) %>% 
        select(Rank, season, player, player_id, team_name, Snaps, OffDef, position, WAR, WARTile)%>%
      mutate(WAR = round(WAR, 3),
             WARTile = round(WARTile, 1),
             WARPer100Snap = round((WAR/Snaps)*100, 3)) 
      
      
      
      
      WARbrks <- quantile(WARtable$WAR, probs = seq(.05, .95, .01), na.rm = TRUE)
      WARbrksy <- round(seq(255, 40, length.out = length(WARbrks) + 1), 0)
      WARbrksclrs <- paste0("rgb(", WARbrksy, "," , 255-WARbrksy , ",", 0, ")")
      
      WARTilebrks <- quantile(WARtable$WARTile, probs = seq(.05, .95, .01), na.rm = TRUE)
      WARTilebrksy <- round(seq(255, 40, length.out = length(WARTilebrks) + 1), 0)
      WARTilebrksclrs <- paste0("rgb(", WARTilebrksy, "," , 255-WARTilebrksy , ",", 0, ")")
      
      WARPer100Snapbrks <- quantile(WARtable$WARPer100Snap, probs = seq(.05, .95, .01), na.rm = TRUE)
      WARPer100Snapbrksy <- round(seq(255, 40, length.out = length(WARPer100Snapbrks) + 1), 0)
      WARPer100Snapbrksclrs <- paste0("rgb(", WARPer100Snapbrksy, "," , 255-WARPer100Snapbrksy , ",", 0, ")")
      
      datatable(WARtable, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 32,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')),
                callback = JS(js))%>%
        formatStyle("WAR", 
                    backgroundColor = styleInterval(WARbrks,WARbrksclrs)
        )%>%
        formatStyle("WARTile", 
                    backgroundColor = styleInterval(WARTilebrks,WARTilebrksclrs)
        )%>%
        formatStyle("WARPer100Snap", 
                    backgroundColor = styleInterval(WARPer100Snapbrks,WARPer100Snapbrksclrs)
        )
      
      
    })
    
    
    output$PlayerWARPlot <- renderPlot({
      
      
      ggplot(IWARData%>% 
               filter(ID == input$Player) %>% 
               mutate(season = as.character(season)),
             aes(x = season, y = WAR)) +
        geom_image(aes(image = url))+
        geom_line(aes(color = primary, group = 1))+
        geom_hline(yintercept = 0, color = "red")+
        geom_hline(aes(yintercept = PosAvg), linetype = "dashed")+
        facet_wrap(~position)+
        theme_reach()+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_x_discrete(breaks = seq(2010,2030,1)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
        labs(
          y= "Wins Above Replacemen (WAR)",
          x= "Season",
          title= paste0("WAR by Season for ", input$Player),
          subtitle = "Dashed line is Positional Averge, Red Line is 0 WAR",
          caption = "@SethDataScience"
        ) 
    })
    
    output$CPlayerWARPlot <- renderPlot({
      
      ggplot(CollegeWARJoin %>% 
               filter(ID == input$CPlayer)%>% 
               mutate(season = as.character(season)),
             aes(x = season, y = WAR)) +
        geom_image(aes(image = CollegeWARJoin$logo[CollegeWARJoin$ID == input$CPlayer]))+
        geom_line(aes(color = CollegeWARJoin$primary[CollegeWARJoin$ID == input$CPlayer], group = 1))+
        geom_hline(yintercept = 0, color = "red")+
        geom_hline(aes(yintercept = PosAvg), linetype = "dashed")+
        facet_wrap(~position)+
        theme_reach()+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_x_discrete(breaks = seq(2010,2030,1)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
        labs(
          y= "Wins Above Replacemen (WAR)",
          x= "Season",
          title= paste0("WAR by Season for ", input$CPlayer),
          subtitle = "Dashed line is Positional Averge, Red Line is 0 WAR",
          caption = "@SethDataScience"
        ) 
    })
    
    
    output$CWARtable = DT::renderDataTable({
      
      CWARtable <- CollegeWARJoin %>% 
        mutate(WAR = round(WAR, 3),
               WARTile = round(WARTile, 3)*100,
               AvgWAR = round(AvgWAR, 3)) %>%
        arrange(desc(WAR)) %>% 
        mutate(Rank = as.integer(seq(1, n(), 1)),
               player = as.factor(player),
               player_id = as.integer(player_id),
               Snaps = as.integer(Snaps),
               season = as.factor(season),
               pos_team = as.factor(pos_team),
               Conference = as.factor(Conference),
               Strength = as.factor(Strength),
               Seasons = as.integer(Seasons),
               Transfer = as.factor(Transfer),
               OffDef = as.factor(OffDef),
               position = as.factor(position))%>% 
        select(Rank, season, pos_team, Conference, Strength, player,
               player_id, Snaps, Seasons, 
               Transfer, OffDef, position, WAR, WARTile) %>%
      mutate(WAR = round(WAR, 3),
             WARTile = round(WARTile, 1),
             WARPer100Snap = round((WAR/Snaps)*100, 3)) 
      
      
      
      
      WARbrks <- quantile(CWARtable$WAR, probs = seq(.05, .95, .01), na.rm = TRUE)
      WARbrksy <- round(seq(255, 40, length.out = length(WARbrks) + 1), 0)
      WARbrksclrs <- paste0("rgb(", WARbrksy, "," , 255-WARbrksy , ",", 0, ")")
      
      WARTilebrks <- quantile(CWARtable$WARTile, probs = seq(.05, .95, .01), na.rm = TRUE)
      WARTilebrksy <- round(seq(255, 40, length.out = length(WARTilebrks) + 1), 0)
      WARTilebrksclrs <- paste0("rgb(", WARTilebrksy, "," , 255-WARTilebrksy , ",", 0, ")")
      
      WARPer100Snapbrks <- quantile(CWARtable$WARPer100Snap, probs = seq(.05, .95, .01), na.rm = TRUE)
      WARPer100Snapbrksy <- round(seq(255, 40, length.out = length(WARPer100Snapbrks) + 1), 0)
      WARPer100Snapbrksclrs <- paste0("rgb(", WARPer100Snapbrksy, "," , 255-WARPer100Snapbrksy , ",", 0, ")")
      
      
      
      datatable(CWARtable, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 32,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')),
                callback = JS(js))%>%
        formatStyle("WAR", 
                    backgroundColor = styleInterval(WARbrks,WARbrksclrs)
        )%>%
        formatStyle("WARTile", 
                    backgroundColor = styleInterval(WARTilebrks,WARTilebrksclrs)
        )%>%
        formatStyle("WARPer100Snap", 
                    backgroundColor = styleInterval(WARPer100Snapbrks,WARPer100Snapbrksclrs)
        )
      
      
    })
    
    
    output$NFLPlayerClusterPlot <- renderPlot({
      
      NFLValueCluster <- WRWARFill %>% 
        group_by(player_id) %>% 
        mutate(CareerGrade = mean(grades_offense, na.rm = T),
               yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception)) %>%  
        filter(CareerGrade >= 50, routes >= 20, position == "WR",
               season >= 2010)  %>% 
        group_by(player, player_id, ID) %>% 
        summarise(RR = mean(routes),
                  TPRR = mean(targets)/RR,
                  TDPRR = mean(touchdowns)/RR,
                  YAC = mean(yards_after_catch_per_reception),
                  YPRR = mean(yprr),
                  ADOT = mean(avg_depth_of_target)) %>% 
        group_by() %>% 
        mutate(RR = percent_rank(RR),
               TPRR = percent_rank(TPRR),
               TDPRR = percent_rank(TDPRR),
               YAC = percent_rank(YAC),
               YPRR = percent_rank(YPRR),
               ADOT = percent_rank(ADOT)) %>% 
        filter(ID == input$NFLPlayer) %>% 
        select(RR, TPRR,  YPRR, TDPRR, ADOT, YAC)
      
      
      NFLValueCluster <- rbind(rep(1,6) , rep(0,6) , NFLValueCluster)
      
      
      radarchart(NFLValueCluster,
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
                 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                 
                 #custom labels
                 vlcex=0.8,
                 title = paste0(input$NFLPlayer, " Career
  Normalized Values from 2010-2022 NFL WRs"))
      
    })
    
    
    output$CollegePlayerClusterPlot <- renderPlot({
      
      CollegeValueCluster <- CollegeWRWARFill %>% 
        group_by(player_id) %>% 
        mutate(CareerGrade = mean(grades_offense, na.rm = T),
               yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception)) %>% 
        filter(CareerGrade >= 50, routes >= 20, position == "WR",
               season >= 2010)  %>% 
        group_by(player, player_id, ID) %>% 
        summarise(RR = mean(routes),
                  TPRR = mean(targets)/RR,
                  TDPRR = mean(touchdowns)/RR,
                  YAC = mean(yards_after_catch_per_reception),
                  YPRR = mean(yprr),
                  ADOT = mean(avg_depth_of_target)) %>% 
        group_by() %>% 
        mutate(RR = percent_rank(RR),
               TPRR = percent_rank(TPRR),
               TDPRR = percent_rank(TDPRR),
               YAC = percent_rank(YAC),
               YPRR = percent_rank(YPRR),
               ADOT = percent_rank(ADOT)) %>% 
        filter(ID == input$CollegePlayer) %>% 
        select(RR, TPRR,  YPRR, TDPRR, ADOT, YAC)
      
      
      CollegeValueCluster <- rbind(rep(1,6) , rep(0,6) , CollegeValueCluster)
      
      
      radarchart(CollegeValueCluster,
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
                 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                 
                 #custom labels
                 vlcex=0.8,
                 title = paste0(input$CollegePlayer, " Career
  Normalized Values from 2010-2022 College WRs"))
      
    })
    
    
    output$NFLPlayerClusterPlotTE <- renderPlot({
      
      NFLValueClusterTE <- WRWARFill %>% 
        group_by(player_id) %>% 
        mutate(CareerGrade = mean(grades_offense, na.rm = T),
               yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception)) %>% 
        filter(CareerGrade >= 50, routes >= 20, position == "TE",
               season >= 2010)  %>% 
        group_by(player, player_id, ID) %>% 
        summarise(RR = mean(routes),
                  TPRR = mean(targets)/RR,
                  TDPRR = mean(touchdowns)/RR,
                  YAC = mean(yards_after_catch_per_reception),
                  YPRR = mean(yprr),
                  ADOT = mean(avg_depth_of_target)) %>% 
        group_by() %>% 
        mutate(RR = percent_rank(RR),
               TPRR = percent_rank(TPRR),
               TDPRR = percent_rank(TDPRR),
               YAC = percent_rank(YAC),
               YPRR = percent_rank(YPRR),
               ADOT = percent_rank(ADOT)) %>% 
        filter(ID == input$NFLPlayerTE) %>% 
        select(RR, TPRR,  YPRR, TDPRR, ADOT, YAC)
      
      
      NFLValueClusterTE <- rbind(rep(1,6) , rep(0,6) , NFLValueClusterTE)
      
      
      radarchart(NFLValueClusterTE,
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
                 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                 
                 #custom labels
                 vlcex=0.8,
                 title = paste0(input$NFLPlayerTE, " Career
  Normalized Values from 2010-2022 NFL TEs"))
      
    })
    
    
    output$CollegePlayerClusterPlotTE <- renderPlot({
      
      CollegeValueClusterTE <- CollegeWRWARFill %>% 
        group_by(player_id) %>% 
        mutate(CareerGrade = mean(grades_offense, na.rm = T),
               yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception)) %>% 
        filter(CareerGrade >= 50, routes >= 20, position == "TE",
               season >= 2010)  %>% 
        group_by(player, player_id, ID) %>% 
        summarise(RR = mean(routes),
                  TPRR = mean(targets)/RR,
                  TDPRR = mean(touchdowns)/RR,
                  YAC = mean(yards_after_catch_per_reception),
                  YPRR = mean(yprr),
                  ADOT = mean(avg_depth_of_target)) %>% 
        group_by() %>% 
        mutate(RR = percent_rank(RR),
               TPRR = percent_rank(TPRR),
               TDPRR = percent_rank(TDPRR),
               YAC = percent_rank(YAC),
               YPRR = percent_rank(YPRR),
               ADOT = percent_rank(ADOT)) %>% 
        filter(ID == input$CollegePlayerTE) %>% 
        select(RR, TPRR,  YPRR, TDPRR, ADOT, YAC)
      
      
      CollegeValueClusterTE <- rbind(rep(1,6) , rep(0,6) , CollegeValueClusterTE)
      
      
      radarchart(CollegeValueClusterTE,
                 pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
                 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                 
                 #custom labels
                 vlcex=0.8,
                 title = paste0(input$CollegePlayerTE, " Career
  Normalized Values from 2010-2022 College TEs"))
      
    })
    
    
    output$Team3Plot <- renderPlot({
        
        WARData %>% 
            filter(season == input$seasons3, 
                   position %in% input$posgroup3 |
                   SideofBall %in% input$posgroup3 |
                     OFFwoQB %in% input$posgroup3 |
                     OFF %in% input$posgroup3 |
                     DEF %in% input$posgroup3 ) %>% 
            group_by(season, team_name) %>% 
            summarise(PosWAR = sum(PosWAR),
                      secondary = head(secondary, 1),
                      primary = head(primary, 1),
                      url = head(url, 1)) %>% 
            ggplot(aes(x = reorder(team_name, -PosWAR), y = PosWAR))+
            geom_col(aes(color = secondary, fill = primary))+
            geom_image(aes(image = url))+
            geom_hline(yintercept = 0, linetype = "dashed")+
            theme_reach()+
            theme(axis.text.x = element_text(size=12))+
            scale_color_identity(aesthetics = c("color", "fill"))+
            labs(title = paste0(
                input$seasons3,
                " ",
                input$posgroup3,
                " ", "Wins Above Replacement"),
                subtitle = "Sum of Wins Above Replacement by position group, min 150 snaps",
                y = "WAR",
                x = "Team",
                caption = "@SethDataScience"
            )
    })
    
    output$Team4Plot <- renderPlot({
        
        CWOE %>% 
            filter(season == input$seasons4, posteam != "CLE*") %>% 
            ggplot(aes(x = reorder(posteam, -.data[[input$Altposgroup]]),
                       y = .data[[input$Altposgroup]]))+
            geom_col(aes(color = secondary, fill = primary))+
            geom_image(aes(image = url))+
            geom_hline(yintercept = 0, linetype = "dashed")+
            theme_reach()+
            theme(axis.text.x = element_text(size=12))+
            scale_color_identity(aesthetics = c("color", "fill"))+
            labs(title = paste0(
                input$seasons4,
                " ",
                plot_title$Title[plot_title$Index == input$Altposgroup]),
                subtitle = "Min. 150 snaps",
                y = "Wins",
                x = "Team",
                caption = "@SethDataScience"
            )
    })
    
    output$Team5Plot <- renderPlot({
      
      
      
      ggplot(WinLoss %>% filter(season == input$seasons6, Conference != input$filter) ,
             aes(x = reorder(posteam, .data[[input$WinType]]),
                 y = .data[[input$WinType]]))+
        geom_col(aes(y = Wins, fill = secondary, color = primary))+
        geom_col(aes(fill = "black", color = "white", alpha = 0.75))+
        geom_image(aes(image = url, y = Wins))+
        geom_hline(yintercept = WinLoss$Yint[WinLoss$season == input$seasons6], linetype = "dashed", alpha = 0.5)+
        scale_color_identity(aesthetics = c("color", "fill"))+
        theme_reach()+
        theme(axis.text.x = element_text(size = 10))+
        scale_y_continuous(breaks = seq(0,20,1))+
        labs(title = paste0(plot_title$Title[plot_title$Index == input$WinType], " in ", input$seasons6),
             subtitle  = "Logo is Actual Wins, Black is Selection, Sorted by Selection, Dashed line is 80% of the Top Selected Value",
             x = "Team") 
      
    })
    
    
    output$Team6Plot <- renderPlot({
      
     
    ggplot(WinLossWeek %>% filter(season == input$seasons6, 
                                posteam == input$team6),
             aes(x = week, y = PosteamADJValue))+
        geom_col(aes(fill = primary, color = secondary))+
        geom_label(aes(y = PosteamADJValue + max(WinLossWeek$PosteamADJValue[WinLossWeek$season == input$seasons6 & WinLossWeek$posteam == input$team6])*0.05, color = if_else(PosAdjustment>0, "green", "red"), fill = "black", label = round(PosAdjustment*2,2)))+
        geom_label(aes(y = max(WinLossWeek$PosteamADJValue[WinLossWeek$season == input$seasons6 & WinLossWeek$posteam == input$team6])*-0.05, color = if_else(PosteamWin==1, "green", "red"), fill = "black", label = defteam))+
        geom_image(aes(image = url.x, y = PosteamADJValue))+
        geom_image(aes(image = url.y, y = 0))+
        theme_reach()+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_y_continuous(breaks = seq(0,20,0.5))+
        scale_x_continuous(breaks = seq(1,23,1))+
        labs(title = paste0(input$team6, " Opponent Adjusted Analytical WP adjusted Wins in ", input$seasons6),
             x = "Week & Opponent") 
      
    })
    
    output$TeamLongPlot <- renderPlot({
      
      SeasonWARData <- WARData %>% 
        filter(position != "QB") %>% 
        group_by(season, team_name) %>% 
        summarise(NonQBRosterWAR = sum(PosWAR, na.rm = T),
                  url = head(url, 1),
                  primary = head(primary, 1)) %>% 
        filter(between(season, input$seasonlong[1], input$seasonlong[2])) %>% 
        group_by(team_name) %>% 
        mutate(TeamAvg = mean(NonQBRosterWAR)) %>% 
        group_by() %>% 
        mutate(TotalAvg = mean(NonQBRosterWAR)) %>% 
        group_by(season) %>% 
        mutate(MaxPoint = max(NonQBRosterWAR),
               MinPoint = min(NonQBRosterWAR))%>% 
        filter(team_name == input$teamlong)
      
      ggplot(SeasonWARData , aes(x = season, y = NonQBRosterWAR))+
        geom_image(aes(image = SeasonWARData$url))+
        geom_line(aes(color = primary))+
        geom_hline(aes(yintercept = SeasonWARData$TeamAvg, color = SeasonWARData$primary), linetype = "dashed")+
        geom_hline(aes(yintercept = 0, color = "red"))+
        geom_hline(yintercept = SeasonWARData$TotalAvg)+
        geom_point(aes(x = season, y = SeasonWARData$MaxPoint, size = 2, color = "green"))+
        geom_point(aes(x = season, y = SeasonWARData$MinPoint, size = 2, color = "red"))+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_x_continuous(breaks = seq(2010, 2030, 1))+
        theme_reach()+
        labs(x = "Season",
             y = "Non-QB Total Roster WAR",
             title = paste0(input$teamlong, " Non-QB Total Roster WAR from ", input$seasonlong[1], "-", input$seasonlong[2]),
             subtitle = "Black line is the Total League Average for the time period selected, Dashed line is for the selected team's average over that time period, and the Red and Green dots represent the top and bottom Roster WAR for that season.",
             caption = "@SethDataScience")
      
    })
    
    output$table1 = DT::renderDataTable({
      
    WatchData1 <- WatchData %>% 
        arrange(desc(WatchIndex)) %>% 
        mutate(
          ClosenessOld = ((input$PRCloseTime * PRCloseTime) +
                            (input$PRCloseEnd * PRCloseEnd) +
                            2*(input$PRAvgTimeWP * PRAvgTimeWP))/4,
          ExcitementOld = (2*(input$PRBigPlays * PRBigPlays) +
                             (input$PRTotalScore * PRTotalScore) +
                             (input$PRChangeWP * PRChangeWP) + 
                             2*(input$PRAvgBigPlays * PRAvgBigPlays) +
                             (input$PRAvgScore * PRAvgScore) + 
                             (input$PRAvgChanges * PRAvgChanges) +
                             (input$PREPA * PREPA) * 5)/13,
          WatchIndex = round(((input$Closeness * ClosenessOld) +
                                2*(input$Excitement * ExcitementOld))/3,2),
          Playoff = case_when(season < 2021 & week <= 17 ~ "No",
                              season >= 2021 & week <= 18 ~ "No",
                              T ~ "Yes")) %>% 
        filter(between(season, input$seasons5[1], input$seasons5[2]),
               Playoff %in% case_when(input$Playoff == "Only" ~ "Yes",
                                    input$Playoff == "No" ~ "No",
                                    T ~ c("No","Yes")),
               PREPA >= input$EPACutoff,
               PRWAR >= input$QBCutoff,
               home_team == case_when(input$TeamSelect == 0 ~ home_team,
                                      input$team5 == "All" ~ home_team,
                                      T ~ input$team5) |
                 away_team == case_when(input$TeamSelect == 0 ~ away_team,
                                        input$team5 == "All" ~ away_team,
                                        T ~ input$team5),
               home_team == case_when(input$TeamSelect == 0 ~ home_team,
                                      input$team6 == "All" ~ home_team,
                                      T ~ input$team6) |
                 away_team == case_when(input$TeamSelect == 0 ~ away_team,
                                        input$team6 == "All" ~ away_team,
                                        T ~ input$team6),
               player.x == case_when(input$TeamSelect == 0 ~ player.x,
                                     input$QB3 == "All" ~ player.x,
                                     T ~ input$QB3) |
                 player.y == case_when(input$TeamSelect == 0 ~ player.y,
                                       input$QB3 == "All" ~ player.y,
                                       T ~ input$QB3),
               player.x == case_when(input$TeamSelect == 0 ~ player.x,
                                     input$QB4 == "All" ~ player.x,
                                     T ~ input$QB4) |
                 player.y == case_when(input$TeamSelect == 0 ~ player.y,
                                       input$QB4 == "All" ~ player.y,
                                       T ~ input$QB4)) %>% 
        group_by() %>% 
        mutate(WatchIndex = round(round(percent_rank(WatchIndex),4)*100,2),
               Wacky = round(round(percent_rank(Wacky), 4)*100,2),
               Penalties = round(round(percent_rank(Penalties), 4)*100,2)) %>% 
        arrange(desc(WatchIndex)) %>% 
        select(season, Playoff, week, home_team, away_team, WatchIndex, Wacky, Penalties) 
      
      
      
      brks <- quantile(WatchData1$WatchIndex, probs = seq(.05, .95, .01), na.rm = TRUE)
      y <- round(seq(255, 40, length.out = length(brks) + 1), 0)
      clrs <- paste0("rgb(", y, "," , 255-y , ",", 0, ")")
      
      
      datatable(WatchData1, rownames = F,
                extensions = 'Buttons', options = list(pageLength = 15,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')))%>%
        formatStyle("WatchIndex", 
                    backgroundColor = styleInterval(brks, clrs)
        ) %>% 
        formatStyle("Playoff",
                    backgroundColor = styleEqual(c("No", "Yes"),
                                                 c("","gold"))
        )
      
    })
    
    output$RandGame = DT::renderDataTable({
      
      
      
      
      randomgame <- eventReactive(
        input$THEBUTTON,
        as.integer(round(runif(1, 0, WatchData$Count),1))
      )
      
      WatchData2 <- WatchData %>% 
        filter(value == randomgame())%>% 
        select(season, week, home_team, away_team) 
      
      datatable(WatchData2, rownames = F,
                options = list(dom = 't')
                
      )
      
    })
    
    output$WRMetrics = DT::renderDataTable({
      
      CollegeWAR <- CollegeWARJoin %>%
        group_by(player_id) %>%
        slice_head(n = 1) %>%
        select(player_id, AvgWAR)
      
      
      SeparatortableFull <- CollegeWRWARFill %>%
        left_join(CollegeWAR, by = c("player_id")) %>%
        group_by(season, player_id) %>%
        mutate(ContestedRate = contested_targets/targets,
               yards_after_catch_per_reception = if_else(is.na(yards_after_catch_per_reception) == 1, 0, yards_after_catch_per_reception),
               contested_catch_rate = if_else(is.na(contested_catch_rate) == 1, 0, contested_catch_rate),
               CCR = mean(contested_catch_rate),
               DRP = mean(drop_rate),
               YAC = mean(yards_after_catch_per_reception)
        ) %>%
        filter(targets >= 20, grades_offense >= 60, position == "WR") %>%
        distinct() %>%
        group_by() %>%
        mutate(ContestedTile = 1-percent_rank(ContestedRate),
               GradeTile = percent_rank(grades_offense),
               ADOTTile = percent_rank(avg_depth_of_target),
               YPRRTile = percent_rank(yprr),
               
               CCR = percent_rank(CCR),
               DRP = 1 - percent_rank(DRP),
               YAC = percent_rank(YAC),
               CollegeWAR = percent_rank(AvgWAR)+0.01,
               Value = (YPRRTile^12 +
                          YAC^3 +
                          DRP^3 +
                          exp(CCR) +
                          log(CollegeWAR)),
               Value = percent_rank(Value)
        ) %>%
        arrange(desc(Value)) %>%
        mutate(NonSeparator = case_when(ContestedTile <= 0.2 ~ 1,
                                        ADOTTile <= 0.15 ~ 1,
                                        ContestedTile <= 0.3 &
                                          ADOTTile <= 0.3 ~ 1,
                                        ContestedTile <= 0.4 &
                                          ADOTTile >= 0.8 ~ 1,
                                        T ~ 0)) %>%
        group_by(player_id) %>%
        arrange(desc(season)) %>%
        summarise(player = head(player, 1),
                  team_name = head(team_name, 1),
                  # Ht = head(Ht, 1),
                  # Wt = head(Wt, 1),
                  # Density = round(head(Density, 1),2),
                  # RAS = head(RAS, 1),
                  Seasons = n(),
                  ContestedTile = round(round(mean(ContestedTile),4)*100, 4),
                  GradeTile = round(round(mean(GradeTile),4)*100, 4),
                  ADOTTile = round(round(mean(ADOTTile),4)*100,4),
                  YPRRTile = round(round(mean(YPRRTile),4)*100,4),
                  Value = round(round(mean(Value),4)*100, 4),
                  TotalNonSepSeasons = sum(NonSeparator),
                  NonSepPercent = round(TotalNonSepSeasons/Seasons,2)) %>%
        mutate(Seasons = factor(Seasons, levels = 1:5),
               Filter = factor(case_when(
                 ADOTTile <= 21 ~ "Gadget",
                 (TotalNonSepSeasons > 1 &                  NonSepPercent > 0.5) ~ "NonSeparator",
                 T ~ "Solid"
                 
               ))
        )%>%
        arrange(desc(Value))
      
      
      ContestedTilebrks <- quantile(SeparatortableFull$ContestedTile, probs = seq(.05, .95, .01), na.rm = TRUE)
      ContestedTilebrksy <- round(seq(255, 40, length.out = length(ContestedTilebrks) + 1), 0)
      ContestedTilebrksclrs <- paste0("rgb(", ContestedTilebrksy, "," , 255-ContestedTilebrksy , ",", 0, ")")
      
      GradeTilebrks <- quantile(SeparatortableFull$GradeTile, probs = seq(.05, .95, .01), na.rm = TRUE)
      GradeTilebrksy <- round(seq(255, 40, length.out = length(GradeTilebrks) + 1), 0)
      GradeTilebrksclrs <- paste0("rgb(", GradeTilebrksy, "," , 255-GradeTilebrksy , ",", 0, ")")
      
      
      
      ADOTTilebrks <- quantile(SeparatortableFull$ADOTTile, probs = seq(.05, .95, .01), na.rm = TRUE)
      ADOTTilebrksy <- round(seq(255, 40, length.out = length(ADOTTilebrks) + 1), 0)
      ADOTTilebrksclrs <- paste0("rgb(", ADOTTilebrksy, "," , 255-ADOTTilebrksy , ",", 0, ")")
      
      
      YPRRTilebrks <- quantile(SeparatortableFull$YPRRTile, probs = seq(.05, .95, .01), na.rm = TRUE)
      YPRRTilebrksy <- round(seq(255, 40, length.out = length(YPRRTilebrks) + 1), 0)
      YPRRTilebrksclrs <- paste0("rgb(", YPRRTilebrksy, "," , 255-YPRRTilebrksy , ",", 0, ")")
      
      
      Valuebrks <- quantile(SeparatortableFull$Value, probs = seq(.05, .95, .01), na.rm = TRUE)
      Valuebrksy <- round(seq(255, 40, length.out = length(Valuebrks) + 1), 0)
      Valuebrksclrs <- paste0("rgb(", Valuebrksy, "," , 255-Valuebrksy , ",", 0, ")")
      
      
      datatable(SeparatortableFull, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 10,
                                                       dom = 'Bfrtip',
                                                       buttons = c('csv', 'excel'))) %>%
        formatStyle("ContestedTile",
                    backgroundColor = styleInterval(ContestedTilebrks,ContestedTilebrksclrs)
        )%>%
        formatStyle("GradeTile",
                    backgroundColor = styleInterval(GradeTilebrks,GradeTilebrksclrs)
        )%>%
        formatStyle("ADOTTile",
                    backgroundColor = styleInterval(ADOTTilebrks,ADOTTilebrksclrs)
        )%>%
        formatStyle("YPRRTile",
                    backgroundColor = styleInterval(YPRRTilebrks,YPRRTilebrksclrs)
        )%>%
        formatStyle("Value",
                    backgroundColor = styleInterval(Valuebrks,Valuebrksclrs)
        )
      
      
    })
    
}
    
# Run the application 
shinyApp(ui = ui, server = server)
