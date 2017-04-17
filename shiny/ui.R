library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(ggvis)

matches <- read.csv("../data/matches.csv", stringsAsFactors = FALSE)
ballbyball <- read.csv("../data/deliveries1.csv", stringsAsFactors = FALSE)
season_map_mapping<-aggregate(id ~ season, matches, c)


shinyUI(navbarPage("T20 Cricket - Indian Premier League (IPL)",
                   tabPanel("Statistics",
                            sidebarPanel(
                              selectInput("team_year","Season",choices = c("All",sort(unique(matches$season))),selected = "All")
                              #selectInput("team_team","Team",choices = c("All",sort(unique(c(matches$team1,matches$team2)))),selected="All")
                              #selectInput("team_venue","Venue",choices = sort(unique(c(matches$city))),selected = "Delhi")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Toss Decisions",plotlyOutput("stat_tossdecision")),
                                tabPanel("Matches played across season",plotlyOutput("stat_matchesPlayed")),
                                tabPanel("Average Runs by teams",plotlyOutput("stat_avgteamruns")),
                                tabPanel("Matchs won by each team",plotlyOutput("stat_wonmatch")),
                                tabPanel("Top Batsman",plotlyOutput("stat_topbatsmen")),
                                tabPanel("Top Bowler",plotlyOutput("stat_topbowler")),
                                tabPanel("Player of the match",plotlyOutput("stat_topplayer"))
                                ##tabPanel("Box plot of total Runs in every match",plotlyOutput("economydistribution"))
                                #tabPanel("Maximum Toss Winner",plotlyOutput("maxTossWinner")),
                                #tabPanel("Chances of Toss Winner",plotlyOutput("chancesofWinningToss")),
                                #tabPanel("Chance of Winning a Match after winning toss",plotlyOutput("winaftertoss")),
                                #tabPanel("Team 1 V/S Team 2",plotlyOutput("teamcomparison")),
                                #tabPanel("Top Batsman",plotlyOutput("topbatsmen")),
                                #tabPanel("Top Bowler",plotlyOutput("topbowler")),
                                #tabPanel("Economy Distribution",plotlyOutput("economydistribution")),
                                #tabPanel("Economy Distribution",plotlyOutput("economydistribution")),
                                #tabPanel("Wickets Fell in over",plotlyOutput("economydistribution"))
                              )
                            )),
                   tabPanel("By Team",
                            sidebarPanel(
                              selectInput("team_team","Team",choices = sort(unique(c(matches$team1,matches$team2)))),
                              selectInput("team_venue","Venue",choices = sort(unique(c(matches$city))),selected = "Delhi")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Toss Winner",plotlyOutput("team_tosswinner")),
                                tabPanel("Matchs won by each team",plotlyOutput("team_wonmatch")),
                                tabPanel("Average Runs across seasons",plotlyOutput("team_avgteamruns"))
                                #tabPanel("Toss Decision",plotlyOutput("tossdecision")),
                                #tabPanel("Winning Percentage",plotlyOutput("winningpercentage"))
                              )
                            )
                   ),
                   tabPanel("By Player",
                            sidebarPanel(
                              selectInput("player_name","Batsmen",choices = sort(unique(ballbyball$batsman))),
                              radioButtons("typeOfChart", label = "Type of Chart",
                                           choices = list("Player of the match",
                                                              "Runs by season",
                                                              "Wickets by season"
                                           ))
                            ),
                            mainPanel(
                              plotlyOutput("player_matchplayer")
                            )
                   )
))
