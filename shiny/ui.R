library(shiny)
library(plotly)
library(ggvis)

matches <- read.csv("../data/matches.csv", stringsAsFactors = FALSE)


shinyUI(navbarPage("T20 Cricket - Indian Premier League (IPL)",
                   tabPanel("Statistics"),
                   tabPanel("By Team",
                            sidebarPanel(
                    selectInput("year","Season",choices = c("All",sort(unique(matches$season))),selected = "2008"),
                    selectInput("team","Team",choices = sort(unique(c(matches$team1,matches$team2)))),
                    selectInput("venue","Venue",choices = sort(unique(c(matches$city))),selected = "Delhi")
                            ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Toss Winner",plotlyOutput("tosswinner"))
                        #tabPanel("Toss Decision",plotlyOutput("tossdecision")),
                        #tabPanel("Winning Percentage",plotlyOutput("winningpercentage"))
                      )
                    )
                    ),
                   tabPanel("By Player",
                            sidebarPanel(
                              selectInput("year","Season",choices = sort(unique(matches$season)),selected = "2008"),
                              selectInput("team","Team",choices = sort(unique(c(matches$team1,matches$team2))),selected = "Rajasthan Royals"),
                              selectInput("venue","Venue",choices = sort(unique(c(matches$city))),selected = "Delhi")
                            ),
                            mainPanel(
                              tabsetPanel(
                                #tabPanel("Winning Percentage",plotOutput("crime")),
                                #tabPanel("location",plotlyOutput("location"))
                              )
                            ))
))
