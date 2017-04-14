library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggvis)

matches <- read.csv("../data/matches.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output,session) {

  observeEvent(input$year,{
    x <- input$team
    if(input$year != "All")
    {
      mat<-matches %>% filter(season == input$year)
    }else
    {
      mat<-matches
    }
    updateSelectInput(session, "team",
                      label = "Team",
                      choices = sort(unique(c(mat$team1,mat$team2))))
  })

  matches_tosswon <- reactive({
    if(input$year != "All")
    {
      matches %>% filter(season == input$year & (team1 == input$team | team2 == input$team)) %>% group_by(toss_winner)
    }else
    {
      matches %>% filter((team1 == input$team | team2 == input$team)) %>% group_by(toss_winner)
    }
  })

  matches_tosswinner <- reactive({
    if(input$year != "All")
    {
      matches %>% filter(season == input$year & (team1 == input$team | team2 == input$team)) %>% group_by(toss_winner)
    }else
    {
      matches %>% filter((team1 == input$team | team2 == input$team)) %>% group_by(toss_winner)
    }
  })

    output$tosswinner <- renderPlotly({
      gg<-ggplot(data = matches_tosswon(), aes(toss_winner)) + geom_bar() +
        ggtitle(paste("Number of Tosses won by ", input$team, "in ", input$year,"season"))
      plotly::ggplotly(gg)
    })

})
