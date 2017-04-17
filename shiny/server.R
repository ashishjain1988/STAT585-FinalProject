library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(ggvis)

matches <- read.csv("../data/matches.csv", stringsAsFactors = FALSE)
ballbyball <- read.csv("../data/deliveries1.csv", stringsAsFactors = FALSE)
season_map_mapping<-aggregate(id ~ season, matches, c)
dataset<-merge(matches,ballbyball,by.x="id",by.y="match_id")
#ballbyball %>% mutate(season = matches[matches$id == match_id,]$season)
#for(i in 1:nrow(ballbyball))
#{
#  print(i)
#  ballbyball[i,]$season<-matches[matches$id == ballbyball[i,]$match_id,]$season
#}

shinyServer(function(input, output,session) {

  stat_data_tosswon <- reactive({
    if(input$team_year != "All")
    {
      Toss<-matches %>% filter(season == input$team_year) %>% group_by(toss_winner) %>% count() %>% mutate(winner = toss_winner) %>% select(-toss_winner)
      Match<-matches %>% filter(season == input$team_year) %>% group_by(winner) %>% filter(winner != "") %>%count()
    }else
    {
      Toss<-matches %>% group_by(toss_winner) %>% count() %>% mutate(winner = toss_winner) %>% select(-toss_winner)
      Match<-matches %>% group_by(winner) %>% filter(winner != "") %>%count()
    }
    Toss$name<-"Toss"
    Match$name<-"Match"
    team_seasons<-matches %>% gather(key=team,value=teamname,c(5:6)) %>% distinct(season,teamname) %>% group_by(teamname) %>% count() %>% filter(n>=input$team_range)
    return(rbind(Match,Toss) %>% filter(winner %in% team_seasons$teamname))

  })
  output$stat_tossdecision <- renderPlotly({
    gg<-ggplot(stat_data_tosswon(), aes(winner,n, fill = name)) + geom_histogram(position = "dodge",stat = "identity") +
      ggtitle(paste("Number of tosses won by the teams in", input$team_year, "season and played at least",input$team_range,"seasons")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Teams") + ylab("#Matches")
    plotly::ggplotly(gg)
  })


  stat_data_avgteamruns <- reactive({
    if(input$team_year != "All")
    {
      teamtotalruns <- ballbyball %>% filter(Season == input$team_year) %>% group_by(batting_team, match_id) %>% summarise(totalscore = sum(total_runs))
    }else
    {
      teamtotalruns <- ballbyball %>% group_by(batting_team, Season) %>% summarise(totalscore = sum(total_runs))
    }
    team_seasons<-matches %>% gather(key=team,value=teamname,c(5:6)) %>% distinct(season,teamname) %>% group_by(teamname) %>% count() %>% filter(n>=input$team_range)
    return(teamtotalruns %>% filter(batting_team %in% team_seasons$teamname))
  })
  output$stat_avgteamruns <- renderPlotly({
    gg<-ggplot(stat_data_avgteamruns(), aes(x=batting_team, y = totalscore,fill = batting_team)) + geom_boxplot() +
      ggtitle(paste("Number of tosses won by the teams in ", input$team_year,"season")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Teams") + ylab("#Total Score")
    plotly::ggplotly(gg)
  })

  stat_data_matchesPlayed <- reactive({
    if(input$team_year != "All")
    {
      winner<-matches %>% filter(winner != "") %>% group_by(winner) %>% count() %>% mutate(teamname = winner) %>% select(-winner)
      total<-matches %>% gather(key=team,value=teamname,c(5:6)) %>% group_by(teamname) %>% count()

    }else
    {
      winner<-matches %>% filter(winner != "") %>% group_by(winner) %>% count() %>% mutate(teamname = winner) %>% select(-winner)
      total<-matches %>% gather(key=team,value=teamname,c(5:6)) %>% group_by(teamname) %>% count()
    }
    winner$name<-"Winner"
    total$name<-"Total Played"
    team_seasons<-matches %>% gather(key=team,value=teamname,c(5:6)) %>% distinct(season,teamname) %>% group_by(teamname) %>% count() %>% filter(n>=input$team_range)
    return(rbind(winner,total) %>% filter(teamname %in% team_seasons$teamname))
  })
  output$stat_matchesPlayed <- renderPlotly({
    gg<-ggplot(stat_data_matchesPlayed(), aes(teamname,n, fill = name)) + geom_histogram(position = "dodge",stat = "identity") +
      ggtitle(paste("Number of matches played in", input$team_year,  "seasons")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Season") + ylab("#Matches Played")
    plotly::ggplotly(gg)
  })

  stat_data_topbatsmen <- reactive({
    if(input$team_year != "All")
    {
      ballbyball %>% filter(input$team_year == Season) %>% group_by(batsman) %>% summarise(Total.Runs = sum(batsman_runs)) %>% arrange(desc(Total.Runs)) %>% head(n=10)
    }else
    {
      ballbyball %>% group_by(batsman) %>% summarise(Total.Runs = sum(batsman_runs)) %>% arrange(desc(Total.Runs)) %>% head(n=10)
    }

  })
  output$stat_topbatsmen <- renderPlotly({
    gg<-ggplot(data = stat_data_topbatsmen()) + geom_histogram(aes(x=batsman,y=Total.Runs,fill = batsman),stat = "identity") + geom_text(aes(x=batsman,y=Total.Runs+50,label=Total.Runs)) +
      ggtitle(paste("Top Batsmen in ", input$team_year,"season")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Batsman name") + ylab("#Total Runs")
    plotly::ggplotly(gg)
  })

  stat_data_topbowler <- reactive({
    dismissal<-ballbyball %>% select(dismissal_kind) %>% unique() %>% subset(dismissal_kind != c("","retired hurt")) %>% subset(dismissal_kind != c("run out","obstructing the field"))
    if(input$team_year != "All")
    {
      ballbyball %>% filter(input$team_year == Season,dismissal_kind %in% dismissal[,1]) %>% select(bowler,batsman,dismissal_kind) %>% count(bowler) %>% arrange(desc(n)) %>% head(n=10)
    }else
    {
      ballbyball %>% filter(dismissal_kind %in% dismissal[,1]) %>% select(bowler,batsman,dismissal_kind) %>% count(bowler) %>% arrange(desc(n)) %>% head(n=10)
    }
  })
  output$stat_topbowler <- renderPlotly({
    gg<-ggplot(data = stat_data_topbowler()) + geom_histogram(aes(x=bowler,y=n,fill = bowler),stat = "identity") + geom_text(aes(x=bowler,y=n+3,label=n)) +
      ggtitle(paste("Top Bowler in ", input$team_year,"season")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Bowler name") + ylab("#Wickets")
    plotly::ggplotly(gg)
  })

  stat_data_topplayer <- reactive({
    if(input$team_year != "All")
    {
      matches %>% filter(season == input$team_year) %>% group_by(player_of_match) %>% count(player_of_match) %>% arrange(desc(n)) %>% head(n=10)
    }else
    {
      matches %>% group_by(player_of_match) %>% count(player_of_match) %>% arrange(desc(n)) %>% head(n=10)
    }
  })
  output$stat_topplayer <- renderPlotly({
    gg<-ggplot(data = stat_data_topplayer()) + geom_histogram(aes(x=player_of_match,y=n,fill = player_of_match),stat = "identity") + geom_text(aes(x=player_of_match,y=n+0.25,label=n)) +
      ggtitle(paste("Top Player of the match in ", input$team_year,"season")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Player name") + ylab("#Player of the Match")
    plotly::ggplotly(gg)
  })



  #  observeEvent(input$team_year,{
  #    x <- input$team_team
  #    year<-input$team_year
  #    if(year != "All")
  #    {
  #      mat<-matches %>% filter(season == year)
  #    }else
  #    {
  #      mat<-matches
  #    }
  #    updateSelectInput(session, "team_team",
  #                      label = "Team",
  #                      choices = sort(unique(c(mat$team1,mat$team2))))
  #  })

  observeEvent(input$team_range1,{

    team_seasons<-matches %>% gather(key=team,value=teamname,c(5:6)) %>% distinct(season,teamname) %>% group_by(teamname) %>% count() %>% filter(n>=input$team_range1)
    updateSelectInput(session, "team_team",
                      label = "Team",
                      choices = team_seasons$teamname)
  })

  team_data_tosswon <- reactive({
    Toss<-matches %>% filter(input$team_team == toss_winner) %>% group_by(season) %>% count()
    Match<-matches %>% filter(input$team_team == winner) %>% group_by(season) %>% filter(winner != "") %>%count()
    Toss$name<-"Toss"
    Match$name<-"Match"
    return(rbind(Match,Toss))

  })

  output$team_tosswinner <- renderPlotly({
    gg<-ggplot(team_data_tosswon(), aes(season,n, fill = name)) + geom_histogram(position = "dodge",stat = "identity") +
      ggtitle(paste("Number of Tosses won by ", input$team_team, "across all seasons")) +
       xlab("Teams") + ylab("#Toss Won")
    plotly::ggplotly(gg)
  })

  team_data_avgteamruns <- reactive({
    teamtotalruns <- ballbyball %>% filter(input$team_team == batting_team) %>% group_by(match_id, Season) %>% summarise(totalscore = sum(total_runs))
  })
  output$team_avgteamruns <- renderPlotly({
    gg<-ggplot(team_data_avgteamruns(), aes(x=Season, y = totalscore,fill = factor(Season))) + geom_boxplot() +
      ggtitle(paste("Number of tosses won by the teams in ", input$team_year,"season")) +
      xlab("Season") + ylab("#Total Score")
    plotly::ggplotly(gg)
  })



  #  observeEvent(input$team_year,{
  #    x <- input$team_team
  #    year<-input$team_year
  #    if(year != "All")
  #    {
  #      mat<-matches %>% filter(season == year)
  #    }else
  #    {
  #      mat<-matches
  #    }
  #    updateSelectInput(session, "team_team",
  #                      label = "Team",
  #                      choices = sort(unique(c(mat$team1,mat$team2))))
  #  })

   observeEvent(input$typeOfChart,{
     if(input$typeOfChart == "Player of the match")
     {
      player_seasons<-ballbyball %>% distinct(Season,batsman) %>% group_by(batsman) %>% count() %>% filter(n>=input$player_range)
      players<-data.frame(player=sort(unique(matches$player_of_match))) %>% filter(player %in% player_seasons$batsman) %>% filter(player != "")
      updateSelectInput(session, "player_name",
                                            label = "Player Of Match",
                       choices = players[,1],selected = "V Kohli")
     }else if(input$typeOfChart == "Runs by season")
     {
       player_seasons<-ballbyball %>% distinct(Season,batsman) %>% group_by(batsman) %>% count() %>% filter(n>=input$player_range)
       players<-data.frame(player=sort(unique(ballbyball$batsman))) %>% filter(player %in% player_seasons$batsman) %>% filter(player != "")
       updateSelectInput(session, "player_name",
                         label = "Batsmen",
                         choices = players[,1],selected = "V Kohli")
     }else if(input$typeOfChart == "Wickets by season")
     {
       player_seasons<-ballbyball %>% distinct(Season,bowler) %>% group_by(bowler) %>% count() %>% filter(n>=input$player_range)
       players<-data.frame(player=sort(unique(ballbyball$bowler))) %>% filter(player %in% player_seasons$bowler) %>% filter(player != "")
       updateSelectInput(session, "player_name",
                         label = "Bowler",
                         choices = players[,1],selected = "P Kumar")
     }
    })

   observeEvent(input$player_range,{
     if(input$typeOfChart == "Player of the match")
     {
       player_seasons<-ballbyball %>% distinct(Season,batsman) %>% group_by(batsman) %>% count() %>% filter(n>=input$player_range)
       players<-data.frame(player=sort(unique(matches$player_of_match))) %>% filter(player %in% player_seasons$batsman) %>% filter(player != "")
       updateSelectInput(session, "player_name",
                         label = "Player Of Match",
                         choices = players[,1],selected = "V Kohli")
     }else if(input$typeOfChart == "Runs by season")
     {
       player_seasons<-ballbyball %>% distinct(Season,batsman) %>% group_by(batsman) %>% count() %>% filter(n>=input$player_range)
       players<-data.frame(player=sort(unique(ballbyball$batsman))) %>% filter(player %in% player_seasons$batsman) %>% filter(player != "")
       updateSelectInput(session, "player_name",
                         label = "Batsmen",
                         choices = players[,1],selected = "V Kohli")
     }else if(input$typeOfChart == "Wickets by season")
     {
       player_seasons<-ballbyball %>% distinct(Season,bowler) %>% group_by(bowler) %>% count() %>% filter(n>=input$player_range)
       players<-data.frame(player=sort(unique(ballbyball$bowler))) %>% filter(player %in% player_seasons$bowler) %>% filter(player != "")
       updateSelectInput(session, "player_name",
                         label = "Bowler",
                         choices = players[,1],selected = "P Kumar")
     }
   })

  player_data_matchplayer <- reactive({
    matches %>% filter(input$player_name == player_of_match) %>% group_by(season)
  })

  player_data_runsbyseason <- reactive({
    ballbyball %>% filter(input$player_name == batsman) %>% group_by(Season,match_id) %>% summarise(totalscore = sum(batsman_runs))
  })

  player_data_wicketsbyseason <- reactive({
    dismissal<-ballbyball %>% select(dismissal_kind) %>% unique() %>% subset(dismissal_kind != c("","retired hurt")) %>% subset(dismissal_kind != c("run out","obstructing the field"))
    return(ballbyball %>% filter(dismissal_kind %in% dismissal[,1] & input$player_name == bowler) %>% select(batsman,dismissal_kind,Season) %>% count(Season) %>% arrange(desc(n)))

  })

  output$player_matchplayer <- renderPlotly({
    if(input$typeOfChart == "Player of the match")
    {
      gg<-ggplot(data = player_data_matchplayer(), aes(season,fill = factor(season))) + geom_bar() +
        ggtitle(paste("Number of player of the match for ", input$player_name, "across all seasons")) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("Season") + ylab("#Player of Match Won")
      plotly::ggplotly(gg)
    }else if(input$typeOfChart == "Runs by season")
    {
      gg<-ggplot(player_data_runsbyseason(), aes(x=Season, y = totalscore,fill = factor(Season))) + geom_boxplot() +
      ggtitle(paste("Total Runs for ", input$player_name, "across all seasons")) +
        xlab("Season") + ylab("#Runs")
      plotly::ggplotly(gg)
    }else if("Wickets by season")
    {
      gg<-ggplot(data = player_data_wicketsbyseason(),aes(x=Season,y=n)) + geom_point() + geom_line() +
      ggtitle(paste("Total wickets for ", input$player_name, "across all seasons")) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("Season") + ylab("#Wickets")
      plotly::ggplotly(gg)
    }else
    {
      gg<-ggplot(data = player_data_wicketsbyseason(),aes(x=Season,y=n)) + geom_point() + geom_line() +
        ggtitle(paste("Total wickets for ", input$player_name, "across all seasons")) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("Season") + ylab("#Wickets")
      plotly::ggplotly(gg)
    }
  })
})
