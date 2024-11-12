library(shiny)
library(tidyverse)
library(duckdb)
library(shinyjs)

# Connect to db
dpv = duckdb("project.db")
con = dbConnect(dpv)

# Set tables
rosterfact = dbGetQuery(con, "SELECT * FROM rosterfact") %>% 
  mutate(SeasonDisplay = as.numeric(substr(Season, 1, 4))) %>% 
  rename(PctRetention = PctTurnover)

dimteam = dbGetQuery(con, "SELECT * FROM dimteam")

colors = read_csv("color_teams.csv")
dimteam$HexColor = colors$Hex_Code

tenures = dbGetQuery(con, "SELECT * FROM playertenures")

# Close db
dbDisconnect(con)

conference_colors = c(
    "#013CA6",
    "#C41230",
    "#ED1A39",
    "#0088CE",
    "#4f2d7f",
    "#22356B",
    "#2CCCD3"
)




server = function(input, output, session) {
  
  
  
  #-------------------------------------
  #---------TAB 1: Roster Sizes---------
  #-------------------------------------
  
  
  observeEvent(input$filternil, {
    updatePickerInput(session, "season_filter", selected = c("2021-22", "2022-23", "2023-24", "2024-25"))
  })

  
  output$rostersizemain = renderPlotly({
    plt = rosterfact %>% 
      group_by(SeasonDisplay, Season) %>% 
      summarize(avg_roster_size = mean(RosterSize)) %>% 
      ggplot(aes(SeasonDisplay, avg_roster_size)) +
      geom_smooth(se = F) +
      geom_point(aes(text = paste("Season: ", Season, "\nAvg. Roster Size: ", round(avg_roster_size, 2), sep = ""))) +
      labs(
        x = "Season",
        y = "Roster Size",
        title = "Average Roster Size by Season"
      ) +
      geom_vline(xintercept = 2020.5, linetype = "dashed") +
      annotate("text", x = 2022.1, y = 15.0, label = "NIL + Transfer Portal")
    
    ggplotly(plt, tooltip = "text") %>% config(displayModeBar = F)

  })
  
  roster_w_conf = reactive({
    return (
      rosterfact %>% 
        left_join(dimteam, by = join_by(Team == Name)) %>% 
        select(Conference, Team, Season, SeasonDisplay, RosterSize, PctRetention)
    )
  })
  
  output$rostersizeconf = renderPlotly({
    plt = roster_w_conf() %>% 
      filter(Season %in% input$season_filter) %>% 
      group_by(Conference, SeasonDisplay, Season) %>% 
      summarize(avg_roster_size = mean(RosterSize)) %>% 
      ggplot(aes(SeasonDisplay, avg_roster_size, color = Conference, text = paste("Season: ", Season, "\nConference: ", Conference, "\nRoster Size: ", round(avg_roster_size, 2), sep = ""))) +
      geom_point(size = 0.8) +
      geom_line(aes(text = paste("Conference: ", Conference, sep = ""))) +
      scale_color_manual(values = conference_colors) +
      geom_vline(xintercept = 2020.5, linetype="dashed") +
      labs(
        x = "Season",
        y = "Roster Size"
      )
    
    if (length(input$season_filter) == 0){
      ggplotly(ggplot() + theme_void())
    } else {
    ggplotly(plt, tooltip = "text") %>% config(displayModeBar = F)
    }
  })
  
  
  team_colors_filt = reactive({
    dimteam %>% 
      filter(Conference == input$conf_filter[1]) %>% 
      pull(HexColor)
  })
  
  output$rostersizeteam = renderPlotly({
    plt = roster_w_conf() %>% 
      filter(Conference == input$conf_filter & Season %in% input$season_filter) %>% 
      group_by(Team) %>% 
      summarize(avg_roster_size = mean(RosterSize)) %>% 
      ggplot(aes(reorder(Team, -avg_roster_size), avg_roster_size, text = 
                   paste("Team: ", Team, "\nRoster Size (avg): ", round(avg_roster_size, 2))
                 )) + 
      geom_col(aes(fill = Team)) +
      scale_fill_manual(values = team_colors_filt()) +
      guides(fill = "none") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(
        x = "Team",
        y = "Roster Size (Avg.)",
        title = paste("Roster Sizes:", input$conf_filter, "Conference")
      )
      
    
    ggplotly(plt, tooltip="text") %>% config(displayModeBar = F)
      
  })
  
  
  
  #-------------------------------------
  #---------TAB 2: Roster Turnover------
  #-------------------------------------
  
  rosterfact_prenil = reactive({
    roster_w_conf() %>% 
      filter(SeasonDisplay < 2021)
  })
  
  rosterfact_postnil = reactive({
    roster_w_conf() %>% 
      filter(SeasonDisplay >= 2021)
  })
  
  pct_format = function (num) {
    return (
      paste(as.character(round(num, 4) * 100), "%", sep="")
    )
  }
  
  output$prenil = renderText({
    switch(input$scope,
           "National" = {
             pct_format(mean(rosterfact_prenil()$PctRetention, na.rm=T))
           },
           "Conference" = {
             pct_format(mean(
               rosterfact_prenil() %>% 
                 filter(Conference == input$conf) %>% 
                 pull(PctRetention),
               na.rm = T
             )
             )
           },
           "Team" = {
              pct_format(mean(
               rosterfact_prenil() %>% 
               filter(Team == input$team) %>% 
                 pull(PctRetention),
               na.rm = T
              )
              )
           }
    )
  })
  

  
  output$postnil = renderText({
    switch(input$scope,
           "National" = {
             pct_format(mean(rosterfact_postnil()$PctRetention, na.rm=T))
           },
           "Conference" = {
             pct_format(mean(
               rosterfact_postnil() %>% 
                 filter(Conference == input$conf) %>% 
                 pull(PctRetention),
               na.rm = T
             )
             )
           },
           "Team" = {
             pct_format(mean(
               rosterfact_postnil() %>% 
                 filter(Team == input$team) %>% 
                 pull(PctRetention),
               na.rm = T
             ))
           }
    )
  })
    
    observe({
      color = "black"

      switch (input$scope,
              "Team" = {
                color = dimteam %>% filter(Name == input$team) %>% pull(HexColor)
              },
              "Conference" = {
                conferences = sort(unique(dimteam$Conference))
                idx = which(conferences %in% c(input$conf))
                color = conference_colors[[idx]]
              }
      )

      runjs(sprintf('document.getElementById("prenil").style.color = "%s";', color))
      runjs(sprintf('document.getElementById("postnil").style.color = "%s";', color))

    })
 
  output$retention = renderPlotly({
    national = roster_w_conf() %>% 
      group_by(SeasonDisplay) %>% 
      summarize(Retention = mean(PctRetention, na.rm = T))
    
    conf = roster_w_conf() %>% 
      filter(Conference == input$conf) %>% 
      group_by(SeasonDisplay) %>% 
      summarize(Retention = mean(PctRetention, na.rm = T))
    
    team_colors_filt2 = reactive({
      dimteam %>% 
        filter(Conference == input$conf) %>% 
        pull(HexColor)
    })
    
    switch(input$scope,
           "National" = {
             ggplotly(roster_w_conf() %>% 
               group_by(Conference, SeasonDisplay) %>% 
               summarize(Retention = mean(PctRetention, na.rm = T)) %>% 
               rename(Season=SeasonDisplay) %>% 
               ggplot(aes(Season, Retention)) +
                geom_smooth(data = national %>% rename(Season=SeasonDisplay), aes(Season, Retention), se=F, color = "black", size = 1.2) +
                geom_line(aes(color = Conference)) +
                geom_point(aes(color = Conference)) +
                scale_color_manual(values = conference_colors)  +
                ggtitle(paste("Retention Rates by Season: Nationally"))
             ) %>% config(displayModeBar=F) %>% layout(hovermode="x")
           },
           
           "Conference" = {
             ggplotly(roster_w_conf() %>% 
                        filter(Conference == input$conf) %>% 
                        group_by(Team, SeasonDisplay) %>% 
                        summarize(Retention = mean(PctRetention, na.rm = T)) %>% 
                        rename(Season=SeasonDisplay) %>% 
                        ggplot(aes(Season, Retention)) +
                        geom_smooth(data = conf %>% rename(Season=SeasonDisplay), aes(Season, Retention), se=F, color = "black", size = 1.2) +
                        geom_line(aes(color = Team)) +
                        geom_point(aes(color = Team)) +
                        scale_color_manual(values = team_colors_filt2())  +
                        ggtitle(paste("Retention Rates by Season:", input$conf, "Conference"))
             ) %>% config(displayModeBar=F)
           },
           
           "Team" = {
             team_color = dimteam %>% filter(Name == input$team) %>% pull(HexColor)
             
             ggplotly(roster_w_conf() %>% 
                        filter(Team == input$team) %>% 
                        select(Team, SeasonDisplay, PctRetention) %>% 
                        rename(Season=SeasonDisplay, Retention=PctRetention) %>% 
                        ggplot(aes(Season, Retention)) +
                        geom_smooth(color="black", size=1.2, se=F) +
                        geom_col(color="black", fill=team_color) +
                        ggtitle(paste("Retention Rates by Season:", input$team))
             ) %>% config(displayModeBar=F)
           }
    )
  })
  
  
  
  output$tenures = renderPlot({
    
    switch(input$tenurescope, 
           "National" = {
               ggplot(tenures, aes(x = num_seasons)) +
                 geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
                 stat_bin(binwidth = 1, geom = "text", size=6, aes(label = ..count.., y = ..count..), 
                          vjust = -0.6) +
                 labs(
                   x = "# of Seasons w/ One Team",
                   y = "# of Players",
                   title = "Distribution of Player Tenures"
                 ) +
                 theme_minimal() +
                 theme(
                   axis.title = element_text(size=20),
                   axis.text = element_text(size=14),
                   plot.title = element_text(size=22, hjust=0.5),
                 )
          },
          
          "Conference" = {
            conf_color = conference_colors[match(input$conftenure, sort(unique(dimteam$Conference)))]
            
            
            ggplot(tenures %>% filter(Conference == input$conftenure), aes(x = num_seasons)) +
              geom_histogram(binwidth = 1, color = "black", fill = conf_color) +
              stat_bin(binwidth = 1, geom = "text", size=6, aes(label = ..count.., y = ..count..), 
                       vjust = -0.6) +
              labs(
                x = "# of Seasons w/ One Team",
                y = "# of Players",
                title = paste("Distribution of Player Tenures:", input$conftenure)
              ) +
              theme_minimal() +
              theme(
                axis.title = element_text(size=20),
                axis.text = element_text(size=14),
                plot.title = element_text(size=22, hjust=0.5),
              )
          },
          
          "Team" = {
            team_color = dimteam %>% filter(Name == input$teamtenure) %>% pull(HexColor)
            
            ggplot(tenures %>% filter(Team == input$teamtenure), aes(x = num_seasons)) +
              geom_histogram(binwidth = 1, color = "black", fill = team_color) +
              stat_bin(binwidth = 1, geom = "text", size=6, aes(label = ..count.., y = ..count..), 
                       vjust = -0.6) +
              labs(
                x = "# of Seasons w/ One Team",
                y = "# of Players",
                title = paste("Distribution of Player Tenures:", input$teamtenure)
              ) +
              theme_minimal() +
              theme(
                axis.title = element_text(size=20),
                axis.text = element_text(size=14),
                plot.title = element_text(size=22, hjust=0.5),
              )
          }
      
    )
  })
  
  output$tenureavg = renderText({
    switch(input$tenurescope,
           "National" = {
             round(mean(tenures$num_seasons), 3)
           },
           
           "Conference" = {
             round(mean(
               tenures %>% 
                 filter(Conference == input$conftenure) %>% 
                 pull(num_seasons)
             ), 3)
           },
           
           "Team" = {
             round(mean(
               tenures %>% 
                 filter(Team == input$teamtenure) %>% 
                 pull(num_seasons)
             ), 3)
           }
           )
  })
  
  output$tenuremed = renderText({
    switch(input$tenurescope,
           "National" = {
             median(tenures$num_seasons)
           },
           
           "Conference" = {
             median(
               tenures %>% 
                 filter(Conference == input$conftenure) %>% 
                 pull(num_seasons)
             )
           },
           
           "Team" = {
             median(
               tenures %>% 
                 filter(Team == input$teamtenure) %>% 
                 pull(num_seasons)
             )
           }
    )
  })
  
  
  # ------------------------
  # TAB 4: Experience
  # ------------------------
  
  observe({
    
    if (input$exptab == "Seasons Played") {
      updatePickerInput(
        session,
        inputId = "expseason",
        choices = c("2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"),
        selected = c("2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
      )
    }
    else {
      fullvec = sort(unique(rosterfact$Season, na.rm = T))
      
      updatePickerInput(
        session,
        inputId = "expseason",
        choices = fullvec[-c(1, length(fullvec))],
        selected = fullvec[-c(1, length(fullvec))]
      )
    }
  })
  
  output$sznexp = renderPlotly({
    
    if (length(input$expseason) == 0){
      return(ggplot() + theme_void())
    }
    
    filtered = rosterfact %>% filter(Season %in% input$expseason) %>% left_join(dimteam, by=join_by(Team==Name))
    
    if (input$expconf != "All") {
      filtered = filtered %>% filter(Conference == input$expconf)
    }
    
    lm_fit = lm(WinPct ~ Experience, filtered)
    
    ggplotly(
      ggplot(filtered, aes(Experience, WinPct, color=Team)) +
        geom_point(aes(text=paste("Team:", Team, "\nSeason:", Season, "\nAvg. Experience:", round(Experience, 3), "\nWin %:", sprintf("%.2f%%", WinPct * 100)))) +
        scale_color_manual(values =setNames(filtered$HexColor, filtered$Team)) +
        geom_abline(slope = coef(lm_fit)[2], intercept = coef(lm_fit)[1], linetype="dashed") +
        labs(
          x = "Years of Experience",
          y = "Winning Percentage",
          title = "The Impacts of Experience on Winning: High-Level Seasons Played"
        )
    , tooltip="text")
  })
  
  output$minsexp = renderPlotly({
    
    if (length(input$expseason) == 0){
      return(ggplot() + theme_void())
    }
    
    filtered = rosterfact %>% filter(Season %in% input$expseason) %>% left_join(dimteam, by=join_by(Team==Name))
    
    if (input$expconf != "All") {
      filtered = filtered %>% filter(Conference == input$expconf)
    }
    
    lm_fit = lm(WinPct ~ MinReturning, filtered)
    
    ggplotly(
      ggplot(filtered, aes(MinReturning, WinPct, color=Team)) +
        geom_point(aes(text=paste("Team:", Team, "\nSeason:", Season, "\n% Min Returning:", round(MinReturning, 3), "\nWin %:", sprintf("%.2f%%", WinPct * 100)))) +
        scale_color_manual(values =setNames(filtered$HexColor, filtered$Team)) +
        geom_abline(slope = coef(lm_fit)[2], intercept = coef(lm_fit)[1], linetype="dashed") +
        labs(
          x = "% of Minutes Returning",
          y = "Winning Percentage",
          title = "The Impacts of Experience on Winning: Minutes Returning"
        )
      , tooltip="text")
  })
  
  
  output$ptsexp = renderPlotly({
    
    if (length(input$expseason) == 0){
      return(ggplot() + theme_void())
    }
    
    filtered = rosterfact %>% filter(Season %in% input$expseason) %>% left_join(dimteam, by=join_by(Team==Name))
    
    if (input$expconf != "All") {
      filtered = filtered %>% filter(Conference == input$expconf)
    }
    
    lm_fit = lm(WinPct ~ PtsReturning, filtered)
    
    ggplotly(
      ggplot(filtered, aes(PtsReturning, WinPct, color=Team)) +
        geom_point(aes(text=paste("Team:", Team, "\nSeason:", Season, "\n% Pts Returning:", round(PtsReturning, 3), "\nWin %:", sprintf("%.2f%%", WinPct * 100)))) +
        scale_color_manual(values =setNames(filtered$HexColor, filtered$Team)) +
        geom_abline(slope = coef(lm_fit)[2], intercept = coef(lm_fit)[1], linetype="dashed") +
        labs(
          x = "% of Scoring Returning",
          y = "Winning Percentage",
          title = "The Impacts of Experience on Winning: Returning Scoring"
        )
      , tooltip="text")
  })
  
  
  
  
  
  
}