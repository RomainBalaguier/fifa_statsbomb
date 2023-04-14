# Debut code projet wyscout Rshiny   ------------------------

#nettoyage console et workspace  ------------------
cat("\104")
rm(list = ls())


# Chargement des packages nécessaires  ------------------------------
pacman::p_load(ggplot2, tidyverse, dplyr, ggsoccer,tidyr,
               shiny, shinydashboard,StatsBombR, scales,
               ggiraph,DT,plotly, formattable, ggpubr,
               gridExtra, ggforce, ggimage,magick, soccermatics)




# Appel des fonctions  --------------
# Fonction pour dessiner la cage  ------------------
post<-function(fill_background = "white"){
  
  ggplot()+
    # Ground
    geom_line(aes(x = c(32, 48), y = c(0,0)))+
    # Post
    geom_rect(aes(xmin = 35.9, xmax = 44.1, ymin = 0, ymax = 2.75), fill = "#D3D3D3", color = "black")+
    geom_rect(aes(xmin = 36, xmax = 44, ymin = 0, ymax = 2.67), fill = fill_background, 
              color = "black", alpha = 0.7)+
    # Linesgeom_line(aes(x = c(36, 36.3), y = c(2.67,2.58)), color = "gray")+ # Left Lines
    geom_line(aes(x = c(36.3, 36.7), y = c(2.58, 0.8)), color = "gray")+
    geom_line(aes(x = c(36.7, 36), y = c(0.8, 0)), color = "gray")+
    geom_line(aes(x = c(44, 43.7), y = c(2.67, 2.58)), color = "gray")+ # Right Lines
    geom_line(aes(x = c(43.7, 43.3), y = c(2.58, 0.8)), color = "gray")+
    geom_line(aes(x = c(43.3, 44), y = c(0.8, 0)), color = "gray")+
    geom_line(aes(x = c(36.7, 43.3), y = c(0.8,0.8)), color = "gray")+ # Ground Line
    # Theme
    theme(
      panel.background = element_rect(fill = fill_background),
      plot.background = element_rect(fill = fill_background),
      legend.background = element_rect(fill = fill_background),
      legend.key = element_rect(fill = fill_background,colour = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
      
    )
  
}



# Create pitch function  ------------------------
createPitch <- function(xmax=125, ymax=80, grass_colour="white", line_colour="gray", background_colour="white", goal_colour="gray", data=NULL, halfPitch=FALSE){
  
  GoalWidth <- 8
  penspot <- 12
  boxedgeW <- 44
  boxedgeL <- 18
  box6yardW <- 20
  box6yardL <- 6
  corner_d=3
  centreCirle_d <- 20
  
  # The 18 Yard Box
  TheBoxWidth <- c(((ymax / 2) + (boxedgeW / 2)),((ymax / 2) - (boxedgeW / 2)))
  TheBoxHeight <- c(boxedgeL,xmax-boxedgeL)
  GoalPosts <- c(((ymax / 2) + (GoalWidth / 2)),((ymax / 2) - (GoalWidth / 2)))
  
  # The 6 Yard Box
  box6yardWidth <- c(((ymax / 2) + (box6yardW / 2)),((ymax / 2) - (box6yardW / 2)))
  box6yardHeight <- c(box6yardL,xmax-box6yardL)
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create leftD arc ####
  Dleft <- circleFun(c((penspot),(ymax/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]
  
  ## create rightD arc  ####
  Dright <- circleFun(c((xmax-(penspot)),(ymax/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dright <- Dright[which(Dright$x <= (xmax-(boxedgeL))),]
  
  #### create center circle ####
  center_circle <- circleFun(c((xmax/2),(ymax/2)),centreCirle_d,npoints = 2000)
  
  
  if (halfPitch==FALSE){
    xmin=0
    ymin=0
    
    ## create corner flag radius ####
    TopLeftCorner <- circleFun(c(xmin,ymax),corner_d,npoints = 1000)
    TopLeftCorner <- TopLeftCorner[which(TopLeftCorner$x > (xmin)),]
    TopLeftCorner <- TopLeftCorner[which(TopLeftCorner$y < (ymax)),]
    TopRightCorner <- circleFun(c(xmax,ymax),corner_d,npoints = 1000)
    TopRightCorner <- TopRightCorner[which(TopRightCorner$x < (xmax)),]
    TopRightCorner <- TopRightCorner[which(TopRightCorner$y < (ymax)),]
    
    BottomLeftCorner <- circleFun(c(xmin,ymin),corner_d,npoints = 1000)
    BottomLeftCorner <- BottomLeftCorner[which(BottomLeftCorner$x > (xmin)),]
    BottomLeftCorner <- BottomLeftCorner[which(BottomLeftCorner$y > (ymin)),]
    
    BottomRightCorner <- circleFun(c(xmax,ymin),corner_d,npoints = 1000)
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$x < (xmax)),]
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$y > (ymin)),]
    
    
    
    ggplot(data=data) + #xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
      # add the theme 
      #theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour)+
      
      # add the 18 yard box Left
      geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box Right
      geom_rect(aes(xmin=TheBoxHeight[2], xmax=xmax, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour) +
      # add the six yard box Left
      geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)  +
      # add the six yard box Right
      geom_rect(aes(xmin=box6yardHeight[2], xmax=xmax, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)  + 
      # Add half way line 
      geom_segment(aes(x = xmax/2, y = ymin, xend = xmax/2, yend = ymax),colour = line_colour) +
      # add left D 
      geom_path(data=Dleft, aes(x=x,y=y), colour = line_colour) + 
      # add Right D 
      geom_path(data=Dright, aes(x=x,y=y), colour = line_colour) +
      # add centre circle 
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) +
      
      # add penalty spot left 
      geom_point(aes(x = penspot , y = ymax/2), colour = line_colour) + 
      # add penalty spot right
      geom_point(aes(x = (xmax-(penspot)) , y = ymax/2), colour = line_colour) + 
      # add centre spot 
      geom_point(aes(x = (xmax/2) , y = ymax/2), colour = line_colour) +
      # add Corner Flag corners
      geom_path(data=TopLeftCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=TopRightCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=BottomLeftCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=BottomRightCorner, aes(x=x,y=y), colour = line_colour) +
      geom_segment(aes(x = xmin-0.2, y = GoalPosts[1], xend = xmin-0.2, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
      # add the goal right
      geom_segment(aes(x = xmax+0.2, y = GoalPosts[1], xend = xmax+0.2, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
      
      coord_fixed() +
      theme(rect = element_blank(),#, #remove additional ggplot2 features: lines, axis, etc...
            line = element_blank(), 
            #legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  else{
    xmin=(xmax/2)
    ymin=0
    center_circle = center_circle[which(center_circle$x>=xmin),]
    
    ## create corner flag radius ####
    BottomRightCorner <- circleFun(c(xmax,ymin),corner_d,npoints = 1000)
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$x < (xmax)),]
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$y > (ymin)),]
    TopRightCorner <- circleFun(c(xmax,ymax),corner_d,npoints = 1000)
    TopRightCorner <- TopRightCorner[which(TopRightCorner$x < (xmax)),]
    TopRightCorner <- TopRightCorner[which(TopRightCorner$y < (ymax)),]
    
    ggplot(data=data) + #xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
      # add the theme 
      #theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour)+ 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=TheBoxHeight[2], xmax=xmax, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour)+ 
      # add the six yard box offensive
      geom_rect(aes(xmin=box6yardHeight[2], xmax=xmax, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)+  
      # add the arc circle 
      geom_path(data=Dright, aes(x=x,y=y), colour = line_colour)+
      #add center arc
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour)+
      # add penalty spot 
      
      geom_point(aes(x = (xmax-(penspot)) , y = ymax/2), colour = line_colour) +
      # add centre spot 
      geom_point(aes(x = (xmax/2) , y = ymax/2), colour = line_colour) +
      #geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
      # add Corner Flag corners
      geom_path(data=BottomRightCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=TopRightCorner, aes(x=x,y=y), colour = line_colour) +
      
      
      # add the goal right
      geom_segment(aes(x = xmax+0.2, y = GoalPosts[1], xend = xmax+0.2, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
      # add the goal offensive
      
      
      coord_fixed() +
      theme(rect = element_blank(), #remove additional ggplot2 features: lines, axis, etc...
            line = element_blank(), 
            #legend.position = "none",
            axis.title.y = element_blank(), 
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  
  
}

backgroundImageCSS <- "background-color: #cccccc; 
                       height: 45vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       background-size: cover;
                       background-image: url('%s');"

# Define UI for application that draws a histogram---------------
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Seesports"),
                    dashboardSidebar(
                      conditionalPanel(
                        condition = "input.password != 'ws'",  
                        textInput("password", "Password:", value = ""),
                      ),
                      conditionalPanel(
                        condition = "input.password == 'ws'",
                        uiOutput("stage_comp"),
                        
                        
                      )),
                    dashboardBody(
                      conditionalPanel(
                        condition = "input.password == 'ws'",
                        uiOutput("reca_team")
                      ))
)

# Define server logic required to draw a histogram----------
server <- function(input, output) {
  
  observeEvent(input$password,{
    
    fifa<-load("./FIFA.Rda")
    matches<-load("./FIFA_match.Rda")
    
# Fonction pour dessiner la cage  ------------------
  function(fill_background = "white"){
    
    ggplot()+
      # Ground
      geom_line(aes(x = c(32, 48), y = c(0,0)))+
      # Post
      geom_rect(aes(xmin = 35.9, xmax = 44.1, ymin = 0, ymax = 2.75), fill = "#D3D3D3", color = "black")+
      geom_rect(aes(xmin = 36, xmax = 44, ymin = 0, ymax = 2.67), fill = fill_background, 
                color = "black", alpha = 0.7)+
      # Linesgeom_line(aes(x = c(36, 36.3), y = c(2.67,2.58)), color = "gray")+ # Left Lines
      geom_line(aes(x = c(36.3, 36.7), y = c(2.58, 0.8)), color = "gray")+
      geom_line(aes(x = c(36.7, 36), y = c(0.8, 0)), color = "gray")+
      geom_line(aes(x = c(44, 43.7), y = c(2.67, 2.58)), color = "gray")+ # Right Lines
      geom_line(aes(x = c(43.7, 43.3), y = c(2.58, 0.8)), color = "gray")+
      geom_line(aes(x = c(43.3, 44), y = c(0.8, 0)), color = "gray")+
      geom_line(aes(x = c(36.7, 43.3), y = c(0.8,0.8)), color = "gray")+ # Ground Line
      # Theme
      theme(
        panel.background = element_rect(fill = fill_background),
        plot.background = element_rect(fill = fill_background),
        legend.background = element_rect(fill = fill_background),
        legend.key = element_rect(fill = fill_background,colour = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
        
      )
    
  }
  
  
# Mise en forme du shiny side bar  -------------------------- 
  
  
  
  output$stage_comp<-renderUI({
    sidebarMenu(
      selectInput(inputId = "choice_team",label = "Pick a team",
                  choices = levels(as.factor(events$team.name))),
      uiOutput("select_player"),
      selectInput(inputId = "choice_event",label = "Pick an event",
                  choices = c("Dashboard after match",
                              "Pass","Duel","Shot", "Defensive metrics"
                              )),
      uiOutput("pass_menu"),
      uiOutput("select_match"),
      uiOutput("slider_minute"),
      uiOutput("seance_penalty")
      
    )
      
  })#fermeture observe event input$choice_radio team/player/stage
  
  observeEvent(input$choice_stage,{
  output$menu <- renderUI({
    if (input$choice_stage == "Group Stage"){
    sidebarMenu(
      selectInput(inputId = "group_choice", label = "Pour quel groupe",
                  choices = levels(as.factor(Matches$home_team.home_team_group)))
    )
    }else{
      sidebarMenu(
        uiOutput("side_bar_choice_stage"),
      )
    }
  }) #fermeture renderUI sidebar menu
}) # fermeture observe event stage comp

  observeEvent(input$choice_event,{
    output$pass_menu <- renderUI({
      if (input$choice_event == "Pass"){
        sidebarMenu(
          selectInput(inputId = "subevent_pass", label = "look for",
                      choices = c("summary","shot assist","pass under pressure"))
        )
      }else{
        
      }
    }) #fermeture renderUI sidebar menu
  }) # fermeture observe event stage comp
  
  
# UI reca_team output  ---------------
output$reca_team<-renderUI({
  if ( input$choice_event=="Pass" &
      input$player_choice_1 == "all the team" &
      input$subevent_pass == "summary"){
    fluidPage(
      fluidRow(
        column( width = 4  ,
                plotlyOutput("successful_passes_team",height = "300px")),
        column(width = 4 ,
               plotlyOutput("pass_distance_tendancy_team",height = "300px")),
        column(width = 4,
               plotlyOutput("pass_height_tendancy_team",height = "300px")
        ),
      ),
      fluidRow(
        column(width = 6, 
               plotOutput("pass_origin_team")),
        column ( width = 6,
                 plotOutput("pass_target_team"))
      ),
      fluidRow(
       column( width = 6,
               formattableOutput("pass_minute_reca_n")
               ),
       column( width = 6,
               formattableOutput("pass_minute_reca_recipient_n")
       )
      )
    )

  } else if ( input$choice_event=="Pass" &
             input$player_choice_1 == "all the team" 
             & input$subevent_pass=="shot assist"){
    
    fluidPage(
      fluidRow(
        column( width = 12,align = "center",
                ggiraphOutput("plot_pass_assist_xg")
                )
        
      ),
      div(style = "margin-top: 20px ;",
      fluidRow(
        column(width = 6, align = "center",
               plotOutput("plot_shot_assist_team"),
               plotOutput("plot_shot_assist_team_endzone")
        ),
        column(width = 6, 
               ggiraphOutput("plot_goal_assist_team"),
               plotOutput("plot_type_pass_endzone")
      )))
      
    )
  }  else if ( input$choice_event=="Pass" &
               input$player_choice_1 == "all the team" 
               & input$subevent_pass=="pass under pressure"){
    fluidPage(
      fluidRow(
        column ( width = 12, align = "center",
                 ggiraphOutput("plot_behavior_pressure_team")
                 )),
      div(style = "margin-top: 15px",
      fluidRow(
        column(width = 6, align = "center", 
               plotOutput("plot_5player_pass_pressure"),
               div(style = "margin-top: 15px",
               plotOutput("plot_5player_dribble_pressure"))),
        
        column(width = 6, align = "center", 
               plotOutput("plot_5player_carry_pressure"),
               div(style = "margin-top: 15px",
               plotOutput("plot_5player_dispossessed_pressure")))
      )
    )
    )
    
  }else if (input$player_choice_1 != "all the team" ){
    fluidPage(
      fluidRow(
        column(width = 12,
               valueBoxOutput("player_shot_box",width = 3),
               valueBoxOutput("player_goal_box",width = 3),
               valueBoxOutput("player_pass_box",width = 3),
               valueBoxOutput("player_duel_box",width = 3)
        )),
      fluidRow(
        column(width = 12, 
               valueBoxOutput("player_foul_box",width = 3),
               valueBoxOutput("player_intercep_box",width = 3),
               valueBoxOutput("player_dribble_box",width = 3),
               valueBoxOutput("player_offside_box",width = 3)
               )
      ),
      box(
        title = paste ("Summary for pass", sep =" "), 
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
      fluidRow(
        column( width = 4  ,
                plotlyOutput("successful_passes",height = "250px")),
        column(width = 4 ,
                plotlyOutput("pass_distance_tendancy",height = "250px")),
        column(width = 4,
                plotlyOutput("pass_height_tendancy",height = "250px")
                ),
        ),
      fluidRow(
        
        column(width = 6,
               plotOutput("target_zone")),
        column (width = 6,
                dataTableOutput("pass_pref_player"))
        )),
      box(
        title = paste ("Behavior under pressure", sep =" "), 
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
      fluidRow(
        column(width = 6,align="center",
        plotlyOutput("behavior_under_pressure",height = "300px")),
        column(width = 6,align="center",
        formattableOutput("favoritepass"))
        
       
      ),
      fluidRow(
        column(width = 6,align="center",
               ggiraphOutput("plot_pass_underpressure_player")),
        column(width = 6,align="center",
               plotOutput("plot_pass_underpressure_density"))
        
      )),

    
      box(
        title = paste ("Pass and goal assist", sep =" "), 
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
      fluidRow(
        column(width = 6, align = "center",
               ggiraphOutput("plot_shot_assist_player")
      ),
      column(width = 6, 
             ggiraphOutput("plot_goal_assist_player"),
             )
    )
      ),# fermeture box 
    
    box(
      title = paste ("Defensive metrics", sep =" "), 
      status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 12,
      fluidRow(
        column(width = 6, align = "center",
               ggiraphOutput("duel_player")
        ),
        column(width = 6, align = "center",
               ggiraphOutput("pressure_player")
        )
      )
    )
    
    
    )
}
  else if ( input$choice_event=="Shot"
           & input$match_selection == "Summary all matches" ){
    fluidPage(
      fluidRow(
        column(width = 12,
               valueBoxOutput("n_shot_box",width = 4),
               valueBoxOutput("n_goal_box",width = 4),
               valueBoxOutput("n_goalm_box",width =4)
        )),
      fluidRow(
        column(width = 12,align = "center",
               plotlyOutput("shot_timeline_xg_team"))
      ),
      div(style = "margin-top: 10px ;",
      fluidRow(
        column(width = 6,
               plotlyOutput("plot_defenders_pos")),
        column(width = 6,
               plotOutput("plot_shot_location_zone"))
      )),
      div(style = "margin-top: 10px ;",
          fluidRow(
            column(width = 6,
                   plotOutput("plot_shot_ratio"))
          ))
      
    )
  }
    else if ( input$choice_event=="Shot"
             & input$match_selection != "Summary all matches" ){
      fluidPage(
        fluidRow(
          column( width = 12,
                  div(style = "text-align:center; margin-top: 10px ; font-size: 30px;
                      border-style: solid; border-color: black;background-color: white;",
                      textOutput("shot_title_page"), style="color:black"))
        ),
        fluidRow(
          column(width = 12,align = "center",
                 div(style = "text-align:center; margin-top: 20px ; font-size: 30px;",
                 plotlyOutput("shot_timeline_xg_team")))
        ),
        fluidRow(
          column(width = 6,
                 div(style = "margin-top: 10px ;",
                 plotlyOutput("plot_defenders_pos"))),
          column(width = 6,
                 div(style = "margin-top: 10px ;",
                 plotOutput("plot_goalmap_selection")))
        ),
        
        
      )
    }
  else if ( input$choice_event=="Duel"){
    
    fluidPage(
      fluidRow(
        column(width = 6, align = "center",
               plotlyOutput("plot_reca_duel",height = "250px")),
        column(width = 6,align = "center",
               plotlyOutput("plot_distrib_duel",height = "250px")),
      ),
      fluidRow(
        column( width = 6, align = "center", 
                formattableOutput("duel_player_won")),
        column( width = 6, align = "center", 
                formattableOutput("duel_player_lost"))
        ),
      fluidRow(
        column(width = 6, align = "center",
               plotOutput("plot_duel_density")),
        column( width = 3,
                div(style = "text-align:center; margin-top: 80px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                    textOutput("duel_first_tiers"), style="color:black"),
                div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #E68613;",
                    textOutput("duel_second_tiers"), style="color:black"),
                div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #CD9600;",
                    textOutput("duel_last_tiers"), style="color:black")
        ),
        column( width = 3,
                div(style = "text-align:center; margin-top: 80px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                    textOutput("duel_first_tiers_n"), style="color:black"),
                div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #E68613;",
                    textOutput("duel_second_tiers_n"), style="color:black"),
                div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #CD9600;",
                    textOutput("duel_last_tiers_n"), style="color:black")
        )
      )
    )
    
  } else if ( input$choice_event=="Defensive metrics"){
    fluidPage(
      box(
        title = paste ("Foul committed",sep =" "), 
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
      fluidRow(
        column (width = 6, align = "center", 
                ggiraphOutput("plot_foul_committed_team")),
        column (width = 6, align = "center", 
                formattableOutput("table_foul_committed_team"))
      )),
      box(
        title = paste ("Interception",sep =" "), 
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
      fluidRow(
        column (width = 6, align = "center", 
                ggiraphOutput("plot_interception_team")),
        column (width = 6, align = "center", 
                formattableOutput("table_interception_team"))
      )),
      box(
        title = paste ("Pressure",sep =" "), 
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        fluidRow(
          column (width = 6, align = "center", 
                  ggiraphOutput("plot_pressure_team")),
          column (width = 6, align = "center", 
                  formattableOutput("table_pressure_team"))
        ))
    )
  }else if ( input$choice_event=="Dashboard after match"
             & input$look_penalty == "Non"){
    fluidPage(
          
fluidRow(
  style = sprintf(backgroundImageCSS,  "https://st3.depositphotos.com/9999814/13134/i/600/depositphotos_131341642-stock-photo-soccer-football-net-background.jpg"),
  
      column(width = 2,
                      align = "center",
                      div(style = "margin-top: 35px",
                          imageOutput("logo_t1"))),
      column( width = 3,
                div(style = "text-align:right; margin-top: 10px ; font-size: 30px;",
                    textOutput("home_team"), style="color:black"),
              div(style = "margin-top: 20px ;
                  border-style: solid; border-color: black;background-color: white;",
              formattableOutput("buteur_match_TH"))
),
        column(width =2,
               div(style = "text-align:center; margin-top: 10px ; font-size: 30px;
            border-style: solid; border-color: black;background-color: #ABBAEA;",
                   textOutput("score_home_team"), style="color:white"),
               div(style = "text-align:center; margin-top: 5px ; font-size: 15px;
            border-style: solid; border-color: black;background-color: #ABBAEA;",
                   textOutput("score_penalty"), style="color:white")),
        column( width = 3,
                div(style = "text-align:left; margin-top: 10px ; font-size: 30px;",
                    textOutput("away_team"), style="color:black"),
                div(style = "margin-top: 20px ;
                    border-style: solid; border-color: black;background-color: white;",
                formattableOutput("buteur_match_TA"))
                ),
      column(width = 2,
             align = "center",
             div(style = "margin-top: 35px",
                 imageOutput("logo_t2")))
  ),
fluidRow(
  box(
    title = paste ("Starting XI",sep =" "), 
    status = "primary", solidHeader = TRUE,
    collapsible = TRUE, width = 4,
    div(style = "text-align:center; margin-top: -25px;",
         formattableOutput("startingXI_TH"))),
      column(width = 4, 
             fluidRow(
               column( width = 3,
                       div(style = "text-align:center; margin-top: 30px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("possession_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("pass_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("shot_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("duel_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("interception_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("foul_TH"), style="color:black")
               ),
               column( width = 6,
                       div(style = "text-align:center; margin-top: 30px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("possession"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("pass_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("shot_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("duel_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("interception_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("foul_per"), style="color:black")
               ),
               column( width = 3,
                       div(style = "text-align:center; margin-top: 30px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("possession_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("pass_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("shot_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("duel_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("interception_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("foul_TA"), style="color:black")
               )
               
             )
             
             ),
  div(style = "text-align:center; margin-top: -65px;",
  box(
    title = paste ("Starting XI",sep =" "), 
    status = "primary", solidHeader = TRUE,
    collapsible = TRUE, width = 4,
    formattableOutput("startingXI_TA")))
      ),
      fluidRow(
        column (width = 6, align = "center",
                plotOutput("team_shot_target")),
        column( width = 6 , 
                ggiraphOutput("plot_shot_match")),
        ),
fluidRow(
        column ( width = 6 ,
        ggiraphOutput("plot_duel_match")),
        column(width = 6, 
               plotOutput("plot_pass_third_match"))
      )
    )
    
    
    
    
  } else if ( input$choice_event=="Dashboard after match"
             & input$look_penalty != "Oui" || is.null(input$look_penalty)){
    fluidPage(
          
fluidRow(
  style = sprintf(backgroundImageCSS,  "https://st3.depositphotos.com/9999814/13134/i/600/depositphotos_131341642-stock-photo-soccer-football-net-background.jpg"),
  
      column(width = 2,
                      align = "center",
                      div(style = "margin-top: 35px",
                          imageOutput("logo_t1"))),
      column( width = 3,
                div(style = "text-align:right; margin-top: 10px ; font-size: 30px;",
                    textOutput("home_team"), style="color:black"),
              div(style = "margin-top: 20px ;
                  border-style: solid; border-color: black;background-color: white;",
              formattableOutput("buteur_match_TH"))
),
        column(width =2,
               div(style = "text-align:center; margin-top: 10px ; font-size: 30px;
            border-style: solid; border-color: black;background-color: #ABBAEA;",
                   textOutput("score_home_team"), style="color:white"),
               div(style = "text-align:center; margin-top: 5px ; font-size: 15px;
            border-style: solid; border-color: black;background-color: #ABBAEA;",
                   textOutput("score_penalty"), style="color:white")),
        column( width = 3,
                div(style = "text-align:left; margin-top: 10px ; font-size: 30px;",
                    textOutput("away_team"), style="color:black"),
                div(style = "margin-top: 20px ;
                    border-style: solid; border-color: black;background-color: white;",
                formattableOutput("buteur_match_TA"))
                ),
      column(width = 2,
             align = "center",
             div(style = "margin-top: 35px",
                 imageOutput("logo_t2")))
  ),
fluidRow(
  box(
    title = paste ("Starting XI",sep =" "), 
    status = "primary", solidHeader = TRUE,
    collapsible = TRUE, width = 4,

         formattableOutput("startingXI_TH")),
      column(width = 4, 
             fluidRow(
               column( width = 3,
                       div(style = "text-align:center; margin-top: 30px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("possession_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("pass_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("shot_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("duel_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("interception_TH"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #F8766D;",
                           textOutput("foul_TH"), style="color:black")
               ),
               column( width = 6,
                       div(style = "text-align:center; margin-top: 30px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("possession"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("pass_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("shot_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("duel_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("interception_per"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: white;",
                           textOutput("foul_per"), style="color:black")
               ),
               column( width = 3,
                       div(style = "text-align:center; margin-top: 30px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("possession_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("pass_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("shot_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("duel_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("interception_TA"), style="color:black"),
                       div(style = "text-align:center; margin-top: 10px ; font-size: 20px;
               border-style: solid; border-color: black;background-color: #00BFC4;",
                           textOutput("foul_TA"), style="color:black")
               )
               
             )
             
             ),
  box(
    title = paste ("Starting XI",sep =" "), 
    status = "primary", solidHeader = TRUE,
    collapsible = TRUE, width = 4,
    
    formattableOutput("startingXI_TA"))
      ),
      fluidRow(
        column (width = 6, align = "center",
                plotOutput("team_shot_target")),
        column( width = 6 , 
                ggiraphOutput("plot_shot_match")),
        ),
fluidRow(
        column ( width = 6 ,
        ggiraphOutput("plot_duel_match")),
        column(width = 6, 
               plotOutput("plot_pass_third_match"))
      )
    )
    
    
    
    
  }
  else if (input$choice_event=="Dashboard after match"
             & input$look_penalty == "Oui"){
    fluidPage(
      fluidRow(
        column (width = 4,align = "center",
               div(style = "margin-top: 5px",
                   imageOutput("logo_t1"))
        ),
        column (width = 4, align = "center",
                div(style = "text-align:center; margin-top: 25px ; font-size: 15px;
            border-style: solid; border-color: black;background-color: #ABBAEA;",
                    textOutput("score_penalty"), style="color:white")),
        column (width = 4, align = "center", 
                div(style = "margin-top: 5px",
                    imageOutput("logo_t2"))
                )
        
      ),
      fluidRow(
      column (width = 6,align = "center",
              div(style = "margin-top: -225px",
                  formattableOutput("player_penalty_TH")),
              div(style = "margin-top: 5px",
    plotOutput("penalty_TH"))
      ),
    column(width = 6, align = "center",
           div(style = "margin-top: -225px",
               formattableOutput("player_penalty_TA")),
           div(style = "margin-top: 5px",
               plotOutput("penalty_TA"))
           )
      )
    )
  }
  
  else{
    
  }
})

# ui output select input stage competition ---------------
observeEvent(input$choice_team,{
  output$side_bar_choice_stage<-renderUI({
  data<-Matches %>% 
    filter(competition_stage.name %in% input$choice_stage) %>% 
    select(match_id,home_team.country.name,away_team.country.name,
           home_score, away_score) %>% 
    unite("Merged",home_team.country.name:away_team.country.name,
          sep= " / ", remove = FALSE)
  
  selectInput(inputId = "match_choice", label = "look the match between",
              choices = levels(as.factor(data$Merged)))
})
})

# ui output slider input  ---------
  output$slider_minute<-renderUI({
  
    if (input$match_selection != "Summary all matches"){
    data<-Matches %>% 
      filter(home_team.home_team_name %in% input$choice_team |
               away_team.away_team_name %in% input$choice_team) %>% 
      select(match_id,home_team.country.name,away_team.country.name,
             home_score, away_score) %>% 
      unite("Merged",home_team.country.name:away_team.country.name,
            sep= " / ", remove = FALSE)
    
    data<-data %>% 
      filter(Merged %in% input$match_selection)
    
    
    minute_select<-events %>% 
      filter(match_id %in% data$match_id)
    
    max_minute<-max(minute_select$minute)

    sliderInput("slider_min",
              "Select a range:",
              min = 0,
              max = max_minute,
              step=5,
              value = c(0, 90))
    }else{

      sliderInput("slider_min",
                  "Select a range:",
                  min = 0,
                  max = 95,
                  step=5,
                  value = c(0, 90))
      
    }
  })
# ui output seance penalty  -------------------
output$seance_penalty<-renderUI({
  
  data<-Matches %>% 
    filter(home_team.home_team_name %in% input$choice_team |
             away_team.away_team_name %in% input$choice_team) %>% 
    select(match_id,home_team.country.name,away_team.country.name,
           home_score, away_score) %>% 
    unite("Merged",home_team.country.name:away_team.country.name,
          sep= " / ", remove = FALSE)
  
  data<-data %>% 
    filter(Merged %in% input$match_selection)   
  
  
  shot<-events %>% 
    filter(match_id %in% data$match_id)       
  
  if(input$match_selection != "Summary all matches" &
      input$choice_event=="Dashboard after match" &
     length(levels(as.factor(shot$period)))>4){
  
  
    selectInput(inputId = "look_penalty", label = "look the penalty",
                choices = c("Non","Oui"))
  }else{
    
  }
  
  
})
# ui output select match for team dashboard  -----------
  
  
  output$select_match<-renderUI({
    data<-Matches %>% 
      filter(home_team.home_team_name %in% input$choice_team |
               away_team.away_team_name %in% input$choice_team) %>% 
      select(match_id,home_team.country.name,away_team.country.name,
             home_score, away_score) %>% 
      unite("Merged",home_team.country.name:away_team.country.name,
            sep= " / ", remove = FALSE)
    sidebarMenu(
      
    selectInput(inputId = "match_selection", label = "Pick the match between",
                choices = c("Summary all matches",levels(as.factor(data$Merged))))
    )
  })
  
  
# textoutput pass dashboard team ---------
  
  output$pass_dashboard_team<-renderText({
    paste ( "Pass dashboard - ", input$choice_team, sep = "")
  })
  
# Uioutput select player  ---------------
  output$select_player<-renderUI({
    sidebarMenu(
      selectInput(inputId = "player_choice_1", label = "Pick a player",
                  choices = c("all the team",levels(as.factor(events$player.name [events$team.name == input$choice_team]) )))
    )
  })
  
  
# Observe event player_choice_1  --------------
  observeEvent(input$player_choice_1,{
    if (input$player_choice_1 == "all the team" ){
      # Ouverture observe event match selection   ---------
      observeEvent(input$match_selection, {
        if (input$match_selection == "Summary all matches"){
          # Output pass origin team plot  ------------
          
          output$pass_origin_team<-renderPlot({
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & !(pass.outcome.name %in% "Unknown") &
                       minute %in% (input$slider_min[1]:input$slider_min[2]))
            
            
            ggplot(pass_minute) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(aes(location.x,location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              theme(legend.position = "none") + 
              labs(title = paste("Passes origin zone",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
          })
          
          # Output pass target team plot  ------------
          
          output$pass_target_team<-renderPlot({
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & !(pass.outcome.name %in% "Unknown") &
                       minute %in% (input$slider_min[1]:input$slider_min[2]))
            
            
            ggplot(pass_minute) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(aes(pass.end_location.x,pass.end_location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              theme(legend.position = "none") + 
              labs(title = paste("Passes favorite target zone",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
          })
          
          
          # Pass reca n team --------------
          output$pass_minute_reca_n<-renderFormattable({
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & !(pass.outcome.name %in% "Unknown" )
                     & minute %in% (input$slider_min[1]:input$slider_min[2]))%>%
              group_by (player.name) %>% 
              summarise(n=n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              arrange(desc(n))
            
            
            pass_minute %>% 
              rename("Player" = player.name,
                     "Number of pass" = n,
                     "Percentage" = per) %>% 
              formattable(align = c("c","c","r"),
                          list(
                            per = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))
                          ))
            
          })
          # Pass reca n team by recipient player --------------
          output$pass_minute_reca_recipient_n<-renderFormattable({
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & !(pass.outcome.name %in% "Unknown" )
                     & minute %in% (input$slider_min[1]:input$slider_min[2]))%>%
              group_by (pass.recipient.name) %>% 
              summarise(n=n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              arrange(desc(n)) %>% 
              na.omit()
            
            
            pass_minute %>% 
              rename("Player who receive the ball" = pass.recipient.name,
                     "Number of pass" = n,
                     "Percentage" = per) %>% 
              formattable(align = c("c","c","r"),
                          list(
                            per = color_bar( "lightblue"),
                            pass.recipient.name = formatter("span", 
                                                       style = ~ style(color = "black",font.weight = "bold"))
                          ))
            
          })
          
          # pass distance tendancy pie  ----------
          
          output$pass_distance_tendancy_team<-renderPlotly({
            pass_length<-events %>% 
              filter(type.name %in% "Pass" &
                       team.name %in% input$choice_team 
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(category=cut(pass.length*0.9144, breaks=c(0, 15, 30,100), 
                                  labels=c("short","medium","long"))) %>% 
              group_by(category) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100)
            
            
            p <- plot_ly() %>%
              add_pie(data = pass_length, labels = ~paste(category,"pass",sep = " "), values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = 'Pass distance tendancy (m)',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)")
            p
            
          })
          
          
          # pass height tendancy pie  ----------
          
          output$pass_height_tendancy_team<-renderPlotly({
            pass_height<-events %>% 
              filter(type.name %in% "Pass" &
                       team.name %in% input$choice_team &
                       minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(pass.height.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100)
            
            
            p <- plot_ly() %>%
              add_pie(data = pass_height, labels = ~pass.height.name, values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = 'Pass height tendancy',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)")
            p
            
          })
          
          # Plot successful passes  -------------------
          output$successful_passes_team<-renderPlotly({
            pass_completion<-events %>% 
              filter(type.name %in% "Pass" &
                       team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>%
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(pass.outcome.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100))
            
            p <- plot_ly() %>%
              add_pie(data = pass_completion, labels = ~paste(pass.outcome.name,"pass",sep = " "), values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = "Pass distribution",
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)",
                     annotations=list(text=paste(paste(pass_completion$per[1], " %", sep=""), sep =" "),
                                                "showarrow"=F, font=list(size = 20)))
            
            p
            
            
          })
          
          
          # Pass team shot assist  -------------
          output$plot_shot_assist_team<-renderPlot({
            pass_event<-events %>% 
              filter(type.name %in% "Pass" 
                     & team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.shot_assist %in% "TRUE") %>% 
              mutate(pass_location = case_when(
                location.x <= 60 ~ "First half",
                location.x >60 & location.x < 80 & location.y > 0 & location.y < 30 ~ "zone 10",
                location.x >60 & location.x < 80 & location.y >= 30 & location.y < 50 ~ "zone 11",
                location.x >60 & location.x < 80 & location.y >= 50  ~ "zone 12",
                location.x >=80 & location.x < 100 & location.y >= 0 & location.y < 30 ~ "zone 13",
                location.x >=80 & location.x < 100 & location.y >= 30 & location.y < 50 ~ "zone 14",
                location.x >=80 & location.x < 100 & location.y >= 50 ~ "zone 15",
                location.x >=100 & location.y >= 0 & location.y < 30  ~ "zone 16",
                location.x >=100  & location.y >= 30 & location.y < 50 ~ "zone 17",
                location.x >=100  & location.y >= 50  ~ "zone 18")) %>% 
              group_by(team.name, pass_location) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            n1 <- paste(round(pass_event$per[pass_event$pass_location == "zone 12" ],1),sep = "%")
            if (length(n1) == "0"){n1<-"0"}
            n2 <- paste(round(pass_event$per[pass_event$pass_location == "zone 11" ],1),sep = "%")
            if (length(n2) == "0"){n2<-"0"}
            
            n3 <- paste(round(pass_event$per[pass_event$pass_location == "zone 10" ],1),sep = "%")
            if (length(n3) == "0"){n3<-"0"}
            
            n4 <- paste(round(pass_event$per[pass_event$pass_location == "zone 15" ],1),sep = "%")
            if (length(n4) == "0"){n4<-"0"}
            
            n5 <- paste(round(pass_event$per[pass_event$pass_location == "zone 14" ],1),sep = "%")
            if (length(n5) == "0"){n5<-"0"}
            
            n6 <- paste(round(pass_event$per[pass_event$pass_location == "zone 13" ],1),sep = "%")
            if (length(n6) == "0"){n6<-"0"}
            
            n7 <- paste(round(pass_event$per[pass_event$pass_location == "zone 18" ],1),sep = "%")
            if (length(n7) == "0"){n7<-"0"}
            
            n8 <- paste(round(pass_event$per[pass_event$pass_location == "zone 17" ],1),sep = "%")
            if (length(n8) == "0"){n8<-"0"}
            
            n9 <- paste(round(pass_event$per[pass_event$pass_location == "zone 16" ],1),sep = "%")
            if (length(n9) == "0"){n9<-"0"}
            
            
            
            
            ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_vline(data = match_event,aes(xintercept = 40)) +
              geom_vline(data = match_event,aes(xintercept = 80))  +
              geom_vline(data = match_event,aes(xintercept = 60)) +
              geom_vline(data = match_event,aes(xintercept = 100)) +
              geom_hline(data = match_event,aes(yintercept = 30)) +
              geom_hline(data = match_event,aes(yintercept = 50)) +
              geom_circle(aes(x0=70, y0=15, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=70, y0=40, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=70, y0=65, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=15, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=90, y0=40, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=65, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=15, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=110, y0=40, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=65, r=6), 
                          color='black',  fill='green', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_text(data = pass_event, x = 70 , y = 15 , 
                        label = n1, size = 4 )+
              geom_text(data = pass_event, x = 70 , y = 40 , 
                        label = n2, size = 4 )+
              geom_text(data = pass_event, x = 70 , y = 65 , 
                        label = n3, size = 4 )+
              geom_text(data = pass_event, x = 90 , y = 15 , 
                        label = n4, size = 4 )+
              geom_text(data = pass_event, x = 90 , y = 40 , 
                        label = n5, size = 4 )+
              geom_text(data = pass_event, x = 90 , y = 65 , 
                        label = n6, size = 4 )+
              geom_text(data = pass_event, x = 110 , y = 15 , 
                        label = n7, size = 4 )+
              geom_text(data = pass_event, x = 110 , y = 40 , 
                        label = n8, size = 4 )+
              geom_text(data = pass_event, x = 110 , y = 65 , 
                        label = n9, size = 4 )+
              labs(
                title = "Origin of pass preceeding shot (%)"
              ) +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5)
              )
          })
          # Pass team shot assist endzone  -------------
          output$plot_shot_assist_team_endzone<-renderPlot({
            pass_event<-events %>% 
              filter(type.name %in% "Pass" 
                     & team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.shot_assist %in% "TRUE") %>% 
              mutate(pass_location = case_when(
                pass.end_location.x <= 60 ~ "First half",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y > 0 & pass.end_location.y < 30 ~ "zone 10",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 11",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 50  ~ "zone 12",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 0 & pass.end_location.y < 30 ~ "zone 13",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 14",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 50 ~ "zone 15",
                pass.end_location.x >=100 & pass.end_location.y >= 0 & pass.end_location.y < 30  ~ "zone 16",
                pass.end_location.x >=100  & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 17",
                pass.end_location.x >=100  & pass.end_location.y >= 50  ~ "zone 18"))%>% 
              group_by(team.name, pass_location) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            n1 <- paste(round(pass_event$per[pass_event$pass_location == "zone 12" ],1),sep = "%")
            if (length(n1) == "0"){n1<-"0"}
            n2 <- paste(round(pass_event$per[pass_event$pass_location == "zone 11" ],1),sep = "%")
            if (length(n2) == "0"){n2<-"0"}
            
            n3 <- paste(round(pass_event$per[pass_event$pass_location == "zone 10" ],1),sep = "%")
            if (length(n3) == "0"){n3<-"0"}
            
            n4 <- paste(round(pass_event$per[pass_event$pass_location == "zone 15" ],1),sep = "%")
            if (length(n4) == "0"){n4<-"0"}
            
            n5 <- paste(round(pass_event$per[pass_event$pass_location == "zone 14" ],1),sep = "%")
            if (length(n5) == "0"){n5<-"0"}
            
            n6 <- paste(round(pass_event$per[pass_event$pass_location == "zone 13" ],1),sep = "%")
            if (length(n6) == "0"){n6<-"0"}
            
            n7 <- paste(round(pass_event$per[pass_event$pass_location == "zone 18" ],1),sep = "%")
            if (length(n7) == "0"){n7<-"0"}
            
            n8 <- paste(round(pass_event$per[pass_event$pass_location == "zone 17" ],1),sep = "%")
            if (length(n8) == "0"){n8<-"0"}
            
            n9 <- paste(round(pass_event$per[pass_event$pass_location == "zone 16" ],1),sep = "%")
            if (length(n9) == "0"){n9<-"0"}
            
            
            
            
            ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_vline(data = match_event,aes(xintercept = 40)) +
              geom_vline(data = match_event,aes(xintercept = 80))  +
              geom_vline(data = match_event,aes(xintercept = 60)) +
              geom_vline(data = match_event,aes(xintercept = 100)) +
              geom_hline(data = match_event,aes(yintercept = 30)) +
              geom_hline(data = match_event,aes(yintercept = 50)) +
              geom_circle(aes(x0=70, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=70, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=70, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=90, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=110, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_text(data = pass_event, x = 70 , y = 15 , 
                        label = n1, size = 4 )+
              annotate("text", label = paste0("Zone 12"), x=70, y = 7, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 70 , y = 40 , 
                        label = n2, size = 4 )+
              annotate("text", label = paste0("Zone 11"), x=70, y = 32, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 70 , y = 65 , 
                        label = n3, size = 4 )+  
              annotate("text", label = paste0("Zone 10"), x=70, y = 58, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 90 , y = 15 , 
                        label = n4, size = 4 )+
              annotate("text", label = paste0("Zone 15"), x=90, y = 7, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 90 , y = 40 , 
                        label = n5, size = 4 )+
              annotate("text", label = paste0("Zone 14"), x=90, y = 32, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 90 , y = 65 , 
                        label = n6, size = 4 )+
              annotate("text", label = paste0("Zone 13"), x=90, y = 58, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 110 , y = 15 , 
                        label = n7, size = 4 )+
              annotate("text", label = paste0("Zone 18"), x=110, y = 7, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 110 , y = 40 , 
                        label = n8, size = 4 )+
              annotate("text", label = paste0("Zone 17"), x=110, y = 32, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 110 , y = 65 , 
                        label = n9, size = 4 )+
              annotate("text", label = paste0("Zone 16"), x=110, y = 58, size = 3, colour = "white")+
              labs(
                title = "Endzone of pass preceeding shot (%)"
              ) +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5)
              )
          })
          
          # Plot goal assist team ---------------

          output$plot_goal_assist_team<-renderGirafe({
            pass<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.goal_assist %in% TRUE)
            
            
            plot<-ggplot(pass) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                           yend = pass.end_location.y,
                                           tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                           "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")),
                                       colour = "#FFA500",
                                       arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                       ))+
              scale_colour_identity()+
              theme(legend.position = "none") + 
              labs(title = paste("Goal assist direction during the competition",sep = " "),
                   caption = "Data Source: StatsBomb") +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5)
              )
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
          })
          

          # plot Pass team shot assist endzone and type of pass  ----------
          output$plot_type_pass_endzone<-renderPlot({
            pass_event<-events %>% 
              filter(type.name %in% "Pass" 
                     & team.name %in% input$choice_team
                     & pass.shot_assist %in% "TRUE"
              ) %>% 
              mutate(pass_location = case_when(
                pass.end_location.x <= 60 ~ "First half",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y > 0 & location.y < 30 ~ "zone 10",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 11",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 50  ~ "zone 12",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 0 & pass.end_location.y < 30 ~ "zone 13",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 14",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 50 ~ "zone 15",
                pass.end_location.x >=100 & pass.end_location.y >= 0 & pass.end_location.y < 30  ~ "zone 16",
                pass.end_location.x >=100  & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 17",
                pass.end_location.x >=100  & pass.end_location.y >= 50  ~ "zone 18")) %>% 
              group_by(team.name, pass_location, 
                       pass.type.name,
              ) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            ggplot(pass_event) +
              aes(x = pass_location, y = n, fill = pass.type.name) +
              geom_col() +
              scale_fill_hue(direction = 1) +
              labs(
                x = "Pass endzone location",
                y = "number of pass",
                title = "Type of pass according to end zone",
                fill = "Type of pass"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5),
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
          })
          # plot Pass assist and xg asociated  -------------
          output$plot_pass_assist_xg<-renderGirafe({
            
            pass<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & pass.shot_assist %in% TRUE
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(player.name, minute,match_id,pass.recipient.name) %>% 
              summarise(n = n())
            
            shot_player_xg<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,match_id,shot.statsbomb_xg) %>% 
              summarise(n = n()) 
            
            pass_goal<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & pass.goal_assist %in% TRUE
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(player.name, minute,match_id,pass.recipient.name) %>% 
              summarise(n = n())
            
            
            goal_player_xg<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & shot.outcome.name %in% "Goal"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,match_id,shot.statsbomb_xg) %>% 
              summarise(n = n()) 
            
            
            data_shot<-left_join(pass,shot_player_xg, by = c("minute","match_id")) %>% 
              mutate (shot_finality = "no goal")
            data_goal<-left_join(pass_goal,goal_player_xg, by = c("minute","match_id")) %>% 
              mutate(shot_finality = "goal")
            
            data<-bind_rows(data_shot,data_goal)
            
            
            plot<-ggplot(data) +
              aes(x = player.name, y = minute, colour = shot_finality, size = shot.statsbomb_xg,
                  tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                  "\n",paste("Value of xG : ",shot.statsbomb_xg,sep = ""),
                                  "\n",paste("Minute : ",minute, sep = ""),
                                  sep="")) +
              geom_point_interactive(shape = "circle") +
              scale_color_manual(values = c(goal = "#0FCF18", `no goal` = "#DF0505")) +
              labs(x = "Player", y = "Minute when pass happened", 
                   title = "Pass assist according to time and xg associated", color = "Shot finality", size = "xG value") +
              coord_flip() +
              theme_minimal() +
              theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5), 
                    axis.title.y = element_text(size = 12L, face = "bold"), axis.title.x = element_text(size = 12L, face = "bold"))
            
           
           girafe(ggobj=plot,height_svg = 4,
                  options=list(opts_zoom(min = .7, max = 5),
                               opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
           
            
          })
          
          
          # Valuebox n shot team   -------
          output$n_shot_box <- renderValueBox({
            shot_n<-events %>% 
              filter(type.name %in% "Shot" & team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              summarise(shot = sum(type.name=="Shot", na.rm = 
                                     TRUE),
                        goal = sum(shot.outcome.name=="Goal", na.rm = 
                                     TRUE),
                        goal.game = sum(shot.outcome.name=="Goal", na.rm = 
                                          TRUE)/n_distinct(match_id)) %>% 
              arrange(desc(shot)) 
            
            
            
            valueBox(paste(round(shot_n$shot,2),sep = ""),
                     subtitle = "shots during the WC", color = "navy",
                     icon = NULL)
            
          })
          
          
          
          
          
          # Valuebox n goal team   -------
          output$n_goal_box <- renderValueBox({
            shot_n<-events %>% 
              filter(type.name %in% "Shot" & team.name==input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              summarise(shot = sum(type.name=="Shot", na.rm = 
                                     TRUE),
                        goal = sum(shot.outcome.name=="Goal", na.rm = 
                                     TRUE),
                        goal.game = sum(shot.outcome.name=="Goal", na.rm = 
                                          TRUE)/n_distinct(match_id)) %>% 
              arrange(desc(shot)) 
            
            
            
            valueBox(paste(round(shot_n$goal,2),sep = ""),
                     subtitle = "goals during the WC", color = "navy",
                     icon = NULL)
            
          })
          
          
          
          
          
          
          
          # Valuebox n goal/m team   -------
          output$n_goalm_box <- renderValueBox({
            shot_n<-events %>% 
              filter(type.name %in% "Shot" & team.name==input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              summarise(shot = sum(type.name=="Shot", na.rm = 
                                     TRUE),
                        goal = sum(shot.outcome.name=="Goal", na.rm = 
                                     TRUE),
                        goal.game = sum(shot.outcome.name=="Goal", na.rm = 
                                          TRUE)/n_distinct(match_id)) %>% 
              arrange(desc(shot)) 
            
            
            
            valueBox(paste(round(shot_n$goal.game,2),sep = ""),
                     subtitle = "goals/match during the WC", color = "navy",
                     icon = NULL)
            
          })
          
          
          
          
          
          
          
          
          # Plot shot_timeline_xg_team ---------------
          output$shot_timeline_xg_team<-renderPlotly({
            
            
            
            shot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     ) %>% 
              group_by(player.name, minute,
                       shot.statsbomb_xg,
                       shot.outcome.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))
            
            
            
            
            fig<-plot_ly(source = "plot", data = shot, y = ~player.name,
                         x = ~minute, type = "scatter" ,
                         color = ~shot.outcome.name, size = ~shot.statsbomb_xg,
                         text = ~shot.statsbomb_xg)%>%
              layout(xaxis = list(title = "minute of the match", titlefont = "Courier New, monospace"), 
                     yaxis = list(title = " ", titlefont = "Courier New, monospace"))
            
            fig 
            
            
          })
          
          
          # Plot defenders position when event shot  ---------
          
          output$plot_defenders_pos<-renderPlotly({
            
            event.data <-event_data("plotly_click", source = "plot")
            
            # Obtain the clicked x/y variables and fit linear model
            point_number <- event.data$pointNumber
            
            
            shot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     ) %>% 
              group_by(player.name, minute,
                       shot.statsbomb_xg,
                       shot.outcome.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))
            
            shot<-shot[point_number,]
            
            
            
            # Obtain the clicked x/y variables and fit linear model
            
            
            shot_position_defender<-events %>% 
              filter(team.name==input$choice_team & type.name %in% "Shot"
                     & shot.statsbomb_xg %in% shot$shot.statsbomb_xg 
                     & player.name %in% shot$player.name) 
            
            x<-shot_position_defender$shot.freeze_frame
            
            x<-as.data.frame(x)%>% 
              unnest_wider(location,names_sep="_") 
            
            plot2<-ggplot(x) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              coord_flip(xlim = c(55, 120),
                         ylim = c(-12, 105))+
              geom_point(aes(x = location_1, y = location_2,
                             colour = teammate
              )) + 
              scale_colour_manual(values = c("#ff4444","#5e9a78"),labels = c("No-Goal","Goal"),
                                  name = "Team") +
              labs(title = paste("Defenders position for the selected shot ",sep = " "))+
              theme(plot.title = element_text(hjust = 0.5))+
              geom_segment(data = shot_position_defender,
                           aes(x = location.x, y = location.y, xend = shot.end_location.x, 
                               yend = shot.end_location.y),
                           colour = "green",
                           arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                           ))
            
            ggplotly(plot2)
            
            
          })
          
          
          
          # Plot goalmap when shot selected  ------------
          output$plot_goalmap_selection<-renderPlot({
            
            event.data <-event_data("plotly_click", source = "plot")
            
            # Obtain the clicked x/y variables and fit linear model
            point_number <- event.data$pointNumber
            
            
              shot<-events %>% 
              filter(team.name %in% input$choice_team 
                     & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     ) %>% 
              group_by(player.name, minute,
                       shot.statsbomb_xg,
                       shot.outcome.name) %>% 
              summarise(n = n()) %>% 
                arrange(desc(minute))
            
            shot<-shot[point_number,]
            
            shot_selected<-events %>% 
              filter(team.name== input$choice_team & type.name %in% "Shot" 
                     & shot.statsbomb_xg %in% shot$shot.statsbomb_xg 
                     & player.name %in% shot$player.name)%>% 
              select(-contains("freeze"))%>% 
              mutate_if(is.factor, as.character) %>% 
              mutate_at(vars(c(location.x, location.y, shot.end_location.x, shot.end_location.y, 
                               shot.end_location.z, shot.statsbomb_xg)), list(as.numeric))
            shot_selected<-shot_selected %>% 
              filter(shot.outcome.name %in% c('Goal','Post', 'Off T', 'Saved', 
                                              'Saved to Post','Saved Off Target')) %>% 
              mutate(goal.seq = 1:length(shot.outcome.name))
            
            post(fill_background = "white")+
              geom_point(shot_selected, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, 
                                                      color = shot.outcome.name, shape = shot.type.name), size = 5)+
              geom_text(shot_selected, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, label = goal.seq),
                        size = 3, color = "darkslategray")+
              theme(
                legend.position = "right",
                plot.title = element_text(hjust=0.5, vjust = -5, size = 15),
                plot.subtitle =  element_text(hjust=0.5, vjust = -5),
                text = element_text(color = "black")
              )+
              labs(color = "Shot Outcome", title = input$match_selection, shape = NULL,
              )+
              scale_color_manual(values = c("darkgreen", "red", "blue", "yellow", "black", "orange"))+
              scale_shape_manual(values = c(16, 15, 17,25))
            
            
          })
          # Plot reca duel  -------------
          output$plot_reca_duel<-renderPlotly({
            duel<-events %>% 
              filter(team.name %in% input$choice_team 
                     & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              group_by(duel_WL) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              na.omit()
            
            
            p <- plot_ly() %>%
              add_pie(data = duel, labels = ~ duel_WL, values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE, marker = list(colors = c('red', 'green'))) %>% 
              layout(title = 'Duel distribution',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)",
                     annotations=list(text=paste(paste(sum(duel$n), " duels", sep=""), sep =" "),
                                      "showarrow"=F, font=list(size = 20)))
            
            p
          })
          
          
          # Plot distrib duel  ----------
          output$plot_distrib_duel<-renderPlotly({
            duel_table<-events %>% 
              filter(team.name %in% input$choice_team 
                     & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(duel.outcome.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              na.omit() %>% 
              arrange(desc(per))
            
            
            p <- plot_ly() %>%
              add_pie(data = duel_table, labels = ~ duel.outcome.name, values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = 'Duel type',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)")
                     
            
          })
          # Plot density duel  ---------------
          output$plot_duel_density<-renderPlot({
            duel_plot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) 
            
            ggplot(duel_plot) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(aes(location.x, location.y, fill = after_stat(level)), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              theme(legend.position = "none") + 
              labs(title = paste("Duel zone",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
          })
          # Plot duel location  -------------
          output$plot_duel_location<-renderGirafe({
            duel_plot_loc<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(position_simple = case_when(
                position.name == "Right Back" ~ "Defender",
                position.name == "Right Center Back" ~ "Defender",
                position.name == "Left Back" ~ "Defender",
                position.name == "Left Center Back" ~ "Defender",
                position.name == "Left Center Midfield" ~ "Midfielder",
                position.name == "Left Defensive Midfield" ~ "Midfielder",
                position.name == "Left Midfield" ~ "Midfielder",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Right Center Forward" ~ "Forward",
                position.name == "Right Center Midfield" ~ "Midfielder",
                position.name == "Right Defensive Midfield" ~ "Midfielder",
                position.name == "Right Midfield" ~ "Midfielder",
                position.name == "Right Wing" ~ "Forward",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Center Attacking Midfield" ~ "Midfielder",
                position.name == "Center Defensive Midfield" ~ "Midfielder",
                position.name == "Center Forward" ~ "Forward"
              )) %>% 
              group_by(position_simple) 
            
            mean_duel<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(position_simple = case_when(
                position.name == "Right Back" ~ "Defender",
                position.name == "Right Center Back" ~ "Defender",
                position.name == "Left Back" ~ "Defender",
                position.name == "Left Center Back" ~ "Defender",
                position.name == "Left Center Midfield" ~ "Midfielder",
                position.name == "Left Defensive Midfield" ~ "Midfielder",
                position.name == "Left Midfield" ~ "Midfielder",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Right Center Forward" ~ "Forward",
                position.name == "Right Center Midfield" ~ "Midfielder",
                position.name == "Right Defensive Midfield" ~ "Midfielder",
                position.name == "Right Midfield" ~ "Midfielder",
                position.name == "Right Wing" ~ "Forward",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Center Attacking Midfield" ~ "Midfielder",
                position.name == "Center Defensive Midfield" ~ "Midfielder",
                position.name == "Center Forward" ~ "Forward"
              )) %>% 
              group_by(position_simple) %>% 
              summarise(Mean = mean(location.x)) 
            
            
            plot<-ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_point_interactive(data = duel_plot_loc,
                                     aes(x = location.x , y = location.y, colour = duel_WL,
                                         shape = position_simple,
                                         tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                         "\n",paste("Position : ",position.name,sep = ""),
                                                         "\n",paste("Minute : ",minute, sep = ""),
                                                         sep="")))+
              geom_vline_interactive(data = mean_duel,aes(xintercept = Mean,
                                                          tooltip = paste(paste("Mean position of duel for : ",position_simple, sep = ""),
                                                                          "\n",paste("Location : ",round(Mean,1),sep = ""),
                                                                          sep="")),linetype = "dotted",size = 1.5)+
              theme(legend.position = "right") + 
              labs(title = paste("Duel zone according to player position",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          # Duel player won -----------
          output$duel_player_won <- renderFormattable({
            
            
            
            duel<-events %>% 
              filter(team.name %in% input$choice_team 
                     & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              group_by(player.name,duel_WL) %>% 
              summarise(n=n())%>% 
              mutate(per = round(prop.table(n) * 100,1))
            
            duel_win<-duel %>% 
              filter(duel_WL %in% "Won") %>% 
              select(-contains(c("duel_WL", "per"))) %>% 
              arrange(desc(n))  %>%       
              rename("Player" = player.name,
                     "Number of duels won" = n) %>% 
              formattable(align = c("c","c"),
                          list(
                            Percentage = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))))
            
          })
          # Duel player lost -----------
          output$duel_player_lost <- renderFormattable({
            duel<-events %>% 
              filter(team.name %in% input$choice_team 
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Duel"
                     ) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              group_by(player.name,duel_WL) %>% 
              summarise(n=n())%>% 
              mutate(per = round(prop.table(n) * 100,1))
            
            duel_win<-duel %>% 
              filter(duel_WL %in% "Lost") %>% 
              select(-contains(c("duel_WL", "per"))) %>% 
              arrange(desc(n))  %>%       
              rename("Player" = player.name,
                     "Number of duels lost" = n) %>% 
              formattable(align = c("c","c"),
                          list(
                            Percentage = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))))
            
          })
          
          
          # duel_first_tiers  ------------
          output$duel_first_tiers<-renderText({
            paste("Duel in the first 1/3",sep = "")
          })
          # duel_second_tiers  ------------
          output$duel_second_tiers<-renderText({
            paste("Duel in the second 1/3",sep = "")
          })
          
          # duel_third_tiers  ------------
          output$duel_last_tiers<-renderText({
            paste("Duel in the last 1/3",sep = "")
          })
          
          # duel_first_tiers number  ------------
          output$duel_first_tiers_n<-renderText({
            
            duel_loca<-events %>% 
              filter(type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & team.name %in% input$choice_team) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(duel_location = case_when(
                location.x <= 40 ~ "First 1/3",
                location.x >40 & location.x < 80 ~ "Second 1/3",
                location.x >= 80 ~ "Last 1/3")) %>% 
              group_by(team.name, duel_location) %>% 
              summarise(n = n()) %>% 
              arrange(desc(duel_location))  %>% 
              na.omit() 
            
            duel_loca<-duel_loca %>% 
              filter (duel_location %in% "First 1/3")
            
            paste(duel_loca$n,sep = "")
          })
          
          # duel_second_tiers number  ------------
          output$duel_second_tiers_n<-renderText({
            
            duel_loca<-events %>% 
              filter(type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & team.name %in% input$choice_team) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(duel_location = case_when(
                location.x <= 40 ~ "First 1/3",
                location.x >40 & location.x < 80 ~ "Second 1/3",
                location.x >= 80 ~ "Last 1/3")) %>% 
              group_by(team.name, duel_location) %>% 
              summarise(n = n()) %>% 
              arrange(desc(duel_location)) %>% 
              na.omit()  
            
            duel_loca<-duel_loca %>% 
              filter (duel_location %in% "Second 1/3")
            
            paste(duel_loca$n,sep = "")
          })
          
          
          # duel_third_tiers number  ------------
          output$duel_last_tiers_n<-renderText({
            
            duel_loca<-events %>% 
              filter(type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & team.name %in% input$choice_team) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(duel_location = case_when(
                location.x <= 40 ~ "First 1/3",
                location.x >40 & location.x < 80 ~ "Second 1/3",
                location.x >= 80 ~ "Last 1/3")) %>% 
              group_by(team.name, duel_location) %>% 
              summarise(n = n()) %>% 
              arrange(desc(duel_location)) %>% 
              na.omit()  
            
            duel_loca<-duel_loca %>% 
              filter (duel_location %in% "Last 1/3")
            
            paste(duel_loca$n,sep = "")
          })
          
          
          
          
          # plot_foul_committed_team  -------------
          output$plot_foul_committed_team<-renderggiraph({
            def_metrics<-events %>% 
              filter(team.name %in% input$choice_team 
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Foul Committed")
            
            
            plot<-ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(data = def_metrics,
                                    aes(location.x, location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') +
              labs(title = paste("Foul committed during the competition",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          # table foul committed team  ------------
          output$table_foul_committed_team<-renderFormattable({
            
            
            def_metrics<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Foul Committed") %>% 
              group_by(player.name) %>%
              summarise(n=n())%>% 
              arrange(desc(n)) %>%
              rename("Player" = player.name,
                     "Number of fouls" = n) %>% 
              formattable(align = c("c","c"),
                          list(
                            Percentage = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))))
            
          }) 
          
          # plot_interception_team  -------------
          output$plot_interception_team<-renderggiraph({

            def_metrics<-events %>% 
              filter(team.name %in% input$choice_team 
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Interception")
            
            
            plot<-ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(data = def_metrics,
                                    aes(location.x, location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') +
              theme(legend.position = "right") + 
              labs(title = paste("Interception during the competition",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          
          # table interception committed team  ------------
          output$table_interception_team<-renderFormattable({

            def_metrics<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Interception") %>% 
              group_by(player.name) %>%
              summarise(n=n())%>% 
              arrange(desc(n)) %>%
              rename("Player" = player.name,
                     "Number of interceptions" = n) %>% 
              formattable(align = c("c","c"),
                          list(
                            Percentage = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))))
            
          }) 
          
          
          
          # plot_pressure_team  -------------
          output$plot_pressure_team<-renderggiraph({
            
            def_metrics<-events %>% 
              filter(team.name %in% input$choice_team 
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Pressure")
            
            
            plot<-ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(data = def_metrics,
                                    aes(location.x, location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              labs(title = paste("Pressure events during the competition",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          
          # table pressure committed team  ------------
          output$table_pressure_team<-renderFormattable({
            
            def_metrics<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & type.name %in% "Pressure") %>% 
              group_by(player.name) %>%
              summarise(n=n())%>% 
              arrange(desc(n)) %>%
              rename("Player" = player.name,
                     "Number of pressure events" = n) %>% 
              formattable(align = c("c","c"),
                          list(
                            Percentage = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))))
            
          }) 
          
          
          
          
          # plot_shot_location_zone  ---------
          output$plot_shot_location_zone<-renderPlot({
            shot_event<-events %>% 
              filter( type.name %in% "Shot" 
                      & team.name %in% input$choice_team
                      & minute %in% (input$slider_min[1]:input$slider_min[2])
              ) %>% 
              mutate(shot_location = case_when(
                location.x <= 60 ~ "First half",
                location.x >60 & location.x < 80 & location.y > 0 & location.y < 30 ~ "zone 10",
                location.x >60 & location.x < 80 & location.y >= 30 & location.y < 50 ~ "zone 11",
                location.x >60 & location.x < 80 & location.y >= 50  ~ "zone 12",
                location.x >=80 & location.x < 100 & location.y >= 0 & location.y < 30 ~ "zone 13",
                location.x >=80 & location.x < 100 & location.y >= 30 & location.y < 50 ~ "zone 14",
                location.x >=80 & location.x < 100 & location.y >= 50 ~ "zone 15",
                location.x >=100 & location.y >= 0 & location.y < 30  ~ "zone 16",
                location.x >=100  & location.y >= 30 & location.y < 50 ~ "zone 17",
                location.x >=100  & location.y >= 50  ~ "zone 18")) %>% 
              group_by(shot_location) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            n1 <- paste(round(shot_event$per[shot_event$shot_location == "zone 12" ],1),sep = "%")
            if (length(n1) == "0"){n1<-"0"}
            n2 <- paste(round(shot_event$per[shot_event$shot_location == "zone 11" ],1),sep = "%")
            if (length(n2) == "0"){n2<-"0"}
            
            n3 <- paste(round(shot_event$per[shot_event$shot_location == "zone 10" ],1),sep = "%")
            if (length(n3) == "0"){n3<-"0"}
            
            n4 <- paste(round(shot_event$per[shot_event$shot_location == "zone 15" ],1),sep = "%")
            if (length(n4) == "0"){n4<-"0"}
            
            n5 <- paste(round(shot_event$per[shot_event$shot_location == "zone 14" ],1),sep = "%")
            if (length(n5) == "0"){n5<-"0"}
            
            n6 <- paste(round(shot_event$per[shot_event$shot_location == "zone 13" ],1),sep = "%")
            if (length(n6) == "0"){n6<-"0"}
            
            n7 <- paste(round(shot_event$per[shot_event$shot_location == "zone 18" ],1),sep = "%")
            if (length(n7) == "0"){n7<-"0"}
            
            n8 <- paste(round(shot_event$per[shot_event$shot_location == "zone 17" ],1),sep = "%")
            if (length(n8) == "0"){n8<-"0"}
            
            n9 <- paste(round(shot_event$per[shot_event$shot_location == "zone 16" ],1),sep = "%")
            if (length(n9) == "0"){n9<-"0"}
            
            
            
            
            ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              coord_flip(xlim = c(55, 120),
                         ylim = c(-12, 105))+
              geom_vline(data = match_event,aes(xintercept = 40)) +
              geom_vline(data = match_event,aes(xintercept = 80))  +
              geom_vline(data = match_event,aes(xintercept = 60)) +
              geom_vline(data = match_event,aes(xintercept = 100)) +
              geom_hline(data = match_event,aes(yintercept = 30)) +
              geom_hline(data = match_event,aes(yintercept = 50)) +
              geom_circle(aes(x0=70, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=70, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=70, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=90, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=110, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_text(data = shot_event, x = 70 , y = 15 , 
                        label = n1, size = 4 )+
              geom_text(data = shot_event, x = 70 , y = 40 , 
                        label = n2, size = 4 )+
              geom_text(data = shot_event, x = 70 , y = 65 , 
                        label = n3, size = 4 )+
              geom_text(data = shot_event, x = 90 , y = 15 , 
                        label = n4, size = 4 )+
              geom_text(data = shot_event, x = 90 , y = 40 , 
                        label = n5, size = 4 )+
              geom_text(data = shot_event, x = 90 , y = 65 , 
                        label = n6, size = 4 )+
              geom_text(data = shot_event, x = 110 , y = 15 , 
                        label = n7, size = 4 )+
              geom_text(data = shot_event, x = 110 , y = 40 , 
                        label = n8, size = 4 )+
              geom_text(data = shot_event, x = 110 , y = 65 , 
                        label = n9, size = 4 )+
              labs(
                title = "Origin of shot (%)"
              ) +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5)
              )
            
            
          })
          # plot_shot_ratio  --------------
          output$plot_shot_ratio<-renderPlot({
            shot_event<-events %>% 
              filter( type.name %in% "Shot" 
                      & team.name %in% input$choice_team
                      & minute %in% (input$slider_min[1]:input$slider_min[2])
                      
              ) %>% 
              mutate(shot_location = case_when(
                location.x <= 60 ~ "First half",
                location.x >60 & location.x < 80 & location.y > 0 & location.y < 30 ~ "zone 10",
                location.x >60 & location.x < 80 & location.y >= 30 & location.y < 50 ~ "zone 11",
                location.x >60 & location.x < 80 & location.y >= 50  ~ "zone 12",
                location.x >=80 & location.x < 100 & location.y >= 0 & location.y < 30 ~ "zone 13",
                location.x >=80 & location.x < 100 & location.y >= 30 & location.y < 50 ~ "zone 14",
                location.x >=80 & location.x < 100 & location.y >= 50 ~ "zone 15",
                location.x >=100 & location.y >= 0 & location.y < 30  ~ "zone 16",
                location.x >=100  & location.y >= 30 & location.y < 50 ~ "zone 17",
                location.x >=100  & location.y >= 50  ~ "zone 18")) %>% 
              mutate(shot.outcome.name = case_when(
                shot.outcome.name == "Blocked" ~ "No Goal",
                shot.outcome.name == "Off T" ~ "No Goal",
                shot.outcome.name == "Saved" ~ "No Goal",
                shot.outcome.name == "Saved to Post" ~ "No Goal",
                shot.outcome.name == "Post" ~ "No Goal",
                shot.outcome.name == "Saved Off target" ~ "No Goal",
                shot.outcome.name == "Wayward" ~ "No Goal",
                shot.outcome.name == "Goal" ~ "Goal"
              )) %>% 
              group_by(shot_location, shot.outcome.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            ggplot(shot_event) +
              aes(x = shot_location, y = per, fill = shot.outcome.name) +
              geom_col() +
              scale_fill_manual(values = c(Goal = "#03B613", 
                                           `No Goal` = "#CE2802")) +
              labs(x = "Shot origin", y = "percentage (%)", title = "ratio Goal / No Goal according to shot origin", 
                   fill = "Shot finality") +
              theme_minimal() +
              theme(plot.title = element_text(size = 12L, face = "bold", 
                                              hjust = 0.5), axis.title.y = element_text(size = 12L, face = "bold"), axis.title.x = element_text(size = 12L, 
                                                                                                                                                face = "bold"))
            
          })
          # plot_behavior_pressure_team  -----------
          output$plot_behavior_pressure_team<-renderGirafe({
            behavior_pressure<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & under_pressure %in% TRUE) %>% 
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(type.name, player.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100),1)%>% 
              arrange(desc(per))%>% 
              select(player.name,type.name,per,n)  
            
            
            
            p<-ggplot() +
              geom_col_interactive( data = behavior_pressure,
                                    aes(x = player.name , y = n, 
                                        fill = type.name,
                                        tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                        "\n",paste("Behavior : ",type.name,sep = ""),
                                                        "\n",paste("Count : ",n,sep = ""),
                                                        sep=""))) +
              scale_fill_hue(direction = 1) +
              labs(
                x = "Number of event under pressure",
                y = "Players",
                title = "Players' behavior under pressure",
                fill = "Behaviors"
              ) +
              coord_flip() +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5),
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
            
            girafe(ggobj=p,
                   options = list(
                     opts_hover_inv(css = "opacity:0.1;"),
                     opts_hover(css = "stroke-width:2;"),
                     opts_selection(only_shiny = FALSE, type = "single", css = "stroke:yellow;")
                   ))
          })
          # plot_5player_pass_pressure  -----------
          output$plot_5player_pass_pressure<-renderPlot({
            behavior_pressure<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & under_pressure %in% TRUE
                     & type.name %in% "Pass") %>% 
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(player.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100),1)%>% 
              arrange(desc(per))%>% 
              slice(1:5)
            
            behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                    levels = behavior_pressure$player.name[order(behavior_pressure$per)])
            
            ggplot(behavior_pressure) +
              aes(x = player.name, weight = per) +
              geom_bar(fill = "#112446") +
              labs(x = "Player", y = "Percentage of pass under pressure") +
              coord_flip() +
              theme_minimal() +
              theme(
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
            
          })
          
          # plot_5player_dribble_pressure  -----------
          output$plot_5player_dribble_pressure<-renderPlot({
            behavior_pressure<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & under_pressure %in% TRUE
                     & type.name %in% "Dribble") %>% 
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(player.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100),1)%>% 
              arrange(desc(per))%>% 
              slice(1:5)
            
            behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                    levels = behavior_pressure$player.name[order(behavior_pressure$per)])
            
            ggplot(behavior_pressure) +
              aes(x = player.name, weight = per) +
              geom_bar(fill = "#112446") +
              labs(x = "Player", y = "Percentage of dribble under pressure") +
              coord_flip() +
              theme_minimal() +
              theme(
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
            
          })
          
          
          # plot_5player_carry_pressure  -----------
          output$plot_5player_carry_pressure<-renderPlot({
            behavior_pressure<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & under_pressure %in% TRUE
                     & type.name %in% "Carry") %>% 
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(player.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100),1)%>% 
              arrange(desc(per))%>% 
              slice(1:5)
            
            behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                    levels = behavior_pressure$player.name[order(behavior_pressure$per)])
            
            ggplot(behavior_pressure) +
              aes(x = player.name, weight = per) +
              geom_bar(fill = "#112446") +
              labs(x = "Player", y = "Percentage of carry under pressure") +
              coord_flip() +
              theme_minimal() +
              theme(
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
            
          })
          
          
          
          # plot_5player_carry_pressure  -----------
          output$plot_5player_dispossessed_pressure<-renderPlot({
            behavior_pressure<-events %>% 
              filter(team.name %in% input$choice_team
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & under_pressure %in% TRUE
                     & type.name %in% "Dispossessed") %>% 
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(player.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100),1)%>% 
              arrange(desc(per))%>% 
              slice(1:5)
            
            behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                    levels = behavior_pressure$player.name[order(behavior_pressure$per)])
            
            ggplot(behavior_pressure) +
              aes(x = player.name, weight = per) +
              geom_bar(fill = "#112446") +
              labs(x = "Player", y = "Percentage of dispossessed under pressure") +
              coord_flip() +
              theme_minimal() +
              theme(
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
            
          })
          
          
          
          
        } else if (input$match_selection != "Summary all matches") {
          # Output pass origin team plot  ------------
          
          output$pass_origin_team<-renderPlot({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & !(pass.outcome.name %in% "Unknown") &
                       minute %in% (input$slider_min[1]:input$slider_min[2]))
            
            
            ggplot(pass_minute) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(aes(location.x,location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              theme(legend.position = "none") + 
              labs(title = paste("Passes origin zone",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
          })
          
          # Output pass target team plot  ------------
          
          output$pass_target_team<-renderPlot({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & !(pass.outcome.name %in% "Unknown") &
                       minute %in% (input$slider_min[1]:input$slider_min[2]))
            
            
            ggplot(pass_minute) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(aes(pass.end_location.x,pass.end_location.y, fill = ..level..), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              theme(legend.position = "none") + 
              labs(title = paste("Passes favorite target zone",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
          })
          
          
          # Pass reca n team --------------
          output$pass_minute_reca_n<-renderFormattable({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & !(pass.outcome.name %in% "Unknown" )
                     & minute %in% (input$slider_min[1]:input$slider_min[2]))%>%
              group_by (player.name) %>% 
              summarise(n=n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              arrange(desc(n))
            
            
            pass_minute %>% 
              formattable(align = c("c","c","r"),
                          list(
                            per = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))
                          ))
            
          })
          # Pass reca n team by recipient player --------------
          output$pass_minute_reca_recipient_n<-renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            pass_minute<-events %>% 
              filter(type.name %in% "Pass" & team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & !(pass.outcome.name %in% "Unknown" )
                     & minute %in% (input$slider_min[1]:input$slider_min[2]))%>%
              group_by (pass.recipient.name) %>% 
              summarise(n=n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              arrange(desc(n)) %>% 
              na.omit()
            
            
            pass_minute %>% 
              formattable(align = c("c","c","r"),
                          list(
                            per = color_bar( "lightblue"),
                            pass.recipient.name = formatter("span", 
                              style = ~ style(color = "black",font.weight = "bold"))
                          ))
            
          })
          
          # pass distance tendancy pie  ----------
          
          output$pass_distance_tendancy_team<-renderPlotly({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            pass_length<-events %>% 
              filter(type.name %in% "Pass" &
                       team.name %in% input$choice_team 
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(category=cut(pass.length*0.9144, breaks=c(0, 15, 30,100), 
                                  labels=c("short","medium","long"))) %>% 
              group_by(category) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100)
            
            
            p <- plot_ly() %>%
              add_pie(data = pass_length, labels = ~paste(category,"pass",sep = " "), values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = 'Pass distance tendancy (m)',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)")
            p
            
          })
          
          
          # pass height tendancy pie  ----------
          
          output$pass_height_tendancy_team<-renderPlotly({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            pass_height<-events %>% 
              filter(type.name %in% "Pass" &
                       match_id %in% data$match_id
                     & team.name %in% input$choice_team &
                       minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(pass.height.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100)
            
            
            p <- plot_ly() %>%
              add_pie(data = pass_height, labels = ~pass.height.name, values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = 'Pass height tendancy',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)")
            p
            
          })
          
          # Plot successful passes  -------------------
          output$successful_passes_team<-renderPlotly({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            pass_completion<-events %>% 
              filter(type.name %in% "Pass" &
                       team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>%
              mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
              group_by(pass.outcome.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100))
            
            p <- plot_ly() %>%
              add_pie(data = pass_completion, labels = ~paste(pass.outcome.name,"pass",sep = " "), values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = "Pass distribution",
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)",
                     annotations=list(text=paste(paste(pass_completion$per[1], " %", sep=""),"\n",
                                                 sep = ""), 
                                      "showarrow"=F, font=list(size = 20)))
            
            p
            
            
          })
          
          
          
          # Pass team shot assist  -------------
          output$plot_shot_assist_team<-renderPlot({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            pass<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.shot_assist %in% TRUE )
            
            
            ggplot(pass) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                           yend = pass.end_location.y,
                                           tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                           "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")),
                                       colour = "#FFA500",
                                       arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                       ))+
              scale_colour_identity()+
              theme(legend.position = "none") + 
              labs(title = paste("Shot assist direction during the competition",sep = " "),
                   caption = "Data Source: StatsBomb") +
              theme(plot.title = element_text(hjust = 0.5))
            
          })
          # Pass team shot assist endzone  -------------
          output$plot_shot_assist_team_endzone<-renderPlot({
            
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            pass_event<-events %>% 
              filter(type.name %in% "Pass" 
                     & team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.shot_assist %in% "TRUE") %>% 
              mutate(pass_location = case_when(
                pass.end_location.x <= 60 ~ "First half",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y > 0 & pass.end_location.y < 30 ~ "zone 10",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 11",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 50  ~ "zone 12",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 0 & pass.end_location.y < 30 ~ "zone 13",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 14",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 50 ~ "zone 15",
                pass.end_location.x >=100 & pass.end_location.y >= 0 & pass.end_location.y < 30  ~ "zone 16",
                pass.end_location.x >=100  & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 17",
                pass.end_location.x >=100  & pass.end_location.y >= 50  ~ "zone 18"))%>% 
              group_by(team.name, pass_location) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            n1 <- paste(round(pass_event$per[pass_event$pass_location == "zone 12" ],1),sep = "%")
            if (length(n1) == "0"){n1<-"0"}
            n2 <- paste(round(pass_event$per[pass_event$pass_location == "zone 11" ],1),sep = "%")
            if (length(n2) == "0"){n2<-"0"}
            
            n3 <- paste(round(pass_event$per[pass_event$pass_location == "zone 10" ],1),sep = "%")
            if (length(n3) == "0"){n3<-"0"}
            
            n4 <- paste(round(pass_event$per[pass_event$pass_location == "zone 15" ],1),sep = "%")
            if (length(n4) == "0"){n4<-"0"}
            
            n5 <- paste(round(pass_event$per[pass_event$pass_location == "zone 14" ],1),sep = "%")
            if (length(n5) == "0"){n5<-"0"}
            
            n6 <- paste(round(pass_event$per[pass_event$pass_location == "zone 13" ],1),sep = "%")
            if (length(n6) == "0"){n6<-"0"}
            
            n7 <- paste(round(pass_event$per[pass_event$pass_location == "zone 18" ],1),sep = "%")
            if (length(n7) == "0"){n7<-"0"}
            
            n8 <- paste(round(pass_event$per[pass_event$pass_location == "zone 17" ],1),sep = "%")
            if (length(n8) == "0"){n8<-"0"}
            
            n9 <- paste(round(pass_event$per[pass_event$pass_location == "zone 16" ],1),sep = "%")
            if (length(n9) == "0"){n9<-"0"}
            
            
            
            
            ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_vline(data = match_event,aes(xintercept = 40)) +
              geom_vline(data = match_event,aes(xintercept = 80))  +
              geom_vline(data = match_event,aes(xintercept = 60)) +
              geom_vline(data = match_event,aes(xintercept = 100)) +
              geom_hline(data = match_event,aes(yintercept = 30)) +
              geom_hline(data = match_event,aes(yintercept = 50)) +
              geom_circle(aes(x0=70, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=70, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=70, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=90, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=90, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=15, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=110, y0=40, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=110, y0=65, r=6), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_text(data = pass_event, x = 70 , y = 15 , 
                        label = n1, size = 4 )+
              annotate("text", label = paste0("Zone 12"), x=70, y = 7, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 70 , y = 40 , 
                        label = n2, size = 4 )+
              annotate("text", label = paste0("Zone 11"), x=70, y = 32, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 70 , y = 65 , 
                        label = n3, size = 4 )+  
              annotate("text", label = paste0("Zone 10"), x=70, y = 58, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 90 , y = 15 , 
                        label = n4, size = 4 )+
              annotate("text", label = paste0("Zone 15"), x=90, y = 7, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 90 , y = 40 , 
                        label = n5, size = 4 )+
              annotate("text", label = paste0("Zone 14"), x=90, y = 32, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 90 , y = 65 , 
                        label = n6, size = 4 )+
              annotate("text", label = paste0("Zone 13"), x=90, y = 58, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 110 , y = 15 , 
                        label = n7, size = 4 )+
              annotate("text", label = paste0("Zone 18"), x=110, y = 7, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 110 , y = 40 , 
                        label = n8, size = 4 )+
              annotate("text", label = paste0("Zone 17"), x=110, y = 32, size = 3, colour = "white")+
              geom_text(data = pass_event, x = 110 , y = 65 , 
                        label = n9, size = 4 )+
              annotate("text", label = paste0("Zone 16"), x=110, y = 58, size = 3, colour = "white")+
              labs(
                title = "Endzone of pass preceeding shot (%)"
              ) +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5)
              )
          })
          
          
          # plot Pass team shot assist endzone and type of pass  ----------
          output$plot_type_pass_endzone<-renderPlot({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            pass_event<-events %>% 
              filter(type.name %in% "Pass" 
                     & team.name %in% input$choice_team
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.shot_assist %in% "TRUE"
              ) %>% 
              mutate(pass_location = case_when(
                pass.end_location.x <= 60 ~ "First half",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y > 0 & location.y < 30 ~ "zone 10",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 11",
                pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 50  ~ "zone 12",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 0 & pass.end_location.y < 30 ~ "zone 13",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 14",
                pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 50 ~ "zone 15",
                pass.end_location.x >=100 & pass.end_location.y >= 0 & pass.end_location.y < 30  ~ "zone 16",
                pass.end_location.x >=100  & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 17",
                pass.end_location.x >=100  & pass.end_location.y >= 50  ~ "zone 18")) %>% 
              group_by(team.name, pass_location, 
                       pass.type.name,
              ) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            ggplot(pass_event) +
              aes(x = pass_location, y = n, fill = pass.type.name) +
              geom_col() +
              scale_fill_hue(direction = 1) +
              labs(
                x = "Pass endzone location",
                y = "number of pass",
                title = "Type of pass according to end zone",
                fill = "Type of pass"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 12L,
                                          face = "bold",
                                          hjust = 0.5),
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              )
            
          })
          
          # output plot goal assist  --------
          output$plot_goal_assist_team<-renderGirafe({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            pass<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & pass.goal_assist %in% TRUE)
            
            
            plot<-ggplot(pass) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                           yend = pass.end_location.y,
                                           tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                           "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")),
                                       colour = "#FFA500",
                                       arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                       ))+
              scale_colour_identity()+
              theme(legend.position = "none") + 
              labs(title = paste("Goal assist direction during the competition",sep = " "),
                   caption = "Data Source: StatsBomb") +
              theme(plot.title = element_text(hjust = 0.5))
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
          })
          

          
          
          # Shot title page  -------------
          output$shot_title_page<-renderText({
            paste("Time of shots during the match (click on shot to view details)")
          })
          # plot Pass assist and xg asociated  -------------
          output$plot_pass_assist_xg<-renderGirafe({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            pass<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & pass.shot_assist %in% TRUE
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(player.name, minute,match_id,pass.recipient.name) %>% 
              summarise(n = n())
            
            shot_player_xg<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,match_id,shot.statsbomb_xg) %>% 
              summarise(n = n()) 
            
            pass_goal<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Pass"
                     & match_id %in% data$match_id
                     & pass.goal_assist %in% TRUE
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(player.name, minute,match_id,pass.recipient.name) %>% 
              summarise(n = n())
            
            
            goal_player_xg<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & match_id %in% data$match_id
                     & shot.outcome.name %in% "Goal"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,match_id,shot.statsbomb_xg) %>% 
              summarise(n = n()) 
            
            
            data_shot<-left_join(pass,shot_player_xg, by = c("minute","match_id")) %>% 
              mutate (shot_finality = "no goal")
            data_goal<-left_join(pass_goal,goal_player_xg, by = c("minute","match_id")) %>% 
              mutate(shot_finality = "goal")
            
            data<-bind_rows(data_shot,data_goal)
            
            
            plot<-ggplot(data) +
              aes(x = player.name, y = minute, colour = shot_finality, size = shot.statsbomb_xg,
                  tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                  "\n",paste("Value of xG : ",shot.statsbomb_xg,sep = ""),
                                  "\n",paste("Minute : ",minute, sep = ""),
                                  sep="")) +
              geom_point_interactive(shape = "circle") +
              scale_color_manual(values = c(goal = "#0FCF18", `no goal` = "#DF0505")) +
              labs(x = "Player", y = "Minute when pass happened", 
                   title = "Pass assist according to time and xg associated", color = "Shot finality", size = "xG value") +
              coord_flip() +
              theme_minimal() +
              theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5), 
                    axis.title.y = element_text(size = 12L, face = "bold"), axis.title.x = element_text(size = 12L, face = "bold"))
            
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          
          
          
          # Plot shot_timeline_xg_team ---------------
          output$shot_timeline_xg_team<-renderPlotly({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            shot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              group_by(player.name, minute,match_id,
                       shot.statsbomb_xg,
                       shot.outcome.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))
            
            
            
            
            fig<-plot_ly(source = "plot", data = shot, y = ~player.name,
                         x = ~minute, type = "scatter" ,
                         color = ~shot.outcome.name, size = ~shot.statsbomb_xg,
                         text = ~shot.statsbomb_xg)%>%
              layout(xaxis = list(title = "minute of the match", titlefont = "Courier New, monospace"), 
                     yaxis = list(title = " ", titlefont = "Courier New, monospace"))
            
            fig 
            
            
          })
          
          
          # Plot defenders position when event shot  ---------
          
          output$plot_defenders_pos<-renderPlotly({

            event.data <-event_data("plotly_click", source = "plot")

            # Obtain the clicked x/y variables and fit linear model
            point_number <- event.data$pointNumber

            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            shot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              group_by(player.name, minute,match_id,
                       shot.statsbomb_xg,
                       shot.outcome.name) %>% 
              summarise(n = n()) 
              
              shot<-shot[point_number + 1,]
            
              

            # Obtain the clicked x/y variables and fit linear model

            
            shot_position_defender<-events %>% 
              filter(team.name==input$choice_team & type.name %in% "Shot"
                     & shot.statsbomb_xg %in% shot$shot.statsbomb_xg 
                     & player.name %in% shot$player.name) 
            
            x<-shot_position_defender$shot.freeze_frame
            
            x<-as.data.frame(x)%>% 
              unnest_wider(location,names_sep="_") 
            
            plot2<-ggplot(x) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              coord_flip(xlim = c(55, 120),
                         ylim = c(-12, 105))+
              geom_point(aes(x = location_1, y = location_2,
                                         colour = teammate
              )) + 
              scale_colour_manual(values = c("#ff4444","#5e9a78"),labels = c("No-Goal","Goal"),
                                  name = "Team") +
              labs(title = paste("Defenders position for the selected shot ",sep = " "))+
              theme(plot.title = element_text(hjust = 0.5))+
              geom_segment(data = shot_position_defender,
                           aes(x = location.x, y = location.y, xend = shot.end_location.x, 
                               yend = shot.end_location.y),
                           colour = "green",
                           arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                           ))
            
            ggplotly(plot2)
            
            
          })
          
          

          # Plot goalmap when shot selected  ------------
          output$plot_goalmap_selection<-renderPlot({
            
            event.data <-event_data("plotly_click", source = "plot")
            
            # Obtain the clicked x/y variables and fit linear model
            point_number <- event.data$pointNumber
            
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            shot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Shot"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              group_by(player.name, minute,match_id,
                       shot.statsbomb_xg,
                       shot.outcome.name) %>% 
              summarise(n = n()) 
            
            shot<-shot[point_number+1,]
            
            shot_selected<-events %>% 
              filter(team.name== input$choice_team & type.name %in% "Shot" 
                     & shot.statsbomb_xg %in% shot$shot.statsbomb_xg 
                     & player.name %in% shot$player.name)%>% 
              select(-contains("freeze"))%>% 
              mutate_if(is.factor, as.character) %>% 
              mutate_at(vars(c(location.x, location.y, shot.end_location.x, shot.end_location.y, 
                               shot.end_location.z, shot.statsbomb_xg)), list(as.numeric))
            shot_selected<-shot_selected %>% 
              filter(shot.outcome.name %in% c('Goal','Post', 'Off T', 'Saved', 
                                              'Saved to Post','Saved Off Target')) %>% 
              mutate(goal.seq = 1:length(shot.outcome.name))
            
            post(fill_background = "white")+
              geom_point(shot_selected, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, 
                                             color = shot.outcome.name, shape = shot.type.name), size = 5)+
              geom_text(shot_selected, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, label = goal.seq),
                        size = 3, color = "darkslategray")+
              theme(
                legend.position = "top",
                plot.title = element_text(hjust=0.5, vjust = -5, size = 15),
                plot.subtitle =  element_text(hjust=0.5, vjust = -5),
                text = element_text(color = "black")
              )+
              labs(color = "Shot Outcome", title = input$match_selection, shape = NULL,
                    )+
              scale_color_manual(values = c("darkgreen", "red", "blue", "yellow", "black", "orange"))+
              scale_shape_manual(values = c(16, 15, 17,25))
            
            
          })
          # Plot reca duel  -------------
          output$plot_reca_duel<-renderPlotly({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            duel<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              group_by(duel_WL) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              na.omit()
            
            
            p <- plot_ly() %>%
              add_pie(data = duel, labels = ~ duel_WL, values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE, marker = list(colors = c('red', 'green'))) %>% 
              layout(title = 'Duel distribution',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)",
                     annotations=list(text=paste(paste(sum(duel$n), " duels", sep=""), sep =" "),
                                      "showarrow"=F, font=list(size = 20)))
            
            p
          })
          
          
          # Plot distrib duel  ----------
          output$plot_distrib_duel<-renderPlotly({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            duel_table<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              group_by(duel.outcome.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = round(prop.table(n) * 100,1)) %>% 
              na.omit() %>% 
              arrange(desc(per))
            
            
            p <- plot_ly() %>%
              add_pie(data = duel_table, labels = ~ duel.outcome.name, values = ~per, hole = 0.6,
                      name = "a",textposition = 'inside',textinfo = 'label+percent',
                      showlegend = FALSE) %>% 
              layout(title = 'Duel type',
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     fig_bgcolor   = "rgba(0, 0, 0, 0)")
            
            
          })
          # Plot density duel  ---------------
          output$plot_duel_density<-renderPlot({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            duel_plot<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) 
            
            ggplot(duel_plot) +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_density2d_filled(aes(location.x, location.y, fill = after_stat(level)), 
                                    alpha=0.8, contour_var = 'ndensity') + 
              theme(legend.position = "none") + 
              labs(title = paste("Duel zone",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
          })
          
          
          # Plot duel location  -------------
          output$plot_duel_location<-renderGirafe({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            duel_plot_loc<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(position_simple = case_when(
                position.name == "Right Back" ~ "Defender",
                position.name == "Right Center Back" ~ "Defender",
                position.name == "Left Back" ~ "Defender",
                position.name == "Left Center Back" ~ "Defender",
                position.name == "Left Center Midfield" ~ "Midfielder",
                position.name == "Left Defensive Midfield" ~ "Midfielder",
                position.name == "Left Midfield" ~ "Midfielder",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Right Center Forward" ~ "Forward",
                position.name == "Right Center Midfield" ~ "Midfielder",
                position.name == "Right Defensive Midfield" ~ "Midfielder",
                position.name == "Right Midfield" ~ "Midfielder",
                position.name == "Right Wing" ~ "Forward",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Center Attacking Midfield" ~ "Midfielder",
                position.name == "Center Defensive Midfield" ~ "Midfielder",
                position.name == "Center Forward" ~ "Forward"
              )) %>% 
              group_by(position_simple) 
            
            mean_duel<-events %>% 
              filter(team.name %in% input$choice_team & type.name %in% "Duel"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])
                     & match_id %in% data$match_id) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              mutate(position_simple = case_when(
                position.name == "Right Back" ~ "Defender",
                position.name == "Right Center Back" ~ "Defender",
                position.name == "Left Back" ~ "Defender",
                position.name == "Left Center Back" ~ "Defender",
                position.name == "Left Center Midfield" ~ "Midfielder",
                position.name == "Left Defensive Midfield" ~ "Midfielder",
                position.name == "Left Midfield" ~ "Midfielder",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Right Center Forward" ~ "Forward",
                position.name == "Right Center Midfield" ~ "Midfielder",
                position.name == "Right Defensive Midfield" ~ "Midfielder",
                position.name == "Right Midfield" ~ "Midfielder",
                position.name == "Right Wing" ~ "Forward",
                position.name == "Left Wing" ~ "Forward",
                position.name == "Center Attacking Midfield" ~ "Midfielder",
                position.name == "Center Defensive Midfield" ~ "Midfielder",
                position.name == "Center Forward" ~ "Forward"
              )) %>% 
              group_by(position_simple) %>% 
              summarise(Mean = mean(location.x)) 
            
            
            plot<-ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_point_interactive(data = duel_plot_loc,
                                     aes(x = location.x , y = location.y, colour = duel_WL,
                                         shape = position_simple,
                                         tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                         "\n",paste("Position : ",position.name,sep = ""),
                                                         "\n",paste("Minute : ",minute, sep = ""),
                                                         sep="")))+
              geom_vline_interactive(data = mean_duel,aes(xintercept = Mean,
                                                          tooltip = paste(paste("Mean position of duel for : ",position_simple, sep = ""),
                                                                          "\n",paste("Location : ",round(Mean,1),sep = ""),
                                                                          sep="")),linetype = "dotted",size = 1.5)+
              theme(legend.position = "right") + 
              labs(title = paste("Duel zone according to player position",sep = " "),
                   caption = "Data Source: StatsBomb")+
              theme(plot.title = element_text(hjust = 0.5))
            
            
            girafe(ggobj=plot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          
          # Text output home team  ------------------
          output$home_team<-renderText({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            paste(data$home_team.country.name)
            
          })
          # Text output score home team  ------------------
          output$score_home_team<-renderText({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            paste(data$home_score, data$away_score, sep = " - ")
            
          })
          
          # Text output away team  ------------------
          output$away_team<-renderText({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            paste(data$away_team.country.name)
            
          })
          
          # Text output score home team  ------------------
          output$score_away_team<-renderText({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            paste(data$away_score)
            
          })
          
          
          
          
          # score penalty  --------------
          output$score_penalty<-renderText({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            goal_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       shot.outcome.name %in% "Goal" &
                       period %in%  "5") %>% 
              group_by(team.name) %>% 
              summarise(n = n())
            
            
            
            paste(goal_event$n[goal_event$team.name == data$home_team.country.name],
                  goal_event$n[goal_event$team.name == data$away_team.country.name],
                  sep = " tab a ")
            
          })
          # Logo team name 1 ------
          
          output$logo_t1<-renderImage({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            filename<-paste(
                            data$home_team.country.name,".png",sep="")
            list(src = filename, alt = "Alternate text",height = "100",width = "100")
          },deleteFile = FALSE)
          
          # Logo team name 2 ------
          
          output$logo_t2<-renderImage({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            filename<-paste(
                            data$away_team.country.name,".png",sep="")
            list(src = filename, alt = "Alternate text",height = "100",width = "100")
          },deleteFile = FALSE)
          
          
          # Kpi essential  -----------
          output$kpi_essential<-renderPlot({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            pass_team<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% "Pass") %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            pass_team_plot <- ggplot(pass_team)+
              aes(x = "", y = per, fill = team.name) + 
              geom_col(width = .25) + 
              scale_fill_manual(values = c("#F8766D", "#039dfc")) + 
              coord_flip() + 
              labs(title = "Pass")+
              theme_void() +
              theme(
                legend.position = "none",
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5, vjust = -5)
              )+
              geom_text(aes(label = n, y = per), size = 7,
                        position = position_stack(vjust = 0.5))
            
            
            
            shot_team<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% "Shot") %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            shot_team_plot<- ggplot(shot_team)+
              aes(x = "", y = per, fill = team.name) + 
              geom_col(width = .25) + 
              scale_fill_manual(values = c("#F8766D", "#039dfc")) + 
              coord_flip() + 
              labs(title = "Shot")+
              theme_void() +
              theme(
                legend.position = "none",
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5, vjust = -5)
              )+
              geom_text(aes(label = n, y = per), size = 7,
                        position = position_stack(vjust = 0.5))
            
            duel_team<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% "Duel") %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            duel_team_plot<- ggplot(duel_team)+
              aes(x = "", y = per, fill = team.name) + 
              geom_col(width = .25) + 
              scale_fill_manual(values = c("#F8766D", "#039dfc")) + 
              coord_flip() + 
              labs(title = "Duel")+
              theme_void() +
              theme(
                legend.position = "none",
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5, vjust = -5)
              )+
              geom_text(aes(label = n, y = per), size = 7,
                        position = position_stack(vjust = 0.5))
            
            
            
            intercep_team<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% "Interception") %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            intercep_team_plot<- ggplot(intercep_team)+
              aes(x = "", y = per, fill = team.name) + 
              geom_col(width = .25) + 
              scale_fill_manual(values = c("#F8766D", "#039dfc")) + 
              coord_flip() + 
              labs(title = "Interception")+
              theme_void() +
              theme(
                legend.position = "none",
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5, vjust = -5)
              )+
              geom_text(aes(label = n, y = per), size = 7,
                        position = position_stack(vjust = 0.5))
            
            
            
            foul_team<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% "Foul Committed") %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              arrange(desc(per))
            
            
            
            foul_team_plot<- ggplot(foul_team)+
              aes(x = "", y = per, fill = team.name) + 
              geom_col(width = .25) + 
              scale_fill_manual(values = c("#F8766D", "#039dfc")) + 
              coord_flip() + 
              labs(title = "Foul Committed")+
              theme_void() +
              theme(
                legend.position = "none",
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5, vjust = -5)
              )+
              geom_text(aes(label = n, y = per), size = 7,
                        position = position_stack(vjust = 0.5))
            

            possession<-events %>% 
              filter(match_id %in% data$match_id &
                       TimeInPoss) %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100)
            
            
            possession_plot<- ggplot(possession)+
              aes(x = "", y = per, fill = team.name) + 
              geom_col(width = .25) + 
              scale_fill_manual(values = c("#F8766D", "#039dfc")) + 
              coord_flip() + 
              labs(title = "Possession(%)")+
              theme_void() +
              theme(
                legend.position = "none",
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5, vjust = -5)
              )+
              geom_text(aes(label = n, y = per), size = 7,
                        position = position_stack(vjust = 0.5))
            
            
            
            ggarrange( pass_team_plot, shot_team_plot,
                      duel_team_plot, intercep_team_plot,
                      foul_team_plot, possession_plot,
                      ncol = 1, nrow = 6)
            
          })
          # Plot shot match  --------------
          output$plot_shot_match<-renderGirafe({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            minute_match<-events %>% 
              filter(match_id %in% data$match_id) 
            
            range_minute<-range(minute_match$minute)
            
            
            
            match_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% c("Foul Committed",
                                        "Shot","Pressure","Duel","Pass")
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,team.name,type.name,shot.outcome.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))
            
            
            goal_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       shot.outcome.name %in% "Goal"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,team.name,
                       player.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))  
            
            
            
            match_event %>%
              filter(type.name %in% "Shot") %>%
              ggplot() +
              aes(x = minute, fill = type.name) +
              geom_density(adjust = 0.3) +
              scale_fill_hue(direction = 1) +
              theme_minimal() +
              facet_wrap(vars(team.name))    
            
            plot_shot<-match_event %>%
              filter(type.name %in% "Shot") %>%
              ggplot() +
              aes(x = minute, fill = team.name) +
              geom_histogram_interactive(bins = 30L) +
              scale_fill_hue(direction = 1) +
              labs(
                x = "Minute",
                y = "Number of shots",
                title = "Number of shot according to time",
                fill = "Team",
                shape = "GOAL"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 15L,
                                          face = "bold",
                                          hjust = 0.5),
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              ) +
              geom_point_interactive(data = goal_event, 
                                     aes(x = minute , y = 1,  shape = team.name,
                                                            tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                                            "\n",paste("Minute : ",minute, sep = ""),
                                                                            sep=""))) + 
              facet_wrap(vars(team.name), ncol = 1L)
            
            girafe(ggobj=plot_shot,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
            
          })
          # Plot duel match  ----------
          output$plot_duel_match <-renderGirafe({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            minute_match<-events %>% 
              filter(match_id %in% data$match_id) 
            
            match_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% c("Foul Committed",
                                        "Shot","Pressure","Duel","Pass")
                      & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,team.name,type.name,shot.outcome.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))   
            
            
            match_event %>%
              filter(type.name %in% "Duel") %>%
              ggplot() +
              aes(x = minute, fill = type.name) +
              geom_density(adjust = 0.3) +
              scale_fill_hue(direction = 1) +
              theme_minimal() +
              facet_wrap(vars(team.name)) 
            
            foul_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% c("Foul Committed")
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              group_by(minute,team.name,type.name, 
                       player.name) %>% 
              summarise(n = n()) %>% 
              arrange(desc(minute))   
            
            
            plot_duel<-match_event %>%
              filter(type.name %in% "Duel") %>%
              ggplot() +
              aes(x = minute, fill = team.name) +
              geom_histogram_interactive(bins = 30L) +
              scale_fill_hue(direction = 1) +
              labs(
                x = "Minute",
                y = "Number of duels",
                title = "Number of duel and fouls according to time",
                fill = "Team",
                shape = "Foul committed"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 15L,
                                          face = "bold",
                                          hjust = 0.5),
                axis.title.y = element_text(size = 12L,
                                            face = "bold"),
                axis.title.x = element_text(size = 12L,
                                            face = "bold")
              ) +
              geom_point_interactive(data = foul_event, aes(x = minute , y = 1,  shape = team.name,
                                                            tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                                            "\n",paste("Minute : ",minute, sep = ""),
                                                                            sep=""))) + 
              facet_wrap(vars(team.name), ncol = 1L) 
            
            
            girafe(ggobj=plot_duel,height_svg = 4,
                   options=list(opts_zoom(min = .7, max = 5),
                                opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            
          })
          # plot pass third match   -------
          output$plot_pass_third_match <- renderPlot({
            
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            pass_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       type.name %in% "Pass"
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(pass_location = case_when(
                location.x <= 40 ~ "First 1/3",
                location.x >40 & location.x < 80 ~ "Second 1/3",
                location.x >= 80 ~ "Last 1/3")) %>% 
              group_by(team.name, pass_location) %>% 
              summarise(n = n()) %>% 
              arrange(desc(pass_location))   
            
            n1 <- pass_event$n[pass_event$team.name == data$home_team.country.name & pass_event$pass_location == "First 1/3" ]
            n2 <- pass_event$n[pass_event$team.name == data$home_team.country.name & pass_event$pass_location == "Second 1/3" ]
            n3 <- pass_event$n[pass_event$team.name == data$home_team.country.name & pass_event$pass_location == "Last 1/3" ]
            n4 <- pass_event$n[pass_event$team.name == data$away_team.country.name & pass_event$pass_location == "First 1/3" ]
            n5 <- pass_event$n[pass_event$team.name == data$away_team.country.name & pass_event$pass_location == "Second 1/3" ]
            n6 <- pass_event$n[pass_event$team.name == data$away_team.country.name & pass_event$pass_location == "Last 1/3" ]
            
            ggplot() +
              annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
              theme_pitch() + 
              geom_vline(data = match_event,aes(xintercept = 40)) +
              geom_vline(data = match_event,aes(xintercept = 80)) + 
              geom_circle(aes(x0=20, y0=60, r=10), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) + 
              geom_circle(aes(x0=60, y0=60, r=10), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=100, y0=60, r=10), 
                          color='black',  fill='#F8766D', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=20, y0=20, r=10), 
                          color='black',  fill='#00BFC4', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=60, y0=20, r=10), 
                          color='black',  fill='#00BFC4', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_circle(aes(x0=100, y0=20, r=10), 
                          color='black',  fill='#00BFC4', lwd=1.5, 
                          inherit.aes=FALSE) +
              geom_text(data = pass_event, x = 20 , y = 60 , 
                        label = n1, size = 6 ) +
              geom_text(data = pass_event, x = 60 , y = 60 , 
                        label = n2, size = 6 ) +
              geom_text(data = pass_event, x = 100 , y = 60 , 
                        label = n3, size = 6 ) +
              geom_text(data = pass_event, x = 20 , y = 20 , 
                        label = n4, size = 6 ) +
              geom_text(data = pass_event, x = 60 , y = 20 , 
                        label = n5, size = 6 ) +
              geom_text(data = pass_event, x = 100 , y = 20 , 
                        label = n6, size = 6 ) +
              labs(
                title = "Number of pass for 1/3 of the pitch"
              ) +
              theme(
                plot.title = element_text(size = 18L,
                                          face = "bold",
                                          hjust = 0.5)
              )
            
          })
          # Buteur match home team  --------------
          output$buteur_match_TH<-renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            goal_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       shot.outcome.name %in% "Goal"&
                       period %in% c ("1","2","3","4")) %>% 
              group_by(player.name,minute,team.name,shot.type.name,
              ) %>% 
              summarise(n = n()) %>% 
              arrange(minute)  
            
            
            goal_teamH<-goal_event %>% 
              filter(team.name %in% data$home_team.country.name) %>% 
              group_by(player.name, minute)
            
            goal_teamH %>% 
              select(-c(team.name, n))%>% 
              unite("Merged",minute:shot.type.name,
                    sep= "min, ", remove = TRUE) %>%
              unite("Buteur",player.name:Merged,
                    sep= " - ", remove = TRUE) %>% 
              formattable(align = c("l"))
            
          })
          # Buteur match away team  --------------
          output$buteur_match_TA<-renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            goal_event<-events %>% 
              filter(match_id %in% data$match_id  &
                       shot.outcome.name %in% "Goal"&
                       period %in% c ("1","2","3","4")) %>% 
              group_by(player.name,minute,team.name,shot.type.name,
              ) %>% 
              summarise(n = n()) %>% 
              arrange(minute)  
            
            
            goal_teamA<-goal_event %>% 
              filter(team.name %in% data$away_team.country.name) %>% 
              group_by(player.name, minute)
            
            goal_teamA %>% 
              select(-c(team.name, n))%>% 
              unite("Merged",minute:shot.type.name,
                    sep= "min, ", remove = TRUE) %>%
              unite("Buteur",player.name:Merged,
                    sep= " - ", remove = TRUE) %>% 
              formattable(align = c("l"))
            
          })
          
          # Plot shot target  team  --------
          output$team_shot_target<-renderPlot({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            shot<-events %>% 
              filter(match_id %in% data$match_id & type.name %in% "Shot") %>% 
              mutate(shot_target = case_when(
                shot.outcome.name != "Off T" ~ "On Target",
                shot.outcome.name == "Off T" ~ "Off Target")) %>% 
              group_by(team.name, shot_target) %>% 
              summarise(n = n()) 

            
            n1 <- paste(round(shot$n[shot$team.name == data$home_team.country.name & shot$shot_target == "Off Target" ],1), " shots", sep = "")
            n2 <- paste(round(shot$n[shot$team.name == data$home_team.country.name & shot$shot_target == "On Target" ],1), " shots", sep = "")
            n3 <- paste(round(shot$n[shot$team.name == data$away_team.country.name & shot$shot_target == "Off Target" ],1), " shots", sep = "")
            n4 <- paste(round(shot$n[shot$team.name == data$away_team.country.name & shot$shot_target == "On Target" ],1), " shots", sep = "")
            
            
            
            image_url <- "https://media.istockphoto.com/id/1403689288/fr/vectoriel/but-soccer-1.jpg?s=612x612&w=0&k=20&c=T7dPGX6iqIXRLAYVnEqGnKsvaeWgbMBhb9FOkpD5oVM="
            pic <- image_read(image_url)
            
            
            ggplot(mapping = aes(1:10, 1:10)) +
              annotation_raster(pic, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
              geom_point(color = "transparent")+
              geom_circle(aes(x0=3.5, y0=7, r=0.8), 
                          color='black',  fill='#F8766D', lwd=1, 
                          inherit.aes=TRUE)+
              geom_circle(aes(x0=3.5, y0=11.5, r=0.75), 
                          color='black',  fill='#F8766D', lwd=1, 
                          inherit.aes=FALSE)+
              geom_circle(aes(x0=7.5, y0=7, r=0.8), 
                          color='black',  fill='#00BFC4', lwd=1, 
                          inherit.aes=TRUE)+
              geom_circle(aes(x0=7.5, y0=11.5, r=0.75), 
                          color='black',  fill='#00BFC4', lwd=1, 
                          inherit.aes=FALSE)+
              theme_void() +
              geom_text(data = shot, x = 3.5 , y = 7 , 
                        label = n2, size = 6 ) +
              geom_text(data = shot, x = 3.5 , y = 11.5 , 
                        label = n1, size = 6 )+
              geom_text(data = shot, x = 7.5 , y = 7 , 
                        label = n4, size = 6 ) +
              geom_text(data = shot, x = 7.5 , y = 11.5 , 
                        label = n3, size = 6 ) +
              geom_text(data = shot, x = 5.5 , y = 11.5 , 
                        label = "Off Target", size = 6, color = "red" )+
              geom_text(data = shot, x = 5.5 , y = 7 , 
                        label = "On Target", size = 6 , color = "darkgreen")
            
            
            
          }, bg="transparent")
          # startingXI_TH  -----------
          output$startingXI_TH<-renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            starting_team<-events %>% 
              filter(match_id %in% data$match_id) 
            
            starting_team<-starting_team[[24]][[1]]
            
            starting_team %>% 
              select(-c(jersey_number, player.id, position.id))%>% 
              rename("Player" = player.name,
                     "Position" = position.name) %>% 
              formattable(align = c("c","c"),
                          list(
                            'Position' = formatter("span", 
                                                   style = ~ style(color = "black",font.weight = "bold")
                            )))
          })
          # startingXI_TA  -----------
          output$startingXI_TA<-renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            starting_team<-events %>% 
              filter(match_id %in% data$match_id) 
            
            starting_team<-starting_team[[24]][[2]]
            
            starting_team %>% 
              select(-c(jersey_number, player.id, position.id))%>% 
              rename("Player" = player.name,
                     "Position" = position.name) %>% 
              formattable(align = c("c","c"),
                          list(
                            'Position' = formatter("span", 
                                                   style = ~ style(color = "black",font.weight = "bold")
                            )))
          })
          
          # Possession team home / team_away  ------
          output$possession_TH<-renderText({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            
            possession_TH<-events %>% 
              filter(match_id %in% data$match_id &
                       TimeInPoss) %>% 
              group_by(team.name) %>% 
              summarise(n = n()) %>% 
              mutate(per = prop.table(n) * 100) %>% 
              filter(team.name %in% data$home_team.country.name)
            
            paste(round(possession_TH$per,1))
            
          })
            
            output$possession_TA<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              
              possession_TA<-events %>% 
                filter(match_id %in% data$match_id &
                         TimeInPoss) %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                filter(team.name %in% data$away_team.country.name)
              
              paste(round(possession_TA$per,1))
              
            
          })
            
            output$possession<-renderText({
              paste ("Possession (%)")
              
            })
          # Pass TH / TA  textoutput  ---------------
            output$pass_TH<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass_TH<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Pass") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$home_team.country.name)
              
              paste(round(pass_TH$n,1))
              
              
              
            })
            
            output$pass_TA<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass_TA<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Shot") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$away_team.country.name)
              
              paste(round(pass_TA$n,1))
              
              
              
            })
            
            output$pass_per<-renderText({
              paste ("Pass (n)")
            })
            
            
            
            
          # Shot TH / TA  textoutput  ---------------
            output$shot_TH<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              shot_TH<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Shot") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$home_team.country.name)
              
              paste(round(shot_TH$n,1))
              
              
              
            })
            
            output$shot_TA<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              shot_TA<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Shot") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$away_team.country.name)
              
              paste(round(shot_TA$n,1))
              
              
              
            })
            
            output$shot_per<-renderText({
              paste ("Shots (n)")
            })
            
            
            
          # Duel TH / TA  textoutput  ---------------
            output$duel_TH<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              duel_TH<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Duel") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$home_team.country.name)
              
              paste(round(duel_TH$n,1))
              
              
              
            })
            
            output$duel_TA<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              duel_TA<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Duel") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$away_team.country.name)
              
              paste(round(duel_TA$n,1))
              
              
              
            })
            
            output$duel_per<-renderText({
              paste ("Duel (n)")
            })
            
            
            
            
          # Interception TH / TA  textoutput  ---------------
            output$interception_TH<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              interception_TH<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Interception") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$home_team.country.name)
              
              paste(round(interception_TH$n,1))
              
              
              
            })
            
            output$interception_TA<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              interception_TA<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Shot") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$away_team.country.name)
              
              paste(round(interception_TA$n,1))
              
              
              
            })
            
            output$interception_per<-renderText({
              paste ("Interception (n)")
            })
            
            
            
            
          # Foul TH / TA  textoutput  ---------------
            output$foul_TH<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              foul_TH<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Foul Committed") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$home_team.country.name)
              
              paste(round(foul_TH$n,1))
              
              
              
            })
            
            output$foul_TA<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              foul_TA<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Foul Committed") %>% 
                group_by(team.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))%>% 
                filter(team.name %in% data$away_team.country.name)
              
              paste(round(foul_TA$n,1))
              
              
              
            })
            
            output$foul_per<-renderText({
              paste ("Foul (n)")
            })
            
          # Plot penalty TH --------
            output$penalty_TH<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              shot<-events %>% 
                filter(match_id %in% data$match_id &
                         type.name %in% "Shot" &
                         period %in% "5" & team.name %in% data$home_team.country.name) %>% 
                mutate(goal.seq = 1:length(shot.outcome.name))
              
              post(fill_background = "white")+
                geom_point(shot, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, 
                                               color = shot.outcome.name, shape = shot.type.name), size = 5)+
                geom_text(shot, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, label = goal.seq),
                          size = 3, color = "darkslategray")+
                theme(
                  legend.position = "top",
                  plot.title = element_text(hjust=0.5, vjust = -5, size = 15),
                  plot.subtitle =  element_text(hjust=0.5, vjust = -5),
                  text = element_text(color = "black")
                )+
                labs(color = "Shot Outcome",
                     title = paste("Penalty for ", data$home_team.country.name, sep = ""), shape = NULL,
                     subtitle = paste(data$home_team.country.name, data$away_team.country.name,
                                      sep = " vs "))+
                scale_color_manual(values = c("darkgreen", "red", "blue", "yellow", "black", "orange"))+
                scale_shape_manual(values = c(16, 15, 17,25))
              
              
            })
            
            
            
            
          # Plot penalty TA --------
            output$penalty_TA<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              shot<-events %>% 
                filter(match_id %in% data$match_id &
                         type.name %in% "Shot" &
                         period %in% "5" & team.name %in% data$away_team.country.name) %>% 
                mutate(goal.seq = 1:length(shot.outcome.name))
              
              post(fill_background = "white")+
                geom_point(shot, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, 
                                               color = shot.outcome.name, shape = shot.type.name), size = 5)+
                geom_text(shot, mapping = aes(x = shot.end_location.y, y = shot.end_location.z, label = goal.seq),
                          size = 3, color = "darkslategray")+
                theme(
                  legend.position = "top",
                  plot.title = element_text(hjust=0.5, vjust = -5, size = 15),
                  plot.subtitle =  element_text(hjust=0.5, vjust = -5),
                  text = element_text(color = "black")
                )+
                labs(color = "Shot Outcome", 
                     title = paste("Penalty for ", data$away_team.country.name, sep = "") , shape = NULL,
                     subtitle = paste(data$home_team.country.name, data$away_team.country.name,
                                      sep = " vs "))+
                scale_color_manual(values = c("darkgreen", "red", "blue", "yellow", "black", "orange"))+
                scale_shape_manual(values = c(16, 15, 17,25))
              
              
            })
            
            
            
            
            
          # player penalty TH  --------------
            output$player_penalty_TH<-renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)  
            
            
            shot<-events %>% 
              filter(match_id %in% data$match_id &
                       type.name %in% "Shot" &
                       period %in% "5" & team.name %in% data$home_team.country.name) %>% 
              group_by(player.name,shot.outcome.name) %>% 
              summarise(n=n())
            
            
            shot %>% 
              select(-c( n))%>% 
              rename("Player" = player.name,
                     "Shot finality" = shot.outcome.name) %>% 
              formattable(align = c("c","c"),
                          list(
                            'Position' = formatter("span", 
                                                   style = ~ style(color = "black",font.weight = "bold")
                            )))
            })
            
            
          # player penalty TA  --------------
            output$player_penalty_TA<-renderFormattable({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)  
              
              
              shot<-events %>% 
                filter(match_id %in% data$match_id &
                         type.name %in% "Shot" &
                         period %in% "5" & team.name %in% data$away_team.country.name) %>% 
                group_by(player.name,shot.outcome.name) %>% 
                summarise(n=n())
              
              
              shot %>% 
                select(-c( n))%>% 
                rename("Player" = player.name,
                       "Shot finality" = shot.outcome.name) %>% 
                formattable(align = c("c","c"),
                            list(
                              'Position' = formatter("span", 
                                                     style = ~ style(color = "black",font.weight = "bold")
                              )))
            })
            
            
            
          # score penalty  --------------
            output$score_penalty2<-renderText({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              goal_event<-events %>% 
                filter(match_id %in% data$match_id  &
                         shot.outcome.name %in% "Goal" &
                         period %in%  "5") %>% 
                group_by(team.name) %>% 
                summarise(n = n())
              
              
              
              paste(goal_event$n[goal_event$team.name == data$home_team.country.name],
                    goal_event$n[goal_event$team.name == data$away_team.country.name],
                    sep = " tab a ")
              
            })
            
          # Duel player won -----------
          output$duel_player_won <- renderFormattable({
            data<-Matches %>% 
              filter(home_team.home_team_name %in% input$choice_team |
                       away_team.away_team_name %in% input$choice_team) %>% 
              select(match_id,home_team.country.name,away_team.country.name,
                     home_score, away_score) %>% 
              unite("Merged",home_team.country.name:away_team.country.name,
                    sep= " / ", remove = FALSE)
            
            data<-data %>% 
              filter(Merged %in% input$match_selection)
            
            
            duel<-events %>% 
              filter(team.name %in% input$choice_team 
                     & type.name %in% "Duel"
                     & match_id %in% data$match_id
                     & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
              mutate(duel_WL = case_when(
                duel.outcome.name == "Success In Play" ~ "Won",
                duel.outcome.name == "Won" ~ "Won",
                duel.outcome.name == "Lost In Play" ~ "Lost",
                duel.outcome.name == "Lost Out" ~ "Lost",
                duel.outcome.name == "Success Out" ~ "Won")) %>% 
              group_by(player.name,duel_WL) %>% 
              summarise(n=n())%>% 
              mutate(per = round(prop.table(n) * 100,1))
            
            duel_win<-duel %>% 
              filter(duel_WL %in% "Won") %>% 
              select(-contains(c("duel_WL", "per"))) %>% 
              arrange(desc(n))  %>%       
              rename("Player" = player.name,
                     "Number of duels won" = n) %>% 
              formattable(align = c("c","c"),
                          list(
                            Percentage = color_bar( "lightblue"),
                            player.name = formatter("span", 
                                                    style = ~ style(color = "black",font.weight = "bold"))))
            
          })
          # Duel player lost -----------
            output$duel_player_lost <- renderFormattable({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              duel<-events %>% 
                filter(team.name %in% input$choice_team 
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & type.name %in% "Duel"
                       & match_id %in% data$match_id) %>% 
                mutate(duel_WL = case_when(
                  duel.outcome.name == "Success In Play" ~ "Won",
                  duel.outcome.name == "Won" ~ "Won",
                  duel.outcome.name == "Lost In Play" ~ "Lost",
                  duel.outcome.name == "Lost Out" ~ "Lost",
                  duel.outcome.name == "Success Out" ~ "Won")) %>% 
                group_by(player.name,duel_WL) %>% 
                summarise(n=n())%>% 
                mutate(per = round(prop.table(n) * 100,1))
              
              duel_win<-duel %>% 
                filter(duel_WL %in% "Lost") %>% 
                select(-contains(c("duel_WL", "per"))) %>% 
                arrange(desc(n))  %>%       
                rename("Player" = player.name,
                       "Number of duels lost" = n) %>% 
                formattable(align = c("c","c"),
                            list(
                              Percentage = color_bar( "lightblue"),
                              player.name = formatter("span", 
                                                      style = ~ style(color = "black",font.weight = "bold"))))
              
            })
            
          # duel_first_tiers  ------------
            output$duel_first_tiers<-renderText({
              paste("Duel in the first 1/3",sep = "")
            })
          # duel_second_tiers  ------------
            output$duel_second_tiers<-renderText({
              paste("Duel in the second 1/3",sep = "")
            })
            
          # duel_third_tiers  ------------
            output$duel_last_tiers<-renderText({
              paste("Duel in the last 1/3",sep = "")
            })
            
          # duel_first_tiers number  ------------
            output$duel_first_tiers_n<-renderText({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              duel_loca<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Duel"
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & team.name %in% input$choice_team) %>% 
                mutate(duel_WL = case_when(
                  duel.outcome.name == "Success In Play" ~ "Won",
                  duel.outcome.name == "Won" ~ "Won",
                  duel.outcome.name == "Lost In Play" ~ "Lost",
                  duel.outcome.name == "Lost Out" ~ "Lost",
                  duel.outcome.name == "Success Out" ~ "Won")) %>% 
                mutate(duel_location = case_when(
                  location.x <= 40 ~ "First 1/3",
                  location.x >40 & location.x < 80 ~ "Second 1/3",
                  location.x >= 80 ~ "Last 1/3")) %>% 
                group_by(team.name, duel_location) %>% 
                summarise(n = n()) %>% 
                arrange(desc(duel_location)) %>% 
                na.omit()  
              
              duel_loca<-duel_loca %>% 
                filter (duel_location %in% "First 1/3")
              
              paste(duel_loca$n,sep = "")
            })
            
          # duel_second_tiers number  ------------
            output$duel_second_tiers_n<-renderText({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              duel_loca<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Duel"
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & team.name %in% input$choice_team) %>% 
                mutate(duel_WL = case_when(
                  duel.outcome.name == "Success In Play" ~ "Won",
                  duel.outcome.name == "Won" ~ "Won",
                  duel.outcome.name == "Lost In Play" ~ "Lost",
                  duel.outcome.name == "Lost Out" ~ "Lost",
                  duel.outcome.name == "Success Out" ~ "Won")) %>% 
                mutate(duel_location = case_when(
                  location.x <= 40 ~ "First 1/3",
                  location.x >40 & location.x < 80 ~ "Second 1/3",
                  location.x >= 80 ~ "Last 1/3")) %>% 
                group_by(team.name, duel_location) %>% 
                summarise(n = n()) %>% 
                arrange(desc(duel_location))%>% 
                na.omit()   
              
              duel_loca<-duel_loca %>% 
                filter (duel_location %in% "Second 1/3")
              
              paste(duel_loca$n,sep = "")
            })
            
            
          # duel_third_tiers number  ------------
            output$duel_last_tiers_n<-renderText({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              duel_loca<-events %>% 
                filter(match_id %in% data$match_id  &
                         type.name %in% "Duel"
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & team.name %in% input$choice_team) %>% 
                mutate(duel_WL = case_when(
                  duel.outcome.name == "Success In Play" ~ "Won",
                  duel.outcome.name == "Won" ~ "Won",
                  duel.outcome.name == "Lost In Play" ~ "Lost",
                  duel.outcome.name == "Lost Out" ~ "Lost",
                  duel.outcome.name == "Success Out" ~ "Won")) %>% 
                mutate(duel_location = case_when(
                  location.x <= 40 ~ "First 1/3",
                  location.x >40 & location.x < 80 ~ "Second 1/3",
                  location.x >= 80 ~ "Last 1/3")) %>% 
                group_by(team.name, duel_location) %>% 
                summarise(n = n()) %>% 
                arrange(desc(duel_location)) %>% 
                na.omit()  
              
              duel_loca<-duel_loca %>% 
                filter (duel_location %in% "Last 1/3")
              
              paste(duel_loca$n,sep = "")
            })
            
            
            
          # plot_foul_committed_team  -------------
            output$plot_foul_committed_team<-renderggiraph({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team 
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & match_id %in% data$match_id
                       & type.name %in% "Foul Committed")
              
              
              plot<-ggplot() +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_point_interactive(data = def_metrics,
                                       aes(x = location.x , y = location.y, 
                                           colour = type.name,
                                           tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                           "\n",paste("Position : ",position.name,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")))+
                theme(legend.position = "right") + 
                labs(title = paste("Foul committed during the match",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
              
              
            })
          # table foul committed team  ------------
            output$table_foul_committed_team<-renderFormattable({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & type.name %in% "Foul Committed") %>% 
                group_by(player.name) %>%
                summarise(n=n())%>% 
                arrange(desc(n)) %>%
                rename("Player" = player.name,
                       "Number of fouls" = n) %>% 
                formattable(align = c("c","c"),
                            list(
                              Percentage = color_bar( "lightblue"),
                              player.name = formatter("span", 
                      style = ~ style(color = "black",font.weight = "bold"))))
              
            }) 

          # plot_interception_team  -------------
            output$plot_interception_team<-renderggiraph({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team 
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & match_id %in% data$match_id
                       & type.name %in% "Interception")
              
              
              plot<-ggplot() +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_point_interactive(data = def_metrics,
                                       aes(x = location.x , y = location.y, 
                                           colour = type.name,
                                           tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                           "\n",paste("Position : ",position.name,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")))+
                theme(legend.position = "right") + 
                labs(title = paste("Interception during the match",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
              
              
            })
            
          # table interception committed team  ------------
            output$table_interception_team<-renderFormattable({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & type.name %in% "Interception") %>% 
                group_by(player.name) %>%
                summarise(n=n())%>% 
                arrange(desc(n)) %>% 
                rename("Player" = player.name,
                       "Number of interceptions" = n) %>% 
                formattable(align = c("c","c"),
                            list(
                              Percentage = color_bar( "lightblue"),
                              player.name = formatter("span", 
                                                      style = ~ style(color = "black",font.weight = "bold"))))
              
            }) 
            
            
          # plot_pressure_team  -------------
            output$plot_pressure_team<-renderggiraph({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team 
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & match_id %in% data$match_id
                       & type.name %in% "Pressure")
              
              
              plot<-ggplot() +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_point_interactive(data = def_metrics,
                                       aes(x = location.x , y = location.y, 
                                           colour = type.name,
                                           tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                           "\n",paste("Position : ",position.name,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")))+
                theme(legend.position = "right") + 
                labs(title = paste("Pressure events during the match",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
              
              
            })
            
          # table pressure committed team  ------------
            output$table_pressure_team<-renderFormattable({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & type.name %in% "Pressure") %>% 
                group_by(player.name) %>%
                summarise(n=n())%>% 
                arrange(desc(n)) %>% 
                rename("Player" = player.name,
                       "Number of pressure events" = n) %>% 
                formattable(align = c("c","c"),
                            list(
                              Percentage = color_bar( "lightblue"),
                              player.name = formatter("span", 
                                                      style = ~ style(color = "black",font.weight = "bold"))))
              
            }) 
            
            
            
          # plot_behavior_pressure_team  -----------
            output$plot_behavior_pressure_team<-renderGirafe({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              behavior_pressure<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & under_pressure %in% TRUE) %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(type.name, player.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100),1)%>% 
                arrange(desc(per))%>% 
                select(player.name,type.name,per,n)  
              
              
              
              p<-ggplot() +
                geom_col_interactive( data = behavior_pressure,
                                      aes(x = player.name , y = n, 
                                          fill = type.name,
                                          tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                          "\n",paste("Behavior : ",type.name,sep = ""),
                                                          "\n",paste("Count : ",n,sep = ""),
                                                          sep=""))) +
                scale_fill_hue(direction = 1) +
                labs(
                  x = "Number of event under pressure",
                  y = "Players",
                  title = "Players' behavior under pressure",
                  fill = "Behaviors"
                ) +
                coord_flip() +
                theme_minimal() +
                theme(
                  plot.title = element_text(size = 12L,
                                            face = "bold",
                                            hjust = 0.5),
                  axis.title.y = element_text(size = 12L,
                                              face = "bold"),
                  axis.title.x = element_text(size = 12L,
                                              face = "bold")
                )
              
              
              girafe(ggobj=p,
                     options = list(
                       opts_hover_inv(css = "opacity:0.1;"),
                       opts_hover(css = "stroke-width:2;"),
                       opts_selection(only_shiny = FALSE, type = "single", css = "stroke:yellow;")
                     ))
            })
          # plot_5player_pass_pressure  -----------
            output$plot_5player_pass_pressure<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              behavior_pressure<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & under_pressure %in% TRUE
                       & type.name %in% "Pass") %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(player.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100),1)%>% 
                arrange(desc(per))%>% 
                slice(1:5)
              
              behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                      levels = behavior_pressure$player.name[order(behavior_pressure$per)])
              
              ggplot(behavior_pressure) +
                aes(x = player.name, weight = per) +
                geom_bar(fill = "#112446") +
                labs(x = "Player", y = "Percentage of pass under pressure") +
                coord_flip() +
                theme_minimal() +
                theme(
                  axis.title.y = element_text(size = 12L,
                                              face = "bold"),
                  axis.title.x = element_text(size = 12L,
                                              face = "bold")
                )
              
              
            })
            
          # plot_5player_dribble_pressure  -----------
            output$plot_5player_dribble_pressure<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              behavior_pressure<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & under_pressure %in% TRUE
                       & type.name %in% "Dribble") %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(player.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100),1)%>% 
                arrange(desc(per))%>% 
                slice(1:5)
              
              behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                      levels = behavior_pressure$player.name[order(behavior_pressure$per)])
              
              ggplot(behavior_pressure) +
                aes(x = player.name, weight = per) +
                geom_bar(fill = "#112446") +
                labs(x = "Player", y = "Percentage of dribble under pressure") +
                coord_flip() +
                theme_minimal() +
                theme(
                  axis.title.y = element_text(size = 12L,
                                              face = "bold"),
                  axis.title.x = element_text(size = 12L,
                                              face = "bold")
                )
              
              
            })
            
            
          # plot_5player_carry_pressure  -----------
            output$plot_5player_carry_pressure<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              behavior_pressure<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & under_pressure %in% TRUE
                       & type.name %in% "Carry") %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(player.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100),1)%>% 
                arrange(desc(per))%>% 
                slice(1:5)
              
              behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                      levels = behavior_pressure$player.name[order(behavior_pressure$per)])
              
              ggplot(behavior_pressure) +
                aes(x = player.name, weight = per) +
                geom_bar(fill = "#112446") +
                labs(x = "Player", y = "Percentage of carry under pressure") +
                coord_flip() +
                theme_minimal() +
                theme(
                  axis.title.y = element_text(size = 12L,
                                              face = "bold"),
                  axis.title.x = element_text(size = 12L,
                                              face = "bold")
                )
              
              
            })
            
            
            
          # plot_5player_carry_pressure  -----------
            output$plot_5player_dispossessed_pressure<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              behavior_pressure<-events %>% 
                filter(team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & under_pressure %in% TRUE
                       & type.name %in% "Dispossessed") %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(player.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100),1)%>% 
                arrange(desc(per))%>% 
                slice(1:5)
              
              behavior_pressure$player.name <- factor(behavior_pressure$player.name,                                    # Factor levels in increasing order
                                                      levels = behavior_pressure$player.name[order(behavior_pressure$per)])
              
              ggplot(behavior_pressure) +
                aes(x = player.name, weight = per) +
                geom_bar(fill = "#112446") +
                labs(x = "Player", y = "Percentage of dispossessed under pressure") +
                coord_flip() +
                theme_minimal() +
                theme(
                  axis.title.y = element_text(size = 12L,
                                              face = "bold"),
                  axis.title.x = element_text(size = 12L,
                                              face = "bold")
                )
              
              
            })
            
            
            
            
            
        }
        
        # Fermeture observe event match selection----------
      })
      
      }  else if (input$player_choice_1 != "all the team"){
        observeEvent(input$match_selection, {
          if (input$match_selection == "Summary all matches"){
      # pass distance tendancy pie  ----------
      
      output$pass_distance_tendancy<-renderPlotly({
      pass_length<-events %>% 
        filter(type.name %in% "Pass" &
                 team.name %in% input$choice_team 
               & minute %in% (input$slider_min[1]:input$slider_min[2])
               & player.name %in% input$player_choice_1) %>% 
        mutate(category=cut(pass.length*0.9144, breaks=c(0, 15, 30,100), 
                            labels=c("short","medium","long"))) %>% 
        group_by(category) %>% 
        summarise(n = n()) %>% 
        mutate(per = prop.table(n) * 100)
      
      
      p <- plot_ly() %>%
        add_pie(data = pass_length, labels = ~paste(category,"pass",sep = " "), values = ~per, hole = 0.6,
                name = "a",textposition = 'inside',textinfo = 'label+percent',
                showlegend = FALSE) %>% 
        layout(title = 'Pass distance tendancy (m)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)")
      p
      
      })
      
      
      # pass height tendancy pie  ----------
      
      output$pass_height_tendancy<-renderPlotly({
        pass_height<-events %>% 
          filter(type.name %in% "Pass" &
                   team.name %in% input$choice_team &
                   minute %in% (input$slider_min[1]:input$slider_min[2])
                   & player.name %in% input$player_choice_1) %>% 
          group_by(pass.height.name) %>% 
          summarise(n = n()) %>% 
          mutate(per = prop.table(n) * 100)
        
        
        p <- plot_ly() %>%
          add_pie(data = pass_height, labels = ~pass.height.name, values = ~per, hole = 0.6,
                  name = "a",textposition = 'inside',textinfo = 'label+percent',
                  showlegend = FALSE) %>% 
          layout(title = 'Pass height tendancy',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)")
        p
        
      })
      
      
      
      # Plot sonar player  -----------
output$sonar_team<-renderPlot({
  #filter to one competion and include only open play passes, and round pass angle
  #to nearest x degrees
  round.angle=15
  pass<-events %>% 
    filter(type.name %in% "Pass")
  
  NWSL.passes<-pass%>%
    filter( ! play_pattern.name %in% c("From Corner", "From Free Kick", "From Throw In"))%>%
    mutate(angle.round=round(pass.angle*180/pi/round.angle)*round.angle)
  
  #create data frame for sonars per player and team for a season
  #this normalizes the most frequent pass angle to 1 and all other angles relative to that angle
  #it also associates pass distance with each angle, but this can be changed to anything
  #such as pass success or any other metric
  sonar=NWSL.passes%>%
    group_by(player.name)%>%
    mutate(N=n())%>%
    ungroup()%>%
    group_by(player.name,team.name, angle.round)%>%
    mutate(n.angle=n()/N)%>%
    ungroup()%>%
    group_by(player.name)%>%
    mutate(maxN=max(n.angle),
           angle.norm=n.angle/maxN)%>%
    ungroup()%>%
    group_by(angle.round, player.name,team.name,N)%>%
    summarize(angle.norm = mean(angle.norm),
              distance = mean(pass.length),# pour convertir yard en m
              distance = ifelse(distance>30, 30,distance))
  
  
  #plot sonar for a single player
  #depending on the data source, you may have to change the start and direction
  #for the polar coordinates
  #hint: choose an outside back or midfielder to verify these parameters
  players=unique(sonar$player.name)
  
  ggplot(sonar%>%filter(player.name == input$player_choice_1))+
    geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
    scale_y_continuous(limits=c(0,1))+
    scale_x_continuous(breaks=seq(-180,180, by=45), limits=c(-180,180))+
    coord_polar(start=pi, direction=1)+
    labs(x='', y='',title= input$player_choice_1)+
    annotate("text", label='Bar length = normalized pass angle frequency; Bar color = mean pass distance', x=-2, y=79, hjust=1, size=3)+
    theme(axis.text.y=element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())+
    theme(plot.title = element_text(hjust=0.5),
          #legend.position = "none", #uncomment to remove colorbar
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "lightgrey",colour = NA))
  
}, bg="transparent")
      # Fonction big donut  ---------
big_number_donut_plot <- function(value, font_family, highlight_color) {
  
  # Wrangle data to get a data frame in the format we need it in to make our donut chart
  df <- tibble(x = 1, y = value) %>% 
    mutate(y_negative = 1 - y) %>% 
    pivot_longer(cols = -x) 
  
  # Create a nicely formatted big number to go in the donut hole
  big_number_text_label <- percent(value, accuracy = 1)
  
  # Create our plot
  ggplot(df,
         aes(x = x,
             y = value,
             fill = name)) +
    
    # Add a bar, but don't add the legend
    geom_col(show.legend = FALSE) +
    
    # A pie/donut chart is a bar chart with polar coordinates
    # Add polar coordinates and set the direction to -1 
    # so the filled in part starts at the top and goes clockwise
    coord_polar(theta = "y",
                direction = -1) +
    
    
    # Set the limits, which is important for adding the hole
    xlim(c(-2, 2)) +
    
    # Set a color scale with the highlighted section in whatever color
    # is chosen with the highlight_color argument and the rest in a light gray
    scale_fill_manual(values = c(highlight_color, "grey90")) +
    
    # Set theme_void() to remove grid lines and everything else from the plot
    theme_void() +
    
    labs(title = paste("Successful passes",sep = " "))+
    theme(plot.title = element_text(hjust = 0.5, vjust = -85,color = "black",
                                    size = 12)) +
    
    # Add the big number in the center of the hole
    annotate("text",
             label = big_number_text_label,
             family = font_family,
             fontface = "bold",
             color = highlight_color,
             size = 12,
             x = -2,
             y = 0) 
  
}



      # Plot successful passes  -------------------
output$successful_passes<-renderPlotly({
  pass_completion<-events %>% 
    filter(type.name %in% "Pass" &
             team.name %in% input$choice_team 
           & minute %in% (input$slider_min[1]:input$slider_min[2])
            & player.name %in% input$player_choice_1 ) %>%
    mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
    group_by(pass.outcome.name) %>% 
    summarise(n = n()) %>% 
    mutate(per = round(prop.table(n) * 100))
  
  p <- plot_ly() %>%
    add_pie(data = pass_completion, labels = ~paste(pass.outcome.name,"pass",sep = " "), values = ~per, hole = 0.6,
            name = "a",textposition = 'inside',textinfo = 'label+percent',
            showlegend = FALSE) %>% 
    layout(title = "Pass distribution",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           fig_bgcolor   = "rgba(0, 0, 0, 0)",
           annotations=list(text=paste(paste(pass_completion$per[1], " %", sep=""),
                                       sep = ""), 
                            "showarrow"=F, font=list(size = 20)))
  
  p
  
  
})
      # Plot pass player  -------------
output$pass_player<-renderGirafe({
  pass_player_game<-events %>% 
    filter(type.name %in% "Pass" & player.name %in% input$player_choice_1
           & !(pass.outcome.name %in% "Unknown")) %>% 
    mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
    group_by (OpposingTeam, pass.outcome.name) %>% 
    summarise(n=n()) %>% 
    mutate(per = prop.table(n) * 100)
  
  
  pass_player<-pass_player_game %>% 
    summarize(total = sum(n))
  
  
  plot<- ggplot(pass_player_game,
                aes(x = OpposingTeam, y = n, fill = pass.outcome.name,
                    tooltip = paste(pass.outcome.name," pass : ",n,
                                    "\n",pass.outcome.name, " pass (%) : " ,round(per,1),
                                    sep=""))) +
    geom_col_interactive(width = 0.5) +
    scale_fill_manual(
      values = c(Complete = "#3BCB65",
                 Incomplete = "#E50F08",
                 Out = "#00C19F",
                 `Pass Offside` = "#619CFF",
                 Unknown = "#FF61C3")
    ) +
    labs(x = element_blank(), y = element_blank(),
         fill = "Pass type",  title = paste("Pass by match",sep = " "))+
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()
          #remove y axis ticks
    )+
    geom_text(data = pass_player,
              aes(x = OpposingTeam, label = total, y = total, fill = NULL), 
              inherit.aes = FALSE, vjust = - 0.4) +
    theme(legend.position = "right")
  
  
  girafe(ggobj=plot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_hover(css = "fill:yellow;stroke:black;stroke-width:3px;"),
                      opts_tooltip(css="background-color:#000080;color:white;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))   
  
  
})
      # Plot target zone  ----------------
output$target_zone<-renderPlot({
  pass_target<-events %>% 
    filter(type.name %in% "Pass" & player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  ggplot(pass_target) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = 'white') +
    theme_pitch() + 
    geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, fill = ..level..), 
                          alpha=0.8, contour_var = 'ndensity') + 
    theme(legend.position = "none",
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()) + 
    labs(title = paste("Passes target zone",sep = " "),
         caption = "Data Source: StatsBomb")+
    theme(plot.title = element_text(hjust = 0.5))
  
}, bg="transparent")

      # Pass pref player datatable  -------------
output$pass_pref_player<-renderDataTable({
  pass_player<-events %>% 
    filter(type.name %in% "Pass" &
             team.name %in% input$choice_team 
           & minute %in% (input$slider_min[1]:input$slider_min[2])
            & player.name %in% input$player_choice_1) %>% 
    group_by (pass.recipient.name) %>% 
    summarise(n=n()) %>% 
    mutate(per = round(prop.table(n) * 100,1)) %>% 
    arrange(desc(per)) 
  
  datatable(pass_player,colnames = c("Player","Number of pass","%"),
            options = list(dom = 't',
                           columnDefs = list(list(className = 'dt-center', 
                                                  targets = 0:3))
            ))
})

      # Plot shot and goal assit  -------
output$plot_shot_assist_player<-renderGirafe({
  pass<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Pass"
           & pass.shot_assist %in% TRUE & 
             player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  
  plot<-ggplot(pass) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                 yend = pass.end_location.y,
                                 tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                 "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                 "\n",paste("Minute : ",minute, sep = ""),
                                                 sep="")),
                             colour = "#FFA500",
                             arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                             ))+
    scale_colour_identity()+
    theme(legend.position = "none") + 
    labs(title = paste(input$player_choice_1," shot assist direction during the competition",sep = " "),
         caption = "Data Source: StatsBomb") +
    theme(plot.title = element_text(hjust = 0.5))
  
  girafe(ggobj=plot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
})

output$plot_shot_assist_xg_player<-renderGirafe({
  pass1<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Pass"
           & pass.shot_assist %in% TRUE & 
             player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  shot_player_xg<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Shot"
           & player.name %in% pass1$pass.recipient.name &
             OpposingTeam %in%  pass1$OpposingTeam &
             minute %in% pass1$minute)
  
  
  plot_shot<-ggplot(shot_player_xg) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_segment_interactive(data = shot_player_xg, aes(x = location.x, y = location.y, xend = shot.end_location.x, 
                                                        yend = shot.end_location.y,
                                                        tooltip = paste(paste("Densitiy incone : ",density.incone, sep = ""),
                                                                        "\n",paste("Distance first defender : ",distance.ToD1,sep = ""),
                                                                        "\n",paste("Defenders incone : ",DefendersInCone, sep = ""),
                                                                        "\n",paste("xG value : ",shot.statsbomb_xg, sep = ""),
                                                                        sep="")),
                             colour = "green",
                             arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                             ))+
    scale_colour_identity()+
    theme(legend.position = "none") + 
    labs(title = paste("Shot following assist from ",input$player_choice_1,sep = " "),
         caption = "Data Source: StatsBomb") +
    theme(plot.title = element_text(hjust = 0.5))
  
  girafe(ggobj=plot_shot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
  
})

output$plot_goal_assist_player<-renderGirafe({
  pass<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Pass"
           & pass.goal_assist %in% TRUE & 
             player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  
  plot<-ggplot(pass) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                 yend = pass.end_location.y,
                                 tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                 "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                 "\n",paste("Minute : ",minute, sep = ""),
                                                 sep="")),
                             colour = "#FFA500",
                             arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                             ))+
    scale_colour_identity()+
    theme(legend.position = "none") + 
    labs(title = paste(input$player_choice_1," Goal assist during the competition",sep = " "),
         caption = "Data Source: StatsBomb") +
    theme(plot.title = element_text(hjust = 0.5))
  
  girafe(ggobj=plot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
})

output$plot_goal_assist_xg_player<-renderGirafe({
  pass1<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Pass"
           & pass.goal_assist %in% TRUE & 
             player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  shot_player_xg<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Shot"
           & player.name %in% pass1$pass.recipient.name &
             OpposingTeam %in%  pass1$OpposingTeam &
             minute %in% pass1$minute)
  
  
  plot_shot<-ggplot(shot_player_xg) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_segment_interactive(data = shot_player_xg, aes(x = location.x, y = location.y, xend = shot.end_location.x, 
                                                        yend = shot.end_location.y,
                                                        tooltip = paste(paste("Densitiy incone : ",density.incone, sep = ""),
                                                                        "\n",paste("Distance first defender : ",distance.ToD1,sep = ""),
                                                                        "\n",paste("Defenders incone : ",DefendersInCone, sep = ""),
                                                                        "\n",paste("xG value : ",shot.statsbomb_xg, sep = ""),
                                                                        sep="")),
                             colour = "green",
                             arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                             ))+
    scale_colour_identity()+
    theme(legend.position = "none") + 
    labs(title = paste("Goal following assist from ",input$player_choice_1,sep = " "),
         caption = "Data Source: StatsBomb") +
    theme(plot.title = element_text(hjust = 0.5))
  
  girafe(ggobj=plot_shot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
  
  
  
  
})

      # Behavior under pressure  ----------------
output$behavior_under_pressure<-renderPlotly({
  pass1<-events %>% 
    filter(team.name %in% input$choice_team
           & under_pressure %in% TRUE & player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
    mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
    group_by(type.name) %>% 
    summarise(n = n()) %>% 
    mutate(per = round(prop.table(n) * 100),1)%>% 
    arrange(desc(per))%>% 
    select(type.name,per)  
  
  suite <- pass1$type.name=="Pass"
  suite<-as.integer(as.logical(suite))
  suite<-as.data.frame(suite)
  suite<-suite %>% 
    mutate(suite = replace(suite, suite == 1, 0.2))
  
  
  p <- plot_ly() %>%
    add_pie(data = pass1, labels = ~paste(type.name,sep = " "), values = ~per, hole = 0.6,
            name = "a",textposition = 'inside',textinfo = 'label+percent',
            showlegend = FALSE, 
            pull = suite$suite) %>% 
    layout(title = 'Behavior under pressure',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           fig_bgcolor   = "rgba(0, 0, 0, 0)")
  
  p
})

      # Plot pass under pressure player  ------------------
output$plot_pass_underpressure_player<-renderGirafe({
  pass2<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Pass"
           & under_pressure %in% TRUE & player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
    mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
    group_by(position.name)
  
  
  plot<-ggplot(pass2) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_segment_interactive(data = pass2, aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                               yend = pass.end_location.y,
                                               colour = pass.outcome.name == "Incomplete",
                                               tooltip = paste("Player targeted : ",pass.recipient.name,
                                                               "\n","Distance (m) : " ,round(pass.length,1),
                                                               "\n",paste(team.name,OpposingTeam,sep = " / "),
                                                               sep="")),
                             arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                             ))+
    scale_colour_manual(values = c("#5e9a78","#ff4444"),
                        labels = levels(as.factor(pass2$pass.outcome.name)),
                        name = "How was the pass under pressure ?") +
    
    theme(legend.position = "top",
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()) + 
    labs(title = NULL,
         caption = "Data Source: StatsBomb") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  girafe(ggobj=plot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;
                          font-style:italic;border-radius:5px;
                          padding:5px;")))
  
})



      # Duel player defensive metrics  ------------
output$duel_player<-renderggiraph({

  def_metrics<-events %>% 
    filter(team.name %in% input$choice_team 
           & player.name %in% input$player_choice_1
           & type.name %in% c("Duel","Interception","Foul Committed", 
                              "Bad Behaviour")
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  
  plot<-ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_point_interactive(data = def_metrics,
                           aes(x = location.x , y = location.y, 
                               colour = type.name,
                               tooltip = paste(paste("Player : ",player.name, sep = ""),
                                               "\n",paste("Position : ",position.name,sep = ""),
                                               "\n",paste("Minute : ",minute, sep = ""),
                                               sep="")))+
    theme(legend.position = "right") + 
    labs(title = paste("Pressure during the match",sep = " "),
         caption = "Data Source: StatsBomb")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  girafe(ggobj=plot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
  
  
})
      # Pressure player  metrics  ------------
output$pressure_player<-renderggiraph({

  def_metrics<-events %>% 
    filter(team.name %in% input$choice_team 
           & player.name %in% input$player_choice_1
           & type.name %in% "Pressure"
           & minute %in% (input$slider_min[1]:input$slider_min[2]))
  
  
  plot<-ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
    theme_pitch() + 
    geom_point_interactive(data = def_metrics,
                           aes(x = location.x , y = location.y, 
                               colour = type.name,
                               tooltip = paste(paste("Player : ",player.name, sep = ""),
                                               "\n",paste("Position : ",position.name,sep = ""),
                                               "\n",paste("Minute : ",minute, sep = ""),
                                               sep="")))+
    theme(legend.position = "right") + 
    labs(title = paste("Defensive metrics during the match",sep = " "),
         caption = "Data Source: StatsBomb")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  girafe(ggobj=plot,height_svg = 4,
         options=list(opts_zoom(min = .7, max = 5),
                      opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
  
  
})


      # Favorite pass  --------------
output$favoritepass<-renderFormattable({
  pass_player<-events %>% 
    filter(team.name %in% input$choice_team & type.name %in% "Pass"
           & under_pressure %in% TRUE & 
             player.name %in% input$player_choice_1
           & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
    group_by (pass.recipient.name) %>% 
    drop_na(pass.recipient.name) %>% 
    summarise(Pass=n()) %>% 
    mutate(Percentage = round(prop.table(Pass) * 100,1)) %>% 
    arrange(desc(Percentage)) %>% 
    rename("Favorite pass recipient" = pass.recipient.name) %>% 
    slice(1:6) 
  
  
  pass_player %>% 
    formattable(align = c("c","c","r"),
                list(
                  Percentage = color_bar( "lightblue"),
                  pass.recipient.name = formatter("span", 
                                                  style = ~ style(color = "black",font.weight = "bold"))
                ))
  
})


          } else if (input$match_selection != "Summary all matches"){
            # pass distance tendancy pie  ----------
            
            output$pass_distance_tendancy<-renderPlotly({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass_length<-events %>% 
                filter(type.name %in% "Pass" &
                         team.name %in% input$choice_team &
                         match_id %in% data$match_id 
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                        & player.name %in% input$player_choice_1) %>% 
                mutate(category=cut(pass.length*0.9144, breaks=c(0, 15, 30,100), 
                                    labels=c("short","medium","long"))) %>% 
                group_by(category) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100)
              
              
              p <- plot_ly() %>%
                add_pie(data = pass_length, labels = ~paste(category,"pass",sep = " "), values = ~per, hole = 0.6,
                        name = "a",textposition = 'inside',textinfo = 'label+percent',
                        showlegend = FALSE) %>% 
                layout(title = 'Pass distance tendancy (m)',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)",
                       fig_bgcolor   = "rgba(0, 0, 0, 0)")
              p
              
            })
            
            
            # pass height tendancy pie  ----------
            
            output$pass_height_tendancy<-renderPlotly({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass_height<-events %>% 
                filter(type.name %in% "Pass" &
                         team.name %in% input$choice_team 
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                        &  match_id %in% data$match_id 
                       & player.name %in% input$player_choice_1) %>% 
                group_by(pass.height.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100)
              
              
              p <- plot_ly() %>%
                add_pie(data = pass_height, labels = ~pass.height.name, values = ~per, hole = 0.6,
                        name = "a",textposition = 'inside',textinfo = 'label+percent',
                        showlegend = FALSE) %>% 
                layout(title = 'Pass height tendancy',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)",
                       fig_bgcolor   = "rgba(0, 0, 0, 0)")
              p
              
            })
            
            
            
            # Plot sonar player  -----------
            output$sonar_team<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              #filter to one competion and include only open play passes, and round pass angle
              #to nearest x degrees
              round.angle=15
              pass<-events %>% 
                filter(type.name %in% "Pass"
                       & match_id %in% data$match_id )
              
              NWSL.passes<-pass%>%
                filter( ! play_pattern.name %in% c("From Corner", "From Free Kick", "From Throw In"))%>%
                mutate(angle.round=round(pass.angle*180/pi/round.angle)*round.angle)
              
              #create data frame for sonars per player and team for a season
              #this normalizes the most frequent pass angle to 1 and all other angles relative to that angle
              #it also associates pass distance with each angle, but this can be changed to anything
              #such as pass success or any other metric
              sonar=NWSL.passes%>%
                group_by(player.name)%>%
                mutate(N=n())%>%
                ungroup()%>%
                group_by(player.name,team.name, angle.round)%>%
                mutate(n.angle=n()/N)%>%
                ungroup()%>%
                group_by(player.name)%>%
                mutate(maxN=max(n.angle),
                       angle.norm=n.angle/maxN)%>%
                ungroup()%>%
                group_by(angle.round, player.name,team.name,N)%>%
                summarize(angle.norm = mean(angle.norm),
                          distance = mean(pass.length),# pour convertir yard en m
                          distance = ifelse(distance>30, 30,distance))
              
              
              #plot sonar for a single player
              #depending on the data source, you may have to change the start and direction
              #for the polar coordinates
              #hint: choose an outside back or midfielder to verify these parameters
              players=unique(sonar$player.name)
              
              ggplot(sonar%>%filter(player.name == input$player_choice_1))+
                geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
                scale_y_continuous(limits=c(0,1))+
                scale_x_continuous(breaks=seq(-180,180, by=45), limits=c(-180,180))+
                coord_polar(start=pi, direction=1)+
                labs(x='', y='',title= input$player_choice_1)+
                annotate("text", label='Bar length = normalized pass angle frequency; Bar color = mean pass distance', x=-2, y=79, hjust=1, size=3)+
                theme(axis.text.y=element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_blank())+
                theme(plot.title = element_text(hjust=0.5),
                      #legend.position = "none", #uncomment to remove colorbar
                      plot.background = element_rect(fill = "transparent",colour = NA),
                      panel.background = element_rect(fill = "lightgrey",colour = NA))
              
            }, bg="transparent")
            # Fonction big donut  ---------
            big_number_donut_plot <- function(value, font_family, highlight_color) {
              
              # Wrangle data to get a data frame in the format we need it in to make our donut chart
              df <- tibble(x = 1, y = value) %>% 
                mutate(y_negative = 1 - y) %>% 
                pivot_longer(cols = -x) 
              
              # Create a nicely formatted big number to go in the donut hole
              big_number_text_label <- percent(value, accuracy = 1)
              
              # Create our plot
              ggplot(df,
                     aes(x = x,
                         y = value,
                         fill = name)) +
                
                # Add a bar, but don't add the legend
                geom_col(show.legend = FALSE) +
                
                # A pie/donut chart is a bar chart with polar coordinates
                # Add polar coordinates and set the direction to -1 
                # so the filled in part starts at the top and goes clockwise
                coord_polar(theta = "y",
                            direction = -1) +
                
                
                # Set the limits, which is important for adding the hole
                xlim(c(-2, 2)) +
                
                # Set a color scale with the highlighted section in whatever color
                # is chosen with the highlight_color argument and the rest in a light gray
                scale_fill_manual(values = c(highlight_color, "grey90")) +
                
                # Set theme_void() to remove grid lines and everything else from the plot
                theme_void() +
                
                labs(title = paste("Successful passes",sep = " "))+
                theme(plot.title = element_text(hjust = 0.5, vjust = -85,color = "black",
                                                size = 12)) +
                
                # Add the big number in the center of the hole
                annotate("text",
                         label = big_number_text_label,
                         family = font_family,
                         fontface = "bold",
                         color = highlight_color,
                         size = 12,
                         x = -2,
                         y = 0) 
              
            }
            
            
            
            # Plot successful passes  -------------------
            output$successful_passes<-renderPlotly({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass_completion<-events %>% 
                filter(type.name %in% "Pass" &
                         team.name %in% input$choice_team &
                         match_id %in% data$match_id &
                         player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2])) %>%
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(pass.outcome.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100))
              
              p <- plot_ly() %>%
                add_pie(data = pass_completion, labels = ~paste(pass.outcome.name,"pass",sep = " "), values = ~per, hole = 0.6,
                        name = "a",textposition = 'inside',textinfo = 'label+percent',
                        showlegend = FALSE) %>% 
                layout(title = "Pass distribution",
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)",
                       fig_bgcolor   = "rgba(0, 0, 0, 0)",
                       annotations=list(text=paste(paste(pass_completion$per[1], " %", sep=""),
                                                   sep = ""), 
                                        "showarrow"=F, font=list(size = 20)))
              
              p
              
              
            })
            # Plot pass player  -------------
            output$pass_player<-renderGirafe({
              
            })
            # Plot target zone  ----------------
            output$target_zone<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass_target<-events %>% 
                filter(type.name %in% "Pass" & player.name %in% input$player_choice_1 
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2]))
              
              ggplot(pass_target) +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = 'white') +
                theme_pitch() + 
                geom_density2d_filled(aes(pass.end_location.x, pass.end_location.y, fill = ..level..), 
                                      alpha=0.8, contour_var = 'ndensity') + 
                theme(legend.position = "none",
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_blank()) + 
                labs(title = paste("Passes target zone",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
            }, bg="transparent")
            
            # Pass pref player datatable  -------------
            output$pass_pref_player<-renderDataTable({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass_player<-events %>% 
                filter(type.name %in% "Pass" &
                         team.name %in% input$choice_team 
                         & match_id %in% data$match_id 
                         & player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2])) %>% 
                group_by (pass.recipient.name) %>% 
                summarise(n=n()) %>% 
                mutate(per = round(prop.table(n) * 100,1)) %>% 
                arrange(desc(per)) 
              
              datatable(pass_player,colnames = c("Player","Number of pass","%"),
                        options = list(dom = 't',
                                       columnDefs = list(list(className = 'dt-center', 
                                                              targets = 0:3))
                        ))
            })
            
            # Plot shot and goal assist  -------------
            output$plot_shot_assist_player<-renderGirafe({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Pass"
                       & pass.shot_assist %in% TRUE 
                       & match_id %in% data$match_id
                       & player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2]))
              
              
              shot_player_xg<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Shot"
                       & player.name %in% pass$pass.recipient.name &
                         OpposingTeam %in%  pass$OpposingTeam &
                         minute %in% pass$minute)
              
              
              plot<-ggplot(pass) +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                             yend = pass.end_location.y,
                                             tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                             "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                             "\n",paste("Minute : ",minute, sep = ""),
                                                             sep="")),
                                         colour = "#FFA500",
                                         arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                         ))+
                scale_colour_identity()+
                theme(legend.position = "none") + 
                labs(title = paste(input$player_choice_1," shot pass assist direction during the competition",sep = " "),
                     caption = "Data Source: StatsBomb") +
                theme(plot.title = element_text(hjust = 0.5))+
                geom_segment_interactive(data = shot_player_xg,
                                         aes(x = location.x, y = location.y, xend = shot.end_location.x, 
                                             yend = shot.end_location.y,
                                             tooltip = paste(paste("value of xG : ",shot.statsbomb_xg, sep = ""),
                                                             "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                             "\n",paste("Minute : ",minute, sep = ""),
                                                             sep="")),
                                         colour = "green",
                                         arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                         ))
              
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            })
            
            output$plot_goal_assist_player<-renderGirafe({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Pass"
                       & pass.goal_assist %in% TRUE
                       & match_id %in% data$match_id
                       & player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2]))
              
              shot_player_xg<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Shot"
                       & player.name %in% pass$pass.recipient.name &
                         OpposingTeam %in%  pass$OpposingTeam &
                         minute %in% pass$minute)
              
              
              plot<-ggplot(pass) +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_segment_interactive(aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                             yend = pass.end_location.y,
                                             tooltip = paste(paste("Player recipient : ",pass.recipient.name, sep = ""),
                                                             "\n",paste("Opposing team : ",OpposingTeam,sep = ""),
                                                             "\n",paste("Minute : ",minute, sep = ""),
                                                             sep="")),
                                         colour = "#FFA500",
                                         arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                         ))+
                scale_colour_identity()+
                theme(legend.position = "none") + 
                labs(title = paste(input$player_choice_1," Goal assist during the competition",sep = " "),
                     caption = "Data Source: StatsBomb") +
                theme(plot.title = element_text(hjust = 0.5)) + 
                geom_segment_interactive(data = shot_player_xg, 
                                         aes(x = location.x, y = location.y, xend = shot.end_location.x, 
                                                              yend = shot.end_location.y,
                                                               tooltip = paste(paste("Densitiy incone : ",density.incone, sep = ""),
                                                                "\n",paste("Distance first defender : ",distance.ToD1,sep = ""),
                                                                   "\n",paste("Defenders incone : ",DefendersInCone, sep = ""),
                                                                     "\n",paste("xG value : ",shot.statsbomb_xg, sep = ""),
                                                                sep="")),
                                                                  colour = "green",
                                                                 arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                                                                       ))
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
            })
            

            # Pass team shot assist endzone and type of pass  ----------
            output$plot_type_pass_endzone<-renderPlot({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              
              pass_event<-events %>% 
                filter(type.name %in% "Pass" 
                       & team.name %in% input$choice_team
                       & match_id %in% data$match_id
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & pass.shot_assist %in% "TRUE"
                ) %>% 
                mutate(pass_location = case_when(
                  pass.end_location.x <= 60 ~ "First half",
                  pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y > 0 & location.y < 30 ~ "zone 10",
                  pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 11",
                  pass.end_location.x >60 & pass.end_location.x < 80 & pass.end_location.y >= 50  ~ "zone 12",
                  pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 0 & pass.end_location.y < 30 ~ "zone 13",
                  pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 14",
                  pass.end_location.x >=80 & pass.end_location.x < 100 & pass.end_location.y >= 50 ~ "zone 15",
                  pass.end_location.x >=100 & pass.end_location.y >= 0 & pass.end_location.y < 30  ~ "zone 16",
                  pass.end_location.x >=100  & pass.end_location.y >= 30 & pass.end_location.y < 50 ~ "zone 17",
                  pass.end_location.x >=100  & pass.end_location.y >= 50  ~ "zone 18")) %>% 
                group_by(team.name, pass_location, 
                         pass.type.name,
                ) %>% 
                summarise(n = n()) %>% 
                mutate(per = prop.table(n) * 100) %>% 
                arrange(desc(per))
              
              ggplot(pass_event) +
                aes(x = pass_location, y = n, fill = pass.type.name) +
                geom_col() +
                scale_fill_hue(direction = 1) +
                labs(
                  x = "Pass endzone location",
                  y = "number of pass",
                  title = "Type of pass according to end zone",
                  fill = "Type of pass"
                ) +
                theme_minimal() +
                theme(
                  plot.title = element_text(size = 12L,
                                            face = "bold",
                                            hjust = 0.5),
                  axis.title.y = element_text(size = 12L,
                                              face = "bold"),
                  axis.title.x = element_text(size = 12L,
                                              face = "bold")
                )
              
            })
            
            # Behavior under pressure  ----------------
            output$behavior_under_pressure<-renderPlotly({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass1<-events %>% 
                filter(team.name %in% input$choice_team
                       & under_pressure %in% TRUE & player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & match_id %in% data$match_id) %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(type.name) %>% 
                summarise(n = n()) %>% 
                mutate(per = round(prop.table(n) * 100),1)%>% 
                arrange(desc(per))%>% 
                select(type.name,per)  
              
              suite <- pass1$type.name=="Pass"
              suite<-as.integer(as.logical(suite))
              suite<-as.data.frame(suite)
              suite<-suite %>% 
                mutate(suite = replace(suite, suite == 1, 0.2))
              
              
              p <- plot_ly() %>%
                add_pie(data = pass1, labels = ~paste(type.name,sep = " "), values = ~per, hole = 0.6,
                        name = "a",textposition = 'inside',textinfo = 'label+percent',
                        showlegend = FALSE, 
                        pull = suite$suite) %>% 
                layout(title = 'Behavior under pressure',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       plot_bgcolor  = "rgba(0, 0, 0, 0)",
                       paper_bgcolor = "rgba(0, 0, 0, 0)",
                       fig_bgcolor   = "rgba(0, 0, 0, 0)")
              
              p
            })
            
            # Plot pass under pressure player  ------------------
            output$plot_pass_underpressure_player<-renderGirafe({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              pass2<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Pass"
                       & under_pressure %in% TRUE & player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & match_id %in% data$match_id) %>% 
                mutate(pass.outcome.name = replace_na(pass.outcome.name, "Complete")) %>% 
                group_by(position.name)
              
              
              plot<-ggplot(pass2) +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_segment_interactive(data = pass2, aes(x = location.x, y = location.y, xend = pass.end_location.x, 
                                                           yend = pass.end_location.y,
                                                           colour = pass.outcome.name == "Incomplete",
                                                           tooltip = paste("Player targeted : ",pass.recipient.name,
                                                                           "\n","Distance (m) : " ,round(pass.length,1),
                                                                           "\n",paste(team.name,OpposingTeam,sep = " / "),
                                                                           sep="")),
                                         arrow = arrow(length = unit(0.25, "cm"),type = "closed"
                                         ))+
                scale_colour_manual(values = c("#5e9a78","#ff4444"),
                                    labels = levels(as.factor(pass2$pass.outcome.name)),
                                    name = "How is the pass ?") +
                
                theme(legend.position = "top",
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_blank()) + 
                labs(title = NULL,
                     caption = "Data Source: StatsBomb") +
                theme(plot.title = element_text(hjust = 0.5))
              
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;
                          font-style:italic;border-radius:5px;
                          padding:5px;")))
              
            })
            
            
            
            # Favorite pass  --------------
            output$favoritepass<-renderFormattable({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass_player<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Pass"
                       & under_pressure %in% TRUE & 
                         player.name %in% input$player_choice_1
                       & minute %in% (input$slider_min[1]:input$slider_min[2])
                       & match_id %in% data$match_id) %>% 
                group_by (pass.recipient.name) %>% 
                drop_na(pass.recipient.name) %>% 
                summarise(Pass=n()) %>% 
                mutate(Percentage = round(prop.table(Pass) * 100,1)) %>% 
                arrange(desc(Percentage)) %>% 
                rename("Favorite pass recipient" = pass.recipient.name) %>% 
                slice(1:6) 
              
              
              pass_player %>% 
                formattable(align = c("c","c","r"),
                            list(
                              Percentage = color_bar( "lightblue"),
                              pass.recipient.name = formatter("span", 
                                                              style = ~ style(color = "black",font.weight = "bold"))
                            ))
              
            })
            
            
            
            # player_shot_box  ----------
            output$player_shot_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Shot"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "shot during the match", color = "navy",
                       icon = NULL)
              
              
            })
            # player_pass_box  ----------
            output$player_pass_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team & type.name %in% "Pass"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Pass during the match", color = "navy",
                       icon = icon("chart-network"))
              
              
            })
            
            # player_goal_box  ----------
            output$player_goal_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team 
                       & shot.outcome.name %in% "Goal"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Goal during the match", color = "navy",
                       icon = icon("futbol"))
              
              
            })
            
            
            # player_duel_box  ----------
            output$player_duel_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team 
                       & type.name %in% "Duel"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Duel during the match", color = "navy",
                       icon = icon("swords" ))
              
              
            })
            
            
            
            # player_foul_box  ----------
            output$player_foul_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team 
                       & type.name %in% "Foul committed"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Foul during the match", color = "navy",
                       icon = icon("whistle"))
              
              
            })
            
            
            
            
            # player_intercep_box  ----------
            output$player_intercep_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team 
                       & type.name %in% "Interception"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Interception during the match", color = "navy",
                       icon = NULL)
              
              
            })
            
            
            
            
            
            # player_dribble_box  ----------
            output$player_dribble_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team 
                       & type.name %in% "Dribble"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Dribble during the match", color = "navy",
                       icon = NULL)
              
              
            })
            
            
            
            
            
            
            # player_offside_box  ----------
            output$player_offside_box<-renderValueBox({
              
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              
              pass<-events %>% 
                filter(team.name %in% input$choice_team 
                       & type.name %in% "Offside"
                       & match_id %in% data$match_id &
                         player.name %in% input$player_choice_1) %>% 
                summarise(n=n())
              
              
              valueBox(paste(round(pass$n,1),sep = ""),
                       subtitle = "Offside during the match", color = "navy",
                       icon = NULL)
              
              
            })
            
            
            
            
            
            
            
            # plot_pass_underpressure_density  -------------
            output$plot_pass_underpressure_density<-renderPlot({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              duel<-events %>% 
                filter(team.name %in% input$choice_team 
                       & under_pressure %in% TRUE
                       & player.name %in% input$player_choice_1
                       & match_id %in% data$match_id)
              
              
              ggplot(duel) +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_density2d_filled(aes(location.x, location.y, fill = after_stat(level)), 
                                      alpha=0.8, contour_var = 'ndensity') + 
                theme(legend.position = "none") + 
                labs(title = paste("Zone of events of behaviors under pressure",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
              
            })
            # Duel player defensive metrics  ------------
            output$duel_player<-renderggiraph({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team 
                       & match_id %in% data$match_id
                       & player.name %in% input$player_choice_1
                       & type.name %in% c("Duel","Interception","Foul Committed", 
                                          "Bad Behaviour")
                       & minute %in% (input$slider_min[1]:input$slider_min[2]))
              
              
              plot<-ggplot() +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_point_interactive(data = def_metrics,
                                       aes(x = location.x , y = location.y, 
                                           colour = type.name,
                                           tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                           "\n",paste("Position : ",position.name,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")))+
                theme(legend.position = "right") + 
                labs(title = paste("Pressure during the match",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
              
              
            })
            # Pressure player  metrics  ------------
            output$pressure_player<-renderggiraph({
              data<-Matches %>% 
                filter(home_team.home_team_name %in% input$choice_team |
                         away_team.away_team_name %in% input$choice_team) %>% 
                select(match_id,home_team.country.name,away_team.country.name,
                       home_score, away_score) %>% 
                unite("Merged",home_team.country.name:away_team.country.name,
                      sep= " / ", remove = FALSE)
              
              data<-data %>% 
                filter(Merged %in% input$match_selection)
              
              def_metrics<-events %>% 
                filter(team.name %in% input$choice_team 
                       & match_id %in% data$match_id
                       & player.name %in% input$player_choice_1
                       & type.name %in% "Pressure"
                       & minute %in% (input$slider_min[1]:input$slider_min[2]))
              
              
              plot<-ggplot() +
                annotate_pitch(dimensions = pitch_statsbomb, fill = '#021e3f', colour = '#DDDDDD') +
                theme_pitch() + 
                geom_point_interactive(data = def_metrics,
                                       aes(x = location.x , y = location.y, 
                                           colour = type.name,
                                           tooltip = paste(paste("Player : ",player.name, sep = ""),
                                                           "\n",paste("Position : ",position.name,sep = ""),
                                                           "\n",paste("Minute : ",minute, sep = ""),
                                                           sep="")))+
                theme(legend.position = "right") + 
                labs(title = paste("Defensive metrics during the match",sep = " "),
                     caption = "Data Source: StatsBomb")+
                theme(plot.title = element_text(hjust = 0.5))
              
              
              girafe(ggobj=plot,height_svg = 4,
                     options=list(opts_zoom(min = .7, max = 5),
                                  opts_tooltip(css="background-color:white;color:black;
                          font-style:bold;border-radius:5px;
                          padding:5px;")))
              
              
            })
            
          }
          
        })
      }

          
          
# fermeture observe event player_choice_1  --------
  })


  })# fermeture observe event  
}# fermeture server

# Run the application 
shinyApp(ui = ui, server = server)
