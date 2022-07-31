library(shiny)
library(plotly)
library(data.table)
library(stringr)
library(shinythemes)
library(shinyWidgets)

episodes <- readRDS("data/episodes.rds")
shows_slopes <- readRDS("data/slopes.rds")

genres_unique <- names(sort(table(unlist(strsplit(shows_slopes$genres,split = ","))),decreasing = T))
titles_unique <- unique(episodes$title)


ui <- fluidPage(
  navbarPage("TV Shows",collapsible = T,
             theme = shinytheme("lumen"),
             id = "navbarID",
             tabPanel("Ratings",value = "page1", fluid = TRUE,
                      fluidRow(selectizeInput(
                        inputId = "titles", 
                        label = "Select a show (delete and start typing below). Also, check out Explorer section in the menu above!", 
                        choices = titles_unique, 
                        selected = "Game of Thrones (2011)",
                        multiple = F, width = "auto",
                        options = list(
                          placeholder = "Start typing show title here",
                          maxItems = 1)
                      )),
                      fluidRow(plotlyOutput(outputId = "p",width = "100%"))
             ),
             tabPanel("Explorer", fluid = TRUE,value = "page2",
                      fluidRow(
                        column(2,p("Show TV shows which..."),
                               checkboxInput(inputId = "filterTopShows",
                                             label =  "... are popular (>20K votes)",value = T),
                               checkboxInput(inputId = "filterHighRating",
                                             label =  "...have high rating (>8.0)",value = T),
                               checkboxInput(inputId = "filterGettingBetter",
                                             label =  "...only get better each season",value = T)
                        ),
                        column(4,
                               pickerInput("genres","Choose genres (all by default)",
                                           choices=genres_unique,
                                           selected = genres_unique,
                                           options = list(`actions-box` = TRUE),
                                           multiple = T,width = "100%"),
                               #selectizeInput(inputId = "genres",label = "Choose genres",
                               #             choices = names(sort(table(unlist(strsplit(shows_slopes$genres,split = ","))),decreasing = T)),
                               #             selected = names(sort(table(unlist(strsplit(shows_slopes$genres,split = ","))),decreasing = T)),
                               #             multiple = TRUE,
                               #             options = list(
                               #               plugins = list("remove_button")),
                               #             width = '100%'),
                        ),
                        column(5,sliderInput("years_start",
                                             "Year came out:",
                                             min = 1930, max = 2025, value = c(1990,2023),
                                             sep = "",step = 1,ticks = F)),
                        column(4,tableOutput("text"))
                      ),
                      fluidRow(plotlyOutput(outputId = "p2",width = "100%"))
             ),
             tabPanel("About",fluidRow(
               h4(p("Project")),
               h5(p("Many TV shows, despite having a great start, get worse over time and epicly fail like Game of Thrones in the last season. Others only get better like Bojack, though unfortunately they are few."),
                  p("This app uses ",a("open data from IMDB", href = 'https://www.imdb.com/interfaces/'), " to analyze TV show ratings over each season. In the second tab it also shows a cloud of TV shows by their average rating and whether a TV show gets better or worse each episode and season, rating-wise."),
                  p("Whether a TV show gets better or worse is a slope of the line drawn through ratings of each episode. Technically speaking, it's a coefficient of season number in the linear regression of the episode rating onto season and episode number. For TV shows with only one season, it's the coefficient for episode number."),
                  p("This app is  using Shiny. Check out ",a("source code on Github.", href = 'https://github.com/alexeyknorre/imdb_tv_shows'))
               ),
               h4(p("Author")),
               h5(p(a("Alex Knorre", href = 'https://alexknorre.com/')
               )
               )
             )
             ),
             #tags$head(tags$style(HTML(navbar_js)))
             
  )
)

server <- function(input, output, ...) {
  
  
  output$p2 <- renderPlotly({
    
    if (input$filterTopShows) {
      shows_slopes <- shows_slopes[numVotes > 20000]
    }
    
    if (input$filterHighRating) {
      shows_slopes <- shows_slopes[averageRating > 8]
    }
    
    if (input$filterGettingBetter) {
      shows_slopes <- shows_slopes[coef > 0]
    }
    
    shows_slopes <- shows_slopes[startYear > input$years_start[1]-1 & 
                                   startYear < input$years_start[2]+1]
    
    shows_slopes <- shows_slopes[str_detect(genres, paste0(input$genres,collapse = "|"))]
    
    graph_y_lower <- min(min(shows_slopes$coef),-0.5)
    
    
    p2 <- ggplotly(height = 500, source = "shows_scatterplot",
                   ggplot(data=shows_slopes,
                          aes(x=averageRating,
                              y=coef,
                              size = log(numVotes),
                              customdata = title,
                              text=sprintf("<b>%s</b><br>Rating: %s<br>Votes: %s<br>Genres: %s<br>Year started: %s <br>Double click/tap to look at ratings dynamics",
                                           title, averageRating, numVotes, genres,startYear)))+
                     geom_jitter(pch=20, alpha=0.1,width = 0.05, height = 0.05)+
                     theme_minimal() +
                     labs(y=" ← Gets worse       Gets better → ", x = "Rating better →")+
                     #scale_x_log10(),
                     scale_y_continuous(limits = c(graph_y_lower,0.9)),
                   tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"),
             xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)
  })
  
  # create reactive for subset plot on second tab
  
  doubleclick_data <- reactive({event_data("plotly_doubleclick", source = "shows_scatterplot",priority = "event")})
  click_data <- reactive({event_data("plotly_click", source = "shows_scatterplot")})
  
  observeEvent(doubleclick_data(), {
    #custom_selection <- reactive({s()$customdata[1]})
    #title_selection <- reactive(click_data()$customdata[1])
    #output$text <- renderText(click_data()$customdata[1])
    #updateSelectInput(inputId = "titles", selected = click_data()$customdata[1])
    #episodes <- episodes[episodes$title == renderText(s()$customdata[1]), ]
    updateNavbarPage(inputId = "navbarID", selected = "page1")})
  
  observeEvent(click_data(), {
    #  req(doubleclick_data())
    updateSelectInput(inputId = "titles", selected = click_data()$customdata[1])
  })
  
  output$p <- renderPlotly({
    
    episodes <- episodes[episodes$title == input$titles, ]
    
    graph_y_lower <- min(episodes$averageRating) - 1
    graph_y_upper <- min(c(max(episodes$averageRating) + 0.5,10))
    
    req(input$titles)
    
    p <- ggplotly(ggplot(episodes, aes(x=id-0.5, y = averageRating,
                                       color = as.factor(season))) +
                    geom_line(aes(alpha = 0.5)) +
                    geom_point(aes(size = numVotes,
                                   text=sprintf("S%sE%s %s <br>Rating: %s<br>Votes: %s",seasonNumber, episodeNumber, eptitle,
                                                averageRating, numVotes)),
                               shape = ".",
                               alpha = 0.5) +
                    theme_minimal() +
                    theme(legend.position = "none",
                          panel.grid.minor.y = element_blank(),
                          panel.grid.major.x = element_blank()) +
                    scale_x_continuous(breaks = 1:max(episodes$seasonNumber), limits = c(NA,max(episodes$seasonNumber)+0.5))+
                    scale_y_continuous(limits = c(graph_y_lower,graph_y_upper))+
                    labs(x="Season",y="IMDB rating",title=input$titles, subtitle = "Average ratings by each episode") + 
                    theme(plot.title = element_text(hjust = 0.5)) +
                    theme(plot.subtitle = element_text(hjust = 0.5)),
                  tooltip="text") %>% 
      layout(hoverlabel=list(bgcolor="white")) %>% 
      config(displayModeBar = F)
  })
  
}

shinyApp(ui, server)
#setwd("shiny")
#rsconnect::deployApp()
