library(shiny)
library(plotly)
library(data.table)
library(shinythemes)

#episodes <- readRDS("data/episodes.rds")[order(numVotes,decreasing = T,seasonNumber,episodeNumber)]
#shows_slopes <- readRDS("data/slopes.rds")
episodes <- readRDS("episodes.rds")[order(numVotes,decreasing = T,seasonNumber,episodeNumber)]
shows_slopes <- readRDS("slopes.rds")

ui <- fluidPage(
  navbarPage("Evidence Based TV Show Viewing", theme = shinytheme("lumen"),
             tabPanel("Seasons and episodes", fluid = TRUE,
                selectizeInput(
                  inputId = "titles", 
                  label = "Select a show", 
                  choices = unique(episodes$title), 
                  selected = "BoJack Horseman (2014)",
                  multiple = F,
                  options = list(
                    placeholder = "TV Show title",
                    maxItems = 1)
                ),
                plotlyOutput(outputId = "p")),
             tabPanel("What gets better each season?", fluid = TRUE,
                      checkboxInput(inputId = "filterTopShows",
                                    label =  "Popular shows",value = T),
                      checkboxInput(inputId = "filterHighRating",
                                    label =  "High rating",value = F),
                      checkboxInput(inputId = "filterGettingBetter",
                                    label =  "Gets better",value = F),
                      plotlyOutput(outputId = "p2")),
             tabPanel("About",fluidRow(
                      h4(p("Project")),
                      h5(p("This Shiny app uses the data from IMDB to look into the ratings of TV shows."),
                         p(a("Read the report", href = 'https://htmlpreview.github.io/?https://github.com/alexeyknorre/imdb_tv_shows/blob/main/code/report.html')),
                         p(a("Source code on Github", href = 'https://github.com/alexeyknorre/imdb_tv_shows'))
                         ),
                      h4(p("Author")),
                      h5(p(a("Alex Knorre", href = 'https://alexknorre.com/')
                           )
                         )
                      )
                      )
             )
)

server <- function(input, output, ...) {
  #episodes <- reactive({episodes[episodes$title == input$titles, ]})
  
  output$p <- renderPlotly({
    episodes <- episodes[episodes$title == input$titles, ]
    
    graph_y_lower <- min(episodes$averageRating) - 1
    graph_y_upper <- min(c(max(episodes$averageRating) + 0.5,10))
    
    p <- ggplotly(
      ggplot(episodes, aes(x=id-0.5, y = averageRating,
                       color = as.factor(season))) +
        geom_line() +
        geom_point(aes(size = numVotes,
                       text=sprintf("S%sE%s %s <br>Rating: %s<br>Votes: %s",seasonNumber, episodeNumber, eptitle,
                                    averageRating, numVotes)),
                   shape = ".") +
        theme_minimal() +
        theme(legend.position = "none",
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank()) +
        scale_x_continuous(breaks = 1:max(episodes$seasonNumber), limits = c(NA,max(episodes$seasonNumber)+0.5))+
        scale_y_continuous(limits = c(graph_y_lower,graph_y_upper))+
        labs(x="Season",y="IMDB rating",title="Average ratings by episode"),
      tooltip="text") %>% 
    layout(hoverlabel=list(bgcolor="white")) %>% 
    config(displayModeBar = F)
    })
  
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
    
    graph_y_lower <- min(min(shows_slopes$coef),-0.5)

    
    p2 <- ggplotly(ggplot(data=shows_slopes,
                         aes(x=averageRating,
                             y=coef,
                             size = log(numVotes),
                             text=sprintf("%s <br>Rating: %s<br>Votes: %s",title, averageRating, numVotes)))+
                    geom_jitter(pch=20, alpha=0.1,width = 0.2,height = .01)+
                    theme_minimal() +
                    labs(y="⟵ Gets worse       Gets better → ", x = "Rating better →")+
                  #scale_x_log10(),
                  scale_y_continuous(limits = c(graph_y_lower,NA)),
                  tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"),
             height = 500) %>% 
      config(displayModeBar = F)
  })
}


shinyApp(ui, server)
#setwd("code/shiny")
#rsconnect::deployApp()

