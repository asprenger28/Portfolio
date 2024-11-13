#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(Lahman)
small=Batting %>%
  select(playerID,H,X2B,X3B,HR,yearID,teamID,AB)      %>%
  filter(yearID %in% c(2020,2021, 2022)) %>%
  filter(H>=1|X2B>=1|X3B>=1|HR>=1)







# Define UI for application that draws a histogram
ui <- navbarPage(
  title = 'Hits by Player',
  tabPanel('Input', 
  titlePanel(title = 'Hits Data'),
  sidebarLayout(
  sidebarPanel(
  selectInput(inputId ='y',
              label ='year:',
              choices = unique(small$yearID)),
  selectInput(inputId ='t',
              label ='team:',
              choices = unique(small$teamID))
  ,
  selectInput(inputId ='p',
              label ='player:',
              choices = unique(small$playerID)),
  checkboxInput(inputId = 'season',label='filter table to team and player')
  ),
  
  
          mainPanel(plotOutput('plot')))),
                 tabPanel('Table', dataTableOutput('table')),
                 tabPanel('About' , includeMarkdown('about.Rmd')))
                 


server <- function(input, output) {
  
  hits_year=reactive({
    small %>%
      filter(yearID==input$y)
  })
  observeEvent(
    eventExpr = input$y,
    handlerExpr = {
      updateSelectInput(inputId = 'team',choices = sort(unique(hits_year()$team)))
      updateSelectInput(inputId = 'p',
                        choices = unique(hits_year_team()$player))
    }
    

  )
  hits_year_team=reactive({
    hits_year() %>%
      filter(teamID==input$t)
  })
  observeEvent(
    eventExpr = input$t,
    handlerExpr = {
      updateSelectInput(inputId = 'p',
                        choices = unique(hits_year_team()$player))
    }
  )
  
  
  
   output$plot=renderPlot({

  small %>%
      filter(yearID==input$y) %>%
      filter(teamID==input$t) %>%
       filter(playerID==input$p) %>%
    pivot_longer(H:HR,names_to = 'Hits',values_to = 'Count') %>%
    group_by(Hits) %>%
    summarise(Count=sum(Count)) %>%
    ggplot()+
    aes(x=Hits,y=Count,fill=Hits) %>%
    geom_bar(stat="identity") +
    theme_bw()
    })
  
   
    output$table=renderDataTable({
      hits_year()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
