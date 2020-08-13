selectionViewUI <- function(id){
  ns <- NS(id)
  tabPanel("Selection",
           fluidPage(
             fluidRow(column(width=10),
                      column(width=2,
                             radioGroupButtons(
                               inputId = ns("growthrate"),
                               label = "Growth Rate",
                               choices = c(3, 5, 10),
                               justified = TRUE,
                               checkIcon = list(
                                 yes = icon("ok", 
                                            lib = "glyphicon")),size = "xs"
                             )))),
           sidebarLayout(
             sidebarPanel(width=3,
                          selectInput(ns("countries"),label="Countries :", choices=NULL, multiple = FALSE, width = "170px"),
                          tags$h5("Instructions"),
                          tags$h6("Lorem Ipsum is simply dummy text of the printing
                                  and typesetting industry. Lorem Ipsum has been the industry's standard
                                  dummy text ever since the 1500s, when an unknown printer took a galley of
                                  type and scrambled it to make a type specimen book.
                                  It has survived not only five centuries, but also the leap into electronic
                                  typesetting, remaining essentially unchanged. It was popularised
                                  in the 1960s with the release of Letraset sheets containing Lorem Ipsum
                                  passages, and more recently with desktop publishing software like Aldus
                                  PageMaker including versions of Lorem Ipsum."),
                          actionButton(inputId = ns("runsimulation"),label = "Simulate")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Economy",
                          rHandsontableOutput(ns('economy'))),
                 tabPanel("Gen.Env",
                          rHandsontableOutput(ns('env'))),
                 tabPanel("H.Edu",
                          rHandsontableOutput(ns('highed'))),
                 tabPanel("I.C.T",
                          rHandsontableOutput(ns('com'))),
                 tabPanel("Pre.Ed.Uni",
                          rHandsontableOutput(ns('unied'))),
                 tabPanel("R&D",
                          rHandsontableOutput(ns('devinn'))),
                 tabPanel("Tech.Voc.Train",
                          rHandsontableOutput(ns('techvoc')))
               )
             )
             )
  )
}

selectionView <- function(input,output,session){
  
  ns <- session$ns
  
  load <- LoadData$new(NULL,NULL,NULL,NULL,NULL)
  
  observe({
    updateSelectInput(session,"countries",choices=unique(load$a.pillars$Country))
  })
  react.country <- eventReactive(input$runsimulation,{input$countries})
  
  react.eco <- eventReactive(input$runsimulation,{hot_to_r(input$economy)})

  react.env <- eventReactive(input$runsimulation,{hot_to_r(input$env)})
 
  react.highed <- eventReactive(input$runsimulation,{hot_to_r(input$highed)})
  
  react.com <- eventReactive(input$runsimulation,{hot_to_r(input$com)})
  
  react.unied <- eventReactive(input$runsimulation,{hot_to_r(input$unied)})
 
  react.devinn <- eventReactive(input$runsimulation,{hot_to_r(input$devinn)})
  
  react.techvoc <- eventReactive(input$runsimulation,{hot_to_r(input$techvoc)})
  
  react.growthrate.selected <- eventReactive(input$runsimulation,{as.numeric(input$growthrate)})
  
  selection.list <- list(eco=reactive({react.eco()}),
                         env=reactive({react.env()}),
                         highed=reactive({react.highed()}),
                         com=reactive({react.com()}),
                         unied=reactive({react.unied()}),
                         devinn=reactive({react.devinn()}),
                         techvoc=reactive({react.techvoc()}),
                         country.selected=reactive({react.country()}),
                         growthrate.selected=reactive({react.growthrate.selected()}),
                         runsimulation.selected = reactive({input$runsimulation}))
  
  
  
  output$economy <- renderRHandsontable({
    res <- Data$new(sector = 'Economy',country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  
  output$env <- renderRHandsontable({
    res <- Data$new(sector = 'General Enabling Environment',country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  
  output$highed <- renderRHandsontable({
    res <- Data$new(sector = "Higher Education",country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  
  output$com <- renderRHandsontable({
    res <- Data$new(sector = "Information and Communications Technology",country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  
  output$unied <- renderRHandsontable({
    res <- Data$new(sector = "Pre-University Education",country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  
  output$devinn <- renderRHandsontable({
    res <- Data$new(sector = "Research, Development and Innovation",country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  
  output$techvoc <- renderRHandsontable({
    res <- Data$new(sector = "Technical and Vocation Education and Training",country = input$countries,year = as.numeric(input$growthrate),user.value = NULL,is.simulated = FALSE)
    rhandsontable(res$tabpanel.selection(),width = 1000, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>% 
      hot_col(3, readOnly = TRUE) %>% 
      hot_col(4, readOnly = FALSE) %>%
      hot_col(5, readOnly = TRUE) 
  })
  return(selection.list)
}
