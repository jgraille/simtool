selectionViewUI <- function(id){
  ns <- NS(id)
  tabPanel("Selection",
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
               fluidPage(
              fluidRow(column(width=8),
                       column(width = 4,
               radioGroupButtons(
                 inputId = ns("growthrate"),
                 label = "Growth Rate",
                 choices = c(3, 5, 10),
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon")),size = "xs"
               )))),
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
  
  simdata <- SimData$new()
  growthrate <- GrowthRate$new()
  
  observe({
    updateSelectInput(session,"countries",choices=simdata$countries())
  })
  
  react.eco <- eventReactive(input$runsimulation,{hot_to_r(input$economy)})
  react.histo.eco <- eventReactive(input$runsimulation,{SimData$new()$load(s = "Economy",country = input$countries)})
  
  react.env <- eventReactive(input$runsimulation,{hot_to_r(input$env)})
  react.histo.env <- eventReactive(input$runsimulation,{SimData$new()$load(s = "General Enabling Environment",country = input$countries)})
  
  react.highed <- eventReactive(input$runsimulation,{hot_to_r(input$highed)})
  react.histo.highed <- eventReactive(input$runsimulation,{SimData$new()$load(s = "Higher Education",country = input$countries)})
  
  react.com <- eventReactive(input$runsimulation,{hot_to_r(input$com)})
  react.histo.com <- eventReactive(input$runsimulation,{SimData$new()$load(s = "Information and Communications Technology",country = input$countries)})
  
  react.unied <- eventReactive(input$runsimulation,{hot_to_r(input$unied)})
  react.histo.unied <- eventReactive(input$runsimulation,{SimData$new()$load(s = "Pre-University Education",country = input$countries)})
  
  react.devinn <- eventReactive(input$runsimulation,{hot_to_r(input$devinn)})
  react.histo.devinn <- eventReactive(input$runsimulation,{SimData$new()$load(s = "Research, Development and Innovation",country = input$countries)})
  
  react.techvoc <- eventReactive(input$runsimulation,{hot_to_r(input$techvoc)})
  react.histo.techvoc <- eventReactive(input$runsimulation,{SimData$new()$load(s = "Technical and Vocation Education and Training",country = input$countries)})
  
  react.growthrate.selected <- eventReactive(input$runsimulation,{as.numeric(input$growthrate)})
  selection.list <- list(eco=reactive({react.eco()}),
                         histo.eco=reactive({react.histo.eco()}),
                         env=reactive({react.env()}),
                         histo.env=reactive({react.histo.env()}),
                         highed=reactive({react.highed()}),
                         histo.highed=reactive({react.histo.highed()}),
                         com=reactive({react.com()}),
                         histo.com=reactive({react.histo.com()}),
                         unied=reactive({react.unied()}),
                         histo.unied=reactive({react.histo.unied()}),
                         devinn=reactive({react.devinn()}),
                         histo.devinn=reactive({react.histo.devinn()}),
                         techvoc=reactive({react.techvoc()}),
                         histo.techvoc=reactive({react.histo.techvoc()}),
                         growthrate.selected=reactive({react.growthrate.selected()}))
  
  output$economy <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "Economy",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE
                                               ),
                                               width = 1000, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Expected Value", readOnly = TRUE) %>%
      hot_col("Expected Rank", readOnly = TRUE) 
  })
  
  output$env <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "General Enabling Environment",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE),width = 1000, height = 600)
  })

  output$highed <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "Higher Education",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE
                                               ),width = 1000, height = 600)
  })

  output$com <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "Information and Communications Technology",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE
                                               ),width = 1000, height = 600)
  })

  output$unied <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "Pre-University Education",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE
                                               ),width = 1000, height = 600)
  })

  output$devinn <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "Research, Development and Innovation",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE
                                               ),width = 1000, height = 600)
  })

  output$techvoc <- renderRHandsontable({
    pillar.subpillar.variable.histo <- SimData$new()$load(s = "Technical and Vocation Education and Training",country = input$countries)
    rhandsontable(growthrate$calculate.method2(year=as.numeric(input$growthrate),
                                               pillar.subpillar.variable.histo,
                                               user.value = NULL,
                                               is.simulated = FALSE
                                               ),width = 1000, height = 600)
  })

  return(selection.list)
}
