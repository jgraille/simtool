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
                                  PageMaker including versions of Lorem Ipsum.")
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
  
  #4. Watching any change made in column 'Value'
  observeEvent(
    input$economy,
    {
      print(hot_to_r(input$economy))
    }
  )
  
  output$economy <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "Economy",
                                               country = input$countries,
                                               year=as.numeric(input$growthrate)),
                                               width = 850, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE)
  })
  
  output$env <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "General Enabling Environment",country = input$countries,year=as.numeric(input$growthrate)),width = 850, height = 600)
  })
  
  output$highed <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "Higher Education",country = input$countries,year=as.numeric(input$growthrate)),width = 850, height = 600)
  })
  
  output$com <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "Information and Communications Technology",country = input$countries,year=as.numeric(input$growthrate)),width = 850, height = 600)
  })
  
  output$unied <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "Pre-University Education",country = input$countries,year=as.numeric(input$growthrate)),width = 850, height = 600)
  })
  
  output$devinn <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "Research, Development and Innovation",country = input$countries,year=as.numeric(input$growthrate)),width = 850, height = 600)
  })
  
  output$techvoc <- renderRHandsontable({
    rhandsontable(growthrate$calculate.method2(s = "Technical and Vocation Education and Training",country = input$countries,year=as.numeric(input$growthrate)),width = 850, height = 600)
  })
  
  
}
