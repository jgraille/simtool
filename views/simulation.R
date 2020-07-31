simulationViewUI <- function(id){
  ns <- NS(id)
  tabPanel("Simulation",
           fluidPage(
           # verbatimTextOutput(ns('test')),
           # verbatimTextOutput(ns('test2')),
           # verbatimTextOutput(ns('test3'))
           ),
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
}
simulationView <- function(input,output,session,selection.list){
  ns <- session$ns
  
  growthrate1 <- GrowthRate$new()
  
  output$test <- renderPrint({selection.list$eco()$Value})
  output$test2 <- renderPrint({selection.list$histo.eco()})
  output$test3 <- renderPrint({selection.list$growthrate.selected()})
  
  output$economy <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.eco(),
                                                user.value = selection.list$eco()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
  output$env <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.env(),
                                                user.value = selection.list$env()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
  output$highed <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.highed(),
                                                user.value = selection.list$highed()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
  output$com <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.com(),
                                                user.value = selection.list$com()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
  output$unied <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.unied(),
                                                user.value = selection.list$unied()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
  output$devinn <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.devinn(),
                                                user.value = selection.list$devinn()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
  output$techvoc <- renderRHandsontable({
    rhandsontable(growthrate1$calculate.method2(selection.list$growthrate.selected(),
                                                selection.list$histo.techvoc(),
                                                user.value = selection.list$techvoc()$Value,
                                                is.simulated = TRUE
    ),
    width = 1400, height = 600) %>%
      hot_col("Rank 2020", readOnly = TRUE) %>%
      hot_col("Indicator", readOnly = TRUE) %>%
      hot_col("Simulated Value", readOnly = TRUE) %>%
      hot_col("Simulated Rank", readOnly = TRUE) 
  })
  
}