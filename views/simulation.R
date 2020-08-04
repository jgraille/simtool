simulationViewUI <- function(id){
  ns <- NS(id)
  tabPanel("Simulation",
           fluidPage(
           # verbatimTextOutput(ns('test')),
           # verbatimTextOutput(ns('test2')),
           # verbatimTextOutput(ns('test3'))
           ),wellPanel(
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
           ))
           #verbatimTextOutput(ns("test")),
           #verbatimTextOutput(ns("test2")),
           #verbatimTextOutput(ns("test3"))
          )
}
simulationView <- function(input,output,session,selection.list){
  ns <- session$ns
  
  output$test <- renderPrint({selection.list$highed()})
  # output$test2 <- renderPrint({
  #   pre <- Preprocessing$new()$load.bind('Economy',selection.list$country.selected())
  #   pre
  # })
  output$test3 <- renderPrint({selection.list$growthrate.selected()})
  
  output$economy <- renderRHandsontable({
    res <- Data$new(sector = 'Economy',country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$eco()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
    width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })
  
  output$env <- renderRHandsontable({
    res <- Data$new(sector = 'General Enabling Environment',country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$env()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
                  width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })

  output$highed <- renderRHandsontable({
    res <- Data$new(sector = "Higher Education",country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$highed()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
                  width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })

  output$com <- renderRHandsontable({
    res <- Data$new(sector = "Information and Communications Technology",country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$com()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
                  width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })

  output$unied <- renderRHandsontable({
    res <- Data$new(sector = "Pre-University Education",country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$unied()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
                  width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })

  output$devinn <- renderRHandsontable({
    res <- Data$new(sector = "Research, Development and Innovation",country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$devinn()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
                  width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })

  output$techvoc <- renderRHandsontable({
    res <- Data$new(sector = "Technical and Vocation Education and Training",country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$techvoc()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
                  width = 1400, height = 600) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })
  
}