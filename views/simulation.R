simulationViewUI <- function(id){
  ns <- NS(id)
  tabPanel("Simulation",
           fluidPage(
           # verbatimTextOutput(ns('test')),
           # verbatimTextOutput(ns('test2')),
           # verbatimTextOutput(ns('test3'))
             fluidRow(column(width=1),
                      column(width=5,uiOutput(ns('title.gki.scores')))),
             fluidRow(
               column(width=1),
               column(width=3,echarts4rOutput(ns('gauge.gkiinitialvalue'))),
               column(width=3,echarts4rOutput(ns('gauge.gkiexpectedvalue'))),
               column(width=3,echarts4rOutput(ns('gauge.gkisimulatedvalue')))),
             fluidRow(
               column(width=1),
               column(width=3,uiOutput(ns('gkiexpectedrank'))),
               column(width=3,uiOutput(ns('gkimodifiedrank'))),
               column(width=3,uiOutput(ns('gkisimulatedrank')))
               )
           ),
           fluidPage(
             fluidRow(column(width=1),
                      column(width=11,
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
                                                  rHandsontableOutput(ns('techvoc'))))
                             )),
             br(),
             fluidRow(column(width=1),
                      column(width=5,uiOutput(ns('title.sector.indices'))))),
           echarts4rOutput(ns('bar.sector'),height = 600,width = 1350),
           fluidPage(fluidRow(column(width=1),
                              column(width=5,uiOutput(ns('title.pillars.indices'))),
                              column(width=4),
                              column(width=1),
                              ),
                     fluidRow(column(width=2),
                              column(width=8,
                                     echarts4rOutput(ns('bar.pillars'),height = 600,width = 800)),
                              column(width=1,
                                     dropdownButton(
                                       inputId = ns('dropdown'),
                                       selectInput(ns('choice.sector'),label="Sector:",choices=NULL),
                                       circle = TRUE, status = "primary",
                                       size = "xs",
                                       icon = icon("gear"), width = "200px",
                                       tooltip = tooltipOptions(title = "Sector selection")
                                     )))
                     ),
           verbatimTextOutput(ns("test")),
           #verbatimTextOutput(ns("test2")),
           verbatimTextOutput(ns("test3"))
          )
}
simulationView <- function(input,output,session,selection.list){
  ns <- session$ns
  
  load2 <- LoadData$new(NULL,NULL,NULL,NULL)
  
  observe({
    updateSelectInput(session,'choice.sector', choices = unique(load2$a.mapping$sector))
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('dropdown')
               } else {
                 shinyjs::show('dropdown')
               })
  
  to.delete1 <- eventReactive(selection.list$runsimulation.selected()[1],{round(runif(1,5,55),0)})
  to.delete2 <- eventReactive(selection.list$runsimulation.selected()[1],{round(runif(1,10,60),0)})
  to.delete3 <- eventReactive(selection.list$runsimulation.selected()[1],{round(runif(1,20,90),0)})
  
  output$gkiexpectedrank <- renderUI(custom.card(text = paste0("GKI expected rank ",forecasted.year,':'),int = to.delete1(),id = 1))
  output$gkimodifiedrank <- renderUI(custom.card(text = paste0("GKI modified rank ",forecasted.year,':'),int = to.delete2(),id = 2))
  output$gkisimulatedrank <- renderUI(custom.card(text = paste0("GKI simulated rank ",forecasted.year + 1,':'),int = to.delete3(),id = 3))
  
  output$test <- renderPrint({selection.list$runsimulation.selected()[1]})
  # output$test2 <- renderPrint({
  #   pre <- Preprocessing$new()$load.bind('Economy',selection.list$country.selected())
  #   pre
  # })
  output$test3 <- renderPrint({selection.list$runsimulation.selected()[1] == 0})
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('gkiexpectedrank')
               } else {
                 shinyjs::show('gkiexpectedrank')
               })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('gkimodifiedrank')
               } else {
                 shinyjs::show('gkimodifiedrank')
               })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('gkisimulatedrank')
               } else {
                 shinyjs::show('gkisimulatedrank')
               })
  
  to.delete4 <- eventReactive(selection.list$runsimulation.selected()[1],{round(runif(1,55,90),1)})
  to.delete5 <- eventReactive(selection.list$runsimulation.selected()[1],{round(runif(1,55,90),1)})
  to.delete6 <- eventReactive(selection.list$runsimulation.selected()[1],{round(runif(1,55,90),1)})
  
  output$title.gki.scores <- renderUI({
    HTML('<font size="6" color=#2c3e50>Global Knowledge Index scores</font>')
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('title.gki.scores')
               } else {
                 shinyjs::show('title.gki.scores')
               })
  
  output$gauge.gkiinitialvalue <- renderEcharts4r({
    
    e_charts() %>%
      e_gauge(
        to.delete4(),
        "",
        radius = "70%",
        axisLine = list(
          lineStyle = list(
            width = 25,
            color = list(
              c(.2, "#6793bf"),
              c(.4, "#577ca1"),
              c(.8, "#466482"),
              c(1, "#2c3e50")
            )
          )
        )
      ) %>%
      e_title("",paste0("GKI expected value ",forecasted.year)) %>%
      e_tooltip() %>%
      e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  })
  
  output$gauge.gkiexpectedvalue <- renderEcharts4r({
    
    e_charts() %>%
      e_gauge(
        to.delete5(),
        "",
        radius = "70%",
        axisLine = list(
          lineStyle = list(
            width = 25,
            color = list(
              c(.2, "#6793bf"),
              c(.4, "#577ca1"),
              c(.8, "#466482"),
              c(1, "#2c3e50")
            )
          )
        )
      ) %>%
      e_title("",paste0("GKI modified value ",forecasted.year)) %>%
      e_tooltip() %>%
      e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  })
  
  output$gauge.gkisimulatedvalue <- renderEcharts4r({
    
    e_charts() %>%
      e_gauge(
        to.delete6(),
        "",
        radius = "70%",
        axisLine = list(
          lineStyle = list(
            width = 25,
            color = list(
              c(.2, "#6793bf"),
              c(.4, "#577ca1"),
              c(.8, "#466482"),
              c(1, "#2c3e50")
            )
          )
        )
      ) %>%
      e_title("",paste0("GKI simulated value ",forecasted.year + 1)) %>%
      e_tooltip() %>%
      e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('gauge.gkiinitialvalue')
               } else {
                 shinyjs::show('gauge.gkiinitialvalue')
               })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('gauge.gkiexpectedvalue')
               } else {
                 shinyjs::show('gauge.gkiexpectedvalue')
               })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('gauge.gkisimulatedvalue')
               } else {
                 shinyjs::show('gauge.gkisimulatedvalue')
               })
  
  output$economy <- renderRHandsontable({
    res <- Data$new(sector = 'Economy',country = selection.list$country.selected(),
                    year = selection.list$growthrate.selected(),user.value = selection.list$eco()[4],
                    is.simulated = TRUE)
    rhandsontable(res$tabpanel.simulation.values(),
    width = 1700, height = 400) %>%
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
                  width = 1700, height = 400) %>%
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
                  width = 1700, height = 400) %>%
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
                  width = 1700, height = 400) %>%
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
                  width = 1700, height = 400) %>%
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
                  width = 1700, height = 400) %>%
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
                  width = 1700, height = 400) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_col(2, readOnly = TRUE) %>%
      hot_col(3, readOnly = TRUE) %>%
      hot_col(4, readOnly = TRUE)
  })
  
  output$title.sector.indices <- renderUI({
    HTML('<font size="6" color=#2c3e50>Sector Indices</font>')
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('title.sector.indices')
               } else {
                 shinyjs::show('title.sector.indices')
               })
  
  output$bar.sector <- renderEcharts4r({
    res <- data.frame(sector = unique(load2$a.mapping$sector),
                      Simulated =  round(runif(length(unique(load2$a.mapping$sector)),60,100),1))
    res %>%
      e_charts(sector) %>%
      e_bar(Simulated, max=100, name=selection.list$country.selected(), barWidth=40,height=200,
            label=list(show=T,rotate=90,formatter=htmlwidgets::JS("function(params){return(params.name);}"))) %>%
      e_legend(show = T, textStyle = list(color='#00538B', fontSize = 10, fontfamily="Lato"),itemGap=150, orient = "horizontal", align="auto", top="20", icons = c("roundRect")) %>%
      e_x_axis(show=FALSE, axisLabel = list(fontSize = 8)) %>%
      e_color(c("#00538B")) %>%
      e_tooltip(trigger = "axis") %>%
      e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('bar.sector')
               } else {
                 shinyjs::show('bar.sector')
               })
  
  output$title.pillars.indices <- renderUI({
    HTML('<font size="6" color=#2c3e50>Pillars Indices</font>')
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('title.pillars.indices')
               } else {
                 shinyjs::show('title.pillars.indices')
               })
  
  output$bar.pillars <- renderEcharts4r({
    res <- cbind(unique(load2$a.mapping[,c(1,2)]),Simulated =  round(runif(nrow(unique(load2$a.mapping[,c(1,2)])),60,100),1))
    res <- res[res$sector==input$choice.sector,]
    res %>%
      e_charts(pillar) %>%
      e_bar(Simulated, max=100, name=selection.list$country.selected(), barWidth=40,height=200,
            label=list(show=T,rotate=90,formatter=htmlwidgets::JS("function(params){return(params.name);}"))) %>%
      e_legend(show = T, textStyle = list(color='#6793bf', fontSize = 10, fontfamily="Lato"),itemGap=150, orient = "horizontal", align="auto", top="20", icons = c("roundRect")) %>%
      e_x_axis(show=FALSE, axisLabel = list(fontSize = 8)) %>%
      e_color(c("#6793bf")) %>%
      e_tooltip(trigger = "axis") %>%
      e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  })
  
  observeEvent(selection.list$runsimulation.selected()[1],
               if (selection.list$runsimulation.selected()[1] == 0){
                 shinyjs::hide('bar.pillars')
               } else {
                 shinyjs::show('bar.pillars')
               })
  
}





