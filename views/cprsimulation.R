source("views/cprutils.R")
cprsimulationViewUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(style="background-image: url('Background image.png'); background-size: cover;", #padding="0px", offset=0,
             fluidRow(
               column(width=1),
               column(width=2,align='center',
                      style = flagstyle(input$countries),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br()),
               column(width=2, align = "center",
                      br(),
                      uiOutput(ns("info.zero"))),
               column(width=1),
               column(width=5, align="center",
                      fluidRow(
                        column(width=4, align="center", #offset = 1,
                               br(),
                               indicators.two(text = "GKI Rank",
                                              int = paste0(genericinfo[which(r$genericinfo$Country == input$countries & genericinfo$Year==2019), ]$GKI..rank.,"/136"),
                                              id = "indicator.rank")),
                        column(width=4, align="center", #offset = 1,
                               br(),
                               indicators.two(text = "GKI Score",
                                              int = "X",
                                              id = "indicator.rank")),
                        column(width=4, align="center", #offset = 1,
                               br(),
                               indicators.two(text = "World Average",
                                              int = "W",
                                              id = "indicator.rank")))),
               column(width=1)
             )
  ))
}

cprsimulationView <- function(input,output,session,selection.list){
  ns <- session$ns
  
  observe({
    updateSelectInput(session,"countries",choices=unique(selection.list$country.selected()))
  })
  
  observe({
    updateSelectInput(session,"years",choices=2019)
  })
  
}
