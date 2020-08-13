source("views/cprsimulation/load.R")
source("views/cprsimulation/cprutils.R")
cprsimulationViewUI <- function(id){
  ns <- NS(id)
  fluidPage(
    uiOutput(ns('show'))
  )
}

cprsimulationView <- function(input,output,session,selection.list){
  ns <- session$ns
  
  output$info.zero <- renderUI({
    if (is.country.fill()){
      HTML('<center><font size="6" color=#00538B family="Lato" style = "font-weight: 700;">',toupper(selection.list$country.selected()), '</font></center>')
    }
  })
  
  flagstyle<-function(country){
    style.char.chain<-paste("background-image:url('", countries.mapping$flag[countries.mapping$official_name==country],
                            "');background-size: contain; background-repeat: no-repeat;margin-left: auto;margin-right: auto;",
                            sep="")
    return(style.char.chain)
  }
  
  output$show = renderUI({
      fluidRow(
        fluidRow(style="background-image: url('Background image.png'); background-size: cover;", #padding="0px", offset=0,
                 fluidRow(
                   column(width=1),
                   column(width=2,align='center',
                          style = flagstyle(selection.list$country.selected()),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br()),
                   column(width=2, align = "center",
                          br(),
                          uiOutput("info.zero")),
                   column(width=1),
                   column(width=5, align="center",
                          fluidRow(
                            column(width=4, align="center", #offset = 1,
                                   br(),
                                   indicators.two(text = "GKI Rank",
                                                  int = paste0(genericinfo[which(genericinfo$Country == selection.list$country.selected() &
                                                                                   genericinfo$Year==2019), ]$GKI..rank.,"/136"),
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
                 ),
                 br(),
                 fluidRow(
                   column(width=1),
                   column(width = 3, align= "center",
                          fixedRow(
                            column(width=4), #imageOutput("pictogram_1", height="30%")),
                            column(width=5, align="center", indicators.one(text = "GDP (US $ billion)",
                                                                           int = paste('$',format(genericinfo[which(genericinfo$Country == selection.list$country.selected() &
                                                                                                                      genericinfo$Year==2019), ]$GDP.per.capita , big.mark = ",", scientific = FALSE)),
                                                                           id = "indicator.gdp"))
                          ),
                          fixedRow(
                            column(width=4, imageOutput("pictogram_2", height="30%")),
                            column(width=5, align="center",  indicators.one(text = "Population",
                                                                            int = "Z",
                                                                            id ="indicator.population"))
                          ),
                          fixedRow(
                            column(width=4, imageOutput("pictogram_3", height="30%")),
                            column(width=5, align="center", indicators.one(text = "HDI",
                                                                           int = round(genericinfo[which(genericinfo$Country == selection.list$country.selected() &
                                                                                                           genericinfo$Year==2019), ]$HDI,3),
                                                                           id = "indicator.hdi"))
                          )
                   ),
                   column(width=5, align = "center", offset=2,
                          fluidRow(
                            column(width=12, align="left")
                                   #uiOutput(ns("title.cluster")))
                          ),
                          fluidRow(
                            column(width=12, align="left")
                                   #echarts4rOutput(ns('cluster.visual.2'),width = "100%",height = "300px"))
                          ))
                 )))
        
      })
  
}
