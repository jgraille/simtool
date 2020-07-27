selectionViewUI <- function(id){
  ns <- NS(id)
  
  tabPanel("Selection",
           sidebarLayout(
             sidebarPanel(width=3,
                          selectInput(ns("countries"),label="Countries :", choices=NULL, multiple = FALSE, width = "170px"),
                          tags$h5("Instructions")
             ),
             mainPanel()
           ))
}

selectionView <- function(input,ouput,session){
  ns <- session$ns
}