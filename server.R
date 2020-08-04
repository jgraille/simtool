server <- function(input,output,session){
  selection.list <- callModule(selectionView,id="selectionView")
  callModule(simulationView,id="simulationView",selection.list=selection.list)
}