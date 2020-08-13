server <- function(input,output,session){
  selection.list <- callModule(selectionView,id="selectionView")
  callModule(cprsimulationView,id="simulationView",selection.list=selection.list)
}