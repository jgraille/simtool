source("config.R")
source("views/selectionView.R")

ui <- navbarPage("GKI-Simulation tool",theme=shinytheme("flatly"),
                 selectionViewUI(id="selectionView")
)