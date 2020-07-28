source("config.R")
source("data/SimData.R")
source("views/selection.R")


ui <- navbarPage("GKI-Simulation tool",theme=shinytheme("flatly"),
                 selectionViewUI(id="selectionView")
)