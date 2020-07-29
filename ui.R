source("config.R")
source("data/SimData.R")
source("views/selection.R")

# les valeurs taggés Simulated ne doivent pas être altérées par les input users
# en recliquant sur les growth rate reinitialise les bonne valeurs. 
# je vais mettre une bouton run simulation qui lancera les calculs pour l'autre onglet + ira altérer la table en ecriture sur le visu definitivement
# bouton reset pour revenir au valeur initiale

ui <- navbarPage("GKI-Simulation tool",theme=shinytheme("flatly"),
                 selectionViewUI(id="selectionView")
)