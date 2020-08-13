source("config.R")
#source("data/SimData.R")
source("views/selection.R")
source("views/cprsimulation.R")
source("views/R6.R")
source("views/utils.R")

# les valeurs taggés Simulated ne doivent pas être altérées par les input users
# en recliquant sur les growth rate reinitialise les bonne valeurs. 
# je vais mettre une bouton run simulation qui lancera les calculs pour l'autre onglet + ira altérer la table en ecriture sur le visu definitivement
# bouton reset pour revenir au valeur initiale



ui <- fluidPage(
  useShinyjs(debug = TRUE),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(style="padding: 1px 0px; width: '100%'",titlePanel(title="",windowTitle = "UNDP")),
  list(
    tags$img(src="https://knowledge4all.com/images/logo2-en.svg",witdh=120,height=60),
    tags$span(tags$span(img(src="https://knowledge4all.com/images/partner_final_2%20_Copy.png", width=350,height=59),
                        style="position:absolute;right:2em;"))
  ),
  br(),
  br(),
  tabsetPanel(id = "tabs",
              tabPanel("Selection",
                       br(),
                       selectionViewUI(id="selectionView")),
              tabPanel("Simulation",
                       br(),
                       cprsimulationViewUI(id="simulationView"))
  )
)