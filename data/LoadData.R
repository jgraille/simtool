LoadData <- R6Class("LoadData",
                    public = list(
                      mapping = NULL,
                      pillars = NULL,
                      subpillars = NULL,
                      variables = NULL,
                      initialize = function(mapping,pillars,subpillars,variables){
                        self$mapping = mapping
                        self$pillars = pillars
                        self$subpillars = subpillars
                        self$variables = variables
                      }),
                    active = list(
                      a.mapping = function(){
                        self$mapping = utils::read.csv("data/mapping.pillars.subpillars.sectorscsv.csv",header = TRUE, sep = ",")
                      },
                      a.pillars = function(){
                        self$pillars = utils::read.csv("data/GKI2019_pillar_all_flat.csv",header = TRUE, sep = ",")
                      },
                      a.subpillars = function(){
                        self$subpillars = utils::read.csv("data/GKI2019_subpillar_all_flat.csv",header = TRUE, sep = ",")
                      },
                      a.variables = function(){
                        self$variables = utils::read.csv("data/GKI2019_variable_all_flat.csv",header = TRUE, sep = ",")
                      }
                    )
                    )

Preprocessing <- R6Class("Preprocessing",
                         public = list(
                           load = NULL,
                           initialize = function(){
                             self$load = LoadData$new(NULL,NULL,NULL,NULL)
                           },
                           load.bind = function(s,country){
                             # 2019 data + histo
                             select <- self$load$a.mapping %>% dplyr::filter(sector==s)
                             pillar.subpillar.variable <- dplyr::bind_rows(
                               # self$data.pillars %>% dplyr::filter(Pillar %in% unique(select$pillar)) %>% select(-Sector) %>% rename(Indicator=Pillar),
                               # self$data.subpillars %>% dplyr::filter(Subpillar %in% unique(select$subpillar)) %>% select(-Pillar) %>% rename(Indicator=Subpillar),
                               self$load$a.variables %>% dplyr::filter(Sub.Pillar %in% unique(select$subpillar)) %>% select(-Pillar,-Metric,-sector_id,-Sub.Pillar) %>% 
                                 rename(Indicator=Variable)) %>% filter(Country==country)
                             # this simulated history is here as a temporary situation.
                             histo <- lapply(seq(from=1,to=5,by = 0.5),function(x) {pillar.subpillar.variable$Value - x})
                             # df.binded is the dataframe with historic datas and the last year (here 2019)
                             pillar.subpillar.variable.histo <- na.omit(bind_cols(pillar.subpillar.variable,bind_cols(histo)))
                             pillar.subpillar.variable.histo
                           }
                         )
                     )


load <- LoadData$new(NULL,NULL,NULL,NULL)