# Global Env
forecasted.year <- 2022 #expected
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
                             print(select)
                             pillar.subpillar.variable <- dplyr::bind_rows(
                               # self$data.pillars %>% dplyr::filter(Pillar %in% unique(select$pillar)) %>% select(-Sector) %>% rename(Indicator=Pillar),
                               # self$data.subpillars %>% dplyr::filter(Subpillar %in% unique(select$subpillar)) %>% select(-Pillar) %>% rename(Indicator=Subpillar),
                               na.omit(self$load$a.variables) %>% dplyr::filter(Sub.Pillar %in% unique(select$subpillar)) %>% select(-Pillar,-Metric,-sector_id,-Sub.Pillar) %>% 
                                 dplyr::rename(Indicator=Variable) %>% dplyr::filter(Country==country))
                             # this simulated history is here as a temporary situation.
                             histo <- lapply(seq(from=1,to=5,by = 0.5),function(x) {pillar.subpillar.variable$Value - x})
                             # df.binded is the dataframe with historic datas and the last year (here 2019)
                             pillar.subpillar.variable.histo <- bind_cols(pillar.subpillar.variable,bind_cols(histo))
                             pillar.subpillar.variable.histo
                           }
                         )
                     )

Forecasted <- R6Class("Forecasted",
                      public = list(
                        preprocessing.load.bind = NULL,
                        sector = NULL,
                        country = NULL,
                        initialize = function(sector,country){
                          self$sector = sector
                          self$country = country
                          self$preprocessing.load.bind = Preprocessing$new()$load.bind(self$sector,self$country)
                        },
                        calculate.method1 = function(){
                          # df is a subset with only the values columns
                          df <- df.binded[,c(3,5:(5+year-1))]
                          # in the df dataframe the columns are ordered by descending
                          # the first column will be the most recent year
                          # the last column will be the oldest year
                          # if year=2020, meaning (Value_2020 - Value_2019) / Value_2019, here we will have (considering the first column the 2020 col)
                          # (df[,1] - df[2])/df[,2]
                          growth.rate.selected <- sapply(1:year, function(x){(df[,x] - df[,(x+1)])/df[,(x+1)]})
                          # growth.rate.selected is a matrix with n growth rate columns for each year and n=year)
                          avg.growth.years.selected <- rowSums(growth.rate.selected)/year
                          expected <- df.binded[,3]*sapply(growth.rate.selected,function(x){1+x})
                          return(cbind(df.binded[,c(1,2,3,4)],expected))
                        },
                        onhisto = function(year,user.value,is.simulated){
                          # df is a subset with only the values columns
                          if (is.simulated & !is.null(user.value)){
                            df <- cbind(user.value,self$preprocessing.load.bind[,c(3,5:(5+year-2))])
                            last.histo.year <- forecasted.year
                            
                            X <- seq(from=last.histo.year,to=(last.histo.year - year), by = -1)
                          } else {
                            df <- self$preprocessing.load.bind[,c(3,5:(5+year-2))]
                            last.histo.year <- forecasted.year - 1
                            X <- seq(from=last.histo.year,to=(last.histo.year - year + 1), by = -1)
                            }
                          model <- sapply(1:nrow(df),function(x){lm(c(unlist(df[x,]))~X)})

                          calculated <- sapply(1:nrow(df),function(x){model[1,][[x]][1] + model[1,][[x]][2]*(last.histo.year+1)})
                          return(calculated)
                        }
                      ))

Data <- R6Class("Data",
                public = list(
                  selection.panel.data = NULL,
                  sector = NULL,
                  country = NULL,
                  year = NULL,
                  user.value = NULL,
                  is.simulated = NULL,
                  calculated = NULL,
                  initialize = function(sector,country,year,user.value,is.simulated){
                    self$sector = sector
                    self$country = country
                    self$year = year
                    self$user.value = user.value
                    self$is.simulated = is.simulated
                    },
                  tabpanel.selection = function(){
                    forecasted = Forecasted$new(self$sector,self$country)
                    calculated = forecasted$onhisto(self$year,NULL,is.simulated = FALSE)
                    calculated.rank <- forecasted$preprocessing.load.bind$Rank
                    out <- data.frame(forecasted$preprocessing.load.bind[,c(1,3,4)],calculated,calculated.rank)
                    new.colnames <- c("Indicator",paste0("Value ",forecasted.year - 1),paste0("Rank ",forecasted.year - 1),
                                      paste0("Value expected ",forecasted.year),paste0("Rank expected ",forecasted.year))
                    out <- out %>% rename_at(vars(colnames(out)), ~ new.colnames)
                    return(out)
                  },
                  tabpanel.simulation.values = function(){
                    forecasted = Forecasted$new(self$sector,self$country)
                    calculated.expected = forecasted$onhisto(3,NULL,is.simulated = FALSE)
                    calculated = forecasted$onhisto(3,self$user.value,is.simulated = TRUE)
                    out <- data.frame(forecasted$preprocessing.load.bind$Indicator,calculated.expected,self$user.value,calculated)
                    new.colnames <- c("Indicator",paste0("Expected initial ",forecasted.year),
                                      paste0("Expected modified ",forecasted.year),paste0("Simulated ",forecasted.year + 1))
                    out <- out %>% rename_at(vars(colnames(out)), ~ new.colnames)
                    print(str(out))
                    return(out)
                  }
                  ))


