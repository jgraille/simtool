SimData <- R6Class("SimData",
                            public = list(
                              data.mapping = NULL,
                              data.pillars = NULL,
                              data.subpillars = NULL,
                              data.variables = NULL,
                            initialize = function(){
                              self$data.mapping <- utils::read.csv("data/mapping.pillars.subpillars.sectorscsv.csv",
                                                                   header = TRUE, sep = ",")
                              self$data.pillars <- utils::read.csv("data/GKI2019_pillar_all_flat.csv",
                                                                   header = TRUE, sep = ",")
                              self$data.subpillars <- utils::read.csv("data/GKI2019_subpillar_all_flat.csv",
                                                                      header = TRUE, sep = ",")
                              self$data.variables <- utils::read.csv("data/GKI2019_variable_all_flat.csv",
                                                                     header = TRUE, sep = ",")
                            },
                            load = function(s,country){
                              select <- self$data.mapping %>% filter(sector==s)
                              pillar.subpillar.variable <- dplyr::bind_rows(self$data.pillars %>%
                                                                              filter(Pillar %in% unique(select$pillar)) %>% select(-Sector) %>%
                                                                              rename(Indicator=Pillar),
                                                                            self$data.subpillars %>%
                                                                              filter(Subpillar %in% unique(select$subpillar)) %>% select(-Pillar) %>%
                                                                              rename(Indicator=Subpillar),
                                                                            self$data.variables %>% 
                                                                              filter(Sub.Pillar %in% unique(select$subpillar)) %>% 
                                                                              select(-Pillar,-Metric,-sector_id,-Sub.Pillar) %>%
                                                                              rename(Indicator=Variable)) %>% filter(Country==country)
                              #histo <- lapply(seq(from=1,to=5,by = 0.5),function(x) {a$Value - x})
                              pillar.subpillar.variable
                            },
                            countries = function(){
                              unique(self$data.pillars$Country)
                            }
                            )
                   )

GrowthRate <- R6Class("GrowRate",
                      public = list(
                        initialize = function(){
                        },
                        calculate = function(s,country,year){
                          pillar.subpillar.variable <- SimData$new()$load(s,country)
                          # this simulated history is here as a temporary situation.
                          histo <- lapply(seq(from=1,to=5,by = 0.5),function(x) {pillar.subpillar.variable$Value - x})
                          # df.binded is the dataframe with historic datas and the last year (here 2019)
                          df.binded <- bind_cols(pillar.subpillar.variable,bind_cols(histo))
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
                        }
                      ))


