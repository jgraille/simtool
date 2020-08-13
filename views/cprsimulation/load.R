data.pillars<-utils::read.csv("data/GKI2019_pillar_all_flat.csv", header = TRUE, sep = ',')
data.pillars.avg <- data.pillars %>% group_by(Sector,Pillar) %>% summarise(avg=mean(Value)) %>% ungroup()
data.pillars <- merge(data.pillars,data.pillars.avg,by = c("Sector","Pillar"),all.x = TRUE)
data.pillars$upper.pillar <- toupper(data.pillars$Pillar)
data.pillars$upper.sector <- toupper(data.pillars$Sector)

data.subpillars <- utils::read.csv("data/GKI2019_subpillar_all_flat.csv", header = TRUE, sep = ',')
data.subpillars.avg <- data.subpillars %>% group_by(Pillar,Subpillar) %>% summarise(avg=mean(Value)) %>% ungroup()
data.subpillars <- merge(data.subpillars,data.subpillars.avg,by = c("Pillar","Subpillar"),all.x = TRUE)

data.variables <- utils::read.csv("data/GKI2019_variable_all_flat.csv", header = TRUE, sep = ',') %>% select(-Metric) %>% rename(Subpillar=Sub.Pillar)
data.variables.avg <- data.variables %>% group_by(Pillar,Subpillar,Variable) %>% summarise(avg=mean(Value)) %>% ungroup()
data.variables <- merge(data.variables,data.variables.avg,by = c("Pillar","Subpillar","Variable"),all.x = TRUE)

variables.comments <- read_csv("data/GKI_definitions_n_sources.csv")
data.variables<-merge(data.variables,unique(variables.comments[,c("variable","text_source")]), by.x ="Variable",by.y = "variable", all.x=TRUE)

with_tooltip <- function(tooltip,value) {
  paste("<span title='",tooltip,"'> ",value,"</span>",sep="")
}

html.cell <- function(data.variables){
  index <- 1
  htmlcell <- c()
  for (index in 1:nrow(data.variables)){
    htmlcell <- c(htmlcell,with_tooltip(tooltip = data.variables$text_source[index],value = data.variables$Variable[index]))
  }
  htmlcell
}
data.variables$htmlcell <- html.cell(data.variables)
data.gki.new<-utils::read.csv("data/GKI2019_gki_flat.csv", header = TRUE, sep = ',')
data.gki.new$Value <- round(data.gki.new$Value,1)
data.sector.for.gkicpr <- readRDS("data/datasectorforgkicpr.RDS")
data.sector.worldaverage <- data.sector.for.gkicpr[data.sector.for.gkicpr$Country=='World Average',]
cluster.info<-utils::read.csv("data/Cluster_lookup_table.csv", header = TRUE, sep = ',')
genericinfo <- utils::read.csv("data/GENERIC INFO - FOK.csv", header = TRUE, sep = ',')
genericinfo$Country <- as.character(genericinfo$Country)
genericinfo[genericinfo$Country=='Viet Nam',]$Country <- "Vietnam"
countries.mapping<-utils::read.csv("data/Countries_mapping_table.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)