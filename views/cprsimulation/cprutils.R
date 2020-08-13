indicators.one <- function(text, int, id){
  HTML(paste0('<div id=',id,' class = "indicators-one">',
              '<div style="font-weight:700;"><font size="5" color=#0C447E>',int,'</font></div>',
              '<div class="text"><font size="2" color=#0C447E>', text,'</font></div>',
              '</div>'))
}

indicators.two <- function(text, int, id){
  HTML(paste0('<div id=',id,' class = "indicators-two" style = "padding:5px">',
              '<div class="text" style="background-color:#0C447E"><font size="2" color=#FFFFFF>', text,'</font></div>',
              '<div style="font-weight:700;"><font size="5" color=#0C447E>',int,'</font></div>',
              '</div>'))
}

flagstyle<-function(country){
  style.char.chain<-paste("background-image:url('", countries.mapping$flag[countries.mapping$official_name==country],
                          "');background-size: contain; background-repeat: no-repeat;margin-left: auto;margin-right: auto;",
                          sep="")
  return(style.char.chain)
}

adaptive.padding.legend <- function(input.country,data.sector.for.gkicpr){
  # Short names list
  level1 <- lapply(unique(data.sector.for.gkicpr$Country),function(x) if(str_length(x)<10) return(x))
  # Medium names list
  level2 <- lapply(unique(data.sector.for.gkicpr$Country),function(x) if(str_length(x)>=10 & str_length(x)<=15) return(x))
  # Long names list
  level3 <- lapply(unique(data.sector.for.gkicpr$Country),function(x) if(str_length(x)>15) return(x))
  if (input.country %in% level1){
    return(50)
  } else if (input.country %in% level2){
    return(80)
  } else if (input.country %in% level3){
    return(150)
  } else{
    return(100)
  }
}

reactable.sector.color.row <- function(index){
  if (index == 1){
    color <- '#8d4893'
  } else if (index == 2){
    color <- '#4ac2bf'
  } else if (index == 3){
    color <- '#de3626'
  } else if (index == 4){
    color <- '#f2cd38'
  } else if (index == 5){
    color <- '#f37f37'
  } else if (index == 6){
    color <- '#92a256'
  } else if (index == 7){
    color <- '#107474'
  } else {
    return(NULL)
  }
  list(color='#FFFFFF',background=color)
}

reactable.pillar.color.text <- function(value){
  if (value == "Knowledge capital"){
    color <- '#8d4893'
  } else if (value == "Educational enabling environment"){
    color <- '#8d4893'
  } else if (value == "Formation and professional training"){
    color <- '#4ac2bf'
  } else if (value == "Features of the labour market"){
    color <- '#4ac2bf'
  } else if (value == "Higher education inputs"){
    color <- '#de3626'
  } else if (value == "Higher education outputs and quality"){
    color <- '#de3626'
  } else if (value == "Research and development"){
    color <- '#f2cd38'
  } else if (value == "Innovation in production"){
    color <- '#f2cd38'
  } else if (value == "Social innovation"){
    color <- '#f2cd38'
  } else if (value == "ICT inputs"){
    color <- '#f37f37'
  } else if (value == "ICT outputs"){
    color <- '#f37f37'
  } else if (value == "Knowledge competitiveness"){
    color <- '#92a256'
  } else if (value == "Economic openness"){
    color <- '#92a256'
  } else if (value == "Financing and value added"){
    color <- '#92a256'
  } else if (value == "Political and institutional"){
    color <- '#107474'
  } else if (value == "Socio-economic"){
    color <- '#107474'
  } else if (value == "Health and environment"){
    color <- '#107474'
  } else {
    return(NULL)
  }
  list(color=color, fontWeight = "bold")
}

reactable.subpillar.color.text <- function(value){
  if (value=="EnroIment"){
    color <- '#8d4893'
  }
  else if (value == "Completion"){
    color <- '#8d4893'
  } else if (value == "Outcomes"){
    color <- '#8d4893'
  } else if (value == "Expenditure on education"){
    color <- '#8d4893'
  } else if (value == "Early childhood education"){
    color <- '#8d4893'
  } else if (value == "School environment"){
    color <- '#8d4893'
  } else if (value == "Continuous training"){
    color <- '#4ac2bf'
  } else if (value == "Educational structure"){
    color <- '#4ac2bf'
  } else if (value == "Qualifications of human capital"){
    color <- '#4ac2bf'
  } else if (value == "Structure of the labour market"){
    color <- '#4ac2bf'
  } else if (value == "Expenditure"){
    color <- '#de3626'
  } else if (value == "Human resources"){
    color <- '#de3626'
  } else if (value == "Graduation"){
    color <- '#de3626'
  } else if (value == "Quality of universities"){
    color <- '#de3626'
  } else if (value == "Employment after graduation"){
    color <- '#de3626'
  } else if (value == "Competency of students"){
    color <- '#de3626'
  } else if (value == "Research and development inputs"){
    color <- '#f2cd38'
  } else if (value == "Research and development outputs"){
    color <- '#f2cd38'
  } else if (value == "Inputs of innovation in production"){
    color <- '#f2cd38'
  } else if (value == "Outputs of innovation in production"){
    color <- '#f2cd38'
  } else if (value == "Inputs of social innovation"){
    color <- '#f2cd38'
  } else if (value == "Outputs of social innovation"){
    color <- '#f2cd38'
  } else if (value == "Infrastructure"){
    color <- '#f37f37'
  } else if (value == "Sector competitiveness"){
    color <- '#f37f37'
  } else if (value == "Subscriptions"){
    color <- '#f37f37'
  } else if (value == "Usage by individuals"){
    color <- '#f37f37'
  } else if (value == "Usage by government and institutions"){
    color <- '#f37f37'
  } else if (value == "Impact on development"){
    color <- '#f37f37'
  } else if (value == "Enrolment"){
    color <- '#de3626'
  } else if (value == "Economic infrastructure and competition"){
    color <- '#92a256'
  } else if (value == "Competitiveness drivers"){
    color <- '#92a256'
  } else if (value == "Creative economy"){
    color <- '#92a256'
  } else if (value == "Trade"){
    color <- '#92a256'
  } else if (value == "Financing and taxes"){
    color <- '#92a256'
  } else if (value == "Domestic value added"){
    color <- '#92a256'
  } else if (value == "Political"){
    color <- '#107474'
  } else if (value == "Institutional"){
    color <- '#107474'
  } else if (value == "Gender parity"){
    color <- '#107474'
  } else if (value == "Empowerment"){
    color <- '#107474'
  } else if (value == "Health"){
    color <- '#107474'
  } else if (value == "Environment"){
    color <- '#107474'
  } else {
    return(NULL)
  }
  list(color=color)
}

reactable.pillar.color.row <- function(index){
  list(background = "rgba(0, 0, 0, 0.05)")
}

selected.values.pillars.avg <- function(field, country,data.pillars){
  data.pillars[data.pillars$Sector==as.character(field) & data.pillars$Country==country,]
}

selected.values.subpillars.avg <- function(pillar,country,data.subpillars){
  data.subpillars$Subpillar <- as.character(data.subpillars$Subpillar)
  data.subpillars[data.subpillars$Pillar=='Knowledge capital' & data.subpillars$Subpillar=='Enrolment', ]$Subpillar <- 'EnroIment'
  data.subpillars$Subpillar <- as.factor(data.subpillars$Subpillar)
  data.subpillars[data.subpillars$Pillar==as.character(pillar) & data.subpillars$Country==country,]
}

selected.values.variables.avg <- function(subpillar,country,data.variables){
  data.variables[data.variables$Subpillar==subpillar & data.variables$Country==country,]
} 