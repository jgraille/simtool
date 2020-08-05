custom.card <- function(text,int,id){
  wellPanel(HTML(
    paste0('<div id=',id,' class = "indicators-two" style = "padding:5px">',
           '<div class="text"><font size="2"><center>',text,'</center></font></div>',
           '<div style="font-weight:700;"><font size="5"><center>',int,'<center/></font></div>',
           '</div>')))
}