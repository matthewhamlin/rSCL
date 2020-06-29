#' @export
scl_theme_minimal <- function (font_size=10) {
    minimal_theme <- ggplot2::theme(
      plot.title = element_text(hjust = 0.5), text = element_text(size = font_size),
      panel.background = element_blank(),
      legend.position = "none",
      strip.background = element_blank()
    )
    return(minimal_theme)
}

#' @export
text_box_plot <- function(box_placement = c(1,2,3),
                          box_dims = c(1,2),
                          fill_box=c("black","blue","green"),
                          fill_text = c("text","text","text"),
                          text_color = "white",
                          text_size = 5,
                          font="Franklin Gothic Medium"){

  rects <- data.frame(x = box_placement,
                      colors = fill_box,
                      text = fill_text)

  plt <-ggplot2::ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
            geom_tile(width = box_dims[1], height = box_dims[2])+
            geom_text(color = text_color, size = text_size,family=font)+
            scale_fill_identity(guide = "none")+
            coord_fixed()+
            theme_void()

  return(plt)
}


# Palettes ####################################################

#' @export
scl_color_warm <- function(palette = c(1,2,3,4)) {
  colors <- custom_warm_palette()
  colors <- colors[palette]
  fill_colors <- ggplot2::scale_fill_manual(values=colors)
  return(fill_colors)
}

#' @export
box_colors <- function(){
  black <- "#333333"
  blue <- "#0046AD"
  apple <-"#40A43B"
  customGrey <- "#a6a6a6"
  colors <- c(black,blue,apple,customGrey)
  return(colors)
}

#' @export
text_color <- function(){
  black <- "#333333"
  white <- "#FFFFFF"
  blue <- "#0046AD"
  purple <- "#4C2C92"
  colors <- c(black,white,blue,purple)
  return(colors)
}

#' @export
custom_warm_palette <- function(){
  colors <- c('dark gray','orange','light blue','dodgerblue4')
  return(colors)
}

#' @export
background_colors <- function(){
  black <- "#26272D"
  dark_grey <- "#515151"
  light_grey <- "#F1F1F1"
  seagull <- "#69B3E7"
  blue <- "#2207EB"
  anakiwa <- "#98E8FF"

  colors <- c(black,dark_grey,light_grey,seagull,blue,anakiwa)
  return(colors)
}

#' @export
notification_colors <- function(){
  red <- "#E4002B"
  red_background <- "#FFEBEE"
  blue <- "#69B3E7"
  light_blue <- "#E1F5FE"
  yellow <- "#F7C305"
  yellow_background <- "#FFF8E1"

  colors <- c(red,red_background,blue,light_blue,yellow,yellow_background)
  return(colors)
}

#' @export
cool_palette <- function(){
  cobalt <- "#0046AD"
  aqua_forest <- "#589E7E"
  daintree <- "#021F22"
  curious_blue <- "#199BCE"
  killarney <- "#315C31"
  antique_brass <- "#C87E5D"
  fun_green <- "#007749"

  colors <- c(cobalt,aqua_forest,daintree,curious_blue,killarney,antique_brass,fun_green)
  return(colors)
}

#' @export
warm_palette <- function(){
  web_orange <- "#F0AB00"
  blue_chill <- "#088490"
  zuccini <- "#04451B"
  fuzzy_wuzzy_brown <- "#C06E51"
  apple <- "#40A43B"
  red_oxide <- "#66050D"
  abbey <- "#565A5C"

  colors <- c(web_orange,blue_chill,zuccini,fuzzy_wuzzy_brown,apple,red_oxide,abbey)
  return(colors)
}
