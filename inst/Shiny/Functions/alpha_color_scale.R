alpha_color_scale <- function(values = NULL, slider_1 = NULL, slider_2 =NULL, alpha = NULL) {
  # prepare opacity
  opacity = values
  opacity[opacity < slider_1] = 0
  opacity[opacity > slider_2] = 1
  opacity[opacity != 0 & opacity != 1] = (opacity[opacity != 0 & opacity != 1] - min(opacity[opacity != 0 & opacity != 1]))/max(opacity[opacity != 0 & opacity != 1]) * alpha
  return(opacity)
}