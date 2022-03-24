#' Complete ggplot2 theme for Radviz projections
#' 
#' A complete Radviz theme based on `ggplot2::theme_light`
#' 
#' @inheritParams ggplot2::theme_light
#' 
#' @details on top of `ggplot2::theme_light` this theme removes axis title, text and ticks, 
#' as well as the reference grid. See \code{\link[ggplot2]{theme}} for details.
#' 
#' @return a complete ggplot2 theme
#' 
#' @example examples/example-do.radviz.R
#' @examples 
#' plot(rv,main='Iris projection')
#' plot(rv,main='Iris projection')+
#'   theme_radviz(base_size=16)
#' 
#' @author Yann Abraham
#' @importFrom ggplot2 theme theme_light element_blank %+replace%
#' @export
theme_radviz <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                          base_rect_size = base_size/22) 
{
  half_line <- base_size/2
  theme_light(base_size = base_size,
              base_family = base_family, 
              base_line_size = base_line_size,
              base_rect_size = base_rect_size) %+replace% 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          complete = TRUE)
}
