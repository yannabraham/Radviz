#' @rdname Radviz-deprecated
#' @export
identify.radviz <- function(x,...) {
  .Defunct(new='plotly::ggplotly',
           msg='Radviz now relies on ggplot2 - to identify points use the plotly package')
  return(x)
}