#' Text annotations for for the Radviz Plots
#' 
#' Text draws the strings given in the vector labels at the coordinates given by the radviz projection
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top if add is \code{TRUE}
#' @param labels the name of the variable used for labeling (see details)
#' @param size [Logical] if \code{TRUE} labels are sized after the number of points they correspond to
#' @param ...	further arguments to be passed to or from other methods (not implemented)
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' text(rv,labels='Species')
#' 
#' @author Yann Abraham
#' 
#' @importFrom stats median reorder
#' @importFrom dplyr `%>%` filter select mutate group_by summarise_at count left_join .data sym
#' @importFrom rlang `:=` 
#' @importFrom ggplot2 ggtitle aes_string geom_text scale_radius
#' 
#' @export
text.radviz <- function (x,..., 
                         main=NULL, 
                         labels = NULL,
                         size = FALSE) {
  if(is.null(labels)) {
    stop('labels must be provided')
  }
  
  if(!labels %in% colnames(x$proj$data)) {
    stop(labels,' is not a valid column')
  }
  
  if(is.numeric(x$proj$data[,labels])) {
    stop('labels must be a valid grouping column')
  }
  
  p <- x$proj+
    ggtitle(main)
  
  dims <- c('rx','ry')
  
  df <- left_join(x$proj$data %>% 
                    filter(!.data$rvalid) %>% 
                    select(c(dims,labels)) %>% 
                    group_by(!!sym(labels)) %>% 
                    summarise_at(.vars=dims,.funs=median),
                  x$proj$data %>% 
                    count(!!sym(labels)),
                  by=labels) %>%
    mutate(!!labels:=factor(!!sym(labels)),
           !!labels:=reorder(!!sym(labels),.data$n,max))
  
  if(is.logical(size)) {
    if(size) {
      slayer <- geom_text(data=df,
                          aes_string(x='rx',y='ry',
                                     label=labels,
                                     size='n'))
    } else {
      slayer <- geom_text(data=df,
                          aes_string(x='rx',y='ry',
                                     label=labels))
    }
  } else {
    if(is.numeric(size)) {
      if(length(size)==1) {
        slayer <- geom_text(data=df,
                            aes_string(x='rx',y='ry',
                                       label=labels),
                            size=size)
      } else if(length(size)==2) {
        slayer <- geom_text(data=df,
                            aes_string(x='rx',y='ry',
                                       label=labels,
                                       size='n'))
      } else {
        stop('size must be a numeric vector of length 1 or 2')
      }
    } else {
      stop('size must be either logical or a numeric vector')
    }
  }
  
  p$layers <- c(slayer,p$layers)
  
  if(is.numeric(size) & length(size)==2) {
    p <- p + scale_radius(range=size)
  }
  
  return(p)
}
