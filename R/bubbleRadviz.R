#' A Plotting Function for the Radviz Object
#' 
#' Plots the Dimensional Anchors and projected data points in a 2D space.
#' 
#' @param x a radviz object as produced by do.radviz
#' @param main [Optional] a title to the graph, displayed on top
#' @param group the name of the grouping variable used to aggregate the data
#' @param color [Optional] the name of the variable used to color the points
#' @param size the size range for the plot
#' 
#' @details This function allows for the projection of clusters in Radviz (for example results of
#'            the SPADE algorithm), where the cluster size is derived from the number of events
#'            that fall into a specific cluster.
#'          If color is not specified the grouping variable is used.
#' 
#' @return the internal ggplot2 object plus added layers, allowing for extra geoms to be added
#' 
#' @example examples/example-do.radviz.R
#' @examples
#' bubbleRadviz(rv, group='Species')
#' 
#' @author Yann Abraham
#' 
#' @importFrom stats median reorder
#' @importFrom dplyr `%>%` filter select mutate group_by summarise_at count left_join .data sym
#' @importFrom rlang `:=` 
#' @importFrom ggplot2 ggtitle aes_string geom_point scale_color_gradient scale_size
#' 
#' @export
bubbleRadviz <-
  function(x, 
           main = NULL, 
           group = NULL,
           color = NULL,
           size = c(3,16)) {
    
    if(is.null(group)) {
      stop('Group must be set to a grouping column')
    }
    
    if(!is.null(color)) {
      if(!color %in% colnames(x$proj$data)) {
        stop(color,'is not a valid column name')
      } else if(!is.numeric(x$proj$data[,color])) {
        stop('color must correspond to a numeric vector')
      }
    }
    
    p <- x$proj+
      ggtitle(main)
    
    dims <- c(color,'rx','ry')
    
    slayer <- geom_point(data=left_join(x$proj$data %>% 
                                          filter(!.data$rvalid) %>% 
                                          select(c(dims,group)) %>% 
                                          group_by(!!sym(group)) %>% 
                                          summarise_at(.vars=dims,.funs=median),
                                        x$proj$data %>% 
                                          count(!!sym(group)),
                                        by=group) %>%
                           mutate(!!group:=factor(!!sym(group)),
                                  !!group:=reorder(!!sym(group),.data$n,max)),
                         aes_string(x='rx',y='ry',
                                    color=ifelse(is.null(color),group,color),
                                    size='n'))
    
    p$layers <- c(slayer,p$layers)
    
    p <- p+
      scale_size(range=size)
    
    if(!is.null(color)) {
      p <- p+scale_color_gradient(low='grey90',high='dodgerblue4')
    }
    
    return(p)
  }
