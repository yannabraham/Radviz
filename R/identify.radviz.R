#' @rdname Radviz-deprecated
#' @export
identify.radviz <- function(x,...) {
  .Defunct(new='hexplot',
           msg='the do.hex function is not required anymore, use hexplot directly')
  return(x)
}