#' print.boxhist
#'
#' Prints both the boxplot and histogram for the given data frame
#'
#' @param x a list
#' @param which which variable to select
#'
#' @return a boxplot and histogram
#' @export
#'
#' @export print.boxhist
#' @examples \dontrun{
#'  l <- boxhist(ddt)
#'  print(l)}
print.boxhist <- function(x, ...){

  patchwork::wrap_plots(boxhl[x])

}
