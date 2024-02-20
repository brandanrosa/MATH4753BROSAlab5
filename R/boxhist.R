#' @title boxhist
#'
#' @description This function takes a df and returns two dfs (one numerical and one categorical). Also a list of potential outliers and plots (boxplot, histogram, and bar-plot)
#'
#'
#' @param df a data frame
#' @param ... passes extra arguments to the function
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_boxplot layer_data aes_
#' @importFrom dplyr select select_if %>%
#' @importFrom graphics boxplot
#'
#' @return two dfs and plots
#' @export
#'
#'
#' @examples
#' \dontrun{boxhist(df = ddt)}
#'
boxhist <- function(df, ...) {

  # Order DF Alphabetically
  x <- NULL

  df <- df %>%
    select(order(colnames(df)))

  # Num vs Char DFs

  df_num <- df %>%
    select_if(is.numeric)
  df_char <- df %>%
    select_if(is.character)

  # n_num and n_char

  n_num <- ncol(df_num)
  n_char <- ncol(df_char)

  # BP taildata for outlier detection

  box_dat <- boxplot(df_num, plot = FALSE)
  taildata <- box_dat$out

  # DFNew Stuff
  #dfnew <- df_num %>%
  # mutate(z = scale(.data[[x]]),
  #   inout = if_else(is.element(.data[[x]], taildata[[1]]), "out", "in")
  # )

  # Z-Transform Method for outlier detection
  # possibleout <- lapply(df_num, FUN = function(x, k=2){
  #zx <- scale(df_num)
  #zpos <- df_num[abs(zx) >= 2 & abs(zx) <= 3]
  #zout <- df_num[abs(zx) > 3]
  #zin <- mean(df_num) + c(-1, 1)*k*sd(df_num)
  #}
  #  )

  # Plots
  cols <- names(df_num[c(1:4)])

  box <- lapply(cols, function(yvar){
    ggplot(df_num) +
      aes_(x = as.name(yvar)) +
      geom_boxplot()
  })


  histo <- lapply(cols, function(yvar){
    ggplot(df_num) +
      aes_(x = as.name(yvar)) +
      geom_histogram()
  })

  box_patch <- lapply(cols, box)
  histo_patch <- lapply(cols, histo)

  boxl <- list(box_patch, histo_patch)

  list(plots = boxl,
       taildata = taildata,
       n_char = n_char,
       n_num = n_num,
       df_num = df_num,
       df_char = df_char,
       data = df
  )
}

