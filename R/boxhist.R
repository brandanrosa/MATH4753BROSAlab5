#' @title boxhist
#'
#' @description This function takes a df and returns two dfs (one numerical and one categorical). Also a list of potential outliers and plots (boxplot, histogram, and bar-plot)
#'
#'
#' @param df a data frame
#' @param ... passes extra arguments to the function
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_boxplot layer_data aes
#' @importFrom dplyr select select_if %>%
#' @importFrom graphics boxplot
#'
#' @return two dfs and plots
#' @export
#'
#'
#' @examples
#' \dontrun{boxhist(df = ddt)}
boxhist <- function(df, ...) {

  # Order DF Alphabetically
  .data <- NULL
  inout <- NULL
  z <- NULL
  zout <- NULL

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
  outliers <- box_dat$out

  # DF NEW

  # Plots
  boxx <- function(x){
    ggplot(df_num) + geom_boxplot(df_num, mapping = aes(x = .data[[x]]))
  }

  histt <- function(x){
    ggplot(df_num) + geom_histogram(df_num, mapping = aes(x = .data[[x]]))
  }

  col <- names(df_num[c(1:n_num)])

  boxl <- lapply(col, boxx)
  histl <- lapply(col, histt)

  requireNamespace(patchwork)

  patch1 <- boxl[[1]] / histl[[1]]

  patch2 <- boxl[[2]] / histl[[2]]

  patch3 <-  boxl[[3]] / histl[[3]]

  patch4 <- boxl[[4]] / histl[[4]]

  boxhl <- list(patch1, patch2, patch3, patch4)

  list(plots = boxhl,
       taildata = outliers,
       n_char = n_char,
       n_num = n_num,
       df_num = df_num,
       df_char = df_char,
       data = df
  )
}
