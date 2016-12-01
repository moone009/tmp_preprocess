#' @title Remove redundant white space.
#' @description
#' Remove whitespace from a character or vector. Note that this will always return a character. 
#' 
#' @param x is the value to be trimmed
#' @return a value without any extra whitespace
#' @examples
#' trim('This value      has    an unnecessary amount    of whitespace    ')
#' 
#' ## Performing the trim function on a vector
#' iris$Species <- paste(iris$Species,'    Species',sep='')
#' head(iris)
#' ## Correct the whitespacing
#' iris$Species <- trim(iris$Species)
#' ## View our corrected dataframe
#' head(iris)
#' rm(iris)

trim <- function(x) {
    require(stringr)
    x <- gsub("  ", " ", x)
    x <- gsub("  ", " ", x)
    x <- gsub("  ", " ", x)
    x <- gsub("  ", " ", x)
    x <- str_trim(x, side = c("both"))
    return(x)
}
