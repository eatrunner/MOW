library(dplyr)
library(readr)
library(GA)

load.data <- function(file_name) {
  data <- read_csv(file_name, col_names = TRUE,
                    col_types = list( col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_integer()))
  return(data)
}
redWine <- load.data("winequality/winequality-red.csv")
whiteWine <- load.data("winequality/winequality-white.csv")