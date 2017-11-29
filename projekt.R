library(dplyr)
library(readr)
library(GA)
library(rpart)

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
v <- split(redWine, 1:2)
redWineT <- v$`1`
redWineV <- v$`2`
v <- split(whiteWine, 1:2)
whiteWineT <- v$`1`
whiteWineV <- v$`2`