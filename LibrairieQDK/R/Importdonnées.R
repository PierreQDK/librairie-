library(usethis)

elus_data <- readr::read_delim("/Users/pierrequintindekercadio/Documents/librairie/librairie/elus_sample.csv")

usethis::use_data(elus_data, overwrite = TRUE)

