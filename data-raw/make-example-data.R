# data-raw/make-example-data.R
library(readr)
library(usethis)

mynet <- read_tsv("data-raw/tsv/mynet.tsv", show_col_types = FALSE)
mynet_nodeset <- read_tsv("data-raw/tsv/mynet_nodesetfile.tsv", show_col_types = FALSE)

use_data(mynet, mynet_nodeset, overwrite = TRUE)
