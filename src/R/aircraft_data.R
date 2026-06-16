library(here)
library(readr)

DATA_FOLDER = here("data")

get_data_file <- function(file_name) {
  result <- read_csv(file.path(DATA_FOLDER, file_name), show_col_types = FALSE)
  return(result)
}

aircraft_data <- list(
  airfoil_wing = get_data_file("airfoil-wing.csv"),
  airfoil_horizontal_stabilizer = get_data_file("airfoil-horizontal-stabilizer.csv"),
  airfoil_vertical_stabilizer = get_data_file("airfoil-vertical-stabilizer.csv"),
  aircraft = get_data_file("aircraft.csv")
)
