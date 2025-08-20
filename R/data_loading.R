#' Load and Prepare Core Datasets
#' @description Reads and joins the Indian e-commerce datasets.
#' @export
#' @return A tibble with the combined data.
#' @import dplyr
#' @import readr
load_core_data <- function() {
  data_path <- system.file("extdata", "data", package = "RetailScope")

  orders_path <- file.path(data_path, "List of Orders.csv")
  details_path <- file.path(data_path, "Order Details.csv")

  orders <- readr::read_csv(orders_path, show_col_types = FALSE)
  details <- readr::read_csv(details_path, show_col_types = FALSE)

  combined_data <- orders %>%
    left_join(details, by = "Order ID")

  return(combined_data)
}
