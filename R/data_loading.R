#' Load and Prepare Core Datasets
#' @description Reads and joins the core olist datasets from within the package.
#' @export
#' @return A tibble with the combined data.
#' @import dplyr
#' @import readr
load_core_data <- function() {
  # Find the data folder inside the installed package
  data_path <- system.file("extdata", "data", package = "RetailScope")

  orders_path <- file.path(data_path, "olist_orders_dataset.csv")
  payments_path <- file.path(data_path, "olist_order_payments_dataset.csv")
  customers_path <- file.path(data_path, "olist_customers_dataset.csv")

  orders <- readr::read_csv(orders_path, show_col_types = FALSE)
  payments <- readr::read_csv(payments_path, show_col_types = FALSE)
  customers <- readr::read_csv(customers_path, show_col_types = FALSE)

  combined_data <- orders %>%
    left_join(customers, by = "customer_id") %>%
    left_join(payments, by = "order_id")

  return(combined_data)
}
