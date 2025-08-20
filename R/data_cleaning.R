#' Clean and Transform Sales Data
#' @param data A dataframe containing the raw sales data.
#' @export
#' @return A cleaned and transformed dataframe.
#' @import dplyr
#' @importFrom lubridate dmy
clean_sales_data <- function(data) {
  cleaned_data <- data %>%
    transmute(
      order_id = `Order ID`,
      customer_unique_id = CustomerName,
      order_purchase_date = dmy(`Order Date`),
      payment_value = Amount,
      customer_state = State
    ) %>%
    filter(
      !is.na(order_id),
      !is.na(order_purchase_date),
      !is.na(payment_value)
    )

  return(cleaned_data)
}
