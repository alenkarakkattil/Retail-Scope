#' Clean and Transform Sales Data
#' @description Takes the raw, combined olist data and performs cleaning.
#' @export
#' @return A cleaned and transformed dataframe.
#' @import dplyr
#' @importFrom lubridate as_date
clean_sales_data <- function(data) {
  cleaned_data <- data %>%
    filter(
      !is.na(order_id),
      !is.na(customer_unique_id),
      order_status == "delivered"
    ) %>%
    mutate(
      order_purchase_date = as_date(order_purchase_timestamp),
      payment_value = if_else(is.na(payment_value), 0, payment_value)
    ) %>%
    select(
      order_id,
      customer_unique_id,
      order_purchase_date,
      payment_value,
      customer_state
    )

  return(cleaned_data)
}
