#' @import data.table
#' @import httr2
#' @import rvest
#' @import stringr
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
portal_url        <- "https://ww3.capsim.com/professor/portal/index.cfm"
decision_url      <-  "https://ww5.capsim.com/capsimplatform/decisions/audit-decision"
measure_weight <- list(
  cumulative_profit     = 50,
  market_share          = 25,
  market_capitalization = 25
)
segment_value <- data.table(
  segment_id      = as.character(1:5),
  segment_name    = c("Traditional","Low End","High End","Performance","Size"),
  nonprice_weight = c(.77,.47,.91,.81,.91),
  high_price      = c(30,25,40,35,35)
)
