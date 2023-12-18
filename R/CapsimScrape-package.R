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
ideal_position <- data.table(
  segment_id  = rep(as.character(1:5), each = 9),
  round       = rep(as.character(0:8), 5),
  performance = c(seq(5.0, 10.6, 0.7),
                  seq(1.7,  5.7, 0.5),
                  seq(8.9, 16.1, 0.9),
                  seq(9.4, 17.4, 1.0),
                  seq(4.0,  9.6, 0.7)),
  size        = c(seq(15.0,  9.4, -0.7),
                  seq(18.3, 14.3, -0.5),
                  seq(11.1,  3.9, -0.9),
                  seq(16.0, 10.4, -0.7),
                  seq(10.6,  2.6, -1.0)),
  x_movement  = rep(c(0.7,0.5,0.9,1.0,0.7), each = 9),
  y_movement  = rep(c(-0.7,-0.5,-0.9,-0.7,-1.0), each = 9)
)
