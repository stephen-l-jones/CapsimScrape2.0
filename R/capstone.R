#' @export
read_capstone <- function (industry_id, round = "0") {
  industries    <- unique(industry_id)
  rounds        <- as.character(round)
  capstone_data <- get_capstone(industry_id, round)

  structure(
    list(
      data                = capstone_data,
      bonds               = function () parse_bonds(capstone_data),
      final_score         = function(
        min_score = 70,
        max_score = 100,
        benchmark_score = 95,
        benchmark_team = "Chester",
        intensity_adjustment = 0,
        intensity_factor = 1
      ) get_final_score(capstone_data, min_score, max_score, benchmark_score, benchmark_team,
                        intensity_adjustment, intensity_factor),
      financial_decisions = function () parse_finance_decisions(capstone_data),
      financials          = function () parse_financials(capstone_data),
      market_products     = function () parse_market_products(capstone_data),
      market_teams        = function () parse_market_teams(capstone_data),
      markets             = function () parse_markets(capstone_data),
      product_decisions   = function () parse_product_decisions(capstone_data),
      products            = function () parse_products(capstone_data),
      profitability       = function () get_profitability(capstone_data),
      rounds              = function () parse_rounds(capstone_data),
      selected_stats      = function () parse_selected_stats(capstone_data),
      stocks              = function () parse_stocks(capstone_data),
      success_measures    = function () get_success_measures(capstone_data),
      teams               = function () parse_teams(capstone_data),
      value_composition   = function (
        segment_level = FALSE,
        profit_type = c("net_margin","net_income","nopat")
      ) get_value_composition(capstone_data, segment_level, profit_type),
    workforce           = function () parse_workforce(capstone_data)
    ),
    class       = "capstone",
    industry_id = industries,
    round       = rounds
  )
}
