get_success_measures <- function (x) {
  merge(
    parse_selected_stats(x)[j = .(
      industry_id, round, team_key, cumulative_profit, market_share = total_market_share
    )],
    parse_stocks(x)[j = .(
      industry_id, round, team_key, market_capitalization
    )],
    by = c("industry_id","round","team_key")
  ) %>%
    .[by = .(industry_id, round),
      j  = ":="(
        cumulative_profit_score =
          pmax(cumulative_profit, 0) / max(cumulative_profit) * measure_weight$cumulative_profit,
        market_share_score =
          market_share / max(market_share) * measure_weight$market_share,
        market_capitalization_score =
          market_capitalization / max(market_capitalization) * measure_weight$market_capitalization
      )] %>%
    .[by = .(industry_id, round),
      j  = ":="(
        total_score =
          cumulative_profit_score +
          market_share_score +
          market_capitalization_score
      )] %>%
    merge(
      parse_teams(x),
      .,
      by = c("industry_id","team_key")
    )
}

get_final_score <- function (
    x, min_score = 70, max_score = 100, benchmark_score = 95, benchmark_team = "Chester",
    relative_factor = 1, intensity_factor = 0
) {
  get_success_measures(x) %>%
    .[by = .(industry_id, round),
      j  = ":="(relative_score = total_score / total_score[team_name == benchmark_team])] %>%
    .[by = .(industry_id, round),
      j  = ":="(intensity = mean(relative_score)^intensity_factor)] %>%
    .[j  = ":="(adjusted_score = benchmark_score +
                  (relative_score^relative_factor - 1) * intensity^sign(relative_score - 1) *
                  (max_score - min_score))] %>%
    .[j  = ":="(final_score = pmin(max_score, pmax(min_score, adjusted_score, total_score)))] %>%
    .[j  = .SD]
}

