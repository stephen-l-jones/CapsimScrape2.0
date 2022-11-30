get_value_composition <- function (
    x, segment_level = FALSE, profit_type = c("net_margin","net_income","nopat")
) {
  profit_type <- match.arg(profit_type)
  value_composition <- parse_market_products(x) %>%
    merge(parse_products(x)[j = .(industry_id, round, product_name, product_id, team_key,
                                 product_unit_sales, product_net_margin,
                                 product_variable_cost, product_period_cost)],
          by = c("industry_id","round","product_name")) %>%
    merge(parse_financials(x)[j = .(
      industry_id, round, team_key,
      sales,
      effective_tax_rate = tax_expense / (ebit - interest_expense),
      nonsegment_costs   = other_cost + interest_expense + tax_expense + profit_sharing_expense
    )],
    by = c("industry_id","round","team_key")) %>%
    merge(parse_teams(x),
          by = c("industry_id","team_key")) %>%
    merge(parse_finance_decisions(x),
          by    = c("industry_id","round","team_name"),
          all.x = TRUE) %>%
    merge(segment_value,
          by = c("segment_id")) %>%
    .[j  = ":="(
      value_score = (design_score * (awareness + accessibility) / 200
                     * ar_demand_effect(accounts_receivable_days))
    )] %>%
    .[order(industry_id, team_name, product_name, segment_id, round)] %>%
    .[by = .(industry_id, team_name, product_name, segment_id),
      j  = ":="(
        value_score = (value_score + shift(value_score, fill = 0)) / 2
      )] %>%
    .[j  = ":="(
        value    = (value_score + high_price) * unit_sales,
        revenues = unit_price * unit_sales,
        costs    = (unit_sales / product_unit_sales) *
          if (profit_type == "net_margin") {
            product_variable_cost + product_period_cost
          } else if (profit_type == "net_income") {
            product_variable_cost + product_period_cost +
              nonsegment_costs * product_unit_sales * unit_price / sales
          } else if (profit_type == "nopat") {
            product_variable_cost + product_period_cost + product_net_margin * effective_tax_rate
          }
      )]
  if (identical(segment_level, FALSE)) {
    group_key <- c("industry_id","round","team_name")
  } else {
    group_key <- c("industry_id","round","team_name","segment_id")
  }
  value_composition %>%
    .[keyby = eval((group_key)),
      j  = .(
        V = sum(value) / sum(unit_sales),
        P = sum(revenues) / sum(unit_sales),
        C = sum(costs) / sum(unit_sales),
        d = sum(unit_sales)
      )]
}

ar_demand_effect <- function (accounts_receivable_days) {
  accounts_receivable_days <- ifelse(is.na(accounts_receivable_days), 30, accounts_receivable_days)
  1 - pmax(1.058^-accounts_receivable_days * .407 - .007, 0)
}
