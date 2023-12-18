get_value_composition <- function (
    x, segment_level = FALSE, profit_type = c("net_margin","net_income","nopat"), ...
) {
  if ("value_expand" %in% ...names()) {
    value_expand <- as.numeric(list(...)[["value_expand"]])
  } else {
    value_expand <- c(0,0)
  }
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
    merge(parse_rounds(x),
          by    = c("industry_id","round"),
          all.x = TRUE) %>%
    merge(segment_value,
          by = c("segment_id")) %>%
    .[j  = ":="(design_score  = design_score / 100,
                marketing_mod = (awareness + accessibility) / 200,
                ar_mod        = ar_demand_effect(accounts_receivable_days),
                days_in_year  = as_day_of_year(as.Date(paste0(year, "-12-31"))),
                revision_year = as_year(revision_date),
                revision_day  = as_day_of_year(revision_date))] %>%
    .[order(industry_id, team_name, product_name, segment_id, round)] %>%
    .[by = .(industry_id, team_name, product_name, segment_id),
      j  = ":="(avg_design_score = fifelse(is.na(shift(design_score)), design_score, fifelse(
        revision_year == year,
        (shift(design_score) * (revision_day - 1) +
           design_score * (days_in_year - revision_day + 1)) / days_in_year,
        (shift(design_score) + design_score) / 2
      )))] %>%
    .[j  = ":="(
        value    = high_price * (1 + avg_design_score * marketing_mod * ar_mod
                                 * (1 + value_expand[1]) + value_expand[2]) * unit_sales,
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

as_day_of_year <- function (x) {
  if (is.character(x)) {
    x <- as.Date(x, "%d-%b-%Y")
  }
  as.numeric(format(x, "%j"))
}

as_year <- function (x) {
  if (is.character(x)) {
    x <- as.Date(x, "%d-%b-%Y")
  }
  as.numeric(format(x, "%Y"))
}
