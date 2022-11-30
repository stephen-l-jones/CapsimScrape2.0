get_profitability <- function (x) {
  parse_financials(x) %>%
    .[j = ":="(
      nopat = ebit * (1 - tax_expense / (ebit - interest_expense)),
      excess_cash = calc_excess_cash(cash, current_liabilities, current_assets)
    )] %>%
    .[by = .(industry_id, team_key),
      j  = ":="(
        cogs     = variable_cost,
        sga      = period_cost,
        avg_ic   =
          ((total_assets - accounts_payable - excess_cash)
           + (shift(total_assets) - shift(accounts_payable) - shift(excess_cash))) / 2,
        avg_nwc  =
          ((current_assets - accounts_payable - excess_cash)
           + (shift(current_assets) - shift(accounts_payable) - shift(excess_cash))) / 2,
        avg_ppe  =
          (fixed_assets + shift(fixed_assets)) / 2
      )] %>%
    merge(
      parse_teams(x),
      by = c("industry_id","team_key")
    ) %>%
    .[j = .(
      industry_id, round, team_name,
      sales, cogs, sga, nopat, excess_cash, avg_ic, avg_nwc, avg_ppe,
      roic             = nopat / avg_ic,
      return_on_sales  = nopat / sales,
      capital_turnover = sales / avg_ic,
      cogs_over_sales  = variable_cost / sales,
      sga_over_sales   = period_cost / sales,
      nwc_over_sales   = avg_nwc / sales,
      ppe_over_sales   = avg_ppe / sales
    )]
}

calc_excess_cash <- function (cash, current_liabilities, current_assets) {
  pmax(0, cash - pmax(0, current_liabilities - current_assets + cash))
}
