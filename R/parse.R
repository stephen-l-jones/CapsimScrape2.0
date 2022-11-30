parse_list <- function (x, treepath, name_domain = NULL) {
  parse_recursive <- function (x, treepath) {
    if (!str_detect(treepath[length(treepath)], "^(table|scalar)$")) {
      treepath <- c(treepath, "table")
    }
    for (i in seq_along(treepath)) {
      if (treepath[i] == "list") {
        list_dt <- lapply(x, parse_recursive, treepath = treepath[-seq_len(i)])
        dt <- mapply(function(elem_name, elem_dt) {
          elem_dt[, names(treepath)[i] := elem_name]
          setcolorder(elem_dt, c(ncol(elem_dt),seq_along(elem_dt)[-ncol(elem_dt)]))
          return(elem_dt)
        }, names(list_dt), list_dt, SIMPLIFY = FALSE) %>%
          rbindlist(fill = TRUE)
        return(dt)
      } else if (str_detect(treepath[i], "^(table|scalar)$")) {
        if (any(class(x) %in% c("matrix","data.frame"))) {
          dt <- as.data.table(x)
        } else {
          dt <- x %>%
            lapply(function(elem) {
              if (class(elem) == "list" && is.vector(elem) && length(elem) == 1) {
                elem <- unlist(elem)
              }
              if (class(elem) == "list" || !is.vector(elem) || length(elem) > 1) {
                if (treepath[i] == "scalar") {
                  return (NULL)
                }
                return (list(elem))
              }
              return (elem)
            }) %>%
            do.call(data.table, .)
        }
        return(dt)
      } else if (treepath[i] == "pass") {
        x <- x[[names(treepath)[i]]]
      } else if (treepath[i] == "first") {
        x <- x[[1]]
      } else if (treepath[i] == "last") {
        x <- x[[length(x)]]
      } else {
        stop(sprintf("Unknown treepath value: %s", treepath[i]))
      }
    }
  }
  dt <- parse_recursive(x, treepath)
  if (!identical(name_domain, NULL)) {
    convert <- name_conv[domain == name_domain & capsim_name %in% names(dt)]
    dt <- dt %>%
      subset(select = !(names(dt) %in% convert[exclude == 1, capsim_name])) %>%
      setnames(convert[exclude == 0, capsim_name], convert[exclude == 0, col_name])
  }
  return(dt)
}

parse_rounds <- function (x) {
  tree_stub <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list"
  )
  merge(
    parse_list(x, c(tree_stub, report = "pass", "scalar"), "industry"),
    parse_list(x, c(tree_stub, session = "pass", "scalar"), "industry"),
    by = c("industry_id","round")
  )
}

parse_teams <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "first",
    report      = "pass",
    roster      = "pass",
    teams       = "pass",
    team_key    = "list"
  )
  parse_list(x, tree, "industry")
}

parse_selected_stats <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    highLevelOverview = "pass",
    teams       = "pass",
    team_key    = "list",
    year        = "last"
  )
  parse_list(x, tree, "finance") %>%
    .[j = ":="(
      market_capitalization = NULL
    )] %>%
    .[j = .SD]
}

parse_products <- function (x) {
  key <- c("industry_id","round","product_name")
  prod <- parse_product_rd(x)
  plnt <- parse_plants(x)
  merge(prod,
        plnt[, .SD, .SDcols = names(plnt)[names(plnt) %in% key | !(names(plnt) %in% names(prod))]],
        by = key) %>%
    merge(parse_production_costs(x),
          by = key) %>%
    merge(parse_product_margins(x),
          by = c(key, "product_id","team_key"),
          suffixes = c("","_is"))
}

parse_product_rd <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    researchAndDevelopment = "pass",
    teamProducts = "pass",
    team_key    = "list",
    product_id  = "list"
  )
  parse_list(x, tree, "product")
}

parse_plants <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    production  = "pass",
    plants      = "pass"
  )
  parse_list(x, tree, "product")
}

parse_production_costs <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    production  = "pass",
    costs       = "pass"
  )
  parse_list(x, tree, "product")
}

parse_product_margins <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    finance     = "pass",
    teamRegionalIncomeStatement = "pass",
    team_key    = "list",
    "1"         = "pass",
    products    = "pass",
    product_id  = "list"
  )
  parse_list(x, tree, "finance_product")
}

parse_markets <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    marketing   = "pass",
    segments    = "pass",
    "1"         = "pass",
    segment_id  = "list"
  )
  parse_list(x, tree, "segment")
}

parse_market_products <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    marketing   = "pass",
    products    = "pass",
    "1"         = "pass",
    segment_id  = "list"
  )
  parse_list(x, tree, "marketing")
}

parse_market_teams <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    marketing   = "pass",
    teams       = "pass",
    "1"         = "pass",
    segment_id  = "list",
    team_key    = "list"
  )
  parse_list(x, tree, "marketing_segment")
}

parse_financials <- function (x) {
  merge(
    parse_income_statement(x),
    parse_balance_sheet(x),
    by = c("industry_id","round","team_key")
  ) %>%
    merge(
      parse_cashflow_statement(x),
      by = c("industry_id","round","team_key"),
      suffixes = c("","_cf")
    )
}

parse_income_statement <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    finance     = "pass",
    teamIncomeStatement = "pass",
    team_key    = "list"
  )
  parse_list(x, tree, "finance")
}

parse_balance_sheet <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    finance     = "pass",
    teamBalanceSheet = "pass",
    team_key    = "list",
    "scalar"
  )
  parse_list(x, tree, "finance")
}

parse_cashflow_statement <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    finance     = "pass",
    teamCashFlowStatement = "pass",
    team_key    = "list"
  )
  parse_list(x, tree, "finance")
}

parse_stocks <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    finance     = "pass",
    teamStockTable = "pass",
    team_key    = "list"
  )
  parse_list(x, tree, "finance") %>%
    .[j = ":="(
      market_capitalization = closing_price * shares_outstanding * 10^-3
    )]
}

parse_bonds <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    finance     = "pass",
    teamBondTable = "pass",
    team_key    = "list",
    maturity_year = "list"
  )
  parse_list(x, tree, "finance")
}

parse_workforce <- function (x) {
  tree <- c(
    industry_id = "list",
    courier     = "pass",
    round       = "list",
    report      = "pass",
    workforce   = "pass",
    team_key    = "list"
  )
  parse_list(x, tree, "hr")
}

parse_finance_decisions <- function (x) {
  tree <- c(
    industry_id = "list",
    decisions   = "pass",
    round       = "list",
    team_name   = "list",
    auditDecisions = "pass",
    teamValue  = "pass",
    "scalar"
  )
  dt <- parse_list(x, tree, "finance")
  if (!("team_name" %in% names(dt))) {
    dt[j = ":="(team_name = NA_character_)]
  }
  return(dt)
}

parse_product_decisions <- function (x) {
  tree_stub <- c(
    industry_id = "list",
    decisions   = "pass",
    round       = "list",
    team_name   = "list",
    auditDecisions = "pass",
    products    = "pass",
    product_id  = "list"
  )
  prod <- parse_list(x, c(tree_stub, "scalar"), "product")
  plnt <- parse_list(x, c(tree_stub, plants = "pass", "1" = "pass"), "product")
  fcst <- parse_list(x, c(tree_stub, forecastOrders = "pass", "1" = "pass"), "product")
  dt <- merge(prod, plnt, by = c("industry_id","round","team_name","product_id")) %>%
   merge(fcst, by = c("industry_id","round","team_name","product_id"))
  if (!("team_name" %in% names(dt))) {
    dt[j = ":="(team_name = NA_character_)]
  }
  return(dt)
}
