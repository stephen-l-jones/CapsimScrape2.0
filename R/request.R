#' @export
get_capstone <- function (industry_id, round) {
  round <- as.character(round)
  capsim_login()
  capstone <- vector("list", length(industry_id))
  names(capstone) <- industry_id
  for (i in seq_along(industry_id)) {
    cat(sprintf("Retrieving industry %s...\n", industry_id[i]))
    industry_key <- navigate_to_industry(industry_id[i])
    team         <- get_teams()
    capstone[[i]][["courier"]]   <- get_couriers(industry_key, round)
    capstone[[i]][["annual"]]    <- get_annuals(round)
    capstone[[i]][["decisions"]] <- get_decisions(industry_key, round, team)
  }
  return(capstone)
}

capsim_login <- function () {
  username <- rstudioapi::askForPassword("Capsim.com username:")
  password <- rstudioapi::askForPassword("Capsim.com password:")

  request("https://ww2.capsim.com/login/") %>%
    req_perform() %>%
    set_cookies(c("CFID","CFTOKEN"))

  login_redirect1 <-
    request("https://ww2.capsim.com/login/logincheck.cfm") %>%
    req_body_raw(sprintf("username=%s&password=%s&button=", username, password)) %>%
    req_headers(
      "Content-Type" = "application/x-www-form-urlencoded",
      "Cookie"       = get_cookie_string(c("CFID","CFTOKEN"))
    ) %>%
    req_options(followlocation = FALSE) %>%
    req_perform()

  if (login_redirect1$headers$location == "/login/index.cfm?error=invalidLogin")
    stop(sprintf("Invalid login for user '%s'", username))

  login_redirect1 %>%
    set_cookies(c("NEW_OLD","USER_ID","USER_ID2","ATTRSEC2"), ignore_empty = TRUE)

  request(login_redirect1$headers$location) %>%
    req_options(followlocation = FALSE) %>%
    req_perform() %>%
    set_cookies(c("PCSESSIONID"))
  return(0)
}

get_annuals <- function (round) {
  annual <- vector("list", length(round))
  names(annual) <- round
  for (i in seq_along(round)) {
    a <- get_annual(round[i])
    if (identical(a, NA))
      next

    annual[[i]] <- a
  }
  return(annual)
}

get_annual <- function (round) {
  annual_list_resp <-
    request(sprintf("%s?template=annualReports&rounds=%s", portal_url, round)) %>%
    req_headers("Cookie" = get_cookie_string(c("CFID","CFTOKEN"))) %>%
    req_perform()

  annual_list_html <-
    annual_list_resp %>%
    resp_body_html()

  annual_html <-
    annual_list_html %>%
    html_elements(xpath = '//a[contains(@class,"btn btn-primary")]') %>%
    lapply(html_attr, "href") %>%
    lapply(read_html)

  annual <-
    annual_html %>%
    lapply(html_element, xpath = '//script[@id="initial-data"]') %>%
    lapply(html_text) %>%
    lapply(str_remove, ".*window.data = ") %>%
    lapply(str_sub, 3, -3) %>%
    lapply(fromJSON)

  if (annual[[1]][["teamName"]] == "")
    return(NA)

  names(annual) <- sapply(annual, "[[", "teamName")

  return(annual)
}

get_couriers <- function (industry_key, round) {
  courier_list_resp <-
    request(sprintf("%s?template=reports&simKey=%s", portal_url, industry_key)) %>%
    req_headers("Cookie" = get_cookie_string(c("CFID","CFTOKEN"))) %>%
    req_perform()

  courier_list_html <-
    courier_list_resp %>%
    resp_body_html()

  courier <- vector("list", length(round))
  names(courier) <- round
  for (i in seq_along(round)) {
    courier_url <-
      courier_list_html %>%
      html_element(xpath = sprintf('//a[contains(@id,"reportBtn%s")]', round[i])) %>%
      html_attr("href")
    if (is.na(courier_url))
      next

    courier[[i]] <-
      courier_url %>%
      read_html() %>%
      html_element(xpath = '//script[@id="initial-data"]') %>%
      html_text() %>%
      str_remove(".*window.data = ") %>%
      str_sub(3, -3) %>%
      fromJSON()
  }
  return(courier)
}

get_decisions <- function (industry_key, round, team) {
  decision_list_resp <-
    request(sprintf("%s?template=decisionSummaries", portal_url)) %>%
    req_headers("Cookie" = get_cookie_string(c("CFID","CFTOKEN"))) %>%
    req_perform()

  decision_list_html <-
    decision_list_resp %>%
    resp_body_html()

  decisions <- vector("list", length(round))
  names(decisions) <- round
  for (i in seq_along(round)) {
    d <- lapply(team, function(x) {
      team_round <- paste0(x, round[i])
      decision_urls <- decision_list_html %>%
        html_elements(xpath = sprintf('//a[contains(@id,"%s")]', team_round)) %>%
        lapply(html_attr, "href")
      url_n <- length(decision_urls)
      if (url_n == 0)
        return(NULL)

      r <-
        decision_urls[[url_n]] %>%
        str_remove(".*&round=")
      decision_key <-
        decision_urls[[url_n]] %>%
        str_remove(".*&key=") %>%
        str_remove("&previousKey=.*")

      "%s?decisionKey=%s&previousKey=0&simKey=%s&round=%s" %>%
        sprintf(decision_url, decision_key, industry_key, r) %>%
        request() %>%
        req_perform() %>%
        resp_body_json()
    })
    names(d) <- team
    if (all(sapply(d, identical, y = NULL)))
      next

    decisions[[i]] <- d
  }
  return(decisions)
}

get_teams <- function () {
  team_list_resp <-
    request(sprintf("%s?template=logIntoTeam", portal_url)) %>%
    req_headers(
      "Cookie"  = get_cookie_string(c("CFID","CFTOKEN")),
      "Referer" = "https://ww3.capsim.com/professor/portal/index.cfm?template=dashboard&simKey=559508"
    ) %>%
    req_perform()

  team_list_resp %>%
    resp_body_html() %>%
    html_element(xpath = '//table') %>%
    html_table() %>%
    as.data.frame() %>%
    .[.[,"Type"] %in% c("Participant","Computer"), "Team"]
}

navigate_to_industry <- function (industry_id) {
  industry_redirect1 <-
    request(sprintf("https://ww2.capsim.com/menuapp/courseMain.cfm?simid=%s", industry_id)) %>%
    req_headers("Cookie" = get_cookie_string(c("NEW_OLD","USER_ID","USER_ID2","ATTRSEC2"))) %>%
    req_options(followlocation = FALSE) %>%
    req_perform()

  industry_redirect2 <-
    request(industry_redirect1$headers$location) %>%
    req_options(followlocation = FALSE) %>%
    req_perform()

  industry_redirect2 %>%
    set_cookies(c("CFID","CFTOKEN"), ignore_empty = TRUE)

  industry_redirect3 <-
    request(paste0("https://ww3.capsim.com", industry_redirect2$headers$location)) %>%
    req_headers("Cookie" = get_cookie_string(c("CFID","CFTOKEN"))) %>%
    req_perform()

  industry_key <-
    industry_redirect3 %>%
    resp_body_html() %>%
    html_element(xpath = sprintf('//a[contains(@data-testid, "%s")]', industry_id)) %>%
    html_attr("href") %>%
    str_remove(".*simKey=")

  request(sprintf("%s?template=dashboard&simKey=%s", portal_url, industry_key)) %>%
    req_headers("Cookie" = get_cookie_string(c("CFID","CFTOKEN"))) %>%
    req_perform()

  return(industry_key)
}
