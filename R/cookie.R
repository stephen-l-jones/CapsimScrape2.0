append_cookie <- function(
    name      = character(),
    value     = character(),
    domain    = character(),
    expires   = character(),
    path      = character(),
    secure    = boolean(),
    http_only = boolean()
) {
  new_cookie <- data.frame(
    name      = name,
    value     = value,
    domain    = domain,
    expires   = expires,
    path      = path,
    secure    = secure,
    http_only = http_only
  )
  assign_cookies(rbind(get_cookies(), new_cookie))
  return(new_cookie)
}

assign_cookies <- function (cookies) {
  assign("capsim_cookies", cookies, envir = .GlobalEnv)
}

get_cookie_string <- function (cookie_names = NULL) {
  cookies <- get_cookies()
  if (is.null(cookies)) {
    return ("")
  }

  if (identical(cookie_names, NULL)) {
    s <- cookies
  } else {
    s <- cookies[cookies$name %in% cookie_names,]
  }
  if (nrow(s) == 0) {
    return ("")
  }

  paste0(s[,"name"], "=", s[,"value"]) %>%
    paste(collapse = "; ")
}

get_cookies <- function () {
  get0("capsim_cookies", envir = .GlobalEnv)
}

remove_cookies <- function (cookie_names = NULL) {
  cookies <- get_cookies()

  if (!identical(cookie_names, NULL)) {
    cookies <- cookies[!(cookies$name %in% cookie_names),]
  }
  assign_cookies(cookies)
  return(cookies)
}

set_cookies <- function (resp, cookie_names = NULL, ignore_empty = FALSE) {
  if (!resp_header_exists(resp, "set-cookie"))
    return(NULL)

  cookie_list <-
    resp %>%
    resp_headers("set-cookie") %>%
    lapply(str_split, ";") %>%
    lapply(str_trim)

  cookie_name_value <-
    cookie_list %>%
    lapply("[", 1) %>%
    do.call(rbind, .) %>%
    str_split_fixed("=", 2)

  cookie_include <-
    cookie_name_value %>%
    { !(.[, 2] %in% c("",'""') & ignore_empty) &
        (.[, 1] %in% cookie_names | identical(cookie_names, NULL)) }

  cookie_name_value[cookie_include, 1] %>%
    unique() %>%
    remove_cookies()

  cookie_list[cookie_include] %>%
    lapply(function(x) {
      name_value <- str_split_fixed(x[1], "=", 2)
      append_cookie(
        name      = name_value[1],
        value     = name_value[2],
        domain    = str_remove(x[str_detect(x, "^Domain=")][1], "^Domain="),
        expires   = str_remove(x[str_detect(x, "^Expires=")][1], "^Expires="),
        path      = str_remove(x[str_detect(x, "^Path=")][1], "^Path="),
        secure    = any(str_detect(x, "Secure")),
        http_only = any(str_detect(x, "HttpOnly"))
      )
    }) %>%
    do.call(rbind, .)
}
