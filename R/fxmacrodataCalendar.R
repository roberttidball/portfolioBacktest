#' Download FXMacroData release-calendar events
#'
#' Fetch official-source macroeconomic release events from FXMacroData and
#' return an \code{xts} object that can be joined with portfolio price windows.
#'
#' @param currency ISO currency code, for example \code{"usd"}.
#' @param limit Maximum number of events to request.
#' @param min_tier Optional market-tier filter. Use \code{1} for top-tier
#'   events, \code{2} for medium-or-higher impact, or \code{NULL} for all rows.
#' @param api_key Optional FXMacroData API key, sent in the \code{X-API-Key}
#'   request header. Defaults to the \code{FXMACRODATA_API_KEY} environment
#'   variable.
#' @param base_url FXMacroData REST API base URL.
#'
#' @return An \code{xts} object indexed by release date with market tier and
#'   top-tier flags.
#' @export
fxmacrodataCalendar <- function(currency = "usd",
                                limit = 100,
                                min_tier = 2,
                                api_key = Sys.getenv("FXMACRODATA_API_KEY"),
                                base_url = "https://api.fxmacrodata.com/v1") {
  limit <- max(1L, min(as.integer(limit), 100L))
  request_url <- paste0(
    sub("/$", "", base_url),
    "/calendar/",
    tolower(currency),
    "?limit=", limit
  )
  headers <- if (nzchar(api_key)) c("X-API-Key" = api_key) else NULL

  con <- url(request_url, headers = headers)
  on.exit(close(con))
  body <- readLines(con, warn = FALSE, encoding = "UTF-8")
  payload <- jsonlite::fromJSON(paste(body, collapse = "\n"))
  events <- payload$data
  if (is.null(events) || NROW(events) == 0L)
    return(xts::xts())

  if (!is.null(min_tier) && "market_tier" %in% names(events))
    events <- events[events$market_tier <= min_tier, , drop = FALSE]

  if (NROW(events) == 0L)
    return(xts::xts())

  events <- utils::head(events, limit)
  idx <- as.Date(events$date)
  values <- data.frame(
    market_tier = events$market_tier,
    top_tier_for_currency = events$top_tier_for_currency,
    release = events$release,
    name = events$name
  )
  xts::xts(values, order.by = idx)
}
