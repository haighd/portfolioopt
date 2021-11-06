#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom rlang .data

tickers <- function(){
  tickers_tiingo <- riingo::supported_tickers(type = "tiingo")
  tickers_iex <- riingo::supported_tickers(type = "iex")
  tickers_crypto <- riingo::supported_tickers(type = "crypto")
  
  df <- tickers_tiingo |>
    dplyr::filter(
      .data$endDate == max(.data$endDate, na.rm = T) 
      & priceCurrency == "USD"
    ) |>
    dplyr::full_join(
      tickers_crypto |> 
        dplyr::mutate(ticker = toupper(.data$baseCurrency), .keep = "unused") |>
        dplyr::filter(stringr::str_detect(quoteCurrency, "busd")) |>
        tibble::add_column("assetType" = "Crypto", "exchange" = "CRYPTO") |>
        dplyr::rename("priceCurrency" = .data$quoteCurrency) |>
        dplyr::select(-.data$description, -.data$name),
      by = c("ticker", "exchange", "assetType", "priceCurrency")
    ) |>
    dplyr::left_join(tickers_iex, by = c("ticker" = "Symbol")) |>
    dplyr::arrange(.data$ticker)
  
  return(df)
  
}

memoise_tickers <- memoise::memoise(tickers)
