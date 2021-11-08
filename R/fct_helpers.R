#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom rlang .data
#' @import PortfolioAnalytics
#' @import DEoptim
#' @import ROI
#' @import ROI.plugin.glpk
#' @import ROI.plugin.quadprog

my_boxplot <- function(x){
  x = data.frame(date=zoo::index(x), zoo::coredata(x))
  
  # Stack columns as rows and add variable role
  x = data.frame(x[1], utils::stack(x[2:ncol(x)]))
  col_names = colnames(x)
  
  col_names[2] = "value"
  col_names[3] = "variable"
  
  colnames(x) = col_names
  
  variable = x[,"variable"]
  value = x[,"value"]
  
  p <- ggplot2::ggplot(data = x, ggplot2::aes(x=variable, y=value)) + 
    ggplot2::geom_boxplot(ggplot2::aes(colour=variable)) +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::labs(y = "Return") +
    ggplot2::scale_colour_manual(values = PerformanceAnalytics::rich6equal)
  return (p)
}

prices <- function(ticker, type, start_date){

  if(type == "Crypto"){
    res <- riingo::riingo_crypto_prices(tolower(paste0(ticker, "usd")), start_date = start_date, resample_frequency = "1day")
    res <- res |> dplyr::select(ticker, date, close)
  } else if (type == "IEX"){
    res <- riingo::riingo_iex_prices(ticker, start_date = start_date, resample_frequency = "1day")
  } else {
    res <- riingo::riingo_prices(ticker, start_date = start_date, resample_frequency = "daily")
    res <- res |> dplyr::select(ticker, date, adjClose) |>
      dplyr::rename("close" = adjClose)
  }
  
  res <- res |>
    dplyr::mutate(date = lubridate::as_date(.data$date)) |> 
    tidyr::pivot_wider(names_from = "ticker", values_from = "close") |>
    tibble::column_to_rownames(var = "date")
  
  res <- xts::as.xts(res)
  
  return(res)
  
}

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

efficientFrontier.fn <- function(R, nPorts){
  
  # Expected returns and covariance matrix
  meanReturns <- colMeans(R)
  covMat <- cov(R)
  stdDevs <- sqrt(diag(covMat))
  
  # create a portfolio
  port <- PortfolioAnalytics::portfolio.spec(assets = colnames(R))
  
  # Adding a "box" constraint that weights should be between 0 and 1
  port <- PortfolioAnalytics::add.constraint(port, type = "box", min = 0, max = 1)
  
  # Adding a leverage constraint for a fully-invested portfolio
  port <- PortfolioAnalytics::add.constraint(portfolio = port, type = "full_investment")
  
  # Find the minimum-variance portfolio
  mvp.port <- PortfolioAnalytics::add.objective(port, type = "risk", name = "var")
   
  mvp.opt <- PortfolioAnalytics::optimize.portfolio(R, mvp.port, momentFUN = "set.portfolio.moments", optimize_method = "DEoptim")
  print(mvp.opt)
  
  # Find minimum and maximum expected returns, and define a grid with nPorts portfolios
  minret <- mvp.opt$weights %*% meanReturns
  maxret <- max(colMeans(R))
  target.returns <- seq(minret, maxret, length.out = nPorts)
  
  #Now that we have the minimum variance as well as the maximum return portfolios, we can build out the efficient frontier. 
  eff.frontier.return <- double(length(target.returns))
  eff.frontier.risk <- double(length(target.returns))
  eff.frontier.weights <- mat.or.vec(nr = length(target.returns), nc = ncol(R))
  colnames(eff.frontier.weights) <- colnames(R)
  
  for (i in 1:length(target.returns)) {
    eff.port <- PortfolioAnalytics::add.constraint(port, type = "return", name = "mean", return_target = target.returns[i])
    eff.port <- PortfolioAnalytics::add.objective(eff.port, type = "risk", name = "var")
    eff.port <- PortfolioAnalytics::optimize.portfolio(R, eff.port, optimize_method = "ROI")
    eff.frontier.risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
    eff.frontier.return[i] <- eff.port$weights %*% meanReturns
    eff.frontier.weights[i,] <- t(eff.port$weights)
    #print(paste(round(i / length(target.returns) * 100, 0), "% done..."))
  }
  
  # save everything as a list
  
  # efficient frontier
  eff.frontier <- as.data.frame(cbind(eff.frontier.risk, eff.frontier.return))
  names(eff.frontier) <- c("Risk", "Return")
  
  # add the weights
  out <- list(eff.frontier, eff.frontier.weights)
  names(out) <- c("EfficientFrontier", "Weights")
  return(out)
}
