rm(list = ls())
library(httr)
library(data.table)
library(ggplot2)
library(anytime)
library(dplyr)
library(stringr)




# functions ---------------------------------------------------------------

get_totalTVL_v2 <- function(){
  
  request <- GET("https://api.llama.fi/v2/historicalChainTvl")
  
  data <- content(request)
  
  res <- data.table("date" = sapply(data,'[[',"date"),
    "tvl_totalDefi_v2" = sapply(data,'[[',"tvl"))
  
  return(res)
  
}

get_totalTVL <- function(){
  
  request <- GET("https://api.llama.fi/charts")
  
  data <- content(request)
  
  res <- data.table("date" = sapply(data,'[[',"date"),
                    "tvl_totalDefi" = sapply(data,'[[',"totalLiquidityUSD"))
  
  return(res)
  
}

get_protocolsInfo <- function(){
  
  request <- GET("https://api.llama.fi/protocols")
  
  data <- content(request)
  
  res <- data.table(
    "slug" = sapply(data,'[[',"slug"),
    "name" = sapply(data,'[[',"name"),
    "module" = sapply(data,'[[',"module"),
    "tvl" = sapply(data,'[[',"tvl"),
    "oracles" = sapply(data,'[[',"oracles"))
  
  #create ID column
  res[ , ID := .I]
  
  #unnest oracle column, keep ID column
  res_unnested = res[ , .(ID = rep(ID, lengths(oracles)), oracles = unlist(oracles))]
  
  # merge back
  res <- merge(res[,-c("oracles")], res_unnested, by = "ID", all.x = T, all.y = T, sort = F)
  
  return(res)
  
}

get_protocolTVL <- function(this_protocolSlug){
  
  request <- GET(paste0("https://api.llama.fi/protocol/", this_protocolSlug))
  
  data <- content(request)
  
  tvldata <- data$tvl
  
  res <- data.table("date" = sapply(tvldata,'[[',"date"),
    "tvl_protocol" = sapply(tvldata,'[[',"totalLiquidityUSD"),
    protocolSlug = this_protocolSlug)
  
  return(res)
  
}

get_oracleTVL <- function(this_oracle){
  
  oracleSecuredProtocols <- get_protocolsInfo()[oracles == this_oracle,]
  
  for (i in 1:nrow(oracleSecuredProtocols)){
    
    print(paste0("progress: ", round(i/nrow(oracleSecuredProtocols)*100, 2), "%"))
    
    this_protocolSlug <- oracleSecuredProtocols$slug[i]
    
    if(i == 1){
      oracle_tvl <- get_protocolTVL(this_protocolSlug)
    }else{
      oracle_tvl <- rbind(oracle_tvl, get_protocolTVL(this_protocolSlug))
    }
    
  }
  
  return(oracle_tvl)
  
}

get_coinMarketData <- function(this_coin, date_start, date_end, cur){
  # this_coin <- "chainlink"
  # date_start <- "2019-12-31"
  # date_end <- "2020-01-01"
  # cur <- "usd"
  
  request <- GET(paste0(
    "https://api.coingecko.com/api/v3/coins/",
    this_coin,
    "/market_chart/range?vs_currency=",
    cur,
    "&from=",
    as.numeric(as.POSIXct(date_start)),
    "&to=", 
    as.numeric(as.POSIXct(date_end))
    )
  )

  data <- content(request)
  
  prices <- data.table("date" = sapply(data$prices,'[[',1),
                       "price" = sapply(data$prices,'[[',2))
  
  marketcap <- data.table("date" = sapply(data$market_caps,'[[',1),
                          "marketcap" = sapply(data$market_caps,'[[',2))
  
  volume <- data.table("date" = sapply(data$total_volumes,'[[',1),
                       "volume" = sapply(data$total_volumes,'[[',2))
  
  dates <- unique(prices$date, marketcap$date, volume$date)
  
  res <- data.table(date = dates) %>%
    left_join(prices, by = c('date' = 'date')) %>%
    left_join(marketcap, by = c('date' = 'date')) %>%
    left_join(volume, by = c('date' = 'date'))

}

# chainlink tvl -----------------------------------------------------------
tvl_chainlink <- get_oracleTVL("Chainlink")
tvl_chainlink <- tvl_chainlink[order(date), ]
tvl_chainlink$date <- anydate(tvl_chainlink$date)
tvl_chainlink_agg <- tvl_chainlink[, .(tvl_ChainlinkSecured_allprotocols = sum(tvl_protocol)), by = date]




# chainlink market data ---------------------------------------------------
date_start <- tvl_chainlink_agg$date[1]
date_end <- tvl_chainlink_agg$date[length(tvl_chainlink_agg$date)]

marketdata_chainlink <- get_coinMarketData("chainlink", date_start, date_end, "usd")
marketdata_chainlink <- marketdata_chainlink[order(date), ]
marketdata_chainlink$date <- anydate(round(marketdata_chainlink$date/1000))



# merge datasets ----------------------------------------------------------
data_chainlink <- marketdata_chainlink %>% left_join(tvl_chainlink_agg, by = c('date' = 'date'))

# ignore today
data_chainlink <- data_chainlink[-nrow(data_chainlink),]

# tests -------------------------------------------------------------------
# tvl_totalDefi <- get_totalTVL()
# 
# tvl_totalDefi_v2 <- get_totalTVL_v2()
# 
# pr <- get_protocolsInfo()
# 
# tvl_protocol <- get_protocolTVL("aave")
# 
# tvl_chainlink <- get_oracleTVL("Chainlink")
# 
# 
# tvl_chainlink[date == "2023-03-30", ][order(-tvl_protocol),]
# tvl_chainlink[date == "2023-03-29", ][order(-tvl_protocol),]


ggplot(data_chainlink, aes(date)) + 
  geom_line(aes(y = log(tvl_ChainlinkSecured_allprotocols), colour = "tvl_secured_Chainlink")) + 
  geom_line(aes(y = log(marketcap), colour = "marketcap_chainlink"))


start_plot <- "2020-01-01"
data_chainlink_plot <- data_chainlink[c(which(data_chainlink$date == start_plot):nrow(data_chainlink)), ]
ggplot(data_chainlink_plot, aes(date)) + 
  geom_line(aes(y = log(marketcap)/log(tvl_ChainlinkSecured_allprotocols), colour = "var0"))
















