rm(list = ls())
library(httr)
library(data.table)
library(ggplot2)


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


# tests -------------------------------------------------------------------
tvl_totalDefi <- get_totalTVL()

tvl_totalDefi_v2 <- get_totalTVL_v2()

pr <- get_protocolsInfo()

tvl_protocol <- get_protocolTVL("aave")

tvl_chainlink <- get_oracleTVL("Chainlink")


ggplot(tvl_chainlink, aes(x= date, y=tvl_protocol, col=protocolSlug)) +
  geom_line() +
  theme(legend.position="none")





