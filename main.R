rm(list = ls())
library(httr)
library(data.table)


# functions ---------------------------------------------------------------

get_totalTVL <- function(){
  
  request <- GET("https://api.llama.fi/v2/historicalChainTvl")
  
  data <- content(request)
  
  res <- data.table("date" = sapply(data,'[[',"date"),
                    
                    "tvl_totalDefi" = sapply(data,'[[',"tvl"))
  
  return(res)
  
}

get_protocolsInfo <- function(){
  
  request <- GET("https://api.llama.fi/protocols")
  
  data <- content(request)
  
  res <- data.table(
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



# tests -------------------------------------------------------------------
pr <- get_protocolsInfo()

# need to list the protocols secured by Chainlink (call protocols and it says which oracles they use)
# and then get the tvl for each of these protocols








