require(tidyr)
require(dplyr)
require(tibble)
require(stringr)

take_cotacao = function(acao = NULL, type='daily',time = 5, fullsize = F, key = NULL){
  if(is.null(key)){stop("No api key")}
    
  temp.dir = tempdir()
  
  if(type=='daily'){
    
    link = paste0('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=',
                  acao,'.SA',ifelse(fullsize==T,"outputsize=full",""),'&apikey=',key)
    
    aux = jsonlite::fromJSON(link)
    
    
    df = data.frame(date = rep(as.Date(names(aux[[names(aux)[2]]])),each = 5),
                    aux = str_remove_all(names(aux[names(aux)[2]][[1]][1][[1]]),'[^a-z]'),
                    valor = as.numeric(unlist(aux[names(aux)[2]]))) %>% 
      spread(aux,valor) %>% arrange(desc(date)) %>% as.tibble()
    
    return(df)
    
  }else{
    
    if(type=='intra_day'){
      if(time %in% c(1,5,15,30,60)){}else{stop("Time interval not accepted")}
      
      link = paste0('https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=',
                    acao,'.SA&interval=',time,'min',
                    ifelse(fullsize==T,"&outputsize=full",""),'&apikey=',key)
      
      aux = jsonlite::fromJSON(link)
      
      df = data.frame(date = rep(as.POSIXct(names(aux[[names(aux)[2]]])),each = 5),
                      aux = str_remove_all(names(aux[names(aux)[2]][[1]][1][[1]]),'[^a-z]'),
                      valor = as.numeric(unlist(aux[[names(aux)[2]]]))) %>% 
        spread(aux,valor) %>% arrange(desc(date)) %>% as.tibble()
      
      return(df)
    }
  }
  
}
