source("key.R")
source("take_cotacao.R")

require(ggplot2)
require(tidyquant)

#####################
# EXTRAÇÃO DE DADOS #
#####################
daily = take_cotacao(acao = "ITSA4",fullsize=F ,type = 'daily' ,key = key)
intra_day = take_cotacao(acao = "ITSA4",fullsize=F, type = 'intra_day', time = 5, key = key)

###############
# Candlestick #
###############
daily %>% 
  filter(format(date,"%Y")>="2018",volume>0) %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  labs(title = "ITSA4: Candlestick daily serie",
       x = "Date", y = "Closing Price")

intra_day %>% 
  filter(volume>0) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  labs(title = "ITSA4: Candlestick intra day serie",
       x = "Date", y = "Closing Price") +
  facet_wrap(~format(date,"%d/%m/%Y"),scales = "free")
