library(jsonlite)
library(rvest)
library(data.table)
library(lubridate)

get_data <- function(){
  link <- "https://coinmarketcap.com/all/views/all/"
  adat_help <-read_html(link)%>%
    html_nodes("table") %>%
    html_table()
 adat <-  adat_help[[1]]
 adat$`% 1h` <- as.numeric(gsub('%','',adat$`% 1h`))
 adat$`% 7d` <- as.numeric(gsub('%','',adat$`% 7d`))
 adat$`% 24h` <- as.numeric(gsub('%','',adat$`% 24h`))

  names(adat) <- c("rank", "Name", 'Symbol', 'Market_cap', 'Price','Curcilating_supply', 'Volume(24h)', '1h_change','24h_change', '7d_change')
  adat$Market_cap <- as.numeric(gsub(',','',gsub('\\$','',ifelse(adat$Market_cap=="?",0,adat$Market_cap))))
  adat$Price<- as.numeric(gsub('\\$','',adat$Price))
  
  
  adat <- data.table(adat)
  t <- adat[`7d_change`<`24h_change`&`24h_change`<`1h_change`& rank <150,]
  setorder(t,`7d_change`)
  return(t)
  }

get_bittrex_list<- function(){
  data <-read_html('https://coinmarketcap.com/exchanges/bittrex/')%>%
    html_nodes("table") %>%
    html_table()
  ered <- sapply(strsplit(data[[1]]$Pair,'/'), '[',1)
  return(unique(ered))
  
}

get_bittrex_name_and_list<- function(){
  data <-read_html('https://coinmarketcap.com/exchanges/bittrex/')%>%
    html_nodes("table") %>%
    html_table()
  ered <- sapply(strsplit(data[[1]]$Pair,'/'), '[',1)
  nevek <- data[[1]]$Currency
  my_df <- data.table('coin_name'= nevek, 'symbol'= ered)
  my_df <- my_df[duplicated(my_df)==F,]
  return(my_df)
  
}
  
get_one_coin <- function(coin,agg_time){

  link<- paste('https://min-api.cryptocompare.com/data/', agg_time,'?fsym=',coin,'&tsym=USD&limit=720',sep ="")
  adat <- fromJSON(link)
  if(adat$Response=="Success"){
    adat<- data.table(adat$Data)
    adat<- adat[high!=0&close!=0&low!=0,]
    adat$time <- as.POSIXct(adat$time, origin="1970-01-01")
    adat$symbol <- coin
    adat <- adat[,c("symbol","time",'close')]
    return(adat)
  }else{
    return(data.frame())
  }
}
#agg_time: 'histominute','histohour','histoday'


get_coin_hist_data <- function(coin_list,agg_time){
  
  adat<-data.table()
  for(i in coin_list){
    print(i)
    adat <- rbind(adat, get_one_coin(i,agg_time))
  }
  return(adat)
}




havi_adatok <- function(){
  # get_hour data from the last month
  my_list<- get_bittrex_list()
  havi <- get_coin_hist_data(my_list, 'histohour')
  perces_adatok <- get_coin_hist_data(my_list, 'histominute')
  havi$datum <- as.Date(havi$time)
  setorder(havi,symbol,datum)
  ###################
  
  mindate<-min(havi$datum)
  maxdate<- max(havi$datum)
  egy_heti_date <- maxdate-7
  ket_napi <- maxdate-2
  ##############################################################x
  
  havi_valt <- data.table(havi[,.SD[c(1,.N)],by=symbol])
  havi_valt$change <- ((havi_valt$close / shift(havi_valt$close))-1)*100
  havi_valt<- havi_valt[seq(from=2, to=nrow(havi_valt), by = 2),]
  #############################################################xx
  
  heti_valtozas <-  havi[datum>=egy_heti_date,]
  heti_valtozas <- data.table(heti_valtozas[,.SD[c(1,.N)],by=symbol])
  heti_valtozas$change <- ((heti_valtozas$close / shift(heti_valtozas$close))-1)*100
  heti_valtozas<- heti_valtozas[seq(from=2, to=nrow(heti_valtozas), by = 2),]
  setorder(heti_valtozas,symbol,datum)
  ##########################################################################################xxx
  
  ketto_napos <-  havi[datum>=ket_napi,]
  ketto_napos <- data.table(ketto_napos[,.SD[c(1,.N)],by=symbol])
  ketto_napos$change <- ((ketto_napos$close / shift(ketto_napos$close))-1)*100
  ketto_napos<- ketto_napos[seq(from=2, to=nrow(ketto_napos), by = 2),]
  setorder(ketto_napos,symbol,datum)
  
  ################################################################################################
  egynapos_time <- max(havi$time) - hours(24)
  
  egynapos <-  havi[time>=egynapos_time,]
  egynapos <- data.table(egynapos[,.SD[c(1,.N)],by=symbol])
  egynapos$change <- ((egynapos$close / shift(egynapos$close))-1)*100
  egynapos<- egynapos[seq(from=2, to=nrow(egynapos), by = 2),]
  setorder(egynapos,symbol,datum)
  
  egyoras_time <- max(havi$time) - hours(1)
  
  egyoras <-  havi[time>=egyoras_time,]
  egyoras <- data.table(egyoras[,.SD[c(1,.N)],by=symbol])
  egyoras$change <- ((egyoras$close / shift(egyoras$close))-1)*100
  egyoras<- egyoras[seq(from=2, to=nrow(egyoras), by = 2),]
  setorder(egyoras,symbol,datum)
  
  tizenotperces_time <- max(perces_adatok$time) - minutes(15)
  
  tizenotperces <-  perces_adatok[time>=tizenotperces_time,]
  tizenotperces <- data.table(tizenotperces[,.SD[c(1,.N)],by=symbol])
  tizenotperces$change <- ((tizenotperces$close / shift(tizenotperces$close))-1)*100
  tizenotperces<- tizenotperces[seq(from=2, to=nrow(tizenotperces), by = 2),]
  setorder(tizenotperces,symbol,time)
  
  
  harmincperces_time <- max(perces_adatok$time) - minutes(30)
  
  harmincperces <-  perces_adatok[time>=harmincperces_time,]
  harmincperces <- data.table(harmincperces[,.SD[c(1,.N)],by=symbol])
  harmincperces$change <- ((harmincperces$close / shift(harmincperces$close))-1)*100
  harmincperces<- harmincperces[seq(from=2, to=nrow(harmincperces), by = 2),]
  setorder(harmincperces,symbol,time)
  
  eredmeny <- data.table('symbol'= havi_valt$symbol, '15m'= tizenotperces$change, 
             '30m'= harmincperces$change, '1h'= egyoras$change, 
             '24h'= egynapos$change,'48h'=ketto_napos$change,
             '7d'=heti_valtozas$change, 'havi'=havi_valt$change)
  
  
  
  
  nev_df <- data.table(get_bittrex_name_and_list())
  setkey(nev_df, symbol)
  setkey(eredmeny, symbol)
  final <- nev_df[eredmeny]
  setorder(final, -"15m")
  return(final)  
}
