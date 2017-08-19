library(jsonlite)
library(rvest)
library(data.table)
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

adat <- get_data()
