
scrape_flow_stats <- function(station_ids,statistic) {
  
  if(!require(rvest)){             
    install.packages("rvest")      
  }
  
  if(!require(dplyr)){             
    install.packages("dplyr")      
  }
  
  library(rvest) 
  library(dplyr)                   
  
  
  N = length(station_ids)
  values <- numeric()
  for(i in 1:N) {
    station <- station_ids[i]
    url <- paste0("https://streamstatsags.cr.usgs.gov/gagepages/html/0",station,".htm")
    
    print(i)
    print(station)
    
    tryCatch(
      hold <- url %>%
        read_html() %>%
        html_nodes(xpath='/html/body/table[3]') %>%
        html_table() %>%
        as.data.frame(),
      error = function(e){NA} 
    )
    
    values[i] <- filter(hold, X1 == statistic)[1,2]
  }

  values <- as.numeric(values)
  results <- data.frame(station_ids=station_ids,stat=values)
  return(results)
}

# example
# station_ids = c(7048600,7049000,7050500,7053250,7055875,7056000)
# statistic = "7_Day_10_Year_Low_Flow"
# 
# data = scrape_flow_stats(station_ids,statistic)
