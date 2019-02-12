# data <- read.csv("data/ProcessedData/china_daily_data.csv", stringsAsFactors = F)

data <- chinaDaily_Main

data$clean_text <- str_split(gsub("[^[:alnum:] ]", "", data$text), " +")
data$clean_text2 <- lapply(data$clean_text, function(x) {x[!(x %in% stops)]})

a <- sort(table(unlist(data$clean_text)), decreasing = T)
b <- sort(table(unlist(data$clean_text2)), decreasing = T)
a[!(names(a) %in% names(b))]


sum(data$date_published=="2017-6-30")


sort(table(unlist(data$clean_text[data$date_published=="2017-6-30"])))
sort(table(unlist(data$clean_text2)), decreasing = T)
sort(table(unlist(data$clean_text2[data$date_published=="2017-6-30"])))

new_data <- as.data.frame(sort(unique(data$date_published)), stringsAsFactors = F)
names(new_data) <- "date"
new_data$date <- as.Date(new_data$date)

new_data$unfair_frequency <- NA
new_data$trade_frequency <- NA
for(i in 1:nrow(new_data)) {
  
  #new_data$unfair_frequency[i] <- table(unlist(data$clean_text2[data$date_published==new_data$date[i]]))["unfair"]
  #new_data$trade_frequency[i] <- table(unlist(data$clean_text2[data$date_published==new_data$date[i]]))["trade"]
  new_data$negotiate_frequency[i] <- table(unlist(data$clean_text2[data$date_published==new_data$date[1]]))["negotiate"]
}
new_data$unfair_frequency <- ifelse(is.na(new_data$unfair_frequency), 0, new_data$unfair_frequency)
new_data$trade_frequency <- ifelse(is.na(new_data$trade_frequency), 0, new_data$trade_frequency)
new_data$negotiate_frequency <- ifelse(is.na(new_data$negotiate_frequency), 0, new_data$negotiate_frequency)



#install.packages("data.table")
library(data.table)
new_data2 <- setDT(new_data)[, .(mn_amt = mean(trade_frequency)), by = .(yr = year(date), mon = months(date))]

new_data2$mon <- match(new_data2$mon, month.name)

new_data2$date <- as.Date(do.call(paste, list(new_data2$yr, new_data2$mon, "01", sep = "-")), 
                          format = "%Y-%m-%d")

plot(new_data2$date, new_data2$mn_amt)

new_dataNEG <- setDT(new_data)[, .(mn_amt = mean(negotiate_frequency)), by = .(yr = year(date), 
                                                                              wk = week(date))]

new_dataNEG$mon <- match(new_dataNEG$mon, month.name)
#new_dataNEG$wk <- match(new_dataNEG$wk, week.name)

new_dataNEG$date <- as.Date(do.call(paste, list(new_dataNEG$yr, new_dataNEG$mon, "01", sep = "-")), 
                          format = "%Y-%m-%d")

plot(new_data$date[226:817], new_data$negotiate_frequency[226:817])
