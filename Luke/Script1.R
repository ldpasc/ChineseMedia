# install.packages("tm")
# install.packages("stringr")
# install.packages("SnowballC")
# install.packages("rJava")
# install.packages("qdap")
# install.packages("ggplot2")
library(rJava)
library(qdap)
library(tm)
library(stringr)
library(SnowballC)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)

raw_china_daily_columnist_data$ID <- 1:nrow(raw_china_daily_columnist_data)
column_all <- raw_china_daily_columnist_data[!duplicated(raw_china_daily_columnist_data$title),c(4,1:3)]
column_all$Year <- NA
column_all$Date <- NA
for(i in 2:nrow(column_all)) {
  str1 <- strsplit(column_all$date_published[i], "Updated: ")
  str2 <- strsplit(str1[[1]][2], "-")
  column_all$Year[i] <- str2[[1]][1]
  str2 <- strsplit(str1[[1]][2], " ")
  column_all$Date[i] <- str2[[1]][1]
}
column_all[1,5:6] <- c(2019, "2019-01-03")
column_all <- column_all[order(column_all$Date),]
column_all$ID <- 1:nrow(column_all)

#NegWords_ID375 <- NegWords_ID1016_Main
# NegWords_ID375_Main <- NegWords_ID375
# NegWords_ID375 <- NegWords_ID375_Main
NegWords0128_Main <- NegWords0128
NegWords0128 <- NegWords0128_Main

# column1_10 <- raw_china_daily_columnist_data[c(1,seq(2,20,2)),c(4,1:3)]
# column1000_1020 <- raw_china_daily_columnist_data[seq(1000,1020,2),c(4,1:3)]

for(u in 1:nrow(column_all)) {
  NegWords0128$new <- NA
  for (i in 1:nrow(NegWords0128)) {
    loc <- gregexpr(NegWords0128$first[i], column_all$text[u])
    if(loc[[1]][1] > 0) {
      NegWords0128$new[i] <- length(loc[[1]])
    } else {
      NegWords0128$new[i] <- 0
    }
  }
  names(NegWords0128)[ncol(NegWords0128)] <- paste0("id", column_all$ID[u])
}

NegWords0128_2 <- data.frame(ID = names(NegWords0128)[4:456])
NegWords0128_2$ID <- gsub("id", "", NegWords0128_2$ID)
NegWords0128_2$ID <- as.numeric(NegWords0128_2$ID)
NegWords0128_2$Year <- column_all$Year
NegWords0128_2$RowSum <- NA
for(i in 1:52) {
  NegWords0128_2 <- cbind(NegWords0128_2,as.vector(unlist(NegWords0128[i,4:456])))
  names(NegWords0128_2)[ncol(NegWords0128_2)] <- NegWords0128$first[i]
}
NegWords0128_2$RowSum <- rowSums(NegWords0128_2[,4:55])
  
NegWords0128_sums <- as.data.frame(colSums(NegWords0128_2[-375,4:55]))
NegWords0128_sums$AppearsIn <- NA
for(u in 4:55) {
  NegWords0128_sums$AppearsIn[u-3] <- nrow(NegWords0128_2[which((NegWords0128_2[-375,u] > 0) == T),])
}

NegWords0128_3 <- NegWords0128_2
NegWords0128_3[,4:55] <- apply(NegWords0128_3[,4:55], 2, function(x) {ifelse((x != 0) == T, 1, 0)})
NegWords0128_3$RowSum <- rowSums(NegWords0128_3[,4:55])
length(NegWords0128_3$ID[which(NegWords0128_3$RowSum > 1)])
column_all$title[203]
column_all$text[203]

tfreq1 <- as.data.frame(termFreq(toupper(column_all$text[1])))

stemDocument(column_all$text[1])
stemDocument(PlainTextDocument(column_all$text[1]))


str_extract_all(column_all$text[1], pattern = " ")
strsplit(column_all$text[1], " ")


stops <- stopwords(kind = "en")
text1 <- gsub(pattern =  "\n", replacement = "", column_all$text[1])
text1 <- gsub('[[:punct:] ]+',' ', text1)
text1 <- strsplit(text1, " ")
text1 <- as.data.frame(text1)
names(text1)[1] <- "word"
text1 <- text1[which(text1$word %in% stops == F),]
text1 <- as.data.frame(text1)
names(text1)[1] <- "word"

stem1 <- stemDocument(as.character(text1$word))
stem_dict <- unique(stem1)
freq1 <- as.data.frame(termFreq(stem1))

text1 <- gsub(pattern =  "\n", replacement = "", column_2017$text[1])
text1 <- gsub('[[:punct:] ]+',' ', text1)
text1 <- strsplit(text1, " ")
text1 <- as.data.frame(text1)
names(text1)[1] <- "word"
text1 <- text1[which(text1$word %in% stops == F),]
text1 <- as.data.frame(text1)
names(text1)[1] <- "word"
stem1 <- stemDocument(as.character(text1$word))
freq1 <- termFreq(stem1)
freqs_2017 <- data.frame(Stem = rownames(as.data.frame(freq1)), Count = as.data.frame(freq1)[,1])
freqs_2017$ID <- column_2017$ID[1]


for (i in 2:nrow(column_2017)) {
  textx <- gsub(pattern =  "\n", replacement = "", column_2017$text[i])
  textx <- gsub('[[:punct:] ]+',' ', textx)
  textx <- strsplit(textx, " ")
  textx <- as.data.frame(textx)
  names(textx)[1] <- "word"
  textx <- textx[which(textx$word %in% stops == F),]
  textx <- as.data.frame(textx)
  names(textx)[1] <- "word"
  
  stemx <- stemDocument(as.character(textx$word))
  freqx <- termFreq(stemx)
  freqs_x <- data.frame(Stem = rownames(as.data.frame(freqx)), Count = as.data.frame(freqx)[,1])
  freqs_x$ID <- column_2017$ID[i]
  freqs_2017 <- rbind(freqs_x, freqs_2017)
}
freqs_2017$Stem <- as.character(freqs_2017$Stem)
freqs_2017 <- freqs_2017[order(freqs_2017$Stem),]


length(unique(freqs_2017$Stem))

unique(as.vector(freqs_2017$Stem))[1:200]
stems2017 <- data.frame(Stem = unique(freqs_2017$Stem)[138:length(unique(freqs_2017$Stem))])
#stems2017 <- stems2017[order(stems2017$Stem),]
for(i in 1:nrow(column_2017)) {
  stems2017 <- merge(stems2017, freqs_2017[which(freqs_2017$ID == column_2017$ID[i]),-3], by = "Stem", all.x = T)
  names(stems2017)[ncol(stems2017)] <- paste0("id", column_2017$ID[i])
}

i <- 1
for(i in 1:nrow(stems2017)) {
  short1 <- stems2017
  short1[is.na(short1)] <- 0
  short1 <- short1[,which((short1[i,2:ncol(short1)] > 0) == T)]
  short1 <- short1[which((rowSums(short1[2:ncol(short1),]) > 1) == T),]
}


#### Sorting ####
loc.us <- gregexpr("US ", column_all$text)
loc.us1 <- vector()
for(i in 1:nrow(column_all)) {
  loc.us1 <- c(loc.us1, loc.us[[i]][1][1])
}
column_all$US <- loc.us1
column_US <- column_all[which(column_all$US != -1),]

loc.unitedstates <- gregexpr("United States", column_all$text)
loc.unitedstates1 <- vector()
for(i in 1:nrow(column_all)) {
  loc.unitedstates1 <- c(loc.unitedstates1, loc.unitedstates[[i]][1][1])
}
column_all$UnitedStates <- loc.unitedstates1
column_US <- column_all[which(column_all$US != -1),]
column_US <- rbind(column_US, column_all[which(column_all$UnitedStates != -1 & (column_all$ID %in% column_US$ID) == F),])
column_US$text <- toupper(column_US$text)

loc.us.trade <- gregexpr("TRADE", column_US$text)
loc.us.trade1 <- vector()
for(i in 1:nrow(column_US)) {
  loc.us.trade1 <- c(loc.us.trade1, loc.us.trade[[i]][1][1])
}
column_US$TRADE <- loc.us.trade1
column_US_trade <- column_US[which(column_US$TRADE != -1),]

loc.us.tariff <- gregexpr("TARIFF", column_US$text)
loc.us.tariff1 <- vector()
for(i in 1:nrow(column_US)) {
  loc.us.tariff1 <- c(loc.us.tariff1, loc.us.tariff[[i]][1][1])
}
column_US$TARIFF <- loc.us.tariff1
column_US_trade <- column_US[which(column_US$TRADE != -1),]
column_US_trade <- rbind(column_US_trade, column_US[which(column_US$TARIFF != -1 & (column_US$ID %in% column_US_trade$ID) == F),])


column_US_trade$text[2]

#### ChinaDaily ####
chinaDaily_Main <- chinaDaily
chinaDaily <- chinaDaily_Main
chinaDaily_Main <- chinaDaily_Main[order(chinaDaily_Main$date_published),]
chinaDaily_Main$ID <- 1:nrow(chinaDaily_Main)
chinaDaily_Main$date_published <- as.character(chinaDaily_Main$date_published)
chinaDaily$Year <- NA
for(i in 1:nrow(chinaDaily)) {
  str2 <- strsplit(chinaDaily$date_published[i], "-")
  chinaDaily$Year[i] <- str2[[1]][1]
}
chinaDaily <- chinaDaily[,c(5,1,3,4,6,2)]

### ChinaDaily_US ###
cd.loc.us <- gregexpr("US ", chinaDaily$text)
cd.loc.us1 <- vector()
for(i in 1:nrow(chinaDaily)) {
  cd.loc.us1 <- c(cd.loc.us1, cd.loc.us[[i]][1][1])
}
chinaDaily$US <- cd.loc.us1
cd_US <- chinaDaily[which(chinaDaily$US != -1),]

cd.loc.unitedstates <- gregexpr("United States", chinaDaily$text)
cd.loc.unitedstates1 <- vector()
for(i in 1:nrow(chinaDaily)) {
  cd.loc.unitedstates1 <- c(cd.loc.unitedstates1, cd.loc.unitedstates[[i]][1][1])
}
chinaDaily$UnitedStates <- cd.loc.unitedstates1
cd_US <- chinaDaily[which(chinaDaily$US != -1),]
cd_US <- rbind(cd_US, chinaDaily[which(chinaDaily$UnitedStates != -1 & (chinaDaily$ID %in% cd_US$ID) == F),])
cd_US$text <- toupper(cd_US$text)

### ChinaDaily_US_Trade ###
cd.loc.us.trade <- gregexpr("TRADE", cd_US$text)
cd.loc.us.trade1 <- vector()
for(i in 1:nrow(cd_US)) {
  cd.loc.us.trade1 <- c(cd.loc.us.trade1, cd.loc.us.trade[[i]][1][1])
}
cd_US$TRADE <- cd.loc.us.trade1
cd_US_trade <- cd_US[which(cd_US$TRADE != -1),]

cd.loc.us.tariff <- gregexpr("TARIFF", cd_US$text)
cd.loc.us.tariff1 <- vector()
for(i in 1:nrow(cd_US)) {
  cd.loc.us.tariff1 <- c(cd.loc.us.tariff1, cd.loc.us.tariff[[i]][1][1])
}
cd_US$TARIFF <- cd.loc.us.tariff1
cd_US_trade <- cd_US[which(cd_US$TRADE != -1),]
cd_US_trade <- rbind(cd_US_trade, cd_US[which(cd_US$TARIFF != -1 & (cd_US$ID %in% cd_US_trade$ID) == F),])

### Extracting All Words from ChinaDaily_US_Trade from 2018 ###
stops <- stopwords(kind = "en")
stops <- toupper(stops)
cd_US_trade$text2 <- cd_US_trade$text

df1 <- cd_US_trade[which(cd_US_trade$Year == "2018"),]
text1 <- gsub('[[:punct:]]',' ', df1$text2[1])
text1 <- gsub('[[:digit:]]',' ', text1)
text1 <- gsub('  ',' ', text1)
text1 <- unlist(strsplit(text1, " "))
text1 <- gsub(" ","", text1)
text1 <- text1[which((nchar(text1) > 3) == T)]
text1 <- as.data.frame(text1, stringsAsFactors = F)
names(text1)[1] <- "word"
text1$stop <- NA
text1$stop <- text1$word %in% stops == T
text2 <- text1[which(text1$stop == F),]
text2 <- text2[order(text2$word),]
text2$ID <- df1$ID[1]
text2$date <- df1$date_published[1]
text2$year <- 2018
text2 <- text2[,c(3,1,5,4)]

cd_US_trWords18 <- text2

df1 <- cd_US_trade[which(cd_US_trade$Year == "2018"),]
for(i in 2:nrow(df1)) {
  text1 <- gsub('[[:punct:]]',' ', df1$text2[i])
  text1 <- gsub('[[:digit:]]',' ', text1)
  text1 <- gsub('  ',' ', text1)
  text1 <- unlist(strsplit(text1, " "))
  text1 <- gsub(' ','', text1)
  text1 <- text1[which((nchar(text1) > 3) == T)]
  text1 <- as.data.frame(text1, stringsAsFactors = F)
  names(text1)[1] <- "word"
  text1$stop <- NA
  text1$stop <- text1$word %in% stops == T
  text2 <- text1[which(text1$stop == F),]
  text2 <- text2[order(text2$word),]
  text2$ID <- df1$ID[i]
  text2$date <- df1$date_published[i]
  text2$year <- 2018
  text2 <- text2[,c(3,1,5,4)]
  cd_US_trWords18 <- rbind(cd_US_trWords18, text2)
}

cd_US_trWords18$date2 <- gsub("-","", cd_US_trWords18$date)
cd_US_trWords18$dateN <- as.numeric(cd_US_trWords18$date2)
cd_US_trWords18 <- cd_US_trWords18[,-(5:6)]
cd_US_trWords18$date <- as.Date(cd_US_trWords18$date)
length(unique(cd_US_trWords18$word))
unq <- unique(cd_US_trWords18$word)
unq.stem <- stemmer(unq, capitalize = T)

cd_US_trWords18 <- left_join(cd_US_trWords18[,1:5], 
                         data.frame(word = unq, stem = as.character(toupper(unq.stem))))
cd_US_trWords18 <- cd_US_trWords18[11:nrow(cd_US_trWords18),]
cd_US_trWords18.2 <- as.data.frame(sort(unique(cd_US_trWords18$date)), stringsAsFactors = F)
names(cd_US_trWords18.2)[1] <- "date"
cd_US_trWords18.2$words <- NA
cd_US_trWords18.2$stem <- NA
for(i in 1:nrow(cd_US_trWords18.2)) {
  cd_US_trWords18.2$words[i] <- list(cd_US_trWords18$word[which(cd_US_trWords18$date == cd_US_trWords18.2$date[i])])
  cd_US_trWords18.2$stem[i] <- list(cd_US_trWords18$stem[which(cd_US_trWords18$date == cd_US_trWords18.2$date[i])])
}
cd_US_trWords18.2 <- as.data.frame(cd_US_trWords18.2, stringsAsFactors= F)
cd_US_trWords18.2$date <- as.Date(cd_US_trWords18.2$date)
cd_US_trWords18.2$wk <- week(cd_US_trWords18.2$date)
cd_US_trWords18.2$freq <- 1
wkART <- setDT(cd_US_trWords18.2[,5:6])[, .(sum_amt = sum(freq)), by = .(wk = wk)]

#### ChinaDaily_US_Trade 2018 Word Stems Weekly Sums ####
cd_US_tradeStems18 <- data.frame(Stem = unique(cd_US_trWords18$stem))
new <- rep(0, nrow(cd_US_tradeStems18))
for (i in 1:52) {
  cd_US_tradeStems18 <- cbind(cd_US_tradeStems18, new)
  names(cd_US_tradeStems18)[i+1] <- paste0("wk",i)
}
wkDTx <- data.frame(wk = 1:52)
for (i in 1:nrow(cd_US_tradeStems18)) {
  stemPres <- cd_US_trWords18[which(cd_US_trWords18$stem == cd_US_tradeStems18$Stem[i]),c(4,6)]
  stemPres$freq <- 1
  stemPres$date <- as.Date(stemPres$date)
  wkDT <- setDT(stemPres)[, .(sum_amt = sum(freq)), by = .(wk = week(date))]
  wkDT <- wkDT[order(wkDT$wk),]
  wkDT <- merge(wkDTx, wkDT, all.x = T)
  wkDT$sum_amt[which(is.na(wkDT$sum_amt) == T)] <- 0 
  cd_US_tradeStems18[i,2:53] <- wkDT$sum_amt 
}

rownames(cd_US_tradeStems18) <- cd_US_tradeStems18$Stem
cd_US_tradeStems18$YrMean <- rowMeans(cd_US_tradeStems18[,2:53])
cd_US_tradeStems18$YrSum <- rowSums(cd_US_tradeStems18[,2:53])
cd_US_tradeStems18 <- cd_US_tradeStems18[which(cd_US_tradeStems18$YrSum > 10),]


hist(cd_US_tradeStems18$YrMean[which(cd_US_tradeStems18$YrMean < 10)])
unique(cd_US_tradeStems18$YrMean)
max(cd_US_tradeStems18$YrMean)

length(cd_US_tradeStems18$wk1[which(cd_US_tradeStems18$YrMean > 5)])
hist(cd_US_tradeStems18$wk1[which(cd_US_tradeStems18$YrMean > 5)])

hist(as.vector(unlist(cd_US_tradeStems18[1000,2:53])))
5/52



#### Plots ####
plot(1:52, cd_US_tradeStems18["NEGOTI",2:53], type = "l", xlab = "2018 Week #", 
     ylab = "Weekly Frequency Sum of Stem 'NEGOTI'")
abline(v = week(ChAnnoun$Date), col = "red")
abline(v = week(ChinaUS_events$Date[which((ChinaUS_events$Actor == "u" | ChinaUS_events$Actor == "b") & 
                                            ChinaUS_events$Announcement == 1)]), col = "blue")
plot(1:52, cd_US_tradeStems18["WILL",2:53], type = "l", xlab = "2018 Week #", 
     ylab = "Weekly Frequency Sum of Stem 'WILL'")
abline(v = week(ChinaUS_events$Date[which((ChinaUS_events$Actor == "u" | ChinaUS_events$Actor == "b") & 
                                            ChinaUS_events$Announcement == 1)]), col = "blue")
abline(v = week(ChAnnoun$Date), col = "red")
points(1:52, cd_US_tradeStems18["NEGOTI",2:53], type = "l", cl = "brown", add = T)
points(1:52, wkART$sum_amt, col = "green", type = "l", add = T)

cor(as.vector(unlist(cd_US_tradeStems18["WILL",2:53])), as.vector(unlist(cd_US_tradeStems18["NEGOTI",2:53])))

ggplot() +
  geom_line(aes(x = 1:52, y = as.vector(unlist(cd_US_tradeStems18["WILL",2:53])), colour = "WILL")) + 
  geom_line(aes(x = 1:52, y = as.vector(unlist(cd_US_tradeStems18["NEGOTI",2:53])), colour = "NEGOTI")) + 
  geom_vline(xintercept = week(ChAnnoun$Date[-c(7:8)])) +
  labs(colour = "Word Stem") +
  xlab("Week # (2018)") + ylab("Weekly Frequency Sum of Word Stem") +
  ggtitle("Weekly Frequencies of Word Stems in 2018 China Daily Articles Mentioning 'Trade' and 'US'", 
          subtitle = "With Chinese Trade Policy Announcements") +
  geom_text(aes(x = week(ChAnnoun$Date)[1] - 0.25, label="China reacts to US List 1, proposes own", y = 85), 
          angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[2] + 0.25, label="China announces antidumping duties against US sorghum", 
                y = 85), angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[3] - 0.25, label="China announces it will end sorghum duties", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[4] + 0.25, label="US and China agree to pause trade war", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[5] - 0.25, label="China reacts to US List 2, proposes own", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[6] - 0.25, label="China Reacts to US List 3, proposes Own", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[9] - 0.25, label="US and China agree to temporary truce", y = 85), 
            angle = 90, text = element_text(size = 1)) 


write.csv(cd_US_tradeStems18, "cd_US_tradeStems18.csv")



  

#####


negoti <- gregexpr("NEGOTI", cd_US_trWords18.2$stem)
negoti1 <- vector()
for(i in 1:nrow(cd_US_trWords18.2)) {
  negoti1 <- c(negoti1, negoti[[i]][1][1])
}
cd_US_trWords18.2$negoti.st <- negoti1
cd_US_trWords18.2$negoti.st[which(cd_US_trWords18.2$negoti.st == -1)] <- 0

length(unique(cd_US_trWords18$word))
length(unique(cd_US_trWords18$stem))
sort(nchar(cd_US_trWords18$word))[1:20]

usTRD <- as.Date(c("2018-02-07", "2018-03-22", "2018-03-23", "2018-04-03", "2018-04-16", "2018-05-03",
                 "2018-05-20", "2018-05-29", "2018-06-07"))
chTRD <- as.Date(c("2018-04-02", "2018-04-04", "2018-04-17", "2018-05-03", "2018-05-18", "2018-05-20", 
                   "2018-06-04", "2018-06-15", "2018-06-16"))
ChinaUS_events$Date <- as.Date(ChinaUS_events$Date)
ChinaUS_events$wk <- week(ChinaUS_events$Date)
ChAnnoun <- ChinaUS_events[which((ChinaUS_events$Actor == "c" | ChinaUS_events$Actor == "b") & 
                                   ChinaUS_events$Announcement == 1),]


plot(cd_US_trWords18.2$date, cd_US_trWords18.2$negoti.st, type = "l")
abline(v = ChAnnoun$Date, col = "red")
abline(v = usTRD, col = "blue", add = T)
abline(v = chTRD, col = "red", add = T)

lo <- loess(cd_US_trWords18.2$date~cd_US_trWords18.2$negoti)
plot(x,y)
lines(predict(lo), col='red', lwd=2)

cd_US_trWords18.3 <- setDT(cd_US_trWords18.2)[, .(sum_amt = sum(negoti.st)), by = .(yr = year(date), 
                                                                                 wk = week(date))]


plot(cd_US_trWords18.3$wk, cd_US_trWords18.3$sum_amt, type = "l", 
     xlab = "2018 Week #", ylab = "Weekly Mean of the Daily Frequency of Stem 'NEGOTI' ")
abline(v = week(ChAnnoun$Date), col = "red")
abline(v = week(chTRD), col = "red", add = T)
abline(v = week(usTRD), col = "blue", add = T)



for(i in 1:nrow(cd_US_trWords18.2)) {
  cd_US_trWords18.2$negotiate_frequency[i] <- table(unlist(data$clean_text2[data$date_published==new_data$date[i]]))["negotiate"]
}
cd_US_trWords18.2$negotiate_frequency <- ifelse(is.na(cd_US_trWords18.2$negotiate_frequency), 0, cd_US_trWords18.2$negotiate_frequency)
sum(cd_US_trWords18.2$negotiate_frequency)

plot(cd_US_trWords18.2$date, cd_US_trWords18.2$negotiate_frequency)

duplicated(cd_US_trWords18$word[8], cd_US_trWords18$word[which(cd_US_trWords18$dateN == 20180102)])
length(unique(cd_US_trWords18$word))
length(cd_US_trWords18$word[which(cd_US_trWords18$dateN >= 20180100 & cd_US_trWords18$dateN < 20180200 
                                  & cd_US_trWords18$word == cd_US_trWords18$word[1])])
length(unique(cd_US_trWords18$word[which(cd_US_trWords18$dateN <= 20180100)]))
length(unique(cd_US_trWords18$ID))

length(unique(cd_US_trWords18.2$ID))
length(unique(cd_US_trade$ID))


stemmer(unlist(cd_US_trWords18.2$words[1]))



