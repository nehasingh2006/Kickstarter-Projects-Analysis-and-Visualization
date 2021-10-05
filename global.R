# Import Libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly) 
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(leaflet)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)

KickStarter <- read.csv("www/data/ks-projects-201801.csv")
loc <- read.csv("www/data/Recode.csv")

#############Function for count per category##################################
CountPerCat <- function(colN) {
  
  TotaCountPerCat <-
    KickStarter %>% 
    filter(main_category == colN) %>%
    group_by(main_category,category) %>% 
    ggplot()+
    geom_bar(aes(x= category, fill = state)) + 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 25))+
    labs(title="Different Sub Category and their status Per Main Category", y="Count", x="Sub Category")
  
  ggplotly(TotaCountPerCat) %>% layout(legend = list(x = 0.0, y = 1))
}
#############################################################################

##############Success Rate for all Main Cat##################################
SumofMain <- KickStarter %>% 
  group_by(main_category) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

SumofSucess <- KickStarter %>% 
  filter(state == 'successful') %>% 
  group_by(main_category) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

SuccessRate <- SumofMain %>% 
  mutate(success = SumofSucess$count / SumofMain$count) %>% 
  ggplot() + 
  geom_bar(aes(x=main_category, y=success), stat = 'identity', fill = 'cadetblue') + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 25)) +
  labs(title="Overall Success Rate", y="Success Rate", x="Main Category")
#############################################################################

###########Total Project Per Main Cat (2018)#################################
TotaCount <-
  KickStarter %>% 
  group_by(main_category) %>% 
  ggplot()+
  geom_bar(aes(x= main_category, fill = state)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 25)) + 
  labs(title=" Main Category Popularity", y="Total Count", x="Main Category") + 
  theme(legend.position = c(0.7, 0.2))

TotaCountPlot <- ggplotly(TotaCount) %>% layout(legend = list(x = 0.0, y = 1))

#############################################################################

######dot plot compare success count vs total count #########################
TopSubSumofSucess <- KickStarter %>% 
  filter(state == 'successful') %>% 
  group_by(main_category) %>% 
  summarise(countsucc=n())

TopSubSum <- KickStarter %>% 
  group_by(main_category) %>% 
  summarise(countsum=n())

mergetop <- TopSubSum %>% inner_join(TopSubSumofSucess, by = "main_category")

ps <- ggplot(data = mergetop, aes(x=countsum, y=countsucc, color = main_category)) + 
  geom_jitter(width = 1, height = 1) + 
  labs(title="Total Vs. Succeeded projects in every Main Category", y="No. of Succeeded projects", x="Total no. of projects per Main Category") +
  theme(legend.position = "none") +
  theme_bw() + theme(legend.title=element_blank())
##############################################################################

############This is average number of backers for main category###############

KickStarter$staterecode <- KickStarter$state
KickStarter$staterecode <- ifelse(KickStarter$staterecode == 'successful', 'successful', 'not successful')


backerplot <- KickStarter %>% 
  group_by(main_category, staterecode) %>% 
  summarise(Avg = mean(backers)) %>% 
  ggplot(aes(x = main_category, y = Avg, fill = staterecode)) + 
  geom_bar(position = 'dodge', stat = "identity")+
  labs(title="Avg Backers Per Main Category", x="Main Category", y="Average No. of Backers") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25))+
  theme(legend.title=element_blank())
#running code
backerplot2 <- ggplotly(backerplot) %>% layout(legend = list(x = 0.0, y = 1))

##############################################################################

##################success count vs total count for main category#############

TopSubSumofSucess1 <- KickStarter %>%
  filter(state == 'successful') %>%
  group_by(main_category,category) %>%
  summarise(countsucc=n())

TopSubSum1 <- KickStarter %>%
  group_by(main_category,category) %>%
  summarise(countsum=n())

mergetop1 <- TopSubSum1 %>% merge(x= TopSubSumofSucess1, y=TopSubSum1,by.x  = c('main_category',"category"),by.y = c('main_category',"category") ,all.x = TRUE)

succsubpermain <- function(colN) {
  pss<- mergetop1 %>%
    filter(main_category == colN) %>%
    ggplot(aes(x=countsum, y=countsucc, color = category)) +
    geom_jitter(width = 1, height = 1) +
    labs(title="Total Vs. Succeeded", y="# Succeeded", x="# Total") +
    theme_bw() + theme(legend.title=element_blank())
  ggplotly(pss)
}
#############################################################################

#################FGoalVSPledge amount#######################################
GoalVSPledge <- function(colN) {
  
  MedianGoal <- KickStarter %>% 
    filter(main_category == colN) %>%
    group_by(main_category,category) %>% 
    summarise(Med=median(usd_goal_real))
  
  MedianPledge <- KickStarter %>% 
    filter(main_category == colN) %>%
    group_by(main_category,category) %>% 
    summarise(Med=median(usd_pledged_real))
  
  p <- ggplot() + 
    geom_bar(data = MedianGoal,aes(x=category, y=Med), stat = 'identity',fill = 'steelblue')+
    geom_line(data = MedianPledge,aes(x=category, y=Med, group = 2), color = 'red', size = 1) + 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 20))+
    labs(title="Goal Vs. Pledged Amount", y="Amount", x="Sub Category")  
  
  ggplotly(p)
}
#############################################################################


################# Does success depend on the length of the campaign period ###
KickStarter$date_diff <- as.Date(as.character(KickStarter$deadline), format="%Y-%m-%d")-
  as.Date(as.character(KickStarter$launched), format="%Y-%m-%d")

Countperdatediff <- KickStarter %>% 
  filter(date_diff < 100) %>% 
  group_by(date_diff) %>% 
  summarise(count = n())

Successperdatediff <- KickStarter %>% 
  filter(date_diff < 100) %>% 
  filter(state == 'successful') %>% 
  group_by(date_diff) %>% 
  summarise(count1 = n())

Successperdatediff$count <- Successperdatediff$count1 / Countperdatediff$count
SuccessperdatediffMatch <- Successperdatediff[-c(2)]

Countperdatediff$panel <- 'Total Count'
SuccessperdatediffMatch$panel <- 'Success Rate'
options(scipen = 999) 
campaign_period <-rbind(Countperdatediff, SuccessperdatediffMatch)



campaign_period_plot <- 
  ggplot(data = campaign_period, mapping = aes(x=date_diff, y=count)) + 
  facet_grid(panel~., scales = 'free') + 
  geom_line(size = 1, color = 'cadetblue1') + 
  theme_bw()+
  labs(title="Campaign Duration Vs Success Rate", y="", x="Campaign Duration (Days)")

#############################################################################

##############Bar chart for success rate of all sub category##############

SuccRate <- function(colN) {
  
  
  SumofSub <- KickStarter %>% 
    filter(main_category == colN) %>%
    group_by(main_category,category) %>% 
    summarise(count=n())
  
  SubSumofSucess <- KickStarter %>% 
    filter(state == 'successful') %>% 
    filter(main_category == colN) %>%
    group_by(main_category,category) %>% 
    summarise(count=n())
  
  SubSuccessRate <- SumofSub %>% 
    mutate(subsuccess = SubSumofSucess$count / SumofSub$count) %>% 
    ggplot() + 
    geom_bar(aes(x=category, y=subsuccess), stat = 'identity', fill = 'cadetblue1') + 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 20))+
    labs(title="Success Rate of each Subcategory", y="Success Rate", x="Sub-Category")
  
  ggplotly(SubSuccessRate)
}
############################################################################

######################wordcloudfun################################
wordcloudfun <- function(colN1, colN2){
  word_cloud_data <- KickStarter %>% 
    filter(main_category == colN1)  %>% 
    filter(category == colN2) %>% 
    filter(state == "successful") 
  
  text_only_word_cloud <- paste(word_cloud_data$name[0:400000], collapse = '')
  
  Rest_docs <- Corpus(VectorSource(text_only_word_cloud))
  
  # Convert the text to lower case
  Rest_docs <- tm_map(Rest_docs, content_transformer(tolower))
  # Remove numbers
  Rest_docs <- tm_map(Rest_docs, removeNumbers)
  # Remove english common stopwords
  Rest_docs <- tm_map(Rest_docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  Rest_docs <- tm_map(Rest_docs, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  Rest_docs <- tm_map(Rest_docs, removePunctuation)
  # Eliminate extra white spaces
  Rest_docs <- tm_map(Rest_docs, stripWhitespace)
  ###############
  Rest_dtm <- TermDocumentMatrix(Rest_docs)
  Rest_m <- as.matrix(Rest_dtm)
  Rest_v <- sort(rowSums(Rest_m),decreasing=TRUE)
  Rest_d <- data.frame(word = names(Rest_v),freq=Rest_v)
  set.seed(1)
  
  wordcloud(words = Rest_d$word, freq = Rest_d$freq, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}
#############################################################################

################This is the Map ############################################
mapdata <- KickStarter %>% 
  group_by(country) %>% 
  summarise(count = n())

loc <- read.csv(file = 'www/data/Recode.csv')

mapdata_with_loc <- merge(x = mapdata, y = loc, by = 'country')

map <- leaflet(data = mapdata_with_loc) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(region), label = ~as.character(count))
#############################################################################

###########################Data table######################################## 
datatable <- KickStarter %>% select(name, main_category, category, goal, pledged,
                                    currency,
                                    backers, date_diff, country, staterecode) %>% 
  rename ("Project Name" = name, 
          "Main Category" = main_category, 
          "Sub Category"=category,
          "Goal Amount" = goal, 
          "Pledge Amount"= pledged,
          "Currency" = currency,
          "No. of Backers"=backers,
          "No of Campaign Days" = date_diff, 
          "Country"= country, 
          "Status" = staterecode)
##############################################################################

#######################Shankey plot############################################
sk <- KickStarter %>% group_by(main_category,category) %>% 
  summarise(count = n()) %>% slice_max(order_by = count, n = 3) %>% as.data.frame()

sk$category<-recode(sk$category, 'Art' = 'Art.', 'Comics' = 'Comics.', 'Crafts' = 'Crafts.', 'Dance'='Dance.', 'Design' = 'Design.',
                    'Fashion' = 'Fashion.', 'Film & Video' = 'Film & Video1','Food' = 'Food1','Technology' = 'Technology.',
                    'Games'='Games.','Journalism' = 'Journalism.','Music'='Music.','Photography'='Photography.','Theater'='Theater.')

nodes <- data.frame(name=c(as.character(sk$main_category), as.character(sk$category)) %>%unique())

sk$IDsource=match(sk$main_category, nodes$name)-1 
sk$IDtarget=match(sk$category, nodes$name)-1

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

skplot <- sankeyNetwork(Links = sk, Nodes = nodes,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "count", NodeID = "name", 
                        sinksRight=FALSE, colourScale=ColourScal,nodeWidth=50, fontSize=13)

#############################################################################