# twitter
library(twitteR)
library(base64enc)
library(curl)
library(XML)
library(RCurl)

setwd("c:/gergo/eunethta/twitter")

# connection setup
download.file(url="https://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

# identification
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# only for collecting data
candidates <- c("EUnetHTA")

for(i in candidates)
{
  myUser <- getUser(i)
  
  # follower list (who follows the user)
  followers   <- myUser$getFollowers()
  
  # friends list (who does the user follow)
  friends     <- myUser$getFriends()
  
  ideigl <- do.call("rbind", lapply(friends, as.data.frame))
  ideigl2 <- do.call("rbind", lapply(followers, as.data.frame))
  
  write.table(ideigl, paste0(i,"_", as.Date(Sys.time()), "_friends.txt"), sep="\t")
  write.table(ideigl2, paste0(i,"_", as.Date(Sys.time()), "_followers.txt"), sep="\t")
}

# housekeeping

friends_files   <- list.files(pattern="_friends.txt")
followers_files <- list.files(pattern="_followers.txt")

# for network graph on friends and # of followers
# who is myUser listening to, and who listens back

collect_nodes <- NULL
collect_edges <- NULL

for (i in followers_files)
{
  myUser <- strsplit(i, "_")[[1]][1]
  adat <- read.table(i, sep="\t")
  adat2 <- read.table(friends_files[grep(myUser, friends_files)], sep="\t")
  
  # get edges
  who_edges <- as.data.frame(cbind(as.character(myUser), as.character(adat$screenName), "Undirected"))
  colnames(who_edges) <-  c('source', 'target', 'type')     
  
  who_edges$type2 <- ifelse(who_edges$target %in% adat2$screenName, 1, 0)
  
  # get nodes
  who_nodes <- as.data.frame(rbind(
    cbind(myUser, myUser, nrow(adat)),
    cbind(as.character(adat$screenName), as.character(adat$name), adat$followersCount)))
  colnames(who_nodes) <-  c('id', 'label', 'followersCount')
  
  collect_nodes <- rbind(collect_nodes, who_nodes)
  collect_edges <- rbind(collect_edges, who_edges)
}

write.csv(collect_nodes, "eunethta_candi_nodes.csv")
write.csv(collect_edges, "eunethta_candi_edges.csv")

# geocode followers
for (i in followers_files)
{
  adat <- read.table(i, sep="\t")
  adat <- adat[adat$location != "",]
  adat$location <- tolower(adat$location)
  
  out_adat <- aggregate(adat$followersCount ~ adat$location, FUN=sum, data=adat)
  colnames(out_adat) <- c('location', 'followersCount')
  
  out_adat2 <- aggregate(adat$followersCount ~ adat$location, FUN=length, data=adat)
  colnames(out_adat2) <- c('location', 'followersNum')
  
  out_adat3 <- merge(out_adat, out_adat2, by.x='location', by.y='location', all.x=T, all.y=T)
  out_adat3 <- out_adat3[order(out_adat3$followersCount, decreasing=T),]
  
  write.csv(out_adat3, paste0("geo_", gsub(".txt", "", i),".csv"))
}

library(tm)
library(wordcloud)
library(RColorBrewer)

retweeters <- NULL

search_out <- searchTwitter("from:EUnetHTA", since = "2016-01-01", n = 1000)
search_out_ids = sapply(search_out, function(x) x$id)
  
search_out_tweets <- sapply(search_out_ids, function(x) retweets(x, n = 100))
  
names_df <- as.data.frame(sapply(unlist(search_out_tweets), function(x) x$screenName))
retweeters <- c(retweeters, as.character(names_df[1:nrow(names_df),]))

write.csv(unique(retweeters), "eunethta_retweeters.csv")

