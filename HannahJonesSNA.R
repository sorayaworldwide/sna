library(tidyverse)
library(tidytext)
library(readxl)
library(writexl)
library(rtweet)
library(tweetrmd)
library(knitr)
library(tidygraph)
library(ggraph)
library(igraph)
install.packages("graphlayouts")
library(graphlayouts)
library(vader)

app_name <- "SNANCState"
api_key <- "WIqfaxZjBWqkO1fAyvIlHiTHd"
api_secret_key <- "QJrba0F1Io3sdN6t3BHJHlNx8cS66T6QVrmx62wcPqqIhZgVS2"
access_token <- "293694037-Zo3NiouzGCVBocrwniIVUdiFikcufHU3xLzlewJI"
access_token_secret <- "IqAutCoY7RR7mdIkGj5d1lx3BwCi2QMK6bNCziYWvFdFy"

## authenticate via web browser
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

#Query
nhj_tweets <- search_tweets(q = "#NikoleHannahJones", n=1000,
                            include_rts=FALSE)
#save to excel
write_xlsx(nhj_tweets, "nhj_tweets.xlsx")

nhj_tweets<-read_excel("nhj_tweets.xlsx")

#flatten
nhj_tweets1 <- rtweet::flatten(nhj_tweets)


#regex
regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

nhj_tweets1_usernames <-
  nhj_tweets1 %>%
  # Use regular expression to identify all the usernames in a tweet
  mutate(mentions_screen_name = str_extract_all(text, regex)) %>%
  unnest(mentions_screen_name)



#Create Edgelist - Ties
ties_1 <-  nhj_tweets1_usernames %>%
  relocate(sender = screen_name, # rename scree_name to sender
           target = mentions_screen_name) %>% # rename to receiver
  select(sender,
         target,
         created_at,
         text)

ties_2 <- ties_1 %>%
  unnest_tokens(input = target,
                output = receiver,
                to_lower = FALSE) %>%
  relocate(sender, receiver)
  
ties<-ties_2%>%
drop_na(receiver)


#nodes - Actors
actors_1 <- ties_2 %>%
  pivot_longer(cols = sender:receiver, 
               names_to = "nodes",
               values_to = "screen_name")
#select distinct
actors <- actors_1 %>%
  select(screen_name) %>%
  distinct() %>%
  drop_na()

#create network
network <- tbl_graph(edges = ties, 
                     nodes = actors)

network

## add degrees
network_1 <- network %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

network_1

## node measures

node_measures <- network_1 %>% 
  activate(nodes) %>%
  data.frame()

summary(node_measures)

# betweeness and closeness

network_2 <- network_1 %>%
  activate(nodes) %>%
  mutate(betweenness = centrality_betweenness()) %>%
  mutate(closeness = centrality_degree(mode = "out"))

# Community detection
network_3 <- network_2 %>%
  activate(nodes) %>%
  mutate(group = group_infomap())

network_3

##sociogram 
network_3 %>%
  ggraph(layout = "kk") + 
  geom_node_point(aes(size = in_degree, 
                      colour = group)) +
  geom_node_text(aes(label = screen_name, 
                     size = degree/2,
                     alpha = degree), 
                 repel=TRUE) +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                 end_cap = circle(3, 'mm'),
                 alpha = .3) + 
  theme_graph()+
  theme(legend.position = "bottom")

network_3 %>%
  ggraph(layout = "stress") + 
  geom_node_point(aes(size = in_degree, 
                      colour = group)) +
  geom_node_text(aes(label = screen_name, 
                     size = degree/2,
                     alpha = degree), 
                 repel=TRUE) +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                 end_cap = circle(3, 'mm'),
                 alpha = .3) + 
  theme_graph()

## Sentiment Analysis
summary_vader <- vader_df(ties$text)

join_ties <- ties %>%
  inner_join(summary_vader, by="text")

vaderties <-join_ties%>%
  select(sender, receiver, created_at, text, compound)%>%
  mutate(sentiment = ifelse(compound > 0, "positive",
                            ifelse(compound <0, "negative", "neutral")))


vaderties_4network<-vaderties %>%
  distinct(sender, receiver, text, .keep_all = TRUE)
  
##create new network object with sentiment values

sentimentnetwork<-tbl_graph(edges = vaderties_4network, 
                            nodes = actors)
  
## add degrees
sentimentnetwork_1 <- sentimentnetwork %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

sentimentnetwork_1

## node measures

sentiment_node_measures <- sentimentnetwork_1 %>% 
  activate(nodes) %>%
  data.frame()

summary(sentiment_node_measures)

# betweeness and closeness

sentimentnetwork_2 <- sentimentnetwork_1 %>%
  activate(nodes) %>%
  mutate(betweenness = centrality_betweenness()) %>%
  mutate(closeness = centrality_degree(mode = "out"))

# Community detection
sentimentnetwork_3 <- sentimentnetwork_2 %>%
  activate(nodes) %>%
  mutate(group = group_infomap())

sentimentnetwork_3



