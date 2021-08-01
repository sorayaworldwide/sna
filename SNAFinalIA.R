library(readxl)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(statnet)
library(kableExtra)

#read in data nodes
leader_collab_nodes <- read_excel("school leader attributes.xlsx", 
                           col_types = c("text", "numeric", "numeric", "numeric", "numeric"))

#read in data leader matrix
leader_collab_matrix<-read_excel("school leader collab adj matrix.xlsx",
                          col_names =FALSE)

#dichotomize matrix
leader_matrix <- leader_collab_matrix %>%
  as.matrix()

leader_matrix[leader_matrix <= 2] <- 0

leader_matrix[leader_matrix >= 3] <- 1

#add row names
rownames(leader_matrix) <- leader_collab_nodes$ID
colnames(leader_matrix) <- leader_collab_nodes$ID

#get edges
adjacency_matrix <- graph.adjacency(leader_matrix,
                                    diag = FALSE)

#convert matrix to standard edge-list
leader_edges <- get.data.frame(adjacency_matrix)

#create network graph object
leader_graph <-tbl_graph(edges=leader_edges,
                         nodes=leader_collab_nodes)

##Descriptives
#Node degree
leader_measures <- leader_graph %>%
  activate(nodes) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

leader_measures%>%
  kbl()%>%
  kable_paper("hover", full_width = F, fixed_thead = T)%>%
  scroll_box(height = "300px")
#node measures
node_measures <- leader_measures %>% 
  activate(nodes) %>%
  data.frame()

summary(node_measures)%>%
kbl()%>%
  kable_paper("hover", full_width = F, fixed_thead = T)%>%
  scroll_box(height = "300px")

#summary stats for in, out, trust, and efficacy
node_measures %>%
  group_by(DISTRICT.SITE) %>%
  summarise(n = n(),
            mean = mean(in_degree), 
            sd = sd(in_degree)
  )%>%
  kbl(caption="Summary Statistics, In-degree")%>%
kable_paper("hover", full_width = F, fixed_thead = T)%>%
  scroll_box(height = "300px")

node_measures %>%
  group_by(DISTRICT.SITE) %>%
  summarise(n = n(),
            mean = mean(out_degree), 
            sd = sd(out_degree)
  )%>%
  kbl(caption="Summary Statistics, Out-degree")%>%
  kable_paper("hover", full_width = F, fixed_thead = T)%>%
  scroll_box(height = "300px")

node_measures %>%
  group_by(DISTRICT.SITE) %>%
  summarise(n = n(),
            mean = mean(TRUST), 
            sd = sd(TRUST)
  )%>%
  kbl(caption="Summary Statistics, Trust")%>%
  kable_paper("hover", full_width = F, fixed_thead = T)%>%
  scroll_box(height = "300px")


node_measures %>%
  group_by(DISTRICT.SITE) %>%
  summarise(n = n(),
            mean = mean(EFFICACY), 
            sd = sd(EFFICACY)
  )%>%
  kbl(caption="Summary Statistics, Efficacy")%>%
  kable_paper("hover", full_width = F, fixed_thead = T)%>%
  scroll_box(height = "300px")

#Visualization
ggraph(leader_measures, layout = "kk") + 
  geom_node_point(aes(size=in_degree,
                      alpha=out_degree,
                      colour=factor(`DISTRICT/SITE`),
                      shape=factor(MALE))) +
  geom_node_text(aes(label = ID), 
                 repel=TRUE) +
  geom_edge_link(color="grey",
                 arrow=arrow(type="closed",
                             angle=15, 
                             length=unit(.5,'mm')),
                 show.legend = FALSE) + 
  theme_graph()
  
##ERGMS
#create network object
leader_network <- as.network(leader_edges,
                             vertices = leader_collab_nodes)



