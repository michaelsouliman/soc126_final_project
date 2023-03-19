setwd("/Users/michaelsouliman/Desktop/soc126/project")

library(igraph)
library(readr)
library(data.table)

#import TSV file into data frame
edges <- read_tsv('soc-redditHyperlinks-body.tsv')
nodes <- read.csv("web-redditEmbeddings-subreddits.csv")

#only graph edges associated with a valid node in the network (some edges are associated with nodes not in my nodes df)
edges <- edges[edges$SOURCE_SUBREDDIT %in% c(nodes$Id) & edges$TARGET_SUBREDDIT %in% c(nodes$Id),]
edges_pos <- edges[edges$LINK_SENTIMENT == 1,]
edges_neg <- edges[edges$LINK_SENTIMENT == -1,]


#graph with all edges for all 40 months
whole_graph <- graph_from_data_frame(
  edges,
  directed=TRUE,
  vertices=nodes
)

#Remove isolates from the graph
Isolated = which(degree(whole_graph)==0)
whole_graph = delete.vertices(whole_graph, Isolated)

vcount(whole_graph)
ecount(whole_graph)

pos_graph <- graph_from_data_frame(
  edges_pos,
  directed=TRUE,
  vertices=nodes
)

Isolated = which(degree(pos_graph)==0)
pos_graph = delete.vertices(pos_graph, Isolated)

neg_graph <- graph_from_data_frame(
  edges_neg,
  directed=TRUE,
  vertices=nodes
)

Isolated = which(degree(neg_graph)==0)
neg_graph = delete.vertices(neg_graph, Isolated)

#Centrality measures
sort(degree(whole_graph, normalized = TRUE), decreasing = TRUE)[1:5]
sort(betweenness(whole_graph, normalized = TRUE), decreasing = TRUE)[1:5]
sort(evcent(whole_graph)$vector, decreasing = TRUE)[1:5]
sort(page.rank(whole_graph)$vector, decreasing = TRUE)[1:5]

sort(degree(pos_graph, normalized = TRUE), decreasing = TRUE)[1:5]
sort(betweenness(pos_graph, normalized = TRUE), decreasing = TRUE)[1:5]
sort(evcent(pos_graph)$vector, decreasing = TRUE)[1:5]
sort(page.rank(pos_graph)$vector, decreasing = TRUE)[1:5]

#Density of the graph
graph.density(whole_graph)

#Average degree of the graph
mean(degree(whole_graph))

#GCC
transitivity(graph, type = "global")

#Color edges based on their sentiment (red is for negative sentiment, blue is for positive sentiment)
E(whole_graph)$color <- NA
E(whole_graph)$color[E(whole_graph)$LINK_SENTIMENT == -1] <- "red"
E(whole_graph)$color[E(whole_graph)$LINK_SENTIMENT == 1] <- "blue"

#Get subset of the graph for only the time around the 2016 Election
political_edges <- edges[edges$TIMESTAMP > "2016-09-30" & edges$TIMESTAMP < "2016-11-11",]
political_edges_pos <- political_edges[political_edges$LINK_SENTIMENT == 1,]
political_edges_neg <- political_edges[political_edges$LINK_SENTIMENT == -1,]

#Create graph of all interactions, both positive and negative
political_graph <- graph_from_data_frame(
  political_edges,
  directed=TRUE,
  vertices=nodes
)

#Create graph for political time period where edges have postive sentiment
political_graph_pos <- graph_from_data_frame(
  political_edges_pos,
  directed=TRUE,
  vertices=nodes
)

#Create graph for political time period where edges have negative sentiment
political_graph_neg <- graph_from_data_frame(
  political_edges_neg,
  directed=TRUE,
  vertices=nodes
)

#Remove isolates from the graph
Isolated = which(degree(political_graph)==0)
political_graph = delete.vertices(political_graph, Isolated)

Isolated = which(degree(political_graph_pos)==0)
political_graph_pos = delete.vertices(political_graph_pos, Isolated)

Isolated = which(degree(political_graph_neg)==0)
political_graph_neg = delete.vertices(political_graph_neg, Isolated)

#See which nodes spread the most of one type of sentiment

sort(degree(political_graph_neg, mode="out"), decreasing = TRUE)[1:5]

#Get the proportion of negative edges that come from the top 1% and 0.1% of nodes respectively
percent_politcal_neg.top1 = sum(sort(degree(political_graph_neg), decreasing = TRUE)[1:7])/sum(degree(political_graph_neg, mode="out"))
percent_politcal_neg.top01 = sum(sort(degree(political_graph_neg), decreasing = TRUE)[1:67])/sum(degree(political_graph_neg, mode="out"))

#Color-code edges for political graph
E(political_graph_no_isolates)$color <- NA
E(political_graph_no_isolates)$color[E(political_graph_no_isolates)$LINK_SENTIMENT == -1] <- "red"
E(political_graph_no_isolates)$color[E(political_graph_no_isolates)$LINK_SENTIMENT == 1] <- "blue"
layout.lgl <- layout_with_lgl(political_graph_no_isolates)

#Color-code edges for political graph negative
E(political_graph_neg)$color <- NA
E(political_graph_neg)$color[E(political_graph_neg)$LINK_SENTIMENT == -1] <- "red"
E(political_graph_neg)$color[E(political_graph_neg)$LINK_SENTIMENT == 1] <- "blue"
set.seed(20)
layout.fr.pgn <- layout.fruchterman.reingold(political_graph_neg, niter=10000)

plot(political_graph_neg,
     layout = layout.fr.pgn,
     vertex.size = 2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.width = 0.5,
     edge.arrow.size = 0.1
)

title("Negative Interactions During the 2016 Election", cex.main=1)

community.pgn.eb <- cluster_edge_betweenness(political_graph_neg, directed=TRUE)
community.pgn.infomap <- infomap.community(political_graph_neg, nb.trials = 100)
plot(community.pgn.eb, political_graph_neg,
     layout = layout.fr.pgn,
     vertex.size = 2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.width = 0.5,
     edge.arrow.size = 0.1)
title("Negative Interactions During the 2016 Election Communities (EB)", cex.main=1)

plot(community.pgn.infomap, political_graph_neg,
     layout = layout.fr.pgn,
     vertex.size = 2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.width = 0.5,
     edge.arrow.size = 0.1)
title("Negative Interactions During the 2016 Election Communities (InfoMap)", cex.main=1)

length(unique(community.pgn.eb$membership))
length(unique(community.pgn.infomap$membership))
modularity(political_graph_neg, community.pgn.eb$membership)
modularity(political_graph_neg, community.pgn.infomap$membership)

hist(degree(political_graph_neg, mode = "out"),
     main="Degree Distribution of Negative Interactions During the 2016 Election",
     xlab="Out-Degree of a Subreddit",
     col="red",
)

non_political_edges <- edges[edges$edges.TIMESTAMP > "2015-09-30" & edges$edges.TIMESTAMP < "2015-11-11",]
non_political_edges_pos <- non_political_edges[non_political_edges$edges.LINK_SENTIMENT == 1,]
non_political_edges_neg <- non_political_edges[non_political_edges$edges.LINK_SENTIMENT == -1,]

non_political_graph <- graph_from_data_frame(
  non_political_edges,
  directed=TRUE,
  vertices=nodes
)

non_political_graph_pos <- graph_from_data_frame(
  non_political_edges_pos,
  directed=TRUE,
  vertices=nodes
)

non_political_graph_neg <- graph_from_data_frame(
  non_political_edges_neg,
  directed=TRUE,
  vertices=nodes
)

Isolated = which(degree(non_political_graph_pos)==0)
non_political_graph_pos = delete.vertices(non_political_graph_pos, Isolated)

Isolated = which(degree(non_political_graph_neg)==0)
non_political_graph_neg = delete.vertices(non_political_graph_neg, Isolated)

sort(degree(non_political_graph_pos, mode="out"), decreasing = TRUE)
sort(degree(non_political_graph_neg, mode="out"), decreasing = TRUE)

vcount(non_political_graph_neg)

percent_non_politcal_neg.top1 = sum(sort(degree(non_political_graph_neg, mode="out"), decreasing = TRUE)[1:6])/sum(degree(non_political_graph_neg, mode="out"))
percent_non_politcal_neg.top01 = sum(sort(degree(non_political_graph_neg, mode="out"), decreasing = TRUE)[1:62])/sum(degree(non_political_graph_neg, mode="out"))

E(non_political_graph_neg)$color <- NA
E(non_political_graph_neg)$color[E(non_political_graph_neg)$edges.LINK_SENTIMENT == -1] <- "red"
E(non_political_graph_neg)$color[E(non_political_graph_neg)$edges.LINK_SENTIMENT == 1] <- "blue"
set.seed(20)
layout.fr.npgn <- layout.fruchterman.reingold(non_political_graph_neg, niter=10000)
plot(non_political_graph_neg,
     layout = layout.fr.npgn,
         vertex.size = 2,
         vertex.label=NA,
         vertex.label.cex=1,
         edge.width = 0.5,
         edge.arrow.size = 0.1
)

title("Negative Interactions Before the 2016 Election", cex.main=1)

community.npgn.eb <- cluster_edge_betweenness(non_political_graph_neg, directed=TRUE)
community.npgn.infomap <- infomap.community(non_political_graph_neg, nb.trials = 100)
plot(community.npgn.eb, non_political_graph_neg,
     layout = layout.fr.npgn,
     vertex.size = 2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.width = 0.5,
     edge.arrow.size = 0.1)
title("Negative Interactions Before the 2016 Election Communities (EB)", cex.main=1)

plot(community.npgn.infomap, non_political_graph_neg,
     layout = layout.fr.npgn,
     vertex.size = 2,
     vertex.label=NA,
     vertex.label.cex=1,
     edge.width = 0.5,
     edge.arrow.size = 0.1)
title("Negative Interactions Before the 2016 Election Communities (InfoMap)", cex.main=1)

length(unique(community.npgn.eb$membership))
length(unique(community.npgn.infomap$membership))
modularity(non_political_graph_neg, community.npgn.eb$membership)
modularity(non_political_graph_neg, community.npgn.infomap$membership)

hist(degree(non_political_graph_neg, mode = "out"),
     main="Degree Distribution of Negative Interactions Before the 2016 Election",
     xlab="Out-Degree of a Subreddit",
     col="red",
)

#Compare stats for the two periods for negative interactions
#Density of the graph
graph.density(political_graph_neg)
graph.density(non_political_graph_neg)

#Average degree of the graph
mean(degree(political_graph_neg))
mean(degree(non_political_graph_neg))

#GCC
transitivity(political_graph_neg, type = "global")
transitivity(non_political_graph_neg, type = "global")
