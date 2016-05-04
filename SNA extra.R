### 0. Libraries----
library(NLP)
library(tm)

topo <- function(x){
  nodes <- vcount(x)
  edges <- ecount(x)
  density <- graph.density(x, loops = T)
  
  topo <- data.frame(nodes,edges,density)
  return(topo)
}

### 1. import the file----
greg = readLines("text.txt")

### 2. Corpus ----
doc.vec <- VectorSource(greg)
doc.corpus <- Corpus(doc.vec)

#cleaning up the text
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))

#stemming & whitespace
library(SnowballC)
doc.corpus <- tm_map(doc.corpus, stemDocument)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)

# TDM
TDM <- TermDocumentMatrix(doc.corpus)

#remove sparse terms
#TDM.common = removeSparseTerms(TDM, 0.05)

### 3. turning to graph ----
# change it to a Boolean matrix
termDocMatrix <- as.matrix(TDM)
termDocMatrix[termDocMatrix>=1] <- 1

# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")

#subsetting based on degree - by providing matrix and required degree
filter.on.degree <- function(matrixY, x) {
  #finding nodes name based on the required degree
  a <- as.data.frame(degree(g))
  colnames(a) <- c("deg")
  a2 <- subset(a, deg > x)
  a2 <- cbind(Row.Names = rownames(a2), a2)
  
  newdata <- as.data.frame(matrixY)
  newdata <- cbind(Row.Names = rownames(newdata), newdata)
  newdata <- newdata[newdata$Row.Names %in% a2$Row.Names, ]
  newdata <- newdata[ , which(names(newdata) %in% a2$Row.Names)]
  
  cleaned_matrix <- as.matrix(newdata)
}

cleaned.termMatrix <- filter.on.degree(termMatrix, 100)
g2 <- graph.adjacency(cleaned.termMatrix, weighted=T, mode = "undirected")

# remove loops
g2 <- simplify(g2)
# set labels and degrees of vertices
V(g2)$label <- V(g2)$name
V(g2)$degree <- degree(g2)

#delete isolates
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}
g2 <- delete.isolates(g2, mode = 'in')

### 4.Plot a Graph----
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g2)*5

V(g2)$size=degree(g2)/5
E(g2)$color <- "white"

pdf("sna_words1.pdf")
plot(g2, layout=layout.lgl,
     edge.arrow.size=0.3,
     vertex.shape="none",
     vertex.label.cex = 0.5,
     vertex.label = ifelse(degree(g2) > 1, V(g2)$label, NA)
)
dev.off()

### 5. Community detection ----
topo(g2)

fgn = edge.betweenness.community (g2, directed = F, edge.betweenness = TRUE, merges = TRUE,
                                  bridges = TRUE, modularity = TRUE, membership = TRUE)  ## run Girvan-Newman partitioning
pdf("fgn.pdf")
plot(fgn, g2)
dev.off()

fwt <- walktrap.community(g2, steps=200,modularity=TRUE) # , labels=TRUE)  ## run random walk partitioning

pdf("fwt.pdf")
plot(fwt, g2)  ## plot R-W partitioning
dev.off()

## compare these methods to each other 
compare(fgn, fwt, method= c("nmi"))
compare(fgn, fwt, method= c("rand"))
compare(fgn, fwt, method= c("adjusted.rand"))

girvan = data.frame(fgn$membership)
rw = data.frame(fwt$membership)
traits <- row.names(cleaned.termMatrix)

fb <- cbind(traits, girvan, rw)
