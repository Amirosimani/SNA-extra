### 0. Libraries----
library(NLP)
library(tm)

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
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

#delete isolates
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}
g2 <- delete.isolates(g, mode = 'in')

#subsetting based on degree
a <- as.data.frame(degree(g2))
colnames(a) <- c("deg")

newdata <- a[ which(a$deg > 100),]





### 4.Plot a Graph----
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

plot(g2, layout=layout.kamada.kawai, vertex.label = ifelse(degree(g) > 50, V(g)$label, NA))

