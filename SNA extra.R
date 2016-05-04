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

#subsetting based on degree - by providing matrix and required degree
filter.on.degree <- function(matrixY, x) {
  #finding nodes name based on the required degree
  a <- as.data.frame(degree(g2))
  colnames(a) <- c("deg")
  a2 <- subset(a, deg > x)
  a2 <- cbind(Row.Names = rownames(a2), a2)
  
  newdata <- as.data.frame(matrixY)
  newdata <- cbind(Row.Names = rownames(newdata), newdata)
  newdata <- newdata[newdata$Row.Names %in% a2$Row.Names, ]
  newdata <- newdata2[ , which(names(newdata) %in% a2$Row.Names)]
  
  cleaned_matrix <- as.matrix(newdata)
}

cleaned.termMatrix <- filter.on.degree(termMatrix, 100)

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(cleaned.termMatrix, weighted=T, mode = "undirected")
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

### 4.Plot a Graph----
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)

l <- layout.fruchterman.reingold(g, repulserad=vcount(g)^3, 
                                 area=vcount(g)^2.4)
V(g)$size=degree(g)/5

pdf("sna_words.pdf")
plot(g, layout=layout.lgl,
     edge.arrow.size=.3,
     vertex.shape="none",
     vertex.label = ifelse(degree(g) > 1, V(g)$label, NA)
)
dev.off()


