
library(SpiecEasi)
library(igraph)


## Generate network, see https://github.com/zdk123/SpiecEasi for a nice walkthrough of this process with this example
#Load dara
data(amgut1.filt)
#Make up some Species IDs since the names aren't included in this matrix
colnames(amgut1.filt) <- make.names(rep(LETTERS[1:26],5),unique = T)[1:ncol(amgut1.filt)]
#Build network w/ spieceasi
se.gl.amgut <- spiec.easi(amgut1.filt, method='glasso', lambda.min.ratio=1e-2,
                          nlambda=20, pulsar.params=list(rep.num=50))

#Extract adjacency matrix from output  - Explain ADJ MAT vs. EDGE LIST
adj.mat <- getRefit(se.gl.amgut)
table(as.numeric(adj.mat))

#We want weights!
se.cor  <- cov2cor(as.matrix(getOptCov(se.gl.amgut)))
weighted.adj.mat <- se.cor*getRefit(se.gl.amgut)

#Let's take a loot at that adjacency matrix
heatmap(as.matrix(weighted.adj.mat))

grph.unweighted <- adj2igraph(adj.mat)
plot(grph.unweighted,vertex.size=1,vertex.label=NA)

grph <- adj2igraph(weighted.adj.mat)
plot(grph,vertex.size=1,
     vertex.label=NA)

plot(grph,vertex.size=1,
     vertex.label=NA,
     edge.width=1,
     layout=layout.circle(grph))

# Simplify

grph.simple <- simplify(grph)

# Node/edge weights and properties

V(grph)
E(grph)

V(grph)$name <- colnames(amgut1.filt)

V(grph)
E(grph)

# Color edges by weight
# Adding metadata

V(grph)$size <- (degree(grph) + 1)
V(grph)$color <- "black"
plot(grph,
     vertex.label=NA,
     layout=layout.circle(grph))

plot(density((E(grph)$weight)),xlab="Edge Weight",main="")

E(grph)$color <- "darkgreen"
E(grph)$color[E(grph)$weight<0] <- "darkred"
E(grph)$width <- abs(E(grph)$weight)*10
plot(grph,
     vertex.label=NA,
     layout=layout.circle(grph))

# Delete edges/vertices

boxplot(abs(E(grph)$weight)~(E(grph)$weight>0),
        xlab="Positive Interaction?",
        ylab="Strength of Interaction")

#Remove edges with very low weight 
weight_threshold <- 0.01
grph <- delete.edges(grph,which(abs(E(grph)$weight)<weight_threshold))

#Remove negative edges 
grph.pos <- delete.edges(grph,which(E(grph)$weight<0))
plot(grph.pos,
     vertex.label=NA)

#Remove unconnected vertices
grph.pos <- delete.vertices(grph.pos,which(degree(grph.pos)<1))
plot(grph.pos,
     vertex.label=NA)


#Cleanup a little
V(grph.pos)$size <- V(grph.pos)$size/3
E(grph.pos)$color <- "gray"
plot(grph.pos,
     vertex.label=NA,
     edge.curved=0.5)

# Layouts

#Fr and circle, sometimes kk

plot(grph.pos,
     vertex.label=NA,
     layout=layout.fruchterman.reingold(grph.pos))

plot(grph.pos,
     vertex.label=NA,
     layout=layout.kamada.kawai(grph.pos))

plot(grph.pos,
     vertex.label=NA,
     layout=layout.reingold.tilford(grph.pos))

plot(grph.pos,
     vertex.label=NA,
     layout=layout.lgl(grph.pos))


# Picking connected components

graph_components <- components(grph.pos)
graph_components

grph.largest.component <- 
  induced.subgraph(grph.pos,V(grph.pos)[which(graph_components$membership == which.max(graph_components$csize))])
plot(grph.largest.component)

#look at graph_components to see which is second largest
grph.second.largest.component <- 
  induced.subgraph(grph.pos,V(grph.pos)[which(graph_components$membership == 2)])
plot(grph.second.largest.component)

plot(grph.second.largest.component,edge.width=E(grph.second.largest.component)$width*5)


# Highlighting a node and it's connections

V(grph.pos)$color[V(grph.pos)$name=="L"] <- "red"
e.L <- E(grph.pos)[from("L")]
E(grph.pos)$color[E(grph.pos) %in% e.L] <- "red"
plot(grph.pos,vertex.label=NA)

# Highlighting a set of nodes and it's neighbors

Ls <- V(grph.pos)[substr(V(grph.pos)$name,1,1)=="L"]

V(grph.pos)$color[V(grph.pos) %in% Ls] <- "red"
e.Ls <- E(grph.pos)[from(Ls)]
E(grph.pos)$color[E(grph.pos) %in% e.Ls] <- "red"

ego.Ls <- ego(grph.pos, order=1, nodes = Ls$name, mode = "all", mindist = 0)

V(grph.pos)$color[V(grph.pos) %in% unlist(ego.Ls) & ! V(grph.pos) %in% Ls ] <- "pink"
plot(grph.pos,vertex.label=NA)

#Labels

plot(grph.pos,vertex.label.dist=0.75,vertex.label.cex=0.5,vertex.label.color="black")

# Finally, what if the letter for each species was some interesting metadata that we wanted to color differently? 

V(grph.pos)$color <- rainbow(26)[as.numeric(as.factor(substr(V(grph.pos)$name,1,1)))]
plot(grph.pos)
