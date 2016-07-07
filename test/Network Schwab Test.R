### Network Test ###

# Input
nodes <- read.csv("visone.csv", header = T, as.is = T, sep = ";")
edges <- read.csv("edgelist.csv", header = T, as.is = T, sep = ";")

# Exploring
head(nodes)
head(edges)

nrow(nodes) ; length(unique(nodes$id))
nrow(edges) ; nrow(unique(edges[,c("from", "to")]))

# Collapse links of the same type -> not functioning with this data
links <- aggregate(edges[,3], edges[,-3] sum)

# net-function takes two data frames! 
# d = edges,first column is ID of the source, second is the target node 
# vertices = start with column of node ID,
net <- graph_from_data_frame(d=edges, vertices=nodes, directed = T)
net

# look at data
View(net)
E(net)
V(net)

# Simple Network
plot(net, edge.arrow.size=.1000, vertex.label=NA)

# Simplifying
net <- simplify(net, remove.multiple = F, remove.loops = T)

# Plotting Network with Labels and Color
plot(net, edge.arrow.size=.1000, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$Sender, vertex.frame.color="black",
     vertex.label.cex=.50)

