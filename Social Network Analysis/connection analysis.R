#Project 2

wd <- "/home/zzq/Social Network/Project 2"
setwd(wd)
library(igraph)
library(reshape2)
require(data.table)

advice <- read.table('http://opsahl.co.uk/tnet/datasets/Cross_Parker-Manufacturing_info.txt')
aware <- read.table('http://opsahl.co.uk/tnet/datasets/Cross_Parker-Manufacturing_aware.txt')
colnames(advice) <- c('ego', 'alter', 'advice_tie')
colnames(aware) <- c('ego', 'alter', 'awareness_tie')

#used for building block modeling
a <- dcast(advice, ego+alter ~ advice_tie, drop = FALSE, value.var = "advice_tie",fill=0)
a$advice_tie <- rowSums(subset(a, select = c(3:8)), na.rm = TRUE)
a <- cbind(a[,1:2],a[,9])
b <- data.frame(1:77)      
b[,2]<-73
b[,3] <- 0
b <- b[,c(2,1,3)]
colnames(a) <- c('ego', 'alter', 'advice_tie')
colnames(b) <- c('ego', 'alter', 'advice_tie')
c <- rbind(a,b)
c <- c[order(c[,1], c[,2]), ]

d <- data.frame(matrix(, nrow=77, ncol=77))
brow <- NULL
erow <- NULL

for (i in 1:77 ) {
  brow <- 1+(i-1)*77
  erow <- 77*i
  d[,i] <- c[brow:erow,3]
}

rownames(d) <- 1:77
colnames(d) <- 1:77

#used for building block modeling
ap <- dcast(aware, ego+alter ~ awareness_tie, drop = FALSE,
           value.var = "awareness_tie",fill=0)
ap$awareness_tie <- rowSums(subset(ap, select = c(3:8)), na.rm = TRUE)
ap <- cbind(ap[,1:2],ap[,9])
colnames(ap) <- c('ego', 'alter', 'advice_tie')

dp <- data.frame(matrix(, nrow=77, ncol=77))
bprow <- NULL
eprow <- NULL

for (i in 1:77 ) {
  bprow <- 1+(i-1)*77
  eprow <- 77*i
  dp[,i] <- ap[bprow:eprow,3]
}

rownames(dp) <- 1:77
colnames(dp) <- 1:77

advice <- graph.data.frame(advice, directed=TRUE)
advice_adj <- as_adjacency_matrix(advice,type='upper', names=TRUE)
A <- as.matrix(advice_adj)

tmp <- A[77,]
tmp2 <- A[,77]
A[73,] <- A['73',]
A[,73] <- A[,'73']
A[77,] <- tmp
A[,77] <- tmp2
rownames(A) <- paste(1:77)
colnames(A) <- paste(1:77)
g1 <- melt(A)

aware <- graph.data.frame(aware, directed=TRUE)
aware_adj <- as_adjacency_matrix(aware)
aware_mx <- as.matrix(aware_adj)
g2 <- melt(aware_mx)

colnames(g1) <- c('ego', 'alter', 'advice_tie')
colnames(g2) <- c('ego', 'alter', 'awareness_tie')
which(g1$alter != g2$alter)#test if the alter in 2 dataframes are identical

#Combine 2 dataframes into one
Manu_full <- cbind(g1,g2)
Manu_full <- Manu_full[-c(4:5)]
head(Manu_full)

#Reduce to non-zero edges so that the edge list only contains actual ties
Manu_full_nonzero_edges <- subset(Manu_full, (advice_tie > 0 | awareness_tie > 0 ))
Manu_full <- graph.data.frame(Manu_full_nonzero_edges, directed=TRUE)

#Read files of attributes
loc <- read.table('Cross_Parker-Manufacturing-location.txt')
tenure <- read.table('Cross_Parker-Manufacturing-tenure.txt')
orglevel <- read.table('Cross_Parker-Manufacturing-orglevel.txt')

Attr <- data.frame(cbind(1:77,t(orglevel),t(loc),t(tenure)))
colnames(Attr) <- c("NodeID","level","location","tenure")

summary(Manu_full)
V(Manu_full)$loc <- Attr$location
V(Manu_full)$level <- Attr$level
V(Manu_full)$tenure <- Attr$tenure

plot(Manu_full, vertex.size=10, vertex.label=V(Manu_full)$loc, edge.arrow.size = 0.1, layout=layout.fruchterman.reingold)

#Create subgraphs
Manu_ad <- delete.edges(Manu_full, E(Manu_full)[get.edge.attribute(Manu_full,name = "advice_tie") == 0])
Manu_ad <- delete.vertices(Manu_ad, V(Manu_ad)[degree(Manu_ad)==0])
ad_layout <- layout.fruchterman.reingold(Manu_ad)
plot(Manu_ad, layout=ad_layout, edge.arrow.size=.5)

Manu_aw <- delete.edges(Manu_full, E(Manu_full)[get.edge.attribute(Manu_full,name = "awareness_tie") == 0])
Manu_aw <- delete.vertices(Manu_aw, V(Manu_aw)[degree(Manu_aw)==0])
aw_layout <- layout.fruchterman.reingold(Manu_aw)
plot(Manu_aw, layout=aw_layout, edge.arrow.size=.5)

#
indegree_ad <- degree(Manu_ad, mode='in')
outdegree_ad<- degree(Manu_ad, mode='out')
incloseness_ad <- closeness(Manu_ad, mode='in')
outcloseness_ad<- closeness(Manu_ad, mode='out')
betweenness_ad<- betweenness(Manu_ad)

ev_obj_ad <- evcent(Manu_ad)
eigen_ad<- ev_obj_ad$vector
eigen_ad

central_Manu_ad <- data.frame(V(Manu_ad)$name, indegree_ad, outdegree_ad, incloseness_ad, outcloseness_ad, betweenness_ad, eigen_ad)
central_Manu_ad

barplot(central_Manu_ad$indegree_ad, names.arg=central_Manu_ad$V.Manu_ad..name)
barplot(central_Manu_ad$outdegree_ad, names.arg=central_Manu_ad$V.Manu_ad..name)
barplot(central_Manu_ad$incloseness_ad, names.arg=central_Manu_ad$V.Manu_ad..name)
barplot(central_Manu_ad$outdegree_ad, names.arg=central_Manu_ad$V.Manu_ad..name)
barplot(central_Manu_ad$betweenness_ad, names.arg=central_Manu_ad$V.Manu_ad..name)
barplot(central_Manu_ad$eigen_ad, names.arg=central_Manu_ad$V.Manu_ad..name)

indegree_aw <- degree(Manu_aw, mode='in')
outdegree_aw<- degree(Manu_aw, mode='out')
incloseness_aw <- closeness(Manu_aw, mode='in')
outcloseness_aw<- closeness(Manu_aw, mode='out')
betweenness_aw<- betweenness(Manu_aw)

ev_obj_aw <- evcent(Manu_aw)
eigen_aw<- ev_obj_aw$vector
eigen_aw

central_Manu_aw <- data.frame(V(Manu_aw)$name, indegree_aw, outdegree_aw, incloseness_aw, outcloseness_aw, betweenness_aw, eigen_aw)
central_Manu_aw

barplot(central_Manu_aw$indegree_aw, names.arg=central_Manu_aw$V.Manu_aw..name)
barplot(central_Manu_aw$outdegree_aw, names.arg=central_Manu_aw$V.Manu_aw..name)
barplot(central_Manu_aw$incloseness_aw, names.arg=central_Manu_aw$V.Manu_aw..name)
barplot(central_Manu_aw$outdegree_aw, names.arg=central_Manu_aw$V.Manu_aw..name)
barplot(central_Manu_aw$betweenness_aw, names.arg=central_Manu_aw$V.Manu_aw..name)
barplot(central_Manu_aw$eigen_aw, names.arg=central_Manu_aw$V.Manu_aw..name)

#Community detection: using walktrap
Manu_full_wt <- walktrap.community(Manu_full, steps=200,modularity=TRUE)
Manu_full_dend <- as.dendrogram(Manu_full_wt, use.modularity=TRUE)
plot(Manu_full_dend)

#Block Modeling
library(sna)
library(igraph)
Manu_full_adj <- as_adjacency_matrix(Manu_full,type='lower')
Manu_full_net <- network(Manu_full_adj,directed=TRUE)

eq <- equiv.clust(Manu_full_net, mode='digraph')
b <- blockmodel(Manu_full_net, eq, k=4)
b$order.vector
b$block.membership
plot(b)

#Obtain image matrix and plot
den <- network.density(Manu_full_net)
bimage <- b$block.model
bimage[is.nan(bimage)] <- 1
bimage

gplot(bimage, diag=TRUE, 
      edge.lwd=bimage*5, 
      label=colnames(bimage),
      vertex.cex=sqrt(table(b$block.membership))*2,
      gmode="digraph", vertex.sides=50,
      vertex.col=gray(1-diag(bimage)/2))

#K-cores
coreness <- graph.coreness(Manu_full) 
maxCoreness <- max(coreness)
verticesHavingMaxCoreness <- which(coreness == maxCoreness) 
kcore <- induced.subgraph(graph=Manu_full,vids=verticesHavingMaxCoreness)

plot(kcore, 
     vertex.label=get.vertex.attribute(kcore,name='vert.names',index=V(kcore)))

coreness <- graph.coreness(Manu_full)
make_k_core_plot <- function (g) {
  lay1 <- layout.fruchterman.reingold(g)
  plot(g, 
       vertex.color = graph.coreness(g), 
       layout=lay1, 
       edge.arrow.size = .1)
} 
make_k_core_plot(Manu_full)

#QAP  
#predictor matrices
aware_ma <- as.matrix(dp)
advice_ma <- as.matrix(d)

dp[dp<3] <- 0
dp[dp>=3] <- 1
d[d<3] <- 0
d[d>=3] <- 1
aware_mt <- as.matrix(dp)
#Load response matrices
advice_mt <- as.matrix(d)

V(advice)$loc <- Attr$location
V(advice)$level <- Attr$level
V(advice)$tenure <- Attr$tenure

size <- length(V(advice)$level)
level <- V(advice)$level
levelmat <- matrix(, nrow = size, ncol = size)

for (i in 1:size) {
  for (j in 1:size) {
    if (i<j) {
      levelmat[i,j] <- level[j]-level[i]
    }
    else if  (i>j) {
      levelmat[i,j] <- level[j]-level[i]
    } 
    else
      levelmat[i,j] <- 0
  }
}


tenure <- V(advice)$tenure
tenuremat <- matrix(, nrow = size, ncol = size)

for (i in 1:size) {
  for (j in 1:size) {
    if (i<j) {
      tenuremat[i,j] <- tenure[j]-tenure[i]
    }
    else if  (i>j) {
      tenuremat[i,j] <- tenure[j]-tenure[i]
    } 
    else
      tenuremat[i,j] <- 0
  }
}


location <- V(advice)$loc
locmat <- matrix(, nrow = size, ncol = size)

for (i in 1:size) {
  for (j in 1:size) {
    if (i<j) {
      locmat[i,j] <- location[j]-location[i]
    }
    else if  (i>j) {
      locmat[i,j] <- location[j]-location[i]
    } 
    else
      locmat[i,j] <- 0
  }
}

g <- array(NA,c(4,77,77))
g[1,,] <- aware_ma
g[2,,] <- levelmat
g[3,,] <- tenuremat
g[4,,] <- locmat

q.12<-qaptest(g,gcor,g1=1,g2=2)
summary(q.12)
plot(q.12)

#Regression between advice and awareness networks
nl<-netlm(advice_ma,aware_ma)
nlLabeled <- list()
nlLabeled <- summary(nl)
nlLabeled$names <- c("Intercept", "Knowledge of each other's skill")
nlLabeled

nl1<-netlm(advice_mt,g)
nl1Labeled <- list()
nl1Labeled <- summary(nl1)
nl1Labeled$names <- c("Intercept", "Knowledge of each other's skill","Organizational level", "Tenure", "Location")
nl1Labeled

##ERGM
library(ergm)
library(intergraph)
Ad <- set_vertex_attr(advice, "level", index = V(advice), Attr[,2])
Ad <- set_vertex_attr(Ad, "location", index = V(advice), Attr[,3])
Ad <- set_vertex_attr(Ad, "tenure", index = V(advice), Attr[,4])
Advice_net <- asNetwork(Ad)

Aw <- set_vertex_attr(aware, "level", index = V(aware), Attr[,2])
Aw <- set_vertex_attr(Aw, "location", index = V(aware), Attr[,3])
Aw <- set_vertex_attr(Aw, "tenure", index = V(aware), Attr[,4])
Aware_net <- asNetwork(Aw)

#A simple model that includes just the edge (density) parameter.
ad.mod.1 <- ergm(Advice_net ~ edges + mutual)
summary(ad.mod.1)#AIC: 6406    BIC: 6420

#check whether edges in the advice business network are predicted by
#edges in the awareness network.
ad.mod.2 <- ergm(Advice_net ~ edges + edgecov(Aware_net)+nodematch('location'))
ad.mod.2
summary(ad.mod.2)#AIC: 2855    BIC: 2868

ad.mod.2.gof <- gof(ad.mod.2 ~ idegree)
ad.mod.2.gof
plot(ad.mod.2.gof)

ad.mod.21.gof <- gof(ad.mod.21 ~ idegree)
ad.mod.21.gof
plot(ad.mod.21.gof)

ad.mod.22.gof <- gof(ad.mod.22 ~ idegree)
ad.mod.22.gof
plot(ad.mod.22.gof)

ad.mod.21 <- ergm(Advice_net ~ edges + edgecov(Aware_net)+nodecov('tenure')+nodematch('location'))
summary(ad.mod.21)#AIC: 2573    BIC: 2600 

ad.mod.22 <- ergm(Advice_net ~ edges + edgecov(Aware_net)+nodecov('tenure')+nodematch('location')+nodematch('level'))
summary(ad.mod.22)#AIC: 2572    BIC: 2605 

#Let's test whether the edge probabilities are a function of tenure
ad.mod.3 <- ergm(Advice_net ~ edges +  nodecov("tenure"))       
summary(ad.mod.3)#AIC: 7780    BIC: 7794

#Are nodes with the same attribute levels more likely to be connected?
# Do people tend to ask advice from people of the same location?
ad.mod.4 <- ergm(Advice_net ~ edges +  nodematch("location"))       
summary(ad.mod.4)#AIC: 4885    BIC: 4899

#Get a separate parameter for each level of the categorical variable. 
#Here, a separate parameter for each location:
ad.mod.5 <- ergm(Advice_net ~ edges +  nodematch("location", diff=T))       
summary(ad.mod.5)#AIC: 4841    BIC: 4874 

#what about level?
ad.mod.6 <- ergm(Advice_net ~ edges +  nodematch("level"))       
summary(ad.mod.6)#AIC: 7747    BIC: 7760 

ad.mod.7 <- ergm(Advice_net ~ edges +  nodematch("level", diff=T))       
summary(ad.mod.7)#AIC: 7752    BIC: 7785 

ad.mod.8 <- ergm(Advice_net ~ edges + nodematch("level") + nodematch("location"))
summary(ad.mod.8)#AIC: 4827    BIC: 4847

#Nodemix
#Nodemix will add a parameter for each combination of levels for the categorical variable.
# Let's look at the parameters for edges between people from different locations:
ad.mod.9  <- ergm(Advice_net ~ edges +nodemix("location"))
summary(ad.mod.9)#AIC: 4834    BIC: 4948 

#Let's look at the parameters for edges between people from different organizations:
ad.mod.10  <- ergm(Advice_net ~ edges +nodemix("level"))
summary(ad.mod.10)#AIC: 7590    BIC: 7704 

table(Advice_net %v% "location")  			# Check out location frequencies
mixingmatrix(Advice_net, "location")

table(Advice_net %v% "level")  			# Check out level frequencies
mixingmatrix(Advice_net, "level")

#Main effect of categorical attribute level
ad.mod.11 <- ergm(Advice_net ~ edges + nodematch("location", diff = T) + nodefactor("level"))
summary(ad.mod.11)#AIC: 4546    BIC: 4599

#Main effect of categorical attribute location
ad.mod.12 <- ergm(Advice_net ~ edges + nodematch("level", diff = T) + nodefactor("location"))
summary(ad.mod.12)#AIC: 7559    BIC: 7613

#Main effect of continuous attribute tenure
ad.mod.13 <- ergm(Advice_net ~ edges + nodecov("tenure") + nodematch("location"))
summary(ad.mod.13)#AIC: 4861    BIC: 4881 

# Absiff
#Are people more likely to be connected to others who have similar values 
#of tenure?
ad.mod.13 <- ergm(Advice_net ~ edges + absdiff("tenure") + nodematch("location"))
summary(ad.mod.13)#AIC: 4850    BIC: 4870 


# Simulating networks based on a model
# Simulate 15 networks based on the ad.mod.10 model
ad.mod.2.sim <- simulate(ad.mod.2, nsim=15)
summary(ad.mod.2.sim)

class(ad.mod.2.sim)
ad.mod.2.sim[[1]]

# Goodnes of Fit and MCMC diagnostics
# goodness of fit for degree distribution
ad.mod.2.gof <- gof(ad.mod.2 ~ idegree)
ad.mod.2.gof
plot(ad.mod.2.gof)

#goodness of fit with regard to geodesic distance
ad.mod.2.gof.2 <- gof(ad.mod.2 ~ distance, nsim =20)
ad.mod.2.gof.2 
plot(ad.mod.2.gof.2 )

ad.mod.2.gof <- gof(ad.mod.2 ~ distance,nsim=20)
ad.mod.2.gof
plot(ad.mod.2.gof)

ad.mod.21.gof <- gof(ad.mod.21 ~ distance,nsim=20)
ad.mod.21.gof
plot(ad.mod.21.gof)

ad.mod.22.gof <- gof(ad.mod.22 ~ distance,nsim=20)
ad.mod.22.gof
plot(ad.mod.22.gof)
#===================================================================================
#Visualization 
library(networkD3)

simpleNetwork(data.frame(advice[,1],advice[,2]))

detach(package:networkD3)

advice <- advice-1
Attr$NodeID <- Attr$NodeID-1
forceNetwork(Links = advice, Nodes = Attr,
             Source = "ego", Target = "alter",
             NodeID = "NodeID", Group = "level",
             opacity = 0.4)


wc <- cluster_walktrap(Manu_aw)
members <- membership(wc)

# Convert to object suitable for networkD3
aw_d3 <- igraph_to_networkD3(Manu_aw, group = members)

# Create force directed network plot
forceNetwork(Links = aw_d3$links, Nodes = aw_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')


