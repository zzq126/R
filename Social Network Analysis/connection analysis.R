
##Project 1

wd = "/home/zzq/Social Network/Project 1"
setwd(wd)
library(igraph) 
library(reshape2)

##Florida
#Reading the csv file
florida.data <- read.csv("Roundtables_SNA_2015_100716_FL How do you know.csv",header=TRUE)

#Converting the NA values into zero
florida.data[is.na(florida.data)] <- 0

#Convert the first column into rownames
rownames(florida.data) <- florida.data[,1]

#Drop the first column
florida.data <- florida.data[-c(1)]

#Convert anything greater than 1 into 1
florida.data[florida.data > 1] <- 1

###Reading the common
attributes.data <- read.csv("Roundtables_SNA_2015_101116_FL Attributes.csv")

rownames(attributes.data )<- attributes.data[,1]

###Reading the info file

florida.info.R <- read.csv("Roundtables_SNA_2015_100716_FL Receive info.csv",header=T)
florida.info.S <- read.csv("Roundtables_SNA_2015_100716_FL Send info.csv",header=T)


rownames(florida.info.R) <- florida.info.R[,1]
florida.info.R <- florida.info.R[,-1]
florida.info.R[florida.info.R > 1] <- 1
florida.info.R[is.na(florida.info.R)] <- 0

rownames(florida.info.S) <- florida.info.S[,1]
florida.info.S <- florida.info.S[,-1]
florida.info.S[florida.info.S > 1] <- 1
florida.info.S[is.na(florida.info.S)] <- 0

#Creating the i-graph object and then plotting it
adj <- as.matrix(florida.data)

g1=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g1,margin=-0.4,edge.arrow.size=0.1)
##Adding the attribute of C Researcher 
vertex_attr(g1)

attributes.data <- read.csv("Roundtables_SNA_2016_081916_FL Attributes.csv")

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g1)$TopicC <- att.data.c

vertex.attributes(g1)

## Color coding the graph
V(g1)$color=V(g1)$TopicC
V(g1)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g1)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g1)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g1,margin=-0.3,edge.arrow.size=0.1)

##2016 data

#Reading the csv file
florida.data.16 <- read.csv("Roundtables_SNA_2016_081916_FL how do you know.csv",header=TRUE)

#Converting the NA values into zero
florida.data.16[is.na(florida.data.16)] <- 0

#Convert the first column into rownames
rownames(florida.data.16) <- florida.data.16[,1]

#Drop the first column
florida.data.16 <- florida.data.16[-c(1)]

#Convert anything greater than 1 into 1
florida.data.16[florida.data.16 > 1] <- 1

###Reading the common
attributes.data.16 <- read.csv("Roundtables_SNA_2016_081916_FL Attributes.csv")

rownames(attributes.data.16 )<- attributes.data.16[,1]
###Reading the info file

florida.info.R.16 <- read.csv("Roundtables_SNA_2016_081916_FL Receive info.csv",header=T)
florida.info.S.16 <- read.csv("Roundtables_SNA_2016_081916_FL Send info.csv",header=T)


rownames(florida.info.R.16) <- florida.info.R.16[,1]
florida.info.R.16 <- florida.info.R.16[,-1]
florida.info.R.16[florida.info.R.16 > 1] <- 1
florida.info.R.16[is.na(florida.info.R.16)] <- 0

rownames(florida.info.S.16) <- florida.info.S.16[,1]
florida.info.S.16 <- florida.info.S.16[,-1]
florida.info.S.16[florida.info.S.16 > 1] <- 1
florida.info.S.16[is.na(florida.info.S.16)] <- 0

#Creating the i-graph object and then plotting it
adj <- as.matrix(florida.data.16)

g3=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g3,margin=-0.4,edge.arrow.size=0.1)

#Creating the i-graph object and then plotting it
adj <- as.matrix(florida.data.16)

g3=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g1,margin=-0.4,edge.arrow.size=0.1)


##Adding the attribute of C Researcher 
vertex_attr(g3)

attributes.data <- read.csv("Roundtables_SNA_2016_081916_FL Attributess.csv")

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g3)$TopicC <- att.data.c

vertex.attributes(g3)

## Color coding the graph
V(g3)$color=V(g3)$TopicC
V(g3)$color=gsub(1,"red",V(g3)$color) #Cancer will be red
V(g3)$color=gsub(0,"blue",V(g3)$color) #Dont will be blue
V(g3)$color=gsub(2,"Yellow",V(g3)$color) #CD will be Yellow
plot.igraph(g3,margin=-0.3,edge.arrow.size=0.1)

library(sna)
g4 <- as.matrix(get.adjacency(g3))
eq <- equiv.clust(g4)
plot(eq)

b <- blockmodel(g4,eq,k=3,mode="graphs")
plot(b)

detach(package:sna,unload=TRUE)
library(ergm)
library(intergraph)

florida_net <- asNetwork(g1)
ad.mod.1 <- ergm(florida_net ~ edges +nodemix("TopicC"))
summary(ad.mod.1)

florida16_net <- asNetwork(g3)
ad.mod.2 <- ergm(florida_net ~ edges +nodemix("TopicC"))
summary(ad.mod.2)

######################################################################################
##Kentucky

##2015
#Reading the csv file
kentucky.data <- read.csv("Roundtables_SNA_2015_100716_KY how do you know.csv",header=TRUE)

#Converting the NA values into zero
kentucky.data[is.na(kentucky.data)] <- 0

#Convert the first column into rownames
rownames(kentucky.data) <- kentucky.data$Person.ID

#Drop the first column
kentucky.data <- kentucky.data[-c(1)]

#Convert anything greater than 1 into 1
kentucky.data[kentucky.data > 1] <- 1

#Creating the i-graph object and then plotting it
adj <- as.matrix(kentucky.data)

g1=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g1,margin=-0.4,edge.arrow.size=0.1)

edge_density(g1)

detach("package:sna", unload=TRUE)
##Adding the attribute of C Researcher 
vertex_attr(g1)

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g1)$TopicC <- att.data.c

vertex.attributes(g1)

## Color coding the graph
V(g1)$color=V(g1)$TopicC
V(g1)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g1)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g1)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g1,margin=-0.3,edge.arrow.size=0.1)
title(main="South Dakota - 2015")
edge_density(g1)

##Creating the block model
library(sna)
g2 <- as.matrix(get.adjacency(g1))
eq <- equiv.clust(list(g2,kentucky.info.R,kentucky.info.S))
plot(eq)

b <- blockmodel(g2,eq,k=3,mode="graphs")
plot(b)

bimage <- b$block.model

bimage[is.na(bimage)] <- 1
bimage[bimage > 0.3571429] <- 1
bimage[bimage < 0.3571429] <- 0
bimage

gplot(bimage, diag=TRUE, 
      edge.lwd=bimage*5, 
      label=colnames(bimage),
      vertex.cex=sqrt(table(b$block.membership))*2,
      gmode="digraph", vertex.sides=50,
      vertex.col=gray(1-diag(bimage)/2))

#######################################################################



library(igraph)

kentucky.data.16 <- read.csv()
#Creating the i-graph object and then plotting it

adj <- as.matrix(kentucky.data.16)

g3=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g3,margin=-0.4,edge.arrow.size=0.1)


##Adding the attribute of C Researcher 
vertex_attr(g3)

attributes.data <- read.csv("Roundtables_SNA_2016_081916_KY Attributes.csv")

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g3)$TopicC <- att.data.c

vertex.attributes(g3)

## Color coding the graph
V(g3)$color=V(g1)$TopicC
V(g3)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g3)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g3)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g3,margin=-0.3,edge.arrow.size=0.1)
title(main="South Dakota 2016")
edge_density(g3)

##Creating the block model
library(sna)
g4 <- as.matrix(get.adjacency(g3))
eq <- equiv.clust(list(g4,kentucky.info.R.16,kentuckyinfo.S.16))
plot(eq)

b <- blockmodel(g4,eq,k=3,mode="graphs")
plot(b)

##Creating the ERGM model
kentucky_net<- asNetwork(g1)
ad.mod.3 <- ergm(kentucky_net ~ edges + nodemix("TopicC"))
summary(ad.mod.3)

kentucky16_net<- asNetwork(g3)
ad.mod.4 <- ergm(kentucky16_net ~ edges + nodemix("TopicC"))
summary(ad.mod.4)

#######################################################################
##Vermont 2015
vermont.data <- read.csv("Roundtables_SNA_2015_100716_VT How do you know.csv",header=TRUE)

#Converting the NA values into zero
vermont.data[is.na(vermont.data)] <- 0

#Convert the first column into rownames
rownames(vermont.data) <- vermont.data[,1]

#Drop the first column
vermont.data <- vermont.data[-c(1)]

#Convert anything greater than 1 into 1
vermont.data[vermont.data > 1] <- 1

###Reading the common
attributes.data <- read.csv("Roundtables_SNA_2015_101116_VT Attributes.csv")

rownames(attributes.data )<- attributes.data[,1]

###Reading the info file

vermont.info.R <- read.csv("Roundtables_SNA_2015_100716_VT Receive info.csv",header=T)
vermont.info.S <- read.csv("Roundtables_SNA_2015_100716_VT Send info.csv",header=T)


rownames(vermont.info.R) <- vermont.info.R[,1]
vermont.info.R <- vermont.info.R[,-1]
vermont.info.R[vermont.info.R > 1] <- 1
vermont.info.R[is.na(vermont.info.R)] <- 0

rownames(vermont.info.S) <- vermont.info.S[,1]
vermont.info.S <- vermont.info.S[,-1]
vermont.info.S[vermont.info.S > 1] <- 1
vermont.info.S[is.na(vermont.info.S)] <- 0

#Creating the i-graph object and then plotting it
adj <- as.matrix(vermont.data)

g1=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g1,margin=-0.4,edge.arrow.size=0.1)

edge_density(g1)

detach("package:sna", unload=TRUE)
##Adding the attribute of C Researcher 
vertex_attr(g1)

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g1)$TopicC <- att.data.c

vertex.attributes(g1)

## Color coding the graph
V(g1)$color=V(g1)$TopicC
V(g1)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g1)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g1)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g1,margin=-0.3,edge.arrow.size=0.1)
title(main="South Dakota - 2015")
edge_density(g1)

##Creating the block model
library(sna)
g2 <- as.matrix(get.adjacency(g1))
eq <- equiv.clust(list(g2,vermont.info.R,vermont.info.S))
plot(eq)

b <- blockmodel(g2,eq,k=3,mode="graphs")
plot(b)

bimage <- b$block.model

bimage[is.na(bimage)] <- 1
bimage[bimage > 0.3571429] <- 1
bimage[bimage < 0.3571429] <- 0
bimage

gplot(bimage, diag=TRUE, 
      edge.lwd=bimage*5, 
      label=colnames(bimage),
      vertex.cex=sqrt(table(b$block.membership))*2,
      gmode="digraph", vertex.sides=50,
      vertex.col=gray(1-diag(bimage)/2))

#######################################################################

##Vermont 2016
#Reading the csv file
vermont.data.16 <- read.csv("Roundtables_SNA_2016_081916_VT How do you know.csv",header=TRUE)

#Converting the NA values into zero
vermont.data.16[is.na(vermont.data)] <- 0

#Convert the first column into rownames
rownames(vermont.data.16) <- vermont.data.16[,1]

#Drop the first column
vermont.data.16 <- vermont.data.16[-c(1)]

#Convert anything greater than 1 into 1
vermont.data.16[vermont.data.16 > 1] <- 1

###Reading the common
attributes.data <- read.csv("Roundtables_SNA_2015_081916_VT Attributes.csv")

rownames(attributes.data )<- attributes.data[,1]

###Reading the info file

vermont.info.R.16 <- read.csv("Roundtables_SNA_2016_081916_VT Receive info.csv",header=T)
vermont.info.S.16 <- read.csv("Roundtables_SNA_2016_081916_VT Send info.csv",header=T)


rownames(vermont.info.R.16 ) <- vermont.info.R.16 [,1]
vermont.info.R.16  <- vermont.info.R.16 [,-1]
vermont.info.R.16 [vermont.info.R.16  > 1] <- 1
vermont.info.R.16 [is.na(vermont.info.R.16 )] <- 0

rownames(vermont.info.S.16 ) <- vermont.info.S.16[,1]
vermont.info.S.16<- vermont.info.S.16[,-1]
vermont.info.S.16[fvermont.info.S.16 > 1] <- 1
vermont.info.S.16[is.na(vermont.info.S.16)] <- 0

library(igraph)

#Creating the i-graph object and then plotting it
adj <- as.matrix(Vermont.data.16)

g3=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g3,margin=-0.4,edge.arrow.size=0.1)


##Adding the attribute of C Researcher 
vertex_attr(g3)

attributes.data <- read.csv("Roundtables_SNA_2016_081916_VT Attributes.csv")

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g3)$TopicC <- att.data.c

vertex.attributes(g3)

## Color coding the graph
V(g3)$color=V(g1)$TopicC
V(g3)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g3)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g3)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g3,margin=-0.3,edge.arrow.size=0.1)
title(main="South Dakota 2016")
edge_density(g3)

##Creating the block model
library(sna)
g4 <- as.matrix(get.adjacency(g3))
eq <- equiv.clust(list(g4,vermont.info.R.16,vermont.info.S.16))
plot(eq)

b <- blockmodel(g4,eq,k=3,mode="graphs")
plot(b)

##Creating the ERGM model
vermont_net <- asNetwork(g1)
ad.mod.5 <- ergm(vermont_net ~ edges +nodemix("TopicC"))
summary(ad.mod.5)

vermont_net.16 <- asNetwork(g3)
ad.mod.6 <- ergm(vermont_net.16 ~ edges +nodemix("TopicC"))
summary(ad.mod.6)

#######################################################################
##South Dakota 2015
#Reading the csv file
sd.data <- read.csv("Roundtables_SNA_2015_100716_SD How do you know.csv",header=TRUE)

#Converting the NA values into zero
sd.data[is.na(sd.data)] <- 0

#Convert the first column into rownames
rownames(sd.data) <- sd.data[,1]

#Drop the first column
sd.data <- sd.data[-c(1)]

#Convert anything greater than 1 into 1
sd.data[sd.data > 1] <- 1

###Reading the common
attributes.data <- read.csv("Roundtables_SNA_2015_101116_SD Attributes.csv")

rownames(attributes.data )<- attributes.data[,1]

###Reading the info file
sd.info.R <- read.csv("Roundtables_SNA_2015_100716_SD Receive info.csv",header=T)
sd.info.S <- read.csv("Roundtables_SNA_2015_100716_SD Send info.csv",header=T)


rownames(sd.info.R) <- sd.info.R[,1]
sd.info.R <- sd.info.R[,-1]
sd.info.R[sd.info.R > 1] <- 1
sd.info.R[is.na(sd.info.R)] <- 0

rownames(sd.info.S) <- sd.info.S[,1]
sd.info.S <- sd.info.S[,-1]
sd.info.S[sd.info.S > 1] <- 1
sd.info.S[is.na(sd.info.S)] <- 0


##2016 data

#Reading the csv file
sd.data.16 <- read.csv("Roundtables_SNA_2016_081916_SD how do you know.csv",header=TRUE)

#Converting the NA values into zero
sd.data.16[is.na(sd.data.16)] <- 0

#Convert the first column into rownames
rownames(sd.data.16) <- sd.data.16[,1]

#Drop the first column
sd.data.16 <- sd.data.16[-c(1)]

#Convert anything greater than 1 into 1
sd.data.16[sd.data.16 > 1] <- 1

###Reading the common
attributes.data.16 <- read.csv("Roundtables_SNA_2016_081916_SD Attributes.csv")

rownames(attributes.data.16 )<- attributes.data.16[,1]
###Reading the info file

sd.info.R.16 <- read.csv("Roundtables_SNA_2016_081916_SD Receive info.csv",header=T)
sd.info.S.16 <- read.csv("Roundtables_SNA_2016_081916_SD Send info.csv",header=T)


rownames(sd.info.R.16) <- sd.info.R.16[,1]
sd.info.R.16 <- sdinfo.R.16[,-1]
sd.info.R.16[sd.info.R.16 > 1] <- 1
sd.info.R.16[is.na(sd.info.R.16)] <- 0

rownames(sd.info.S.16) <- sd.info.S.16[,1]
sd.info.S.16 <- sd.info.S.16[,-1]
sd.info.S.16[sd.info.S.16 > 1] <- 1
sd.info.S.16[is.na(sd.info.S.16)] <- 0

#Creating the i-graph object and then plotting it
adj <- as.matrix(sd.data)

g1=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g1,margin=-0.4,edge.arrow.size=0.1)

edge_density(g1)

detach("package:sna", unload=TRUE)
##Adding the attribute of C Researcher 
vertex_attr(g1)

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g1)$TopicC <- att.data.c

vertex.attributes(g1)

## Color coding the graph
V(g1)$color=V(g1)$TopicC
V(g1)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g1)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g1)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g1,margin=-0.3,edge.arrow.size=0.1)
title(main="South Dakota - 2015")
edge_density(g1)

##Creating the block model
library(sna)
g2 <- as.matrix(get.adjacency(g1))
eq <- equiv.clust(list(g2,sd.info.R,sd.info.S))
plot(eq)

b <- blockmodel(g2,eq,k=3,mode="graphs")
plot(b)

bimage <- b$block.model

bimage[is.na(bimage)] <- 1
bimage[bimage > 0.3571429] <- 1
bimage[bimage < 0.3571429] <- 0
bimage

gplot(bimage, diag=TRUE, 
      edge.lwd=bimage*5, 
      label=colnames(bimage),
      vertex.cex=sqrt(table(b$block.membership))*2,
      gmode="digraph", vertex.sides=50,
      vertex.col=gray(1-diag(bimage)/2))

#######################################################################
#######################################################################
#######################################################################
#######################################################################


library(igraph)

#Creating the i-graph object and then plotting it
adj <- as.matrix(sd.data.16)

g3=graph.adjacency(adj,mode="directed",weighted=NULL,diag=FALSE)

plot(g3,margin=-0.4,edge.arrow.size=0.1)


##Adding the attribute of C Researcher 
vertex_attr(g3)

attributes.data <- read.csv("Roundtables_SNA_2016_081916_SD Attributes.csv")

rownames(attributes.data) <- attributes.data$PersonID

attributes.data <- attributes.data[,-1]

attributes.data[is.na(attributes.data)] <-0

attributes.data[attributes.data > 2] <- 0

att.data.c <- attributes.data$CancerCD

V(g3)$TopicC <- att.data.c

vertex.attributes(g3)

## Color coding the graph
V(g3)$color=V(g1)$TopicC
V(g3)$color=gsub(1,"red",V(g1)$color) #Cancer will be red
V(g3)$color=gsub(0,"blue",V(g1)$color) #Dont will be blue
V(g3)$color=gsub(2,"Yellow",V(g1)$color) #CD will be Yellow
plot.igraph(g3,margin=-0.3,edge.arrow.size=0.1)
title(main="South Dakota 2016")
edge_density(g3)

##Creating the block model
library(sna)
g4 <- as.matrix(get.adjacency(g3))
eq <- equiv.clust(list(g4,sd.info.R.16,sd.info.S.16))
plot(eq)

b <- blockmodel(g4,eq,k=3,mode="graphs")
plot(b)

sd_net <- asNetwork(g1)
ad.mod.7 <- ergm(sd_net ~ edges + nodemix("TopicC"))
summary(ad.mod.7)

sd_net.16 <- asNetwork(g3)
ad.mod.8 <- ergm(sd_net.16 ~ edges + nodemix("TopicC"))
summary(ad.mod.8) 