library(parallel)
library(doParallel)
library(igraph)
library(rlist) 
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)


library(ggridges)
library(ggraph)
library(stringr)
library(wesanderson)

?count

#GLOBAL PARAMETERS

#this parameter controls the number of initial nodes, as well as the number of nodes each new node
#is connected to
m <- 2

#parameter to control number of time steps model runs for
cycles <- 100
#controls how much each node influences its neighbors
connection_weight <- 0.5

#global threshold


#copying over the draw net function for visualizing networks and modifying it slightly
draw.net <- function(net) {
  
  net.ig <- igraph::as.directed(igraph::graph_from_adj_list(net))
  
  deg <- degree(net.ig, mode="out")
  V(net.ig)$size <- deg*5
  
  graphics::plot(net.ig, edge.arrow.size=.5, edge.curved=.2, arrow.mode="forward")
  
}

#modifying the draw net function to properly display large networks
draw.net.large.network<- function(net, influencer) {
  
  net.ig <- igraph::as.directed(igraph::graph_from_adj_list(net))
  
  dist.from.influencer<- distances(net.ig,v=V(net.ig)[influencer], to=V(net.ig))
  oranges <- colorRampPalette(c("dark red", "gold"))
  col <- oranges(max(dist.from.influencer)+1)
  col <- col[dist.from.influencer+1]
  
  
  deg <- degree(net.ig, mode="out")
  V(net.ig)$size <- (deg*1)+5
  l<- layout_with_lgl(net.ig, root=influencer) %>%
    layout.norm(l, ymin=-0.8, ymax=0.8, xmin=-2, xmax=2)
  V(net.ig)$label<- 1:influencer
  graphics::plot(net.ig, vertex.color=col, vertex.label.color="white",
                 edge.arrow.size=.2, edge.curved=.2, arrow.mode="forward", rescale=F,layout=l*1,
                 vertex.label=ifelse(degree(net.ig, mode="out")>(metric.degree.median(net.ig)*5), V(net.ig)$label, NA))
  
  legend("bottom", legend=levels(as.factor(dist.from.influencer)), fill=oranges(max(dist.from.influencer)+1), title="Distance from Influencer",
         bty="n", title.col="black", xjust=0, horiz=T)
}



#copying over the function that finds the median degree
metric.degree.median <- function(g) {
  
  if (!is.list(g)) stop("Parameter 'g' must be a list",call. = FALSE)
  
  stats::median(lengths(g))
  
}


#writing the function to take the list generated by test and use it to populate the empty matrix
#all connections except the influencer node are bidirectional
initialize_matrix <- function(empty_matrix,network, population) {
for(i in 1:population){
  connection_vector <- network[[i]]
  for (j in 1: population) {
    ifelse(j %in% connection_vector, empty_matrix[j,i]<-1, empty_matrix[j,i]<-0)
  }
}
  return(empty_matrix)
}


#function to randomly select "seed" nodes, takes empty activation values vector and returns
#randomly selected nodes
select_influencers <- function(activation_values, seed) {
  influencer_node <- sample(1:population, seed, replace=FALSE)
  activation_values[influencer_node] <- 1
  return(activation_values)
  #potentially add code to remove ingoing connections from influencer nodes
}


#write a function that instead of randomly selecting an influencer adds an influencer to the network
#takes in a network and returns same network with an added influencer node


add_influencer<- function(network, influencer.degree, population) {
  network<-list.append(network,sample(1:population, size=influencer.degree))
  return(network)
}



#the function to initially set the influencer node to active
activate_influencer<- function(activation_values, population) {
  activation_values[1,population+1]<- 1
  return(activation_values)
}





#the function for the update rule
update.rule<- function(activation_values, connection.matrix, population) {
for(cycle in 2:cycles) {
  #this vector contains for each node what its total input is
  input.to.each.node <- rep(0,(population+1))
  #for each node determine its inputs and the activation of those nodes, then 
  #multiply by connection weight and sum using combination rule
  for(node in 1:(population+1)) {
    #this vector contains for each node all the individual inputs
    input.vector <- rep(0, population+1)
    for(input in 1:population+1){
      ##add ifelse for first cycle
      input.vector[input]<-if_else(connection.matrix[node,input]==1,activation_values[cycle-1,input]*connection_weight,0)
    }
    #creating vector for threshold
    thresholds<- (sample(global.threshold, population+1, replace=T)*0.01)
    steepness.values<- sample(global.steepness, population+1, replace=T)
    input.to.each.node[node]<- decay.rate*(combination.rule(unlist(input.vector), thresholds,steepness.values, node)-activation_values[cycle-1,node])
  }
  #add another dimension here to track activation over each cycle
  activation_values[cycle,]<-activation_values[cycle-1,]+input.to.each.node
  #activity needs to be bounded between 0 and 1
  for(i in 1:(population+1)) {
    activation_values[cycle,i]<- ifelse(activation_values[cycle,i]>=1, 1, activation_values[cycle,i])
  }
}
  return(activation_values)
}

#different version of update rule w/multiple influencer engagements
multi.update.rule<- function(activation_values, connection.matrix, population, post.times) {
  for(cycle in 2:cycles) {
    #this vector contains for each node what its total input is
    input.to.each.node <- rep(0,(population+1))
    #for each node determine its inputs and the activation of those nodes, then 
    #multiply by connection weight and sum using combination rule
    for(node in 1:(population+1)) {
      #this vector contains for each node all the individual inputs
      input.vector <- rep(0, population+1)
      for(input in 1:population+1){
        ##add ifelse for first cycle
        input.vector[input]<-if_else(connection.matrix[node,input]==1,activation_values[cycle-1,input]*connection_weight,0)
      }
      #creating vector for threshold
      thresholds<- (sample(global.threshold, population+1, replace=T)*0.01)
      steepness.values<- sample(global.steepness, population+1, replace=T)
      input.to.each.node[node]<- decay.rate*(combination.rule(unlist(input.vector), thresholds,steepness.values, node)-activation_values[cycle-1,node])
    }
    #add another dimension here to track activation over each cycle
    activation_values[cycle,]<-activation_values[cycle-1,]+input.to.each.node
    #activity needs to be bounded between 0 and 1
    for(i in 1:(population+1)) {
      activation_values[cycle,i]<- ifelse(activation_values[cycle,i]>=1, 1, activation_values[cycle,i])
    }
    if(cycle%in%post.times) {
      activation_values[cycle,(population+1)]<-1
    }
  }
  return(activation_values)
}






#write the function for the combination of different inputs using advanced logistic function
combination.rule <- function(input.vector, threshold.vector, steepness.values,node) {
  if(length(input.vector)<=1) {
    combined.input<- (1/(1+exp(-steepness.values[node]*(input.vector-threshold.vector[node])))-(1/(1+exp(threshold.vector[node]*steepness.values[node]))))*(1+exp(-threshold.vector[node]*steepness.values[node]))
  } else{
  combined.input<- (1/(1+exp(-steepness.values[node]*(sum(input.vector)-threshold.vector[node])))-(1/(1+exp(threshold.vector[node]*steepness.values[node]))))*(1+exp(-threshold.vector[node]*steepness.values[node]))
  }
  return(if_else(combined.input<0, 0, combined.input) )
}

#writing functions for visualization
#plots activity of just one node
plot.one.node<- function(activity.data, Node.2.plot) {
  ggplot(subset(activity.data, Node==Node.2.plot), aes(x=cycle, y=activation, ymax=1.0))+
    geom_line(color="darkred")
}

#plots multiple selected nodes
plot.multiple.nodes<- function(activity.data, Nodes.2.plot) {
  filter(activity.data, Node %in% Nodes.2.plot) %>%
    group_by(Node) %>%
    ggplot(., aes(x=cycle, y=activation, ymax=1.0))+
    geom_line(aes(color=Node))
}


#plot all activity 
plot.all.activity<- function(activity.data) {
  ggplot(activity.data, aes(x=cycle, y=activation, ymax=1.0))+
    geom_line(aes(color=Node))
}

#plot activity by degree
plot.by.degree<- function(activity.data, connection.matrix, population) {
  degree.vector <-vector(length=(population+1))
  for(i in 1:(population+1)){
    connection.vector <- as.vector(connection.matrix[i,])
    degree.vector[i]<- length(which(connection.vector==1))
  }
  activity.data %>%
    cbind(in.degree=rep(degree.vector, each=cycles)) %>%
    head(.,-cycles) %>%
    ggplot(., aes(x=cycle, y=activation, ymax=1.0))+
    geom_line(aes(color=in.degree, line=Node)) +
    scale_color_viridis(option="D")
}

#plot at each cycle how many nodes have reached a set "threshold" for action

global.activity.plot<- function(activation.matrix, population){
  action.count.vector<- rep(0, cycles)
  for(i in 1:cycles) {
    for(node in 1:(population+1)) {
      action.count.vector[i]<- ifelse(activation.matrix[i,node]>=threshold.for.action, 
                                   action.count.vector[i]+1,action.count.vector[i])
    }
  }
  as.data.frame(action.count.vector) %>%
    cbind.data.frame(cycle=rep(seq(1:cycles))) %>%
    ggplot(., aes(x=cycle, y=action.count.vector))+
    geom_line()
  
}

new.actions.each.cycle.plot<- function(activation.matrix, population){
  action.count.vector<- rep(0, cycles)
  for(i in 2:cycles) {
    for(node in 1:(population+1)) {
      action.count.vector[i]<- ifelse(activation.matrix[i,node]>=threshold.for.action & 
                                        all(activation.matrix[1:(i-1),node]<threshold.for.action), 
                                      action.count.vector[i]+1,action.count.vector[i])
    }
  }
  as.data.frame(action.count.vector) %>%
    cbind.data.frame(cycle=rep(seq(1:cycles))) %>%
    ggplot(., aes(x=cycle, y=action.count.vector))+
    geom_line()+
    labs(title="New Activations each Cycle", x="Cycle", y="Acitvated Nodes")
  
}


ridgeline.plot<- function(activity.data) {
  ggplot(activity.data, aes(x=cycle, y=Node, height=activation, fill=stat(height)))+
    geom_ridgeline_gradient(scale=0.8)+
    scale_fill_viridis_c(name="Activation", option="C")
}



  
