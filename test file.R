test.network.1 <- net.barabasi.albert(50,2, detectCores(), FALSE)%>%
  add_influencer(influencer.degree = 25, population=50)

draw.net.large.network(test.network.1)


connection.matrix.test.1 <- initialize_matrix(matrix(,51,51),test.network.1,51)

activation.matrix.test.1<- matrix(0,cycles,51) %>%
  activate_influencer(population=50)

activation.matrix.test.1<- update.rule(activation.matrix.test.1,connection.matrix=connection.matrix.test.1, population=50)

test.1.data<- data.frame(activation.matrix.test.1) %>%
  gather(key="Node", value= "activation") %>%
  cbind.data.frame(cycle=rep(seq(1:100),51))

plot.all.activity(test.1.data)
plot.by.degree(test.1.data, test.network.1)

#creating network
small.test.network <- net.barabasi.albert(10,2,detectCores(),FALSE) %>%
  add_influencer(influencer.degree =5, population=10)
#visualizing network
draw.net(small.test.network)
#creating empty matrix and initializing with network structure
connection.matrix.small.test<- initialize_matrix(matrix(,11,11),small.test.network,11)
#creating empty matrix of activation values
activation.matrix.small.test<- matrix(0,cycles,11) %>%
  activate_influencer(population=10)

#create vector for threshold and steepness


activation.matrix.small.test<-update.rule(activation.matrix.small.test,connection.matrix.small.test, population=10)
#tidying data set
small.test.data<-data.frame(activation.matrix.small.test) %>%
  gather(key="Node", value= "activation") %>%
  cbind.data.frame(cycle=rep(seq(1:100),11))
  

plot.all.activity(small.test.data)


##visualizing the activity data

sparkline.test <- ggplot(test.1.data, aes(x=cycle, y=activation))+
  facet_grid(Node~ ., scales = "fixed")+
  geom_line()

sparkline.test

node.3.test.graph<- ggplot(subset(small.test.data, Node=="X3"), aes(x=cycle, y=activation))+
  geom_line()


node.3.test.graph

ridgeline.plot<- ggplot(test.1.data, aes(x=cycle, y=Node, height=activation))+
  geom_ridgeline_gradient(scale=0.6)

ridgeline.plot

#writing generic functions for visualization
plot.one.node<- function(activity.data, Node.2.plot) {
  ggplot(subset(activity.data, Node==Node.2.plot), aes(x=cycle, y=activation, ymax=1.0))+
    geom_line(color="darkred")
}

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
plot.by.degree<- function(activity.data, connection.matrix) {
  degree.vector <-vector(length=(population+1))
  for(i in 1:(population+1)){
    connection.vector <- as.vector(connection.matrix[i,])
    degree.vector[i]<- length(which(connection.vector==1))
  }
  activity.data %>%
  cbind(degree=rep(degree.vector, each=cycles)) %>%
    ggplot(., aes(x=cycle, y=activation, ymax=1.0))+
    geom_line(aes(color=degree, line=Node)) +
    scale_color_viridis(option="C")
}


plot.by.degree(test.1.data, connection.matrix.test.1)

plot.multiple.nodes(test.1.data, Nodes.2.plot=c("X1","X3","X4","X46"))
plot.one.node(test.1.data, "X3")
plot.all.activity(test.1.data)

plot.one.node(test.1.data, "X2")
plot.one.node(small.test.data, "X8")
  
sparkline.test


?degree
