test.network.1 <- net.barabasi.albert(50,2, detectCores(), FALSE)%>%
  add_influencer(influencer.degree = 15)

matrix.test.1 <- matrix(test.network.1,population,population)

connection.matrix.1 <- initialize_matrix(matrix.test.1, test.network.1, 50)

activation_values <- rep(0,population)

activation_values<-select_influencers(activation_values)

activation_values<-update.rule(activation_values)

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



##visualizing 
small.test.graph<- ggplot(small.test.data, aes(x=cycle, y=Node, height=activation))+
  geom_density_ridges()+
  theme_ridges()+
  theme(legend.position="none")

small.test.graph

sparkline.test <- ggplot(small.test.data, aes(x=cycle, y=activation))+
  facet_grid(Node~ ., scales = "fixed")+
  geom_line()

sparkline.test

node.3.test.graph<- ggplot(subset(small.test.data, Node=="X3"), aes(x=cycle, y=activation))+
  geom_line()


node.3.test.graph




#writing generic functions for visualization
plot.one.node<- function(activity.data, Node.2.plot) {
  ggplot(subset(small.test.data, Node==Node.2.plot), aes(x=cycle, y=activation, ymax=1.0))+
    geom_line()
}


plot.one.node(small.test.data, "X3")
plot.one.node(small.test.data, "X7")
  
sparkline.test

