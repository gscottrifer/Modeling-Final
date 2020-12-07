test.network.1 <- net.barabasi.albert(50,2, detectCores(), FALSE)%>%
  add_influencer(influencer.degree = 15)

matrix.test.1 <- matrix(test.network.1,population,population)

connection.matrix.1 <- initialize_matrix(matrix.test.1, test.network.1, 50)

activation_values <- rep(0,population)

activation_values<-select_influencers(activation_values)

activation_values<-update.rule(activation_values)

#creating network
small.test.network <- net.barabasi.albert(10,2,detectCores(),FALSE) %>%
  add_influencer(influencer.degree = 5)
#visualizing network
draw.net(small.test.network)
#creating empty matrix and initializing with network structure
connection.matrix.small.test<- initialize_matrix(matrix(,11,11),small.test.network,11)
#creating empty matrix of activation values
activation.vector.small.test<- rep(0,11)

activation.vector.small.test<-update.rule(activation.vector.small.test,small.test.network)
