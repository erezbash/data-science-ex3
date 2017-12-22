# Q1

load the graph
```{r}
library(igraph)
ga.data <- read.csv('ga_edgelist.csv', header = T)
g <- graph.data.frame(ga.data,directed = F)
```

find the big component
```{r}
comps <- components(g)
maxCompId <- as.numeric(which(max(comps$csize) == comps$csize))
maxCompVertex <- as.numeric(which(comps$membership == maxCompId))
```

create sub-graph from the big component 
```{r}
curG<-delete.vertices(g, -maxCompVertex)
```


## a:
 
```{r}
b <- betweenness(curG, v = V(curG))
b
```

find the Actor with max betweeness
```{r}
which(max(b) == b)
```
![pic](/Q1_pics/1_a_betweeness_max.PNG)

```{r}
c <- closeness(curG, vids = V(curG))
c
```

find the Actor with max closeness
```{r}
which(max(c) == c)
```
![pic](/Q1_pics/1_a_c_max.PNG)

```{r}
e <- eigen_centrality(curG)
e$vector
```

find the Actor with max eigenvector
```{r}
which(max(e$vector) == e$vector)
```
![pic](/Q1_pics/1_a_e_max.PNG)


## b:

* ALGO1: Girvan-Newman community detection

```{r}
gc1 <-  edge.betweenness.community(curG)
gc1
```

  * 1: print community with colors
  
using the membership function that returns community ids for each vertex.
```{r}
 memb1 <- membership(gc1)
```

```{r}
plot(curG, vertex.size=5, vertex.color=memb1, asp=FALSE)
```
![pic](/Q1_pics/1_b_ALG1_1.PNG)

  * 2.1: number of communities
```{r}
length(groups(gc1))
```
![pic](/Q1_pics/1_b_ALG1_2_1.PNG)

  * 2.2: size of each community
```{r}
for (value in groups(gc1)) {
    print(length(value))
}
```
![pic](/Q1_pics/1_b_ALG1_2_2.PNG)

  * 3: modularity
  
modularity for each phase 
```{r}
gc1$modularity
```
![pic](/Q1_pics/1_b_ALG1_3_1.PNG)

best modularity score
```{r}
max(gc1$modularity)
```
![pic](/Q1_pics/1_b_ALG1_3_2.PNG)

index (phase, i.e. partitioning) with the best score
```{r}
which.max(gc1$modularity)
```
![pic](/Q1_pics/1_b_ALG1_3_3.PNG)

* ALGO2:walktrap.community

```{r}
gc2 <- walktrap.community(curG)
gc2
```

  * 1: print community with colors
  
using the membership function that returns community ids for each vertex.
```{r}
memb2 <- membership(gc2)
```

```{r}
plot(curG, vertex.size=5, vertex.color=memb2, asp=FALSE)
```
![pic](/Q1_pics/1_b_ALG2_1.PNG)

  * 2.1: number of communities
```{r}
length(groups(gc2))
```
![pic](/Q1_pics/1_b_ALG2_2_1.PNG)

  * 2.2: size of each community
```{r}
for (value in groups(gc2)) {
    print(length(value))
}
```
![pic](/Q1_pics/1_b_ALG2_2_2.PNG)

  * 3: modularity
modularity for each phase 
```{r}
gc2$modularity
```
![pic](/Q1_pics/1_b_ALG2_3_1.PNG)

best modularity score
```{r}
max(gc2$modularity)
```
![pic](/Q1_pics/1_b_ALG2_3_2.PNG)

index (phase, i.e. partitioning) with the best score
```{r}
which.max(gc2$modularity)
```
![pic](/Q1_pics/1_b_ALG2_3_3.PNG)






