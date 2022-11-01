# Name: 'Joba Adisa
# Lab 9
# October 21, 2022

#---------------- Set up -------------------------#

# load libraries
library(tidyverse)
library(tidygraph)
library(ggraph)

# Six Degrees of Kristen Stewart
# We are all part of a social network with a relatively small diameter (~ 6)

# Example: Determining the centrality of Kevin Bacon
# In the graph: 
# Each actor is a node
# Two actors share an edge if they have appeared in a movie together


# Reading the Data
edges <-  read.csv("data/edges_2012.csv")
V <- read.csv("data/vertices_2012.csv")

edges %>% summarize(num_rows = n(),
                    num_titles = n_distinct(title)) # outputs 10223 rows and 55 movies

# From the IMDB query, we can see that BAtman received the most user ratung on IMDB
movies <-  edges %>% 
  group_by(movie_id) %>% 
  summarize(title = max(title), N = n(), numRatings = max(ratings)) %>% 
  arrange(desc(numRatings))

# view movies
movies


# Building the hollywood network
#' To build a graph, we specify the edges, whether we want them to be directed or not
#' and add information about the vertices

g <-  tbl_graph(nodes = V, directed = FALSE, edges = edges)
summary(g)  # outputs 1010 actors and 10223 edges

# We have associated meta data with the edge information
# We can visualize this network using the ggraph function in the ggplot package

ggraph(g, 'drl') +
  # draw edges as curves of different curvatures
  geom_edge_fan(width = 0.1) +
  geom_node_point(color = "dodgerblue") +
  theme_void()

# Actors who have appeared in multiple movies tend to be more central to the network
# If an actor has appeared in multiple movies, then it stands to reason that they will have 
# more connections to other actors. This is captured by degree centrality

g <-  g %>% 
  # calculate node and edge centrality
  mutate(degree = centrality_degree())

g %>% 
  as_tibble() %>% 
  arrange(desc(degree)) %>% 
  head()

# retrieving the list of movies for a particular actor
show_movies <- function(g, id) {
  g %>% 
    activate(edges) %>% 
    as_tibble() %>% 
    filter(src == id | dest == id) %>% 
    group_by(movie_id) %>% 
    summarize(title = first(title), num_connections = n())
}


# calling the function to get the list if movies for Bryan Cranston with id = 502126
show_movies(g, 502126)


#-------------------Exercise 1----------------------------------------#

# calling the show_movie function to get the list if movies for Kristen Stewart with id = 3945132
show_movies(g, 3945132)

# using the select and filter function on as_tibble(g) to determine Kristen Stewart's degree centrality
g %>% 
  as_tibble() %>% 
  filter(actor_id == 3945132) %>% 
  select(actor_name, degree)

# View the list of actors in the V dataset. Select two that you recognize and make note of their actor_id value
# a. Detertime the 2012 movie that they appear in 
# b. Determine their degree of centrality

# Actor 1: Sylvester Stallione, id  = 2276804

# Getting the 2012 movies he appeared in
show_movies(g, 2276804)

# Detrmining his degree of centrality
g %>% 
  as_tibble() %>% 
  filter(actor_id == 2276804) %>% 
  select(actor_name, degree) # outputs 18 degree of centrality

# Actor 2: Denzel Washington, id  = 2537865

# Getting the 2012 movies he appeared in
show_movies(g, 2537865)

# Detrmining his degree of centrality
g %>% 
  as_tibble() %>% 
  filter(actor_id == 2537865) %>% 
  select(actor_name, degree)  # outputs 38 degree of centrality

#--------------- End Exercise 1------------------------#


# Distribution of Degrees
ggplot(data = enframe(igraph::degree(g)), aes(x = value)) +
  geom_density(size = 1)


# Returning to the network grapg, the nodes can be shaded according to their degree of centrality and the 
# The transparency of edges can be scaled relative to their weight measures.

hollywood <- ggraph(g, layout = 'drl') +
  geom_edge_fan(aes(alpha=weight), color="lightgray") +
  geom_node_point(aes(color=degree), alpha = 0.6) +
  scale_edge_alpha_continuous(range = c(0, 1)) +
  scale_color_viridis_c() + 
  theme_void()

# We do not want to show the vertex labels so our network looks cleaner
hollywood + 
  geom_node_label(aes(filter = degree > 40, label = str_replace_all(actor_name, ", ", ", \n")), 
                  repel = TRUE)
# The Hollywood network for popular 2012 movies. Color is mapped to degree centrality


#-----------Building a Kristen Stewart oracle--------------------#

# To emphasize the pathways through leading actors, we could considerr the betweenness centrality
g <-  g %>% 
  mutate(btw = centrality_betweenness(weights = weight, normalized = TRUE))


g %>% 
  as_tibble() %>% 
  arrange(desc(btw)) %>% 
  head(10)

# From the result, Kristen Stewart has the highest betweenness centrality while Joseph Gordon-Levitt
# and Tom Hardyand others have the highest degree of centrality.
# Christian Bale has the third highest betweenness despite appearing in just one movie.

# Further analysis:
#' If Kristen Steward is very central to this network, then perhaps, instead of a Bacon number,
#' We can consider a Stewart number
#' Kristen Stewart distance from Charlize Theron is 1, since they both appear in the movie "Snow White and the Huntsman" together
 ks <- V %>% 
   filter(actor_name == "Stewart, Kristen")
 
 ct <-  V %>% 
   filter(actor_name == 'Theron, Charlize')
 
 g %>%
   convert(to_shortest_path, from = ks$id, to = ct$id)
 
 # On the flip side, her distance from Gordon-Levitt is 4
 # What this means is that: 
 #' The interpretation here is that Joseph Gordon-Levitt was in The Dark Night Rises with Tom Hardy, 
 #' who was in Lawless with Guy Pearce, who was in Prometheus with Charlize Theron, 
 #' who was in Snow White and the Huntsman with Kristen Stewart.

 jgl <-  V %>% 
   filter(actor_name == "Gordon-Levitt, Joseph")
 
 set.seed(47)
 h <- g %>% 
   convert(to_shortest_path, from = jgl$id, to = ks$id, weights = NA)
 
 h %>% 
   ggraph('gem') +
   geom_node_point() +
   geom_node_label(aes(label= actor_name)) +
   geom_edge_fan2(aes(label = title)) +
   coord_cartesian(clip = "off") +
   theme(plot.margin = margin(6, 36, 6, 36))

 # These shortest parts are not unique.
 
 # in fact there are 9 possible shortest paths between Kristen Stewart and Joseph Gordon-Levitt
 # Each having a length of 4
 igraph::all_shortest_paths(g, from = ks$id, to = jgl$id, weights=NA) %>% 
   pluck("res") %>% 
   length()

 
 #-----------Exxercise 2---------------------------------------#
 # Create a stewart number and visualization path for the 
 # two hollywood actors selected in Exercise 1 above
 
 # Kristen Stewart number for Sylvester Stallone (id = 2276804) is 3
ss <- V %>% 
  filter(actor_id == 2276804)
 
 # getting the shortest path
ss_ks_path <- g %>%
  convert(to_shortest_path, from = ks$id, to = ss$id, weights = NA)

# visualizing the path
ss_ks_path %>% 
  ggraph('gem') +
  geom_node_point() +
  geom_node_label(aes(label= actor_name)) +
  geom_edge_fan2(aes(label = title)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(6, 36, 6, 36))
 
 # Kristen Stewart number for Denzel Washington (id  = 2537865) is 5
 
dw <- V %>% 
  filter(actor_id == 2537865)

# getting the shortest path
dw_ks_path <- g %>%
  convert(to_shortest_path, from = ks$id, to = dw$id, weights = NA)

# visualizing the path
dw_ks_path %>% 
  ggraph('gem') +
  geom_node_point() +
  geom_node_label(aes(label= actor_name)) +
  geom_edge_fan2(aes(label = title)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(6, 36, 6, 36))

 
 #---------------End-------------------------------------------#


#' As we saw in the network is not connected, and thus its diameter is infinite. 
#' However, the diameter of the largest connected component can be computed. 
#' Thus number (in this case, 10) indicates how many hops separate the two most distant actors in the network.

igraph::diameter(g, weights = NA)  # outputs 10


# Determining Kristen Stewart eccentricity
g %>% 
  mutate(eccentricity = node_eccentricity()) %>% 
  filter(actor_name == "Stewart, Kristen")  # outputs 6

#' Kristen Stewart’s eccentricity is 6. 
#' This means that there is no actor in the connected part of the network who is more than 6 hops away from Kristen Stewart. 
#' That is, Six degrees of separation.
