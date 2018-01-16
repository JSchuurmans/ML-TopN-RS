install.packages(c('SPARQL','igraph','network','ergm'),dependencies=TRUE)

library(SPARQL)
library(igraph)
library(network)
library(ergm)

# Live DBpedia endpoint
endpoint <- 'http://live.dbpedia.org/sparql'
options <- NULL

# Local Jena Fuseki endpoint
# endpoint <- "http://localhost:3030/movie/sparql"
# options <- "output=xml"

prefix <- c("db","http://dbpedia.org/resource/")

sparql_prefix <- "PREFIX dbp: <http://dbpedia.org/property/>
                  PREFIX dc: <http://purl.org/dc/terms/>
                  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
"

# ?countryOfOrigin ?musicBy ?productionCompany ?author ?genre ?keywords ?producer
# 
q <- paste(sparql_prefix,
           'SELECT ?actor ?movie ?director ?movie_date
            WHERE {
                ?m dc:subject <http://dbpedia.org/resource/Category:American_films> .
                ?m rdfs:label ?movie .
                FILTER(LANG(?movie) = "en")
                ?m dbp:released ?movie_date .
                FILTER(DATATYPE(?movie_date) = xsd:date)
                ?m dbp:starring ?a .
                ?a rdfs:label ?actor .
                FILTER(LANG(?actor) = "en")
                ?m dbp:director ?d .
                ?d rdfs:label ?director .
                FILTER(LANG(?director) = "en")
           } LIMIT 50 ')
q1 <- paste(sparql_prefix,
        'SELECT ?movie ?movieTitle ?title ?directorName ?DistributorName ?ArtistName ?Country (STR(YEAR(?ReleaseDate)) as ?releaseYear)
        WHERE{ 
        ?movie rdf:type <http://dbpedia.org/ontology/Film> .
        ?movie foaf:name ?movieTitle .
        ?movie rdfs:label ?title .
        ?movie dbp:director ?directorName .
        ?movie dbp:distributor ?DistributorName .
        ?movie dbp:starring ?ArtistName .
        ?movie dbp:country ?Country .
        ?movie <http://dbpedia.org/ontology/releaseDate> ?ReleaseDate  .
        FILTER langMatches(lang(?title), "EN") .
        FILTER(!isLiteral(?movieTitle) || langMatches(lang(?movieTitle), "EN")) .
        FILTER ((?ReleaseDate >= "1950-01-01"^^xsd:date) && (?ReleaseDate < "2018-01-01"^^xsd:date)) .
        FILTER regex(?year, "[0-9]{4} film", "i")
        }
        ORDER BY DESC(?ReleaseDate)
        LIMIT 10000 OFFSET 0')

res <- SPARQL(url=endpoint,query=q,ns=prefix)$results

res1 <- SPARQL(url=endpoint,query=q1,ns=prefix)$results

DT_dbpedia <- as.data.table(res1)


DT_dbpedia2<-DT_dbpedia
DT_dbpedia2$title<-gsub('"', '', DT_dbpedia2$title)
DT_dbpedia2$title<-gsub('@en', '', DT_dbpedia2$title)
DT_dbpedia2$title<-gsub(' (film)', '', DT_dbpedia2$title,fixed=TRUE)
DT_dbpedia2$title<-gsub(' (film series)', '', DT_dbpedia2$title,fixed=TRUE)
DT_dbpedia2$title<-gsub(' film', '', DT_dbpedia2$title,fixed=TRUE)

year<-as.character(c(1900:2020))
year<-paste0(' (',year,')')
for (i in year){
  DT_dbpedia2$title<-sub(i, '', DT_dbpedia2$title, fixed=TRUE)
}

DT_dbpedia2$title<-paste0(DT_dbpedia2$title,' (',DT_dbpedia$releaseYear,')')

test<-length(unique(DT_dbpedia2$title))

dt_M20_movies<-as.data.table(M20_movies_the)
dt_res3<-as.data.table(res3)

test <- merge(dt_users_ratings_movies,DT_dbpedia2,by='title')







#
# Actor-movie relations, director as attribute of movie
#
actor_movie_matrix <- as.matrix(ifelse(table(res2$ArtistName,res2$movie) > 0, 1, 0))
#a_m <- graph.incidence(actor_movie_matrix)

md <- cbind(res2$movie,res2$directorName)
movie_directors <- md[!duplicated(md[,1]),]

movie_director_matrix <- as.matrix(ifelse(table(res2$movie,res2$directorName) > 0, 1, 0))

movie_distributor_matrix <- as.matrix(ifelse(table(res2$movie,res2$DistributorName) > 0, 1, 0))

movie_country_matrix <- as.matrix(ifelse(table(res2$movie,res2$Country) > 0, 1, 0))

movie_year_matrix <- as.matrix(ifelse(table(res2$movie,res2$releaseYear) > 0, 1, 0))






n_actors <- dim(actor_movie_matrix)[1]
n_movies <- dim(actor_movie_matrix)[2]
n_directors <- length(unique(res$director))

# Set vertex properites
V(a_m)[1:n_actors]$kind <- "actor"
V(a_m)[n_actors+1:n_movies]$kind <- "movie"
V(a_m)[n_actors+1:n_movies]$director <- movie_directors[,2]

# Find dates of movies
md <- cbind(res$movie,res$date)
movie_dates <- md[!duplicated(md[,1]),]
V(a_m)[n_actors+1:n_movies]$date_label <- sapply(movie_dates,as.character) # convert to string for Gephi
V(a_m)[n_actors+1:n_movies]$date_epoch <- movie_dates # for coloring

# Remove non-ASCII characters (for Gephi)
V(a_m)$name <- iconv(V(a_m)$name,to='ASCII',sub="")
V(a_m)$director <- iconv(V(a_m)$director,to='ASCII',sub="")

# Use names as labels in Gephi
V(a_m)$Label <- V(a_m)$name

write.graph(a_m,'starring_dbpedia_american.graphml',format="graphml")

#
# Actor-director relations, movies as weight
#
movie_director_matrix <- as.matrix(ifelse(table(res$movie,res$director) > 0, 1, 0))

actor_director_matrix <- actor_movie_matrix %*% movie_director_matrix
#a_d <- graph.incidence(actor_director_matrix,weighted=TRUE,mode="undirected")
a_d <- graph.incidence(actor_director_matrix,directed=FALSE,weighted=TRUE)
V(a_d)[1:n_actors]$kind <- "actor"
V(a_d)[n_actors+1:n_directors]$kind <- "director"

# Remove non-ASCII characters (for Gephi)
V(a_d)$name <- iconv(V(a_d)$name,to='ASCII',sub="")

# Gephi weights and labels
E(a_d)$Weight <- E(a_d)$weight / max(E(a_d)$weight) #FIXME
V(a_d)$Label <- V(a_d)$name

write.graph(a_d,'actor_director_dbpedia_american.graphml',format="graphml")

#
# Actor-actor costar relations
#
costar_matrix <- actor_movie_matrix %*% t(actor_movie_matrix)
diag(costar_matrix) <- 0 # remove self-loops
a_a <- graph.adjacency(costar_matrix,weighted=TRUE,mode="undirected")

# Compute SNA metrics
V(a_a)$betweenness <- betweenness(a_a, directed=FALSE, normalized=TRUE)
E(a_a)$betweenness <- edge.betweenness(a_a, directed=FALSE)
V(a_a)$centrality <- evcent(a_a, weights=V(a_a)$weight)$vector
V(a_a)$lec_community <- as.character(leading.eigenvector.community(a_a)$membership)

# Kevin Bacon degree calculation
kevin_bacon <- V(a_a)[V(a_a)$name=='"Kevin Bacon"@en']
kevin_bacon_degree <- shortest.paths(a_a,v=kevin_bacon,to=V(a_a))
V(a_a)$kevin_bacon_degree <- kevin_bacon_degree
kevin_bacon_hist <- hist(V(a_a)$kevin_bacon_degree,xlab="degrees of Kevin Bacon",main="Six degrees of Kevin Bacon")
sum(kevin_bacon_hist$intensities[1:6]) # percentage of people with KB degree <= 6

# Power-law distribution check
dd <- degree.distribution(a_a, cumulative=TRUE)
plot(dd,xlab="degree",ylab="cumulative frequency",main="Co-star degree distribution in Hollywood")
plot(dd,log="xy",xlab="degree",ylab="cumulative frequency",main="Co-star degree distribution in Hollywood")
d <- degree(a_a)
power.law.fit(d, xmin=1) 
power.law.fit(d, xmin=10) 

# Gephi weights and labels
E(a_a)$Weight <- E(a_a)$weight
V(a_a)$name <- iconv(V(a_a)$name,to='ASCII',sub="")
V(a_a)$Label <- V(a_a)$name

write.graph(a_a,'costarring_dbpedia_american.graphml',format="graphml")


#
# Exponential Random Graph Models
#
a_m_n <- as.network.matrix(actor_movie_matrix,bipartite=TRUE,directed=FALSE)
a_m_n %v% 'kind' <- V(a_m)$kind
a_m_n %v% 'director' <- V(a_m)$director

# Calculate influence of director on forming edges
# ***very computationally intensive***
#
# e <- ergm(n ~ edges + b2factor('director'))
# summary(e)

