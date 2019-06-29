creds <- read_csv("tmdb_5000_credits.csv/tmdb_5000_credits.csv")

acting <- creds %>%      
  filter(nchar(cast)>2) %>%        
  mutate(                                 
    js  =  lapply(cast, fromJSON)  
    )  %>%                           
  unnest(js) %>%
  select(movie_id, title, id, name)


genre_dat <- read_csv("tmdb_5000_movies.csv/tmdb_5000_movies.csv")

genres_readable <- genre_dat %>%      
  filter(nchar(genres)>2) %>%        
  mutate(                                 
    js  =  lapply(genres, fromJSON)  
  )  %>%                           
  unnest(js) %>%
  select(id, title, id1, name)

directors <- creds %>%       
  filter(nchar(crew)>2) %>%        
  mutate(                                 
    js  =  lapply(crew, fromJSON)  
  )  %>%                           
  unnest(js) %>%
  select(movie_id, title, id, job, name) %>%
  filter(job=='Director')
