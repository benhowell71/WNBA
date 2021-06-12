library(tidyverse)
library(mclust)

df <- read.csv("pct_stats.csv")

df <- df %>%
  filter(MIN >= 200)
#leaves 105 observations from 2020

data <- read.csv("wnba_data_two.csv") %>%
  mutate(Player = as.character(Player),
         Team = as.character(Team),
         Position = as.character(Position))

qual <- data %>%
  filter(MP >= 200) #leaves us with nearly 600 instances from 5 seasons

x <- qual %>%
  dplyr::select(TSpct, X3PAr, FTr, REBpct, ASTpct, STLpct, BLKpct) %>%
  scale()

# data <- df %>% 
#   dplyr::select(USGpct_:pct_FTA, pct_REB, pct_AST, pct_STL, pct_STL) %>%
#   scale()

# set.seed(235)
# mixture_mod <- Mclust(data)
# mm_sum <- summary(mixture_mod)
# mm_sum
# 
# mbc_centers <- mixture_mod$parameters$mean %>%
#   t() %>%
#   as.data.frame()

theme_stern <- function() {
  theme(text = element_text(family='Tahoma', color="#232D4B"), # set font and color of all text
        axis.title = element_text(face='bold'), # make all axis titles bold
        plot.title = element_text(face='bold', hjust=0.5), # make all plot titles bold
        legend.title = element_text(face='bold'), # make all legend titles bold
        plot.subtitle = element_text(face='italic', hjust=0.5)) # make all subtitles italic
}

set.seed(222) # set seed to ensure reproduceability b/c k-means relies on random states for initialization 
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(x, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE = sse) %>%
  ggplot(aes(x=k, y=SSE)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE", title = "Where does this level off?") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() + theme_stern() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) # manually alter theme

tibble(k = 1:MAX_K, SSE_difference = sse-lead(sse)) %>%
  dplyr::filter(k<MAX_K) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE Rolling Difference", title = "A Clearer Picture") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() + theme_stern() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) # manually alter theme

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + theme_stern() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

set.seed(22)
# re-run K-Means with 10 clusters
K <- 5
kmeans10 <- kmeans(x, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans10$centers) # SCALED cluster centers/means

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', "Cluster 5")
                        #, "Cluster 6", "Cluster 7") 


tibble(cluster = kmeans10$cluster, name = qual$Player, team = qual$Team) %>%
  dplyr::filter(cluster == 5) 

