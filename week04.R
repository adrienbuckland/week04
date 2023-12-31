#pacman::p_load(tidyverse)
library(tidyverse)
disasters <- read.csv("EMDAT.csv")

disastersgrouped <- disasters %>%
  group_by(Entity)

view(disastersgrouped)
options(scipen = 999)


df <- disasters %>%
  select(Entity, Year, deaths_all_disasters)
glimpse(df)

decader <- function(x){
  if(is.integer(x) == TRUE){
    x <- as.character(signif(x, 3))
    return(x)
  } else {
    return(x)
  }
}
test <- df
test <- map_df(df, decader)
group_by(Entity)
test %>%
  group_by(Entity) %>%
  summarise(test)
test <- df %>%
  group_by(Entity)
glimpse(test)
view(test)
group_by(test$Year)



disasters <- read.csv("EMDAT.csv")

df <- disasters %>%
  select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters)
library(glue)

  
    



ggplot(data = filtered_df, aes(x = Year, y = deaths_all_disasters)) +
  geom_point()

filtered_df <- df %>%
  filter(Entity == "Soviet Union" | Entity == "China" | Entity == "Afghanistan" | Entity == "Albania" | Entity == "Turkey")

  

plotting <- function(i){
  ggplot(data = filtered_df, aes(x = filtered_df[[2]], y = filtered_df[[i]])) +
    geom_point() +
    labs(
      title = glue("{names(filtered_df)[i]} in {filtered_df[1,1]}"),
      x = names(filtered_df)[2],
      y = names(filtered_df)[i]
    )
}

plots_list <- map(3:ncol(filtered_df), plotting)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3) 

airquality
df2 <- unique(filtered_df$Entity)
view(df2)
