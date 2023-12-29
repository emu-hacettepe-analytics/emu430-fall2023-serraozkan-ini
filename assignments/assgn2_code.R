library(tidyverse)
library(rvest)
library(stringr)

site_2010 <- read_html("https://m.imdb.com/search/title/?title_type=feature&release_date=,2010-01-01&num_votes=2500,&country_of_origin=TR&count=250")
movies_1 <- site_2010 |> html_elements("h3") |> html_text()

site_2023 <- read_html("https://m.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,&num_votes=2500,&country_of_origin=TR&count=250")
movies_2 <- site_2023 |> html_elements("h3") |> html_text()

movies <- c(movies_1, movies_2)
movies <- movies[! movies == "Recently viewed"]
movies <- data.frame(movies) |>
  separate(movies, into = c("number", "titles"), sep = ". ", extra = "merge")
movies <- movies[,2] |> data.frame()
colnames(movies) <- c("title")

rates_1 <- site_2010 |> 
  html_elements(".dli-ratings-container") |> 
  html_text() |> 
  substr(0,3)

rates_2 <- site_2023 |> 
  html_elements(".dli-ratings-container") |> 
  html_text() |> 
  substr(0,3)

rates <- c(rates_1, rates_2) |> as.numeric() |> data.frame() 
colnames(rates) <- c("rate")

dli_1 <- site_2010 |> 
  html_elements(".dli-title-metadata-item") |> 
  html_text()

dli_2 <- site_2023 |> 
  html_elements(".dli-title-metadata-item") |> 
  html_text()

years <- c(dli_1 |> str_extract("\\d{4}") |> as.numeric(), 
           dli_2 |> str_extract("\\d{4}") |> as.numeric())

years <- years[complete.cases(years)] |> data.frame()
colnames(years) <- c("year")

dur_detect <- c(dli_1 |> str_detect("[hm]"), dli_2 |> str_detect("[hm]"))
durations <- c(dli_1,dli_2)[dur_detect] |> data.frame()
colnames(durations) <- c("duration")

vote_read <- c(site_2010 |> html_elements(".kRnqtn") |> html_text(), 
               site_2023 |> html_elements(".kRnqtn") |> html_text()) |>
  substring(6, last = 10000) |> 
  str_replace(",", "") |>
  as.numeric() |>
  data.frame()

colnames(vote_read) <- c("votes")

movie_info <- data.frame(movies, years, rates, vote_read, durations)

movie_info <- arrange(movie_info, desc(rate))
first_5 <- movie_info[1:5,1:3]
last_5 <- tail(movie_info[,1:3], n=5)

grouped_rate <- group_by(movie_info, year) |> summarise(rate_avg = mean(rate))

plot_rate <- ggplot(grouped_rate, aes(year, rate_avg)) +
  geom_point(col = "#CC0066")

plot_rate

grouped_num <- group_by(movie_info, year) |> summarise(n_movies = n())

plot_num <- ggplot(grouped_num, aes(year, n_movies)) +
  geom_point(col = "#330066")

plot_num

plot_box <- ggplot(movie_info, aes(factor(year), rate)) +
  geom_boxplot(fill = "#339999")+
  xlab("year")+
  ylab("rate")+
  theme(axis.text.x = element_text(angle = 90))

plot_box

plot_votes <- ggplot(movie_info, aes(rate, votes)) +
  geom_point(col = "#333366")+
  scale_y_log10()+
  ylab("votes (scaled by log10)")

plot_votes

grouped_rate <- group_by(movie_info, rate) |> summarise(vote_avg = mean(votes))

plot_vote_avg <- ggplot(grouped_rate, aes(rate, vote_avg)) +
  geom_point(col = "#005566")

grouped_duration <- group_by(movie_info, duration) |> summarise(rate = mean(rate))

plot_dur <- ggplot(grouped_duration, aes(duration, rate)) +
  geom_point(col = "#FF33CC")+
  theme(axis.text.x = element_text(angle = 90))

plot_dur

site_top1000 <- read_html("https://m.imdb.com/search/title/?title_type=feature&groups=top_1000&country_of_origin=TR")
top1000_title <- site_top1000 |> html_elements("h3") |> html_text()
top1000_title <- top1000_title[! top1000_title == "Recently viewed"]
top1000_title <- data.frame(top1000_title) |>
  separate(top1000_title, into = c("number", "titles"), sep = ". ", extra = "merge")
top1000_title <- data.frame(top1000_title$titles)
colnames(top1000_title) <- c("title")

top1000_year <- site_top1000 |> html_elements(".dli-title-metadata-item") |> html_text()
top1000_year <- c(top1000_year |> str_extract("\\d{4}") |> as.numeric())
top1000_year <- top1000_year[complete.cases(top1000_year)] |> data.frame()
colnames(top1000_year) <- c("year")

top1000 <- data.frame(top1000_title,top1000_year)

joined_df <- left_join(top1000, movie_info, by = "title")

joined_df <- joined_df[order(desc(joined_df$rate)),]
