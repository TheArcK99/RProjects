install.packages('xlsx')
install.packages('devtools')
install.packages('RCurl')
install.packages('httr')
install.packages("Rcpp")
install.packages("dplyr")
install.packages("reshape2")
install.packages("gapminder")
install.packages("gganimate")

library(xlsx)
library(devtools)
library(RCurl)
library(httr)
library(ggplot2)
library(gganimate)
library(dplyr)
library(gapminder)
library(reshape2)

set_config(config(ssl_verifypeer = 0L))
devtools::install_github("thomasp85/gganimate", force = TRUE)

pop_xlsx <- read.xlsx("pop.xlsx", encoding = "UTF-8", stringsAsFactors = FALSE, sheetIndex = 1, as.data.frame = TRUE, header = TRUE)
fer_xlsx <- read.xlsx("tf.xlsx", encoding = "UTF-8", stringsAsFactors = FALSE, sheetIndex = 1, as.data.frame = TRUE, header = TRUE)
le_xlsx <- read.xlsx("lex.xlsx", encoding = "UTF-8", stringsAsFactors = FALSE, sheetIndex = 1, as.data.frame = TRUE, header = TRUE)
print("Data loaded")

vars <- paste("X", 1962:2015, sep = "")

pop <- pop_xlsx[, c(vars)]
fer <- fer_xlsx[, c(vars)]
le <- le_xlsx[, c(vars)]

colnames(pop)[1] <- "Country"
colnames(fer)[1] <- "Country"
colnames(le)[1] <- "Country"

pop <- pop[1:275, ]
le <- le[1:275, ]

pop_m <- melt(pop, id = c("Country"))
le_m <- melt(le, id = c("Country"))
fer_m <- melt(fer, id = c("Country"))

colnames(pop_m)[3] <- "pop"
colnames(le_m)[3] <- "life"
colnames(fer_m)[3] <- "fer"

bigdf <- merge(le_m, fer_m, by = c("Country", "variable"), header = TRUE)
bigdf <- merge(bigdf, pop_m, by = c("Country", "variable"), header = TRUE)

con <- gapminder %>% group_by(continent, country) %>% distinct(country, continent)
con <- data.frame(lapply(con, as.character), stringsAsFactors = FALSE)
colnames(con)[1] <- "Country"

bigdf_f <- bigdf %>% filter(Country %in% unique(con$Country))
bigdf_f <- merge(bigdf_f, con, by = c("Country"), header = TRUE)




bigdf_f$year <- as.integer(gsub("X", "", bigdf_f$variable))
print(range(bigdf_f$year))

bigdf_f[is.na(bigdf_f)] <- 0 
bigdf_f$pop <- round(as.numeric(as.character(bigdf_f$pop)) / 1000000, 1)
bigdf_f$life <- as.numeric(as.character(bigdf_f$life))
bigdf_f$fer <- as.numeric(as.character(bigdf_f$fer))

theme_set(theme_grey() + theme(legend.box.background = element_rect(), legend.box.margin = margin(6, 6, 6, 6)))

p <- ggplot(bigdf_f, aes(fer, life, size = pop, color = continent, frame = year)) +
  labs(x = "Fertility Rate", y = "Life expectancy at birth (in years)", caption = "(Based on data from Hans Rosling)", color = 'Continent', size = "Population (in millions)") +
  geom_point() +
  scale_color_brewer(type = 'div', palette = 'Spectral') +
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(p, width = 450, height = 450, res = 96, nframes = 100, fps = 10)
