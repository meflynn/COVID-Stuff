library(data.table)
library(tidyverse)
library(here)
library(ggimage)

sysfonts::font_add_google(name = "Lobster")
showtext::showtext_auto()

dates <- data.frame(date = seq.Date(from = as.Date("2022-06-20"), to = as.Date("2022-07-04"), length.out = 28))

names <- data.frame(name = c("Michael", "Jessica", "Maggie", "Ben"))

base <- expand_grid(name = names$name, date = dates$date)

data <- readxl::read_xlsx(here("covid-test-results.xlsx")) |>
  mutate(date = as.Date(date)) |>
  dplyr::select(-time)

data.complete <- base |>
  full_join(data) |>
  arrange(name, date) |>
  group_by(name) |>
  mutate(date = format(as.Date(date), "%Y-%m-%d")) |>
  tidyr::fill(test, .direction = "down") |>
  mutate(image = ifelse(test == "Positive", "https://www.cnjg.org/sites/default/files/coronavirus_0.png", "https://i.pinimg.com/originals/cb/7d/48/cb7d48c589412612f5fd4a554e36a325.png"))


ggplot(data.complete, aes(x = date, y = name, fill = test)) +
  geom_tile(color = "black") +
  geom_image(aes(image = image), asp = 4) +
  coord_equal() +
  theme_minimal() +
  theme(text = element_text(family = "Lobster"),
        plot.background = element_rect(fill = "lightyellow"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightblue", "red")) +
  labs(x = "Date",
       y = "",
       fill = "Test Result",
       title = "Adventures in Covid")

ggsave(here::here("covid-timeline.pdf"), height = 2.9, width = 8, units = "in")
