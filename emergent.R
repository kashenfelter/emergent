library(ggart)
library(ggforce)
library(tidyverse)
library(viridis)

n <- 100
X <- 100
Y <- 100
r <- 1
R <- 100
N <- 100000

df <- data.frame(point = numeric((n + 1) * N),
                 x = numeric((n + 1) * N), y = numeric((n + 1) * N),
                 frame = numeric((n + 1) * N))
df_last <- data.frame(point = 1:n, frame = 0) %>%
  mutate(alpha = runif(n, 0, 2 * pi), R2 = runif(n, 0, R), x = R2 * cos(alpha), y = R2 * sin(alpha)) %>%
  select(point, x, y, frame)
df[1:n, ] <- df_last

for(i in 1:N) {
  valid <- FALSE
  while(!valid) {
    temp <- sample_n(df_last, 1)
    df_last2 <- df_last %>%
      mutate(dist = sqrt((temp$x - x)^2 + (temp$y - y)^2)) %>%
      arrange(dist) %>%
      filter(dist >= 2 * r)
    if(nrow(df_last2) == 0) {
      next
    } else {
      v <- - c(temp$x - df_last2$x[1], temp$y - df_last2$y[1]) /
        sqrt((temp$x - df_last2$x[1])^2 + (temp$y - df_last2$y[1])^2)
      valid <- TRUE
    }
  }
  temp$x <- temp$x + 2 * r * v[1]
  temp$y <- temp$y + 2 * r * v[2]
  df_new <- df_last %>% mutate(frame = i)
  df_new[rownames(temp[1, ]), ] <- temp
  df[seq(i*n+1,(i+1)*n), ] <- df_new
  df_last <- df_new
  print(i)
}

df <- df %>% filter(point != 0)

# df <- readRDS("df.Rds")

max_frame <- max(df$frame)

p <- ggplot() +
  geom_path(aes(x, y, group = point), df, size = 0.35, alpha = 1, colour = "black") +
  geom_circle(aes(x0 = x, y0 = y, r = r), df %>% filter(frame == 0), size = 0.35, colour = "black") +
  geom_circle(aes(x0 = x, y0 = y, r = r), df %>% filter(frame == max_frame), size = 0.1, fill = "red") +
  theme_blankcanvas(bg_col = "#F0F0F0") +
  xlim(-97, 97) + ylim(-97, 97) +
  coord_equal() +
  theme(plot.margin = unit(c(-1, 0, -2, -1), "mm"))

ggsave("plots/plot002.png", p, width = 30, height = 30, units = "cm", dpi = 600)
