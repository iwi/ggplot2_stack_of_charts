# library(plyr)
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# library(scales)

set.seed(12345)

my_labels <- function(variable, value){
  names_li <- list("percentage", "cost in pounds")
  return(names_li[value])
}


df <- data.frame(
  rate = runif(10, 0.1, 0.65),
  cost = rnorm(10, 0, 10000),
  years = seq(from = 2001, to = 2010)
)

max_min <- data.frame(
  rate = runif(2, 0.1, 0.65),
  cost = c(-20000, 50000),
  years = c(2009,2010)
)

df$cost <- ifelse(df$cost > 0.8 |
                    df$cost < 0.3, df$cost, 0)

df %>%
  gather(type_of_var,
         value,
         rate:cost) ->
  df2

max_min %>%
  gather(type_of_var,
         value,
         rate:cost) ->
  max_min2

label_fun <- function (x) {
  if(max(x, na.rm = TRUE) > 0.8 |
       max(x,na.rm = TRUE) < 0.3) {
    paste("£", x, sep = "")
  } else {
    percent(x)
  }
}


df2 %>%
  ggplot(aes(x = years,
             y = value,
             ymin = 0,
             ymax = .1)) +
  facet_grid(type_of_var ~ .,
             scales = 'free_y',
             labeller = my_labels) +
  labs(x = "Year",
       y = "") +

  geom_point(subset = . (type_of_var == "rate")) +
  geom_line(subset = . (type_of_var == "rate"),
            colour = "grey") +

  geom_bar(subset = . (type_of_var == "cost"),
           stat = "identity") +
  geom_blank(data = max_min2) +

  theme_bw() +
  theme(strip.text.y = element_text(size = 15,
                                    colour = "black"),
        plot.title = element_text(lineheight = 0.8,
                                  face = "bold")) +
  scale_x_continuous(breaks = seq(2001, 2010, 1)) +
  scale_y_continuous(labels = label_fun) +
  labs(title = "free_y y axis labels")