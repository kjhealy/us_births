###--------------------------------------------------
### Aaron Penne's population dataviz.
### Data forked from github.com/aaronpenne/data_visualization
###--------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(gtable)
library(ggforce)
library(patchwork)
## library(janitor)

library(tsibble)
library(fable)
library(feasts)

library(showtext)
showtext_auto()
library(myriad)
import_myriad_semi()

theme_set(theme_myriad_semi())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

###--------------------------------------------------
### Data
###--------------------------------------------------

days <- read_csv("data/days_of_month.csv")
births <- read_csv("data/usa_birth_1933-2015.csv")
pop <- read_csv("data/usa_pop_1917-2017.csv")

## Tidy
days_lon <- gather(days, year, n_days, -Month)
days_lon <- janitor::clean_names(days_lon)
days_lon$year <- as.integer(days_lon$year)

births <- janitor::clean_names(births)

## Extract yearly birth totals
births_per_yr <- births %>%
    select(year, month, births) %>%
    filter(month == "TOT") %>%
    select(year, births)

colnames(births_per_yr) <- c("year", "annual_births")

## Extract monthly birth counts
births <- births %>% select(year, month, births) %>%
    filter(month != "TOT")
births$month <- as.integer(births$month)

## US population by year
pop_us <- pop %>% select(Year, US) %>% janitor::clean_names()

## Join everything up
data <- left_join(births, days_lon)
data <- left_join(data, pop_us)
data <- left_join(data, births_per_yr)

## Calculate births / month / day
data <- data %>% mutate(births_pct = births / us,
                        births_pct_day = (births_pct / n_days) * 1e6,
                        date = yearmonth(ymd(paste(year, month, 1, sep="-"))))



### STL decomposition using Hyndman's tsibble and fable
data <- as_tsibble(data)

data_stl <- data %>% 
    model(STL(births_pct_day ~ trend(window = 21) + season(window = 13))) %>%
    components()

stl_df <- data_stl

###--------------------------------------------------
### Plots
###--------------------------------------------------

break_vec <- seq(from = as.Date("1935-06-01"), to = as.Date("2015-06-01"), by = "year")
break_vec <- break_vec[seq(1, 80, 5)]

## Tiled monthly plot
p <- ggplot(data,
            aes(y = factor(month,
                           levels = c(12:1),
                           labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           ordered = TRUE),
                x = factor(year)))
p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") +
    scale_x_discrete(breaks = seq(1935, 2015, 5)) +
    scale_fill_viridis(option = "inferno") +
    labs(y = NULL) + 
    theme(legend.position = "top",
          legend.justification = "left",
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(face = "bold", size = rel(2)),
          plot.caption = element_text(size = 6)) +
    labs(x = "Year", title = "The Baby Boom",
         fill = "Average births per day per million people.",
         caption = "Data: Human Mortality Database; US Census Bureau.\nData Visualization: A Practical Introduction. Princeton University Press, 2018. http://socviz.co")


png("figures/births_monthly_tile.png", width = 1600, height = 320, res = 100)
print(p_tile)
dev.off()

pdf("figures/births_monthly_tile.pdf", width = 8, height = 3)
print(p_tile)
dev.off()



## Tiled monthly plot - vertical
p <- ggplot(data,
            aes(x = factor(month,
                           levels = c(1:12),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           ordered = TRUE),
                y = factor(year, levels = c(2015:1933), ordered = TRUE)))
p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") + labs(x = "", y = "") +
    scale_y_discrete(breaks = seq(1935, 2015, 5)) +
    scale_x_discrete(position = "top") +
    scale_fill_viridis(option = "inferno") +
    theme(legend.position = "top",
          plot.title = element_text(size = rel(0.8)),
          plot.subtitle = element_text(size = rel(0.6)),
          plot.caption = element_text(size = rel(0.4)),
          axis.text.y = element_text(size = rel(1)),
          axis.text.x = element_text(size = rel(0.7))) +
    labs(x = "", fill = "", title = "US Monthly Birth Rate 1933-2015",
         subtitle = "Average births per day per million people.",
         caption = "Kieran Healy (kieranhealy.org). Data: Human Mortality Database;\nUS Census Bureau; Aaron Penne (github.com/aaronpenne).")


png("figures/births_monthly_tile_vert.png", height = 1800, width = 320, res = 100)
print(p_tile)
dev.off()

pdf("figures/births_monthly_tile_vert.pdf", height = 16, width = 3)
print(p_tile)
dev.off()

### Haha patchwork makes this trivial
data_lon <- data_stl %>%
    as_tibble() %>%
    pivot_longer(cols = births_pct_day:remainder) %>%
    mutate(name = factor(name, levels = c("births_pct_day", "trend", "season_year"), 
                         ordered = TRUE))

break_vec <- seq(from=as.Date("1935-01-01"), to=as.Date("2015-01-01"), by="year")
break_vec <- break_vec[seq(1, 80, 5)]
break_labs <- stringr::str_remove(break_vec, "-01-01")


p_trends <- ggplot(data_lon, aes(x = date, y = value)) + 
    geom_line(color = "gray20") + 
    scale_x_date(breaks = break_vec, labels = break_labs, expand = c(0,0)) + 
    facet_col(~ name, space = "free", scales = "free") + 
    labs(y = NULL, x = NULL) + 
    theme(  strip.background = element_blank(),
            strip.text.x = element_blank()) + 
    labs(y = NULL, x = "Year")

p <- ggplot(data,
            aes(y = factor(month,
                           levels = c(12:1),
                           labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           ordered = TRUE),
                x = factor(year)))
p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") +
    scale_x_discrete(breaks = break_labs, labels = break_labs, position = "top") +
    scale_fill_viridis(option = "inferno") +
    labs(y = NULL, x = NULL) + 
    theme(legend.position = "top",
          legend.justification = "left",
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(face = "bold", size = rel(2)),
          plot.caption = element_text(size = 6), 
          )

out <- (p_tile / p_trends) + plot_layout(heights = c(30, 70)) + 
    labs(x = "Year")

ggsave("figures/okboomer_composite.pdf", plot = out, width = 18, height = 12)

tmp <- data_stl %>%
    filter(date > "1954-06-01" & date < "1956-01-01") 

## Seasonal call-outs
p_season_out <- data_stl %>%
    filter(date > "1954-06-01" & date < "1956-01-01") %>%
    ggplot(aes(x = date, y = season_year)) + 
    geom_line(color = "gray20", size = 1.2) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) + 
    labs(y = NULL, x = NULL) 

ggsave("figures/seasonal-1.pdf", plot = p_season_out, width = 3, height = 3)

p_season_out <- data_stl %>%
    filter(date > "1999-06-01" & date < "2001-01-01") %>%
    ggplot(aes(x = date, y = season_year)) + 
    geom_line(color = "gray20", size = 1.2) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) + 
    ylim(c(-5, 6)) +
    labs(y = NULL, x = NULL) 

ggsave("figures/seasonal-2.pdf", plot = p_season_out, width = 3, height = 3)



data_stl %>%
    mutate(month = month(date), 
           year = year(date)) %>%
    as_tibble() %>%
    group_by(month) %>%
    summarize(births = (mean(births_pct_day)))


