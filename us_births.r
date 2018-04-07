###--------------------------------------------------
### Aaron Penne's population dataviz.
### Data forked from github.com/aaronpenne/data_visualization
###--------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(gtable)
## library(janitor)


## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

## Nice alignment solution for the STL plots, from Baptiste Auguié
## http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot/22984913
## (Probably don't really need this approach anymore, but I know it works.)
rbind_gtable_max <- function(...) {

    gtl <- list(...)
    stopifnot(all(sapply(gtl, gtable::is.gtable)))
    bind2 <- function(x, y) {
        stopifnot(ncol(x) == ncol(y))
        if (nrow(x) == 0)
            return(y)
        if (nrow(y) == 0)
            return(x)
        y$layout$t <- y$layout$t + nrow(x)
        y$layout$b <- y$layout$b + nrow(x)
        x$layout <- rbind(x$layout, y$layout)
        x$heights <- gtable:::insert.unit(x$heights, y$heights)
        x$rownames <- c(x$rownames, y$rownames)
        x$widths <- grid::unit.pmax(x$widths, y$widths)
        x$grobs <- append(x$grobs, y$grobs)
        x
    }

    Reduce(bind2, gtl)
}

draw_stl <- function(data_stl = data,
                     bar.width = 0.5,
                     p_col = "gray30") {
    theme_set(theme_minimal())

    break_vec <- seq(from=as.Date("1935-01-01"), to=as.Date("2015-01-01"), by="year")
    break_vec <- break_vec[seq(1, 80, 5)]

    p <- ggplot(data_stl, aes(x = date, y = births_pct_day))
    p1 <- p + geom_line(color = p_col) + ylab("Data") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)), plot.title = element_text(size = rel(1))) #+
#        ggtitle("US Births per month per million people")

    p <- ggplot(data_stl, aes(x = date, y = trend))
    p2 <- p + geom_line(color = p_col) + ylab("Trend") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data_stl, aes(x = date, y = seasonal))
    p3 <- p + geom_line(color = p_col) + ylab("Seasonal") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data_stl, aes(x = date, ymax = remainder, ymin = 0))
    p4 <- p + geom_linerange(size = bar.width, color = p_col) +
        scale_x_date(breaks = break_vec, date_labels = "%Y") +
        ylab("Remainder") + xlab("Year") +
        theme(axis.title.y = element_text(size = rel(0.8)))

    g1 <- ggplotGrob(p1)
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    g4 <- ggplotGrob(p4)
    out <- rbind_gtable_max(g1, g2, g3, g4)
    out
}


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
                        date = ymd(paste(year, month, 1, sep="-")))


## LOESS time-series decomposition
data_stl <- stl(ts(data$births_pct_day, start = c(1933, 1), frequency = 12), s.window = "periodic")
stl_df <- data.frame(data_stl$time.series)

data <- cbind(data, stl_df)


###--------------------------------------------------
### Plots
###--------------------------------------------------

theme_set(theme_minimal())

break_vec <- seq(from = as.Date("1935-01-01"), to = as.Date("2015-01-01"), by = "year")
break_vec <- break_vec[seq(1, 80, 5)]


## Trend line plot
p <- ggplot(data, aes(x = date, y = births_pct_day))
p_line <- p + geom_line(size = 0.6, color = "gray30") +
    scale_x_date(breaks = break_vec, date_labels = "%Y") +
    labs(x = "Date", y = "Monthly Births per Million Population",
         title = "US Average Monthly Births, 1933-2015.",
                       caption = "Data: US Census Bureau and Aaron Penne")

png("figures/births_monthly_line.png", width = 1600, height = 480, res = 100)
print(p_line)
dev.off()

pdf("figures/births_monthly_line.pdf", width = 9, height = 4)
print(p_line)
dev.off()

## Time series decomposition plot
pdf("figures/births_monthly_stl.pdf", width = 12, height = 5)
out <- draw_stl(bar.width = 0.5)
grid.draw(out)
dev.off()

png("figures/births_monthly_stl.png", width = 1600, height = 600)
out <- draw_stl(bar.width = 0.5)
grid.draw(out)
dev.off()


## Tiled monthly plot
p <- ggplot(data,
            aes(y = factor(month,
                           levels = c(12:1),
                           labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           ordered = TRUE),
                x = factor(year)))
p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") + labs(x = "", y = "") +
    scale_x_discrete(breaks = seq(1935, 2015, 5)) +
    scale_fill_viridis(option = "inferno") +
    theme(legend.position = "right", plot.caption = element_text(size = 6)) +
    labs(x = "Year", fill = "", title = "US Monthly Birth Rate 1933-2015",
         subtitle = "Average births per day per million people.",
         caption = "Kieran Healy (kieranhealy.org). Data: Human Mortality Database; US Census Bureau; Aaron Penne (github.com/aaronpenne).")


png("figures/births_monthly_tile.png", width = 1600, height = 320, res = 100)
print(p_tile)
dev.off()

pdf("figures/births_monthly_tile.pdf", width = 12, height = 2.5)
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


### Plot of population size
pop$AK <- stringr::str_replace(pop$AK, "N/A", "NA")
pop$HI <- stringr::str_replace(pop$HI, "N/A", "NA")
pop$AK <- as.integer(pop$AK)
pop$HI <- as.integer(pop$HI)


pop_lon <- pop %>% select(-US, -US_SUM) %>%
    gather(state, pop, AK:WY)

curr_pop <- pop_lon %>% filter(Year == 2017)
o <- order(curr_pop$pop)

p <- ggplot(pop_lon, aes(x = factor(Year),
                         y = factor(state, levels = curr_pop$state[o], ordered = TRUE)))

p_tile <- p + geom_tile(aes(fill = pop), color = "white") + labs(x = "", y = "") +
    scale_x_discrete(breaks = seq(1920, 2015, 5)) +
    scale_fill_viridis(option = "inferno", trans = "log",
                       labels = c("100k", "250k", "1m", "5m", "20m"),
                       breaks = c(1e5, 2.5e5, 1e6, 5e6, 20e6)) +
    guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             keywidth = 2,
                             nrow = 1)) +
    theme(legend.position = "top", legend.justification = "left",
          plot.caption = element_text(size = 6)) +
    labs(x = "Year", fill = "Population", title = "US State Populations, 1917-2017",
         subtitle = "States ordered from highest to lowest current population",
         caption = "Kieran Healy (kieranhealy.org). Data: US Census Bureau.")


pdf("figures/pop_all.pdf", width = 14, height = 9)
print(p_tile)
dev.off()



### Example of categorical variable for @surlyurbanist

## Cut population into 4 categories
pop_lon$pop_cat <- cut_interval(pop_lon$pop / 1000, 4, labels = c("1", "2", "3", "4"))

table(pop_lon$pop_cat)

p <- ggplot(pop_lon, aes(x = factor(Year),
                         y = factor(state, levels = curr_pop$state[o], ordered = TRUE)))

p_cat <- p + geom_tile(aes(fill = pop_cat), color = "white") + labs(x = "", y = "") +
    scale_x_discrete(breaks = seq(1920, 2015, 5)) +
    guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom",
                             keywidth = 2,
                             nrow = 1)) +
    theme(legend.position = "top", legend.justification = "left",
          plot.caption = element_text(size = 6)) +
    labs(x = "Year", fill = "Population", title = "Categorical Var. example")
