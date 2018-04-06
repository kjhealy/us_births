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
