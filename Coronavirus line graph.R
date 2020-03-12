library(readr)  # for read_csv
library(knitr)  # for kable
library(tidyverse)
library(ggthemes)
library(gganimate)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(plotly)
library(directlabels)

## set work directory
setwd("C:/Users/inetc/OneDrive/Documents/R/Coronavirus")

## load data from GitHub
raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

## gather time into one column
confirmed_cases <- gather(raw,"time","count",-"Province/State", -"Country/Region", -"Lat",-"Long") %>% drop_na(count)

## set time to time format

confirmed_cases$time <- lubridate::mdy(confirmed_cases$time)


###

confirmed_cases1 <-  confirmed_cases %>% group_by(`Country/Region`,time) %>% mutate(sumcount = sum(count)) %>% ungroup()

confirmed_cases1 <-  confirmed_cases1 %>% mutate(log_sumcount = if_else(sumcount==0,0,log(sumcount)))


## filter

to_plot <- confirmed_cases1 %>% 
  filter(`Country/Region` %in% c("Italy", "Germany", "France","Korea, South","United Kingdom","Japan"))


# Plot
plot <- to_plot %>%
  ggplot( aes(x=time, y=log_sumcount, group=`Country/Region`, color=`Country/Region`)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis(discrete = TRUE) +
  ggtitle("Cases of Coronavirus") +
  theme_classic() +
  ylab("Number of cases (log)") +
  transition_reveal(time)

plot

## make interactive
ggplotly(plot)




################ Plot growth since 100th case

confirmed_cases2 <- filter(confirmed_cases1, sumcount>=100)

confirmed_cases2 <- group_by(confirmed_cases2,`Country/Region`) %>% mutate(date = min(time)) %>% ungroup()

confirmed_cases2 <- confirmed_cases2 %>% mutate(time_since_100 = time-date)


## filter countries

to_plot1 <- confirmed_cases2 %>% 
  filter(`Country/Region` %in% c("Italy", "Germany", "France","Korea, South","United Kingdom","Japan","China","Iran","Netherlands"))


# Plot
to_plot1 <- to_plot1 %>%
  ggplot( aes(x=time_since_100, y=sumcount, group=`Country/Region`, color=`Country/Region`)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis(discrete = TRUE) +
  labs(title="Growth of Coronavirus since passsing 100 cases") +
  theme_classic() +
  ylab("Number of cases (log scale)") +
  xlab("Days since passing 100 cases") +
  scale_y_continuous(trans = "log10",limits = c(10,100000),labels = scales::comma) +
  geom_dl(aes(label = `Country/Region`), method = list(dl.trans(x = x+0.2), "last.points")) + 
  transition_reveal(time)

to_plot1


