
setwd('ukol03')
rm(list = ls())

# libraries
library(OECD)
library(tidyverse)
library(mFilter)
library(lubridate)
library(zoo)

country <- 'DEU'

# consumer price index
pi_cpi <- get_dataset(dataset = 'PRICES_CPI', 
                     filter = list(c(country), 
                                   c('CPALTT01'), #all items
                                   c('IXOBSA'), #seasonally adjusted index
                                   c('Q'))) %>%
    mutate(pi_cpi = log(obsValue) - dplyr::lag(log(obsValue))) %>%
    select(obsTime, pi_cpi)

# gdp deflator
pi_def <- get_dataset(dataset = 'MEI', 
                      filter = list(c(country),
                                    c('NAGIGP01'), #expenditure approach
                                    c('IXOBSA'), #real gdp, seasonally adjusted
                                    c('Q'))) %>%
    mutate(pi_def = log(obsValue) - dplyr::lag(log(obsValue))) %>%
    select(obsTime, pi_def, obsValue)

# real gdp
gdp <- get_dataset(dataset = 'QNA', 
                   filter = list(c(country),
                                 c('B1_GA'), #expenditure approach
                                 c('LNBQRSA'), #real gdp, seasonally adjusted
                                 c('Q'))) %>%
    mutate(gap = hpfilter(log(obsValue), type = 'lambda', freq = 1600)$cycle) %>%
    select(obsTime, gap)

# unit labor cost
ulc <- get_dataset(dataset = 'ULC_EEQ', 
                   filter = list(c(country), 
                                 c('ULQEUL01'), 
                                 c('IXOBSA'), #seasonally adjusted index
                                 c('Q'))) %>%
    inner_join(pi_def, by = 'obsTime') %>%
    mutate(obsValue = (obsValue.x /obsValue.y)*100) %>% #get real values!
    mutate(ulc = hpfilter(log(obsValue), type = 'lambda', freq = 1600)$cycle) %>%
    select(obsTime, ulc, obsValue)

# interest rate spread    
ir <- get_dataset(dataset = 'MEI_FIN', 
                  filter = list(c('IR3TIB'), #3 months interest rate
                                c(country), 
                                c('Q')
                                )) %>%
    inner_join(get_dataset(dataset = 'MEI_FIN', 
                           filter = list(c('IRLT'), #10 years interest rate
                                         c(country), 
                                         c('Q')
                           )), by = 'obsTime') %>%
    mutate(spread = (obsValue.y - obsValue.x)/100) %>%
    select(obsTime, spread)

# wages inflation = hourly earnings (% changes) - gdp per emp person (productivity) (% changes)
pi_w <- get_dataset(dataset = 'EAR_MEI', 
                  filter = list(c('LCEAMN01_IXOBSA'), # hourly earnings, seasonally adjusted
                                c(country), 
                                c('Q')
                  )) %>%
    mutate(obsValue = log(obsValue) - dplyr::lag(log(obsValue))) %>%
    inner_join(get_dataset(dataset = 'ULC_EEQ', 
                           filter = list(c(country), 
                                         c('ULQELP01'), # gdp per emp person
                                         c('GPSA'), 
                                         c('Q')
                                         )), by = 'obsTime')%>%
    mutate(pi_w = obsValue.x - (obsValue.y/100)) %>%
    select(obsTime, pi_w)

# commodities index ~ total producer prices (manufacturing)
pi_c <- get_dataset(dataset = 'MEI_PRICES_PPI', 
                   filter = list(c('PIEAMP02'), 
                                 c(country), 
                                 c('GP'), 
                                 c('Q'))) %>%
    mutate(pi_c = obsValue/100) %>%
    select(obsTime, pi_c)


# full dataset
dataset <- pi_cpi %>%
            inner_join(pi_def, by = 'obsTime') %>%
            inner_join(gdp, by = 'obsTime') %>%
            inner_join(ulc, by = 'obsTime') %>%
            inner_join(ir, by = 'obsTime') %>%
            inner_join(pi_w, by = 'obsTime') %>%
            inner_join(pi_c, by = 'obsTime') %>%
    mutate(obsTime = as_date(as.yearqtr(obsTime, format = '%Y-Q%q')))

write_csv(dataset, path = 'dataset.csv')


# plots

var_labels <- c('Inflace spotøebitelských cen', 
                'Inflace deflátor HDP',
                'Mezera HDP', 
                'Inflace cen komodit', 
                'Mzdová inflace', 
                'Rozpìtí úrokové míry',
                'Mezní pracovní náklady')
names(var_labels) <- c('pi_cpi',
                       'pi_def',
                       'gap', 
                       'pi_c', 
                       'pi_w', 
                       'spread',
                       'ulc')

x_axis <- dataset$obsTime[seq(from = 4, to = length(dataset$obsTime), by = 16)]
x_axis_lab <- str_extract(x_axis, '^....')

dataset %>%
    pivot_longer(cols = c(#pi_cpi, 
                          pi_def, gap, ulc, spread, pi_w, pi_c)) %>%
    ggplot(aes(x = obsTime, 
               y = value, 
               group = 1)
           ) +
    coord_cartesian(xlim = c(min(dataset$obsTime)+180, max(dataset$obsTime)-180)) +
    geom_line(size = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue", size = 0.1) +
    
    theme_bw() +
    facet_wrap(. ~ name, 
               scales = 'free',
               labeller = labeller(name = var_labels),
               ncol = 2
               ) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text=element_text(size = 7),
          strip.text.x = element_text(size = 8),
          strip.background = element_rect(
              color = 'black', 
              fill = '#B5CDE1', 
              size = 0.1, 
              linetype = "solid")
          ) +
    scale_x_continuous(breaks = x_axis, 
                       labels = x_axis_lab)

ggsave(
    'time_series_wrap.pdf',
    plot = last_plot(),
    device = 'pdf',
    scale = 1,
    width = 18,
    height = 18,
    units = 'cm',
    dpi = 300,
    limitsize = TRUE,
)




dataset %>%
    pivot_longer(cols = c(#pi_cpi, 
                          pi_def, gap, ulc, spread, pi_w, pi_c)) %>%
    ggplot(aes(x = obsTime, 
               y = value, 
               group = 1)
    ) +
    coord_cartesian(xlim = c(min(dataset$obsTime)+365, max(dataset$obsTime)-365)) +
    geom_line(size = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue", size = 0.1) +
    
    theme_bw() +
    facet_grid(name ~ ., 
               scales = 'free',
               labeller = labeller(name = var_labels)
    ) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 7),
          strip.text.y = element_text(size = 7),
          strip.background = element_rect(
              color = 'black', 
              fill = '#B5CDE1', 
              size = 0.1, 
              linetype = "solid")
    ) +
    scale_x_continuous(breaks = x_axis, 
                       labels = x_axis_lab)

ggsave(
    'time_series_col.pdf',
    plot = last_plot(),
    device = 'pdf',
    scale = 1,
    width = 16,
    height = 22,
    units = 'cm',
    dpi = 300,
    limitsize = TRUE,
)

