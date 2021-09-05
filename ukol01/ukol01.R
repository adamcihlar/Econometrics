rm(list = ls())

library(tidyverse)
library(tidyr)
library(lubridate)
library(quantmod)
library(car)


### Variables

#symbols <- read_csv('https://datahub.io/core/nasdaq-listings/r/nasdaq-listed-symbols.csv') # all companies from NASDAQ Composite

.from_date <- '2013-12-01'
.to_date <- '2021-02-28'

.M3_tres <- 'TB3MS'
.stockmarket <- '^NDX'
.symbols <- c('ATVI', 'AMD','ADBE','ALGN','ALXN','AMZN','AMGN','AEP','ADI','ANSS',
             'AAPL','AMAT','ASML','TEAM','ADSK','ADP','AVGO','BIDU','BIIB','BMRN',
             'BKNG','CDNS','CDW','CERN','CHKP','CHTR','CPRT','CTAS','CSCO','CMCSA',
             'COST','CSX','CTSH','DOCU','DXCM','DLTR','EA','EBAY','EXC','FAST','FB',
             'FISV','FOX','FOXA','GILD','GOOG','GOOGL','ILMN','INCY','INTC','INTU',
             'ISRG','MRVL','IDXX','JD','KDP','KLAC','KHC','LRCX','LULU','MELI','MAR',
             'MTCH','MCHP','MDLZ','MRNA','MNST','MSFT','MU','MXIM','NFLX','NTES','NVDA',
             'NXPI','OKTA','ORLY','PAYX','PCAR','PDD','PTON','PYPL','PEP','QCOM','REGN',
             'ROST','SIRI','SGEN','SPLK','SWKS','SBUX','SNPS','TCOM','TSLA','TXN','TMUS',
             'VRSN','VRSK','VRTX','WBA','WDAY','XEL','XLNX','ZM') 
            # NASDAQ 100 companies as of 2021-03-17



### Functions

.select_and_filter <- function(title_df, from,  to, only_filter = FALSE) {
    
    if (only_filter) {
        result <- title_df %>%
            as_tibble(rownames = 'date') %>%                        # index to column
            mutate(date = as_date(date),
                   year_month = format(date, format = "%Y-%m")) %>% # convert
            filter(date >= from, date <= to)                        # filter
        
    } else {
        
        # get list of last trading days in months of the particular company
        last_trading_day <- title_df %>%
            as_tibble(rownames = 'date') %>%                        # index to column
            mutate(date = as_date(date)) %>%                        # convert
            select(date, contains('.Close')) %>%                    # select cols
            drop_na() %>%                                           # drop rows with missing values
            filter(date >= from, date <= to) %>%                    # filter date
            mutate(year_month = format(date, format = "%Y-%m")) %>% # substract year-month
            group_by(year_month) %>%                                # group by months
            summarise(last_day = max(date))                         # get last days
        
        # inner join last days with values to get last  
        result <- title_df %>%
            as_tibble(rownames = 'date') %>%
            mutate(date = as_date(date)) %>%
            inner_join(last_trading_day, by = c('date' = 'last_day')) %>%
            select(date, year_month, contains('.Close'))
    }
    return(result)
}

.get_perc_growth <- function(title_df, r_index_j_m = 'j') {
    
    r_ind <- paste('r_', r_index_j_m, sep = '')
        
    result <- title_df %>%
        mutate(diff_log = unlist(log(.[,3]) - dplyr::lag(log(.[,3]), n = 1))) %>% # proc se z toho udelal list v ramci tibblu netusim
        select(year_month, contains('diff_log'))
    
    colnames(result) <- c('year_month', r_ind)
        
    return(result)
}


# 1)

### Get stock prices

getSymbols(Symbols = .symbols,
           src = 'yahoo')

titles <- ls(all.names = FALSE)

full_list <- map(titles, ~ eval(as.name(.)))# prochazi promennou titels, ty ktere se uspesne stahly, 
                                            # se jako stringy sypou do eval(as.name(.)) 
                                            # -> vyhodnoti string jako kus kodu
names(full_list) <- titles  # pojmenovat casti listu

stock_returns <- map(full_list, ~ .select_and_filter(title_df = ., 
                                                 from = .from_date, 
                                                 to = .to_date, 
                                                 only_filter = FALSE)) %>%
                 map(., ~ .get_perc_growth(title_df = .,
                                        r_index_j_m = 'j'))

chartSeries(AMZN,
            subset = paste(.from_date, '/', sep=''),
            bar.type = 'ohlc',
            theme = chartTheme('white'))



### Get stock market prices

getSymbols(.stockmarket,  # NASDAQ 100
           src = 'yahoo')

market <- NDX %>% # nemuzu pres eval - ^NDX se importuje jako NDX
        .select_and_filter(.,
                      from = .from_date, 
                      to = .to_date, 
                      only_filter = FALSE) %>%
        .get_perc_growth(r_index_j_m = 'm')



### Get 3-Month Treasury Bill rates

getSymbols(.M3_tres,
           src = 'FRED')

base_ir <- eval(as.name(.M3_tres)) %>%
        .select_and_filter(., from = .from_date, to = .to_date, only_filter = TRUE) %>%
        mutate(r_f = TB3MS / (12*100)) %>%
        select(year_month, r_f)


### Kompletni dataset
data <- map(stock_returns, ~ inner_join(., market, by = c('year_month' = 'year_month'))) %>%
        map(., ~ inner_join(., base_ir, by = c('year_month' = 'year_month'))) %>%
        map(., ~ drop_na(.))
    

# drop titles with series shorter than 48Q
temp <- data
data <- vector(mode = "list", length = length(temp))
names(data) <- names(temp)
for (i in 1:length(temp)) {
  if (nrow(temp[[i]]) >= 48) {
    data[i] <- temp[i]
  }
}
if (sum(unlist(map(data, is.null))) > 0) { # if empty parts of the list exist
  data <- data[-which(unlist(map(data, is.null)))] # drop them
}


### data pro Gretl - jedno csv - sloupec = jedna akcie
for (i in 1:length(data)) {
  
  if (i == 1) {
    dataset <- data[[i]]
    colnames(dataset)[2] <- names(data)[i]
  } else {
    temp_table <- data[[i]][,1:2]
    colnames(temp_table)[2] <- names(data)[i]
    
    dataset <- dataset %>%
      left_join(temp_table, by = c('year_month' = 'year_month'), keep = FALSE)
  }
}

write_csv(dataset, path = 'dataset.csv')


# save the data, drop everything, load
save(data, file = 'CAPM_dataset.Rdata')
rm(list = ls())
load('CAPM_dataset.Rdata')


### model pro kazdy titul
modely <- map(data, 
        ~ lm(I(r_j - r_f) ~ I(r_m - r_f), .) %>%
          summary())


# 2)

### beta koeficienty
beta_coefs <- map_dfr(data, 
                      ~ lm(I(r_j - r_f) ~ I(r_m - r_f), .) %>%
                        summary() %>%
                        coef() %>%
                        .[2,1]) %>%
                  t() %>%
                  data.frame(beta = .) %>%
                  as_tibble(rownames = 'title')

# charakteristicke krivky prvnich 12 titulu
for (i in 1:12) {  
  x11()
  p <- data[[i]] %>%
    ggplot(mapping = aes(x = I(r_m - r_f), y = I(r_j - r_f))) +
    geom_point() +
    geom_smooth(method = 'lm',
                formula = 'y ~ x',
                se = FALSE,
                size = 0.5,
                color = 'red') +
    coord_cartesian(xlim = c(-0.2, 0.2),
                    ylim = c(-0.2, 0.2)) +
    theme_bw()
  print(p)
}



# 3)

### urovnove konstanty
intercepts <- map_dfr(data, 
                      ~ lm(I(r_j - r_f) ~ I(r_m - r_f), .) %>%
                        summary() %>%
                        coef() %>%
                        .[1,c(1,4)]) %>%
                  cbind(title = names(data)) %>%
                  as_tibble() %>%
                  rename(., intercept = Estimate, pval = `Pr(>|t|)`) %>%
                  select(title, intercept, pval)

# nektere jsou statisticky i vecne vyznamne od nuly (napr. NVDA) 
# - akcie roste za dane obdobi prumerne statisticky vyznamne rychleji/pomaleji nez trh
# dlouhodobe by se nejspis nemelo stavat 

# prvni 4 statisticky vyznamne na 5 %
beta_coefs %>%
  left_join(intercepts, by = c('title')) %>%
  arrange(pval)


# 4)
# a) portfolio = 1 akcie
capm_data <- map_dbl(data, ~ mean(.$r_j) - mean(.$r_f)) %>% # prumerne vynosy akcii (minus base ir)
      as_tibble(rownames = 'title') %>%
      rename(r_p = value) %>%
      left_join(beta_coefs, by = 'title') %>% # join s beta koeficienty
      arrange(beta)

capm_model <- capm_data %>%
      lm(r_p ~ beta, .) %>%
      summary()

# trzni rizikova premie - celkem odpovida gamma_1 - dala by se testovat stat vyznamnost rozdilnosti
mean(data[[1]]$r_m - data[[1]]$r_f)

# b) portfolio = 4 akcie
capm_data_grouped <- capm_data %>%
      mutate(group_nr = row_number() %/% 4) %>%
      group_by(group_nr) %>%
      summarise(beta = mean(beta),
                r_p = mean(r_p))

capm_model_grouped <- capm_data_grouped %>%
      lm(r_p ~ beta, .) %>%
      summary()
# trzni rizikova premie, v podstate = gamma_1
mean(data[[1]]$r_m - data[[1]]$r_f)


# Welch t-test na stejny prumer (nepredpokladame stejny rozptyl)
mean_diff <- coef(capm_model_grouped)[2,1] - mean(data[[1]]$r_m - data[[1]]$r_f) # rozdil vyberovych prumeru

t_stat <- mean_diff / sqrt((coef(capm_model_grouped)[2,2]^2 / (capm_model_grouped$df[2] + 1)) + (var(data[[1]]$r_m - data[[1]]$r_f) / length(data[[1]]$r_m)))
df <- ((coef(capm_model_grouped)[2,2]^2 / (capm_model_grouped$df[2] + 1)) + (var(data[[1]]$r_m - data[[1]]$r_f) / length(data[[1]]$r_m)))^2 /
  (coef(capm_model_grouped)[2,2]^4 / ((capm_model_grouped$df[2] + 1)^2 * capm_model_grouped$df[2]) + (var(data[[1]]$r_m - data[[1]]$r_f)^2 / (length(data[[1]]$r_m)^2 * (length(data[[1]]$r_m)-1))))



(pt(t_stat, df, lower.tail = TRUE) < 0.5) * pt(t_stat, df, lower.tail = TRUE) * 2 +
  (pt(t_stat, df, lower.tail = FALSE) < 0.5) * pt(t_stat, df, lower.tail = FALSE) * 2

# security market line
capm_data_grouped %>% 
  ggplot(mapping = aes(x = beta, y = r_p)) +   # pro zgrupovana data
  geom_point(fill = 'steelblue',
             size = 4,
             shape = 21) +
  geom_smooth(method = 'lm',
              formula = 'y ~ x',
              se = FALSE,
              size = 1.2,
              color = 'darkblue') + 
  geom_point(data = capm_data,                 # pro samostatne tituly
             mapping = aes(x = beta, y = r_p),
             fill = 'darkorange',
             size = 2,
             shape = 21) +
  geom_smooth(data = capm_data,
              mapping = aes(x = beta, y = r_p),
              method = 'lm',
              formula = 'y ~ x',
              se = FALSE,
              size = 0.8,
              color = 'firebrick') +
  coord_cartesian(xlim = c(0.0, 1.75),
                  ylim = c(-0.015, 0.04)) +
  theme_bw()



# 5)

# pro dalsi casti se bude hodit dat data do jednoho velkyho tibblu 
# kvuli prumerovani vynosu pro jednotlive tituly za urcita obdobi

title_to_rows <- function(title_df) {
    title <- names(title_df)
    result <- title_df[[1]] %>%
        mutate(title = title) %>%
        select(title, year_month, r_j, r_m, r_f)
    return(result)
}

data_one_table <- map_dfr(seq_along(data), ~ title_to_rows(data[.]))

chow_test <- function(title_df, break_time) {
    # staci pridat umelou promennou pro obdobi po zlomu a testovat jeji nulovost
    # stejny jako kdybych omezil i puvodni promennou (na obdobi pred zlomem) a testoval rovnost
    Chow_stat <- title_df %>%
        mutate(j = r_j - r_f,
               m = r_m - r_f,
               m_break = (.$year_month >= break_time) * m) %>%
        lm(j ~ m + m_break , .) %>%
        summary() %>%
        coef()
    Chow_stat <- c(Chow_stat[2:3,1],Chow_stat[3,3:4]) %>%
        set_names(c('beta_before', 'beta_after', 't_val' , 'p_val')) # t statistika (= F statistika) Chow testu
    
    return(Chow_stat)
}

qlr_test <- function(title_df, from, to) { # title_df musi byt ve forme list (se jmenem titulu)
    
    title <- names(title_df)
    title_df <- title_df[[1]]
    
    break_times <- title_df[title_df$year_month >= from & title_df$year_month <= to,]$year_month
    Chow_stats <- map_dfr(break_times, ~ chow_test(title_df, .)) %>%
        bind_cols(break_times = break_times, .) %>%
        mutate(sign_flag = (p_val <= 0.05),
               title = title)

    return(Chow_stats)
}

beta_breaks <- map_dfr(seq_along(data), ~ qlr_test(title_df = data[.], from = data[.][[1]]$year_month[11], to = '2020-03'))
# from = data[.][[1]]$year_month[11] -> ruzne dlouhe casove rady, z testovani vynechavam vzdy prvnich 10 obdobi

struc_break <- beta_breaks %>% # 2019-02, jen o 1 mene pak 2020-02
    group_by(break_times) %>%
    summarise(null_rejected_count = sum(sign_flag)) %>%
    #.[.$null_rejected_count == 19,] %>%
    #.[2,]
    .[which.max(.$null_rejected_count),]

# data pro capm pred zlomem
capm_data_before_grouped <- struc_break %>%
        inner_join(beta_breaks, by = 'break_times') %>% # vyberu modely akcii, kde zlom je v identifikovanem obdobi
        inner_join(data_one_table, by = 'title') %>% # join vsech dat kvuli spocitani prumernych vynosu pred/po zlomu
        filter(year_month < break_times) %>% # data pouze pred zlomem
        group_by(title, beta_before, beta_after, t_val, p_val) %>% 
        summarise(r_p = mean(r_j - r_f)) %>% # prumerne vysnosy pro kazdou akcii pro obdobi pred zlomem
        arrange(beta_before) %>% # seradim podle bet pred zlomem
        ungroup() %>% # odgrupovat kvuli row_number
        mutate(group_nr = row_number() %/% 4) %>%
        group_by(group_nr) %>% # rozradim do skupin po 4
        summarise(beta_before = mean(beta_before), # a spocitam pro skupiny prumerne bety a vynosy
                  r_p = mean(r_p))

# data pro capm po zlomu - analogicky
capm_data_after_grouped <- struc_break %>%
    inner_join(beta_breaks, by = 'break_times') %>%
    inner_join(data_one_table, by = 'title') %>%
    filter(year_month >= break_times) %>% # data pouze po zlomu
    group_by(title, beta_before, beta_after, t_val, p_val) %>% 
    summarise(r_p = mean(r_j - r_f)) %>% # prumerne vysnosy pro kazdou akcii pro obdobi pred zlomem
    arrange(beta_before) %>% # seradim podle bet po zlomu
    ungroup() %>% # odgrupovat kvuli row_number
    mutate(beta_after = beta_before + beta_after) %>%
    arrange(beta_after) %>%
    mutate(group_nr = row_number() %/% 4) %>%
    group_by(group_nr) %>%
    summarise(beta_after = mean(beta_after),
              r_p = mean(r_p))


# capm pred zlomem
capm_model_before <- capm_data_before_grouped %>%
    lm(r_p ~ beta_before, .) %>%
    summary()

r_before <- data[[1]] %>%
  filter(year_month < struc_break$break_times) %>%
  mutate(r = r_m - r_f) %>%
  select(r) %>%
  as_vector()

# Welch t-test na stejny prumer (nepredpokladame stejny rozptyl) (gamma_1 = trzni vynis za dane obdobi)
mean_diff <- coef(capm_model_before)[2,1] - mean(r_before) # rozdil vyberovych prumeru

t_stat <- mean_diff / sqrt((coef(capm_model_before)[2,2]^2 / (capm_model_before$df[2] + 1)) + (var(r_before) / length(r_before)))
df <- ((coef(capm_model_before)[2,2]^2 / (capm_model_before$df[2] + 1)) + (var(r_before) / length(r_before)))^2 /
  (coef(capm_model_before)[2,2]^4 / ((capm_model_before$df[2] + 1)^2 * capm_model_before$df[2]) + (var(r_before)^2 / (length(r_before)^2 * (length(r_before)-1))))

(pt(t_stat, df, lower.tail = TRUE) < 0.5) * pt(t_stat, df, lower.tail = TRUE) * 2 +
  (pt(t_stat, df, lower.tail = FALSE) < 0.5) * pt(t_stat, df, lower.tail = FALSE) * 2


# capm po zlomu    
capm_model_after <- capm_data_after_grouped %>%
  lm(r_p ~ beta_after, .) %>%
  summary()

r_after <- data[[1]] %>%
  filter(year_month >= struc_break$break_times) %>%
  mutate(r = r_m - r_f) %>%
  select(r) %>%
  as_vector()

# Welch t-test na stejny prumer (nepredpokladame stejny rozptyl) (gamma_1 = trzni vynis za dane obdobi)
mean_diff <- coef(capm_model_after)[2,1] - mean(r_after) # rozdil vyberovych prumeru

t_stat <- mean_diff / sqrt((coef(capm_model_after)[2,2]^2 / (capm_model_after$df[2] + 1)) + (var(r_after) / length(r_after)))
df <- ((coef(capm_model_after)[2,2]^2 / (capm_model_after$df[2] + 1)) + (var(r_after) / length(r_after)))^2 /
  (coef(capm_model_after)[2,2]^4 / ((capm_model_after$df[2] + 1)^2 * capm_model_after$df[2]) + (var(r_after)^2 / (length(r_after)^2 * (length(r_after)-1))))

(pt(t_stat, df, lower.tail = TRUE) < 0.5) * pt(t_stat, df, lower.tail = TRUE) * 2 +
  (pt(t_stat, df, lower.tail = FALSE) < 0.5) * pt(t_stat, df, lower.tail = FALSE) * 2


# -> na kratsim obdobi se capm potvrzuje tak napul, na puvodnim celem ano


# strukturalni zlomy pro jednotlive akcie - statisticka vyznamnost
struc_break %>%
  inner_join(beta_breaks, by = 'break_times') %>%
  select(title, beta_before, beta_after, t_val, p_val) %>%
  arrange(p_val)



# 6)

data_asym <- map(seq_along(data), ~ data[[.]] %>%
            mutate(j = r_j - r_f,
                   m = r_m - r_f,
                   m_plus = r_m - r_f, # tohle pojmenovani je tady trochu zavadejici, ale pro jmena koeficientu nasledne regrese dava smysl
                   m_diff = (m < 0) * m
                   #,m_plus = (m > 0) * m
                   ) %>%
            select(year_month, j, m_plus, m_diff)
        ) %>%
        set_names(names(data))

assymetry_models <- map(data_asym,
                        ~ lm(j ~ m_plus + m_diff, .) %>%
                            summary())

get_coef <- function(model) {
    title <- names(model)
    coefs <- coef(model[[1]])
    result <- c(coefs[2:3,1], coefs[3,3:4]) %>%
        set_names(c('beta_plus', 'beta_diff', 't_val' , 'p_val'))
    return(result)
}

capm_data_assymetry <- map_dfr(seq_along(assymetry_models), ~ get_coef(assymetry_models[.])) %>%
        mutate(title = names(assymetry_models),
               beta_minus = beta_plus + beta_diff) %>% # koeficienty z regresi
        select(title, beta_plus, beta_minus, beta_diff, t_val, p_val) %>% 
        inner_join(data_one_table, by = 'title') %>%
        mutate(m = r_m - r_f,
               r_p = r_j - r_f,
               r_p_plus = (m > 0) * r_p,
               r_p_minus = (m < 0) * r_p) %>%
        group_by(title, beta_plus, beta_minus, beta_diff, t_val, p_val) %>%
        summarise(r_p_plus = mean(r_p_plus),
                  r_p_minus = mean(r_p_minus)) %>% 
        arrange(beta_plus) %>%
        ungroup()


capm_data_assymetry %>%
    arrange(p_val)
# 11 (5) akcii se chova statisticky vyznamne jinak v obdobi poklesu a rustu celeho trhu
capm_data_assymetry[capm_data_assymetry$p_val <= 0.1, ]


capm_data_assymetry_grouped_plus <- capm_data_assymetry %>%
    mutate(group_nr = row_number() %/% 4) %>%
    group_by(group_nr) %>%
    summarise(beta_plus = mean(beta_plus),
              r_p_plus = mean(r_p_plus),
              beta_minus = mean(beta_minus),
              r_p_minus = mean(r_p_minus))

# v obdobi rustu volatilnejsi akcie rostou rychleji 
capm_data_assymetry_grouped_plus %>%
    lm(r_p_plus ~ beta_plus , .) %>%
    summary()


capm_data_assymetry_grouped_minus <- capm_data_assymetry %>%
  arrange(beta_minus) %>%
  mutate(group_nr = row_number() %/% 4) %>%
  group_by(group_nr) %>%
  summarise(beta_plus = mean(beta_plus),
            r_p_plus = mean(r_p_plus),
            beta_minus = mean(beta_minus),
            r_p_minus = mean(r_p_minus))

# v obdobi poklesu agrsivnejsi akcie klesaji vice nez defenzivni
capm_data_assymetry_grouped_minus %>%
    lm(r_p_minus ~ beta_minus , .) %>%
    summary()

# security market line
pivot1 <- capm_data_assymetry_grouped %>%
    pivot_longer(cols = c(beta_plus, beta_minus), names_to = 'Trh', values_to = 'beta') %>%
    mutate(Trh = case_when(Trh == 'beta_minus' ~ 'klesá', Trh == 'beta_plus' ~ 'roste',)) %>%
    select(group_nr, Trh, beta)

pivot2 <- capm_data_assymetry_grouped %>%
    pivot_longer(cols = c(r_p_plus, r_p_minus), names_to = 'Trh', values_to = 'r_p') %>%
    mutate(Trh = case_when(Trh == 'r_p_minus' ~ 'klesá', Trh == 'r_p_plus' ~ 'roste',)) %>%
    select(group_nr, Trh, r_p)

# security Trh line pro obdobi rustu a poklesu trhu
pivot1 %>%
  inner_join(pivot2, by = c('group_nr', 'Trh')) %>%
  select(group_nr, Trh, beta, r_p) %>%
  ggplot(mapping = aes(x = beta, y = r_p, group = Trh)) +   
  geom_point(aes(fill = Trh, shape = Trh),
             size = 2.5,
             color = 'black') +
  geom_smooth(aes(color = Trh),
              method = 'lm',
              formula = 'y ~ x',
              se = TRUE,
              size = 1.2,
              linetype = 'longdash'
              ) +
  scale_shape_manual(values = c(25, 24)) +
  scale_color_manual(values = c('#C62121', '#0ABB00')) +
  scale_fill_manual(values = c('#D5212E', 3)) +
  labs(title = 'Security market line podle poklesu/rùstu trhu',x = 'Beta', y = 'Riziková prémie') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))





# 7)

get_coef_7 <- function(model) {
    title <- names(model)
    coefs <- coef(model[[1]])
    var_e <- var(model[[1]]$residuals)
    result <- c(coefs[2,1], var_e) %>%
        set_names(c('beta', 'var_e'))
    return(result)
}

capm_data_Fama <- map_dfr(seq_along(modely), ~ get_coef_7(modely[.])) %>%
    mutate(title = names(modely),
           beta2 = beta ^ 2) %>%
    inner_join(capm_data, by = 'title') %>%
    select(title, r_p, beta = beta.x, beta2, var_e)

capm_data_Fama %>%
    lm(r_p ~ beta + beta2 + var_e, .) %>%
    summary()
# vsechny koeficienty nevyznamne, takze asi ok

capm_data_Fama %>%
  lm(r_p ~ beta, .) %>%
  summary()
# regrese bez beta2 a var_e ma vyssi Adjusted R2



# charakteristicke krivky
n_titles <- 10

data_one_table %>%
  filter(title %in% names(data)[1:n_titles]) %>%
  inner_join(beta_coefs, by = 'title') %>%
  mutate(j = r_j - r_f,
         m = r_m - r_f,
         Akcie = factor(title, levels = c(arrange(beta_coefs[1:n_titles,], desc(beta))[,1])$title)) %>%
  arrange(beta) %>%
  select(Akcie, j, m) %>%
  ggplot(mapping = aes(x = m, y = j, group = Akcie)) +
  geom_smooth(aes(color = Akcie),
              method = 'lm',
              formula = 'y ~ x',
              se = FALSE,
              linetype = 'longdash') +
  scale_color_brewer(palette = 'RdYlGn') +
  labs(title = 'Charakteristické køivky vyrbaných akcií',x = 'Výnos trhu', y = 'Výnos akcie') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))




# security market line pred a po zlomu
capm_data_before_grouped %>%
  mutate(Období = 'Pøed zlomem',
         beta = beta_before) %>%
  bind_rows(capm_data_after_grouped %>%
              mutate(Období = 'Po zlomu',
                     beta = beta_after)) %>%
  select(group_nr, r_p, Období, beta) %>%
  ggplot(mapping = aes(x = beta, y = r_p, group = Období)) +   
  geom_point(aes(fill = Období, shape = Období),
             size = 3,
             color = 'black') +
  geom_smooth(aes(color = Období),
              method = 'lm',
              formula = 'y ~ x',
              se = TRUE,
              size = 1.2,
              linetype = 'longdash'
  ) +
  scale_shape_manual(values = c(21, 21)) +
  scale_color_manual(values = c('#B79906', '#026AA7')) +
  scale_fill_manual(values = c('#FCCF3E', '#5BA4CF')) +
  labs(title = 'Security market line pøed a po zlomu',x = 'Beta', y = 'Riziková prémie') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


data_one_table %>%
  filter(title %in% names(data)[1:n_titles]) %>%
  select(Akcie = title, year_month, r_j) %>%
  inner_join(beta_coefs, by = c('Akcie' = 'title')) %>%
  bind_rows(bind_cols(Akcie = 'Trh' , year_month = data[[1]]$year_month, r_j = data[[1]]$r_m)) %>%
  bind_rows(bind_cols(Akcie = 'Bezriziková úroková míra' , year_month = data[[1]]$year_month, r_j = data[[1]]$r_f)) %>%
  mutate(year_month = as_date(str_c(year_month, '-01'))) %>%
  arrange(beta) %>%
  ggplot(mapping = aes(x = year_month, y = r_j, color = Akcie)) +
  geom_line() +
  #scale_color_brewer(palette = 'RdYlGn') +
  labs(title = 'Výnosy',x = NULL, y = 'Výnos') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))







