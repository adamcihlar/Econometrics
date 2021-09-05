
#setwd('ukol02')
rm(list = ls())

# libraries
library(eurostat)
library(tidyverse)
library(zoo)
library(magrittr)
library(car)
library(stats4)
library(MASS)

# data
country <- "SI"
# CZ, SK, PL, HU

ur <- get_eurostat(id = "une_rt_q", 
                   time_format = "date"
)

pi_ind <- get_eurostat(id = "prc_hicp_midx",
                       time_format = "date",
)

gdp <- get_eurostat(id = "namq_10_gdp", 
                    time_format = "date"
)

ur %<>%
    dplyr::filter(s_adj == "SA",    # seasonally adjusted data
                  age == "Y15-74",
                  unit == "PC_ACT", # percentage of active population
                  sex == "T",       # both
                  geo == country    # country set in the beginning
    ) %>%
    arrange(time) %>%
    mutate(ur = values) %>%
    mutate(ur_1 = dplyr::lag(values, 1)) %>%
    dplyr::select(c("time", "ur"))

pi_ind %<>%
    dplyr::filter(unit == "I15",  # index (base 2015)
                  coicop == "CP00", # all items
                  geo == country,    # country set in the beginning
                  time >= '1996-01-01' # set fix beginning to control the rownumber filtering
    ) %>%
    arrange(time) %>%
    mutate(rn = row_number(time)) %>%
    dplyr::filter(rn %% 3 == 1) %>%
    mutate(pi = values) %>%
    dplyr::select(c("time", "pi")) %>%
    mutate(pi = pi/100)

gdp %<>%
    dplyr::filter(s_adj == 'SCA',
                  unit == 'CP_MNAC',
                  geo == country,
                  na_item == 'B1GQ'
    ) %>%
    inner_join(pi_ind, by = 'time') %>%
    mutate(rgdp = values / pi) %>%
    mutate(l_rgdp = log(rgdp)) %>%
    arrange(time) %>%
    dplyr::select(time, rgdp, l_rgdp)

dataset <- ur %>%
        inner_join(gdp, by = 'time')

save(dataset, country, file = "dataset.Rdata")
write_csv(dataset, path = 'dataset.csv')
rm(list = ls())



# modelling
load("dataset.Rdata")

# plots
dataset %>%
    ggplot(mapping = aes(x = time, y = l_rgdp)) +
    geom_line() +
    theme_bw() +
    labs(title = 'Log of Real GDP', x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))

dataset %>%
    ggplot(mapping = aes(x = time, y = ur)) +
    geom_line() +
    theme_bw() +
    labs(title = 'Unemployment Rate', x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))

# 2 - 3
# detrendovani, zadani je trochu zmateny - udelam trend podle rovnic 5 a 6 (lin polynom se zlomem)

.chow_test_polyn_trend <- function(x, break_time, ur_gdp, polyn, return_model = FALSE) {
    'return F value (equal to t value) and p value of Chow test of break in linear trend'
    dat <- tibble(x = x) %>%
        mutate(time_ind = seq_along(x),
               time_ind_change = (time_ind >= break_time) * (time_ind - break_time + 1),
               inter_change = time_ind >= break_time)
    if (ur_gdp == 'ur') {
        form <- 'x ~ inter_change'
    } else if (polyn == 2) {
        form <- str_c('x ~ time_ind + time_ind_change', str_c(str_c('I(', names(dat)[2:3]), '^2)', collapse = ' + '), sep = ' + ')
    } else form <- 'x ~ time_ind + time_ind_change'
    model <- dat %>%
        lm(formula = form, data = .)
    F_stat <- model %>%
        summary() %>% 
        coef() %>%
        .[nrow(.), 3:4]
    if (return_model) return(model)
    return(F_stat)
}

.qlr_test <- function(x, from, to, ur_gdp = 'ur', polyn = 1) {
    F_stats <- map_dbl(from:to, ~ .chow_test_polyn_trend(x, ., ur_gdp, polyn)[1])
    max_F_stat <- which.max(abs(F_stats))
    return(max_F_stat + from - 1)
}

from_10perc <- round(length(dataset$l_rgdp) * 0.1)
to_90perc <- round(length(dataset$l_rgdp) * 0.9)

break_time_gdp <- .qlr_test(dataset$l_rgdp, from = from_10perc, to = to_90perc, ur_gdp = 'gdp', polyn = 1)
break_time_gdp2 <- .qlr_test(dataset$l_rgdp, from = from_10perc, to = to_90perc, ur_gdp = 'gdp', polyn = 2)

break_time_ur <- .qlr_test(dataset$ur, from = from_10perc, to = to_90perc, ur_gdp = 'ur')

dataset$gdp_fitted <- fitted(.chow_test_polyn_trend(dataset$l_rgdp, break_time = break_time_gdp2, ur_gdp = 'gdp', polyn = 2, return_model = TRUE))
dataset$ur_fitted <- fitted(.chow_test_polyn_trend(dataset$ur, break_time = break_time_ur, ur_gdp = 'ur', polyn = 1, return_model = TRUE))

dataset %>%
    ggplot(aes(x = time, y = gdp_fitted)) +
    geom_line(linetype = 'longdash', color = 'steelblue', size = 1) +
    geom_line(aes(x = time, y = l_rgdp), size = 1) +
    labs(title = 'Log of Real GDP and trend', x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))

dataset %>%
    ggplot(aes(x = time, y = ur_fitted)) +
    geom_line(linetype = 'longdash', color = 'firebrick', size = 1) +
    geom_line(aes(x = time, y = ur), size = 1) +
    labs(title = 'Log of Real GDP and trend', x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.5))

# cycles
data <- tibble(
    time = dataset$time,
    y = residuals(.chow_test_polyn_trend(dataset$l_rgdp, break_time = break_time_gdp2, ur_gdp = 'gdp', polyn = 2, return_model = TRUE)) * 100,
    u = residuals(.chow_test_polyn_trend(dataset$ur, break_time = break_time_ur, ur_gdp = 'ur', polyn = 1, return_model = TRUE))
)

data %>%
    pivot_longer(c(u, y), names_to = 'var') %>%
    mutate(Variable = case_when(var == 'u' ~ 'Unemployment Rate', var == 'y' ~ 'Real GDP')) %>%
    ggplot(aes(x = time, y = value, group = Variable, color = Variable)) +
    geom_line(linetype = 'longdash', size = 1) +
    labs(title = 'Cyclical values', x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = c('firebrick', 'steelblue'))

# 4 - staticke OLS a Chow testy (+ 6)
data %>%
    lm(u ~ y - 1, .) %>%
    summary()

# "Klasicky" Chow test
.chow_static <- function(data, break_time_ind) {
    data %>%
        mutate(time_ind = seq_along(time)
               , D_y = y * (time_ind >= break_time_ind)) %>%
        lm(u ~ y + D_y - 1, .)
}

chow_F_static <- map_dbl(from_10perc:to_90perc, ~ .chow_static(data, break_time_ind = .) %>%
            summary() %>% # Chow v kazdem case (korme 10 % na obou stranach)
            coef() %>%
            .[2,3] %>% # t statistika druhe (umele) promenne odpovida F testu na shodu koeficientu pred a po
            abs())

break_time_static <- chow_F_static %>%
    which.max() + from_10perc - 1 # maximalni hodnota t statistiky
data$time[break_time_static] # obdobi nejpravdepodobnejsiho zlomu

summary(.chow_static(data, break_time_ind = break_time_static)) # model se zlomem podle Chow testu, koeficient po zlomu je soucet obou koeficientu

vcov4 <- .chow_static(data, break_time_ind = break_time_static) %>%
    summary() %>%
    vcov()
.chow_static(data, break_time_ind = break_time_static) %>%
    coef %>%
    sum 
std_after <- sqrt(vcov4[1,1] + vcov4[2,2] + 2*vcov4[2,1]) 


# Chow test of predictive failure
# meni se pocty stupnu volnosti pro F test - nejde koukat na F statistiky, ale az na p-hodnoty
.chow_pred_static <- function(data, break_time_ind){
    RSSE <- data %>%
        lm(u ~ y - 1, .) %>%
        deviance()
    SSE1 <- data %>%
        .[1:break_time_ind, ] %>%
        lm(u ~ y - 1, .) %>%
        deviance()
    F_stat <- (RSSE - SSE1) * (break_time_ind - 1) / (SSE1 * (nrow(data) - break_time_ind))
    return(F_stat)
}

# chow predikcni testy pro vsechna obdobi, krome prvniho a posledniho - vysledkem p-hodnoty
chow_pred_F_p_val <- map_dbl(2:97, ~ pf(.chow_pred_static(data, .), nrow(data) - ., . - 1, lower.tail = FALSE))

break_time_static_pred <- chow_pred_F_p_val %>%
    which.min() + 2 - 1
data$time[break_time_static_pred] # obdobi nejpravdepodobnejsiho zlomu

data %>%
    mutate(D_y = y * (seq_along(time) >= break_time_static_pred)) %>%
    lm(u ~ y + D_y - 1, .) %>%
    summary() # model se zlomem podle predpovedniho Chow testu, koeficient po zlomu je soucet obou koeficientu

vcov4b <- data %>%
    mutate(D_y = y * (seq_along(time) >= break_time_static_pred)) %>%
    lm(u ~ y + D_y - 1, .) %>%
    summary() %>%
    vcov()
sum_coef_b <- data %>%
    mutate(D_y = y * (seq_along(time) >= break_time_static_pred)) %>%
    lm(u ~ y + D_y - 1, .) %>%
    coef %>%
    sum

std_after_b <- sqrt(vcov4b[1,1] + vcov4b[2,2] + 2*vcov4b[2,1]) 

### hezky popsany rozdil mezi obema testy + odkaz na dalsi literaturu http://rpierse.esy.es/rpierse/files/qm7.pdf



# 5
data_dyn <- data %>%
    mutate(u1 = dplyr::lag(u, 1),
           u2 = dplyr::lag(u, 2),
           u3 = dplyr::lag(u, 3),
           u4 = dplyr::lag(u, 4),
           y1 = dplyr::lag(y, 1),
           y2 = dplyr::lag(y, 2),
           y3 = dplyr::lag(y, 3),
           y4 = dplyr::lag(y, 4))

factors <- c("u1", "y1", "u2", "y2", "u3", "y3", "u4", "y4")

create_form <- function(factors) {
    as.formula(paste("u ~", paste(factors, collapse = "+"), ' - 1'))
}

formulas <- map(1:4, ~ create_form(factors = factors[1:(2*.)]))

best_k <- which.min(unlist(map(formulas, ~ AIC(lm(., data = data_dyn))))) # AIC i BIC davaji zpozdeni 1
best_k <- which.min(unlist(map(formulas, ~ BIC(lm(., data = data_dyn)))))

lm(formulas[best_k][[1]], data = data_dyn) %>%
    summary()

dyn_coefs <- lm(formulas[best_k][[1]], data = data_dyn) %>%
    summary() %>%
    coef()

sum(dyn_coefs[c(2),1]) / (1 - sum(dyn_coefs[c(1),1])) # dlouhodoby koeficient (pro zpozdeni 1)

# 7 - strukturalni zmeny v GDP v dynamicke rovnici

# bereme uz jen jedno zpozdeni (podle predchozich vysledku)
.chow_dyn <- function(data, break_time_ind) {
    data %>%
        mutate(time_ind = seq_along(time)
               , D_y1 = y1 * (time_ind >= break_time_ind)) %>%
        lm(u ~ u1 + y1 + D_y1 - 1, .)
}

chow_F_dyn <- map(from_10perc:to_90perc, ~ .chow_dyn(data_dyn, break_time_ind = .) %>%
                             summary() %>% # Chow v kazdem case (korme 10 % na obou stranach)
                             coef() %>%
                             .[3,3] %>% # t statistika druhe (umele) promenne odpovida F testu na shodu koeficientu pred a po
                             abs())

break_time_dyn <- chow_F_dyn %>%
    which.max() + from_10perc - 1 # maximalni hodnota t statistiky
data$time[break_time_dyn] # obdobi nejpravdepodobnejsiho zlomu

summary(.chow_dyn(data_dyn, break_time_ind = break_time_dyn)) # dynamicky model se zlomem v gdp podle Chow testu, koeficient po zlomu je soucet obou koeficientu
dyn_coefs_break <- coef(summary(.chow_dyn(data_dyn, break_time_ind = break_time_dyn)))

sum(dyn_coefs_break[c(2),1]) / (1 - sum(dyn_coefs_break[c(1),1])) # dlouhodoby koeficient (pro zpozdeni 1) PRED
sum(dyn_coefs_break[c(2,3),1])  / (1 - sum(dyn_coefs_break[c(1),1])) # dlouhodoby koeficient (pro zpozdeni 1) PO

# 8 - strukturalni zmeny v dynamicke rovnici

.chow_dyn2 <- function(data, break_time_ind) {
    SSR_U <- data %>%
        mutate(time_ind = seq_along(time)
               , D_y1 = y1 * (time_ind >= break_time_ind)
               , D_u1 = u1 * (time_ind >= break_time_ind)) %>%
        lm(u ~ u1 + D_u1 + y1 + D_y1 - 1, .) %>%
        deviance()
    SSR_R <- data %>%
        lm(u ~ u1 + y1 - 1, .) %>%
        deviance()
    F_stat <- ((SSR_R - SSR_U) / 2) / (SSR_U / (nrow(data) - 4))
    pf(F_stat, 2, (nrow(data) - 4), lower.tail = FALSE)
}

chow_F_p_val_dyn2 <- map_dbl(from_10perc:to_90perc, ~ .chow_dyn2(data_dyn, .))
break_time_dyn2 <- which.min(chow_F_p_val_dyn2) + from_10perc - 1
data_dyn$time[break_time_dyn2]

data_dyn %>%
    mutate(time_ind = seq_along(time)
           , D_y1 = y1 * (time_ind >= break_time_dyn2)
           , D_u1 = u1 * (time_ind >= break_time_dyn2)) %>%
    lm(u ~ u1 + D_u1 + y1 + D_y1 - 1, .) %>%
    summary()

dyn_coefs_break2 <- data_dyn %>%
    mutate(time_ind = seq_along(time)
           , D_y1 = y1 * (time_ind >= break_time_dyn2)
           , D_u1 = u1 * (time_ind >= break_time_dyn2)) %>%
    lm(u ~ u1 + D_u1 + y1 + D_y1 - 1, .) %>%
    summary() %>%
    coef()

sum(dyn_coefs_break2[c(3),1]) / (1 - sum(dyn_coefs_break2[c(1),1])) # dlouhodoby koeficient (pro zpozdeni 1) PRED
sum(dyn_coefs_break2[c(3,4),1]) / (1 - sum(dyn_coefs_break2[c(1,2),1])) # dlouhodoby koeficient (pro zpozdeni 1) PO

# 9 - smerodatne chyby dlouhodobych odhadu
# a) simulacne
set.seed(13)
S <- 1000
one_sim_a <- function(model, struc_break) {
    'struc_break = FALSE / 1 / 2 -> 1 = zmena v y, 2 = zmena v obojim'
    if (struc_break == FALSE) {
        n <- 2
    } else if (struc_break == 1) {
        n <- 3
    } else {
        n <- 4
    }
    dyn_vcov <- vcov(summary(model))
    dyn_coefs <- coef(summary(model))
    L <- t(chol(dyn_vcov))
    coefs <- dyn_coefs[,1] + L %*% mvrnorm(n = n, mu = 0, Sigma = 1)
    if (struc_break == FALSE) {
        res <- sum(coefs[2]) / (1 - sum(coefs[1]))
    } else if (struc_break == 1) {
        res <- c(pred = sum(coefs[2]) / (1 - sum(coefs[1])), po = sum(coefs[2:3]) / (1 - sum(coefs[1])))
    } else if (struc_break == 2) {
        res <- c(pred =  sum(coefs[3]) / (1 - sum(coefs[1])), po = sum(coefs[3:4]) / (1 - sum(coefs[1:2])))
    }
    return(res)
}

# i - bez strukturalnich zmen
set.seed(13)
alphas_i <- map_dbl(1:S, ~ one_sim_a(lm(formulas[best_k][[1]], data = data_dyn), struc_break = FALSE))
mean(alphas_i)
sd(alphas_i)
quantile(alphas_i, c(0.025, 0.975))

# ii - strukturalni zmena v gdp
alphas_ii <- map_dfr(1:S, ~ one_sim_a(.chow_dyn(data_dyn, break_time_ind = break_time_dyn), struc_break = 1))
colMeans(as.matrix(alphas_ii)) # prumery pred a po
sd(alphas_ii$pred)
quantile(alphas_ii$pred, c(0.025, 0.975))
sd(alphas_ii$po)
quantile(alphas_ii$po, c(0.025, 0.975))

# iii - strukturalni zmena v gdp i ur
set.seed(3)
alphas_iii <- map_dfr(1:S, ~ one_sim_a(data_dyn %>%
                                       mutate(time_ind = seq_along(time)
                                              , D_y1 = y1 * (time_ind >= break_time_dyn2)
                                              , D_u1 = u1 * (time_ind >= break_time_dyn2)) %>%
                                       lm(u ~ u1 + D_u1 + y1 + D_y1 - 1, .), struc_break = 2))
colMeans(as.matrix(alphas_iii)) # prumery pred a po
sd(alphas_iii$pred)
quantile(alphas_iii$pred, c(0.025, 0.975))
sd(alphas_iii$po)
quantile(alphas_iii$po, c(0.025, 0.975))


# b) DELTA metoda
# i - bez strukturalnich zmen
dyn_coefs <- coef(summary(lm(formulas[best_k][[1]], data = data_dyn)))
dyn_vcov <- vcov(summary(lm(formulas[best_k][[1]], data = data_dyn)))

C_1 <- sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1]))^2 # derivace podle U
C_2 <- 1 / (1 - sum(dyn_coefs[1])) # derivace podle Y
C <- t(c(rep(C_1, 1), rep(C_2, 1))) # pozor jak ma R vektory - proto transpozice
a_Delta_cov <- C %*% dyn_vcov %*% t(C) 

sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1])) # prumer
sqrt(a_Delta_cov) # smerodatna odchylka dlouhodobeho vztahu (alpha)
qnorm(0.025, mean = sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))
qnorm(0.975, mean = sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))

# ii - strukturalni zmena v gdp
dyn_coefs <- coef(summary(.chow_dyn(data_dyn, break_time_ind = break_time_dyn)))
dyn_vcov <- vcov(summary(.chow_dyn(data_dyn, break_time_ind = break_time_dyn)))

# pred
C_1 <- sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1]))^2 # derivace podle U
C_2 <- 1 / (1 - sum(dyn_coefs[1])) # derivace podle Y
C <- t(c(rep(C_1, 1), rep(C_2, 1))) # pozor jak ma R vektory - proto transpozice
a_Delta_cov <- C %*% dyn_vcov[1:2,1:2] %*% t(C) 

sum(dyn_coefs[3]) / (1 - sum(dyn_coefs[1])) # prumer PRED
sqrt(a_Delta_cov) # smerodatna odchylka dlouhodobeho vztahu PRED
qnorm(0.025, mean = sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))
qnorm(0.975, mean = sum(dyn_coefs[2]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))

# po
C_1 <- sum(dyn_coefs[2:3]) / (1 - sum(dyn_coefs[1]))^2 # derivace podle U
C_2 <- 1 / (1 - sum(dyn_coefs[1])) # derivace podle Y
C <- t(c(rep(C_1, 1), rep(C_2, 2))) # pozor jak ma R vektory - proto transpozice
a_Delta_cov <- C %*% dyn_vcov %*% t(C) 

sum(dyn_coefs[2:3]) / (1 - sum(dyn_coefs[1])) # prumer PO
sqrt(a_Delta_cov) # smerodatna odchylka dlouhodobeho vztahu PO
qnorm(0.025, mean = sum(dyn_coefs[2:3]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))
qnorm(0.975, mean = sum(dyn_coefs[2:3]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))

# iii - strukturalni zmena v gdp i v ur
mdl <- data_dyn %>%
    mutate(time_ind = seq_along(time)
           , D_y1 = y1 * (time_ind >= break_time_dyn2)
           , D_u1 = u1 * (time_ind >= break_time_dyn2)) %>%
    lm(u ~ u1 + D_u1 + y1 + D_y1 - 1, .)

dyn_coefs <- coef(summary(mdl))
dyn_vcov <- vcov(summary(mdl))

# pred
C_1 <- sum(dyn_coefs[3]) / (1 - sum(dyn_coefs[1]))^2 # derivace podle U
C_2 <- 1 / (1 - sum(dyn_coefs[1])) # derivace podle Y
C <- t(c(rep(C_1, 1), rep(C_2, 1))) # pozor jak ma R vektory - proto transpozice
a_Delta_cov <- C %*% dyn_vcov[c(1,3),c(1,3)] %*% t(C) 

sum(dyn_coefs[3]) / (1 - sum(dyn_coefs[1])) # prumer PRED
sqrt(a_Delta_cov) # smerodatna odchylka dlouhodobeho vztahu PRED
qnorm(0.025, mean = sum(dyn_coefs[3]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))
qnorm(0.975, mean = sum(dyn_coefs[3]) / (1 - sum(dyn_coefs[1])), sd = sqrt(a_Delta_cov))

# po
C_1 <- sum(dyn_coefs[3:4]) / (1 - sum(dyn_coefs[1:2]))^2 # derivace podle U
C_2 <- 1 / (1 - sum(dyn_coefs[1:2])) # derivace podle Y
C <- t(c(rep(C_1, 2), rep(C_2, 2))) # pozor jak ma R vektory - proto transpozice
a_Delta_cov <- C %*% dyn_vcov %*% t(C) 

sum(dyn_coefs[3:4]) / (1 - sum(dyn_coefs[1:2]))
sqrt(a_Delta_cov) # smerodatna odchylka dlouhodobeho vztahu PO
qnorm(0.025, mean = sum(dyn_coefs[3:4]) / (1 - sum(dyn_coefs[1:2])), sd = sqrt(a_Delta_cov))
qnorm(0.975, mean = sum(dyn_coefs[3:4]) / (1 - sum(dyn_coefs[1:2])), sd = sqrt(a_Delta_cov))





