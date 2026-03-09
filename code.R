library(tidyverse)
library(patchwork)
library(gt)
library(zoo)
library(tseries)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(plm)
library(car)
library(stargazer)

# Import gold and silver price data
silver <- read.csv('chart_20260219T160151.csv', sep = ';', dec = ',')
gold <- read.csv('chart_20260219T160403.csv', sep = ';', dec = ',')

# Convert dates
silver$Date <- as.Date(silver$Date, format = c('%m/%d/%Y'))
gold$Date <- as.Date(gold$Date, format = c('%m/%d/%Y'))

# Merge datasets
commodities <- merge(gold, silver, by = 'Date')
colnames(commodities) <- c('date','gold', 'silver')

par(mfrow = c(2, 1))

# Gold price (log) plot
p1 <- ggplot(aes(date, log(gold)), data = commodities) +
  geom_line() +
  geom_smooth(method = 'lm', fullrange=F) +
  theme_classic() +
  labs(
    x = 'Fecha',
    y = 'Precio oro log'
  )

# Silver price (log) plot
p2 <- ggplot(aes(date, log(silver)), data = commodities) +
  geom_line() +
  geom_smooth(method = 'lm', fullrange=F) +
  theme_classic() +
  labs(
    x = 'Fecha',
    y = 'Precio plata log'
  )

p1 / p2

# Correlation between gold and silver
cor(commodities$gold, commodities$silver)

# Import macroeconomic data
ip <- read.csv('INDPRO1.csv')
unemp <- read.csv('UNRATE.csv')

summary(ip)

# Convert dates
ip$date <- as.Date(ip$observation_date, format = c('%Y-%m-%d'))
unemp$date <- as.Date(unemp$observation_date, format = c('%Y-%m-%d'))

# Industrial production plot
ggplot(ip, aes(date, INDPRO)) +
  geom_line() +
  geom_smooth(method = 'lm') +
  theme_classic() + 
  labs(
    x = 'Fecha',
    y = 'Índice producción industrial'
  )

max(ip$date)

# Merge datasets
df <- merge(commodities, ip, by = 'date')
df <- df %>% select(-observation_date)

df <- merge(df, unemp, by = 'date')
df <- df %>% select(-observation_date)

# Rename variables
colnames(df) <- c('Fecha', 'Oro', 'Plata', 'Producción', 'Desempleo')

# Correlation matrix
values <- df %>% select(-Fecha)
tabla <- cor(values, use = 'complete.obs')

head(values)

# Sample size excluding NA
n <- nrow(na.omit(values))

# t-statistic for correlations
t <- tabla * sqrt((n - 2) / (1 - tabla^2))

# p-values
p_value <- 2 * pt(-abs(t), df = n - 2)

p_value

# Correlation table formatted with gt
tabla %>%
  as.data.frame() %>%
  rownames_to_column("Correlación entre variables") %>%
  gt() %>%
  fmt_number(decimals = 3)

# Import volatility indices
VXSLV <- read.csv('VXSLVCLS.csv')
GVZ <- read.csv('GVZCLS.csv')

head(GVZ)

# Convert dates
GVZ$observation_date <- as.Date(GVZ$observation_date, format = c('%Y-%m-%d'))
VXSLV$observation_date <- as.Date(VXSLV$observation_date, format = c('%Y-%m-%d'))

# Interpolate missing values
GVZ$GVZCLS <- na.approx(GVZ$GVZCLS)
VXSLV$VXSLVCLS <- na.approx(VXSLV$VXSLVCLS)

# Merge volatility datasets
volatility <- inner_join(VXSLV,
                         GVZ,
                         by = 'observation_date')

any(is.na(volatility))
length(volatility[,1])
head(volatility)

# Ensure no missing values remain
volatility$VXSLVCLS <- na.approx(volatility$VXSLVCLS)
volatility$GVZCLS <- na.approx(volatility$GVZCLS)

# Descriptive statistics for volatility
tabla <- with(volatility, sapply(list(GVZ = GVZCLS, VXSLV = VXSLVCLS),
                                 function(x) c('Media' = mean(x),
                                               'DE' = sd(x),
                                               'CV' = sd(x)/mean(x))))
tabla <- t(tabla)

tabla |>
  as.data.frame() |>
  rownames_to_column("Estadisticos") |>
  gt() |>
  fmt_number(decimals = 3)

# Correlation test between volatility indices
cor.test(volatility$VXSLVCLS, volatility$GVZCLS)

# Frequency comparison
table(volatility$VXSLVCLS > volatility$GVZCLS)

# Volatility plot
ggplot(aes(x = observation_date),data = volatility) +
  geom_line(aes(y = VXSLVCLS, color = 'Plata')) +
  geom_line(aes(y = GVZCLS, color = 'Oro')) +
  theme_classic() +
  labs(
    color = 'Índice',
    x = 'Fecha',
    y = 'Índice de volatilidad'
  )
  
max(volatility$observation_date)

# Stationarity test (ADF)
adf.test(diff(log(df$Producción), lag = 12))

names(volatility) <- c('Fecha', 'VXSLVCLS', 'GVZCLS')

# Population data
popu <- read.csv('POPTHM.csv')
popu$observation_date <- as.Date(popu$observation_date, format = c('%Y-%m-%d'))
colnames(popu) <- c('Fecha', 'poblacion')

# Merge all datasets
dftotal <- inner_join(df,
                      volatility,
                      by = 'Fecha')

dftotal <- inner_join(dftotal,
                      popu,
                      by = 'Fecha')

# Create model variables
dfmodel <- dftotal %>% mutate(
  l12_oro = log(Oro) - dplyr::lag(log(Oro), 12),
  l12_plata = log(Plata) - dplyr::lag(log(Plata), 12),
  l12_ip = log(Producción) - dplyr::lag(log(Producción), 12),
  l12_des = log(Desempleo) - dplyr::lag(log(Desempleo), 12),
  l12_vol_oro = log(GVZCLS) - dplyr::lag(log(GVZCLS), 12),
  l12_vol_pla = log(VXSLVCLS) - dplyr::lag(log(VXSLVCLS), 12),
  l12_vol_pop = log(poblacion) - dplyr::lag(log(poblacion), 12),
  lag_oro = dplyr::lag(l12_oro, 1),
  lag_plata = dplyr::lag(l12_plata, 1),
  lag_vol_oro = dplyr::lag(l12_vol_oro, 1),
  lag_vol_pla = dplyr::lag(l12_vol_pla, 1),
  lag_vol_pop = dplyr::lag(l12_vol_pop, 1),
  covid = ifelse(Fecha >= as.Date("2020-03-01") & 
                   Fecha <= as.Date("2020-12-01"), 1,0)
) %>% na.omit()

# Regression models
modeliporo <- lm(l12_ip ~ lag_oro + lag_vol_oro + covid, data = dfmodel)
coeftest(modeliporo, vcov = NeweyWest(modeliporo))

modeldesoro <- lm(l12_des ~ lag_oro + lag_vol_oro + lag_vol_pop + covid, data = dfmodel)
coeftest(modeldesoro, vcov = NeweyWest(modeldesoro))

modelipplata <- lm(l12_ip ~ lag_plata + lag_vol_pla + lag_vol_oro + covid + lag_oro, data = dfmodel)
coeftest(modelipplata, vcov = NeweyWest(modelipplata))

modeldesplata <- lm(l12_des ~ lag_plata + lag_vol_pla + lag_vol_oro + lag_vol_pop + covid + lag_oro, data = dfmodel)
coeftest(modeldesplata, vcov = NeweyWest(modeldesplata))

# Nested model for Wald test
modeldesplatab <- lm(l12_des ~lag_vol_pla + lag_vol_oro + lag_vol_pop, data = dfmodel)

waldtest(modeldesplata, modeldesplatab, NeweyWest(modeldesplata))

# Multicollinearity diagnostics
viff <- vif(modeldesplata)
tol <- 1/viff
cbind(viff, tol)

# Hypothesis test
linearHypothesis(modeldesplata, 'lag_plata = 0', vcov=NeweyWest(modeldesplata))

# Newey-West standard errors
nw1 <- sqrt(diag(NeweyWest(modeliporo)))
nw2 <- sqrt(diag(NeweyWest(modeldesoro)))
nw3 <- sqrt(diag(NeweyWest(modelipplata)))
nw4 <- sqrt(diag(NeweyWest(modeldesplata)))

coef(modeliporo)
coef(modeldesoro)
coef(modelipplata)
coef(modeldesplata)

# Export regression table
stargazer(
          modelipplata,
          modeldesplata,
          type = "html",
          out = "tabla_resultados.html",
          
          se = list(nw3, nw4),
          
          order = c("lag_oro",
                    "lag_plata",
                    "lag_vol_oro",
                    "lag_vol_pla",
                    "lag_vol_pop",
                    'covid'),
          
          covariate.labels = c(
            "Oro (t-1)",
            "Plata (t-1)",
            "Volatilidad Oro (t-1)",
            "Volatilidad Plata (t-1)",
            "Población (t-1)",
            'Dummy covid'
          ),
          
          column.labels = c(
                            "PI",
                            "Desempleo"),
          
          dep.var.labels = c(
                             "Crec. interanual PI",
                             "Crec. interanual Desempleo"),
          
          omit.stat = c("f", "ser"),
          digits = 4,
          no.space = TRUE,
          notes = "Errores estándar Newey-West entre paréntesis."
)
