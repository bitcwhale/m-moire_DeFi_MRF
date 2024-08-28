install.packages("readxl")
install.packages("tidyverse")
install.packages("plm")
install.packages("gplots")
install.packages("foreign")
install.packages("imputeTS")
install.packages("panelView")
install.packages("zoo")
install.packages("dplyr")
install.packages("nlme")
install.packages("lmtest")
install.packages("punitroots")


if (!require(gplots)) {
  install.packages("gplots")
  library(gplots)
}

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)
library(panelView)
library(punitroots)

# Data parametrage ________________________________________________________
data <- read_excel("C:\\Users\\jerem\\Documents\\test_python\\der.xls")
head(data)

datanew <- data %>% mutate(
  log_trading_volume = log(trading_volume+1),
  log_incentives = log(incentives+1),
  log_revenue = log(revenue+1),
  log_fees = log(fees+1),
  log_expenses = log(expenses+1),
  log_core_dev = log(core_dev+1),
  log_active_users = log(active_users+1),
  log_tvl = log(tvl+1)
)
head(datanew)

data_pd <- pdata.frame(data, index = c("Name", "Date"), drop.index = TRUE)
head(data_pd)
data_pd <- data_pd[!duplicated(index(data_pd)), ]
head(data_pd)

#test stationarity for each variables (LLC)_____________________
data[is.na(data)] <- 0


data <- data[order(data$Name, data$Date), ]
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
pdata <- pdata.frame(data, index = c("Name", "Date"))

llc_test_tvl <- purtest(pdata$tvl, test = "levinlin")
llc_test_trading_vol <- purtest(pdata$trading_volume, test = "levinlin")
llc_test_incentives <- purtest(pdata$incentives, test = "levinlin")
llc_test_revenue <- purtest(pdata$revenue, test = "levinlin")
llc_test_fees <- purtest(pdata$fees, test = "levinlin")
llc_test_expenses <- purtest(pdata$expenses, test = "levinlin")
llc_test_core_dev <- purtest(pdata$core_dev, test = "levinlin")
llc_test_active_users <- purtest(pdata$active_users, test = "levinlin")

# Summarize the results
summary(llc_test_tvl)
summary(llc_test_trading_vol)
summary(llc_test_incentives)
summary(llc_test_revenue)
summary(llc_test_fees)
summary(llc_test_expenses)
summary(llc_test_core_dev)
summary(llc_test_active_users)

# Plot the data to get an overview _______________________________________________
plotmeans(log_tvl ~ Name, main = "Heterogeneity across protocols", data = datanew)
plotmeans(log_tvl ~ Date, main = "Heterogeneity across protocols", data = datanew)

datanew %>% filter(log_fees != "0") %>% ggplot(mapping = aes(x = log_fees, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Fees")

datanew %>% filter(log_trading_volume != "0") %>% ggplot(mapping = aes(x = log_trading_volume, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Trading Volume")

datanew %>% filter(log_expenses != "0") %>% ggplot(mapping = aes(x = log_expenses, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Expenses")

datanew %>% filter(log_incentives != "0") %>% ggplot(mapping = aes(x = log_incentives, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Incentives")

datanew %>% filter(log_core_dev != "0") %>% ggplot(mapping = aes(x = log_core_dev, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Core Dev")

# Which model is best suited? POLS / FEM / REM ____________________________________
data <- na.omit(datanew)

## OLS model
summary(data)
ols <- lm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1), data = data_pd)
summary(ols)

## Panel view
panelview(log(tvl + 1) ~ log(fees) + log(expenses) + log(incentives) + log(core_dev) + log(trading_volume), data = data_pd, index = c("Name", "Date"))

## FE-model
fe <- plm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"), model = "within")
summary(fe)
Pe <- plm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"), model = "pooling")
summary(Pe)
# Checking if the pooling model is suitable BREUSCH-PAGAN
pooltest(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"), model = "within")

## FE model statistics
fixef(fe)
pFtest(fe, ols) # H0: pooled OLS model is consistent / H1: fixed effect model is consistent / Here H0 is rejected
plmtest(fe, c = "time", type = "bp")

## Random model
datanew_aggregated <- data %>%
  group_by(Name, Date) %>%
  summarise_all(.funs = mean, na.rm = TRUE)
table(index(datanew_aggregated), useNA = "ifany")
re <- plm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(trading_volume + 1), data = data_pd, model = "random")
summary(re)

# FEM or REM is suitable? HAUSMAN TEST _____________________________________
# Hausman test => which model from the fixed or random is best suited
phtest(fe, re) # Fixed model is best suited

pdwtest(fe) # We got serial autocorrelation
bptest(fe, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fe, vcovHC) # Only trading_volume is significant
coeftest(fe, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fe, test = "lm") # There is cross-sectional dependence
pcdtest(fe, test = "cd") # There is cross-sectional dependence
pbgtest(fe) # There is autocorrelation or serial correlation in error term

# The deal here is that the fixed model isn't perfect and modification is necessary

# BREUSCH-GODFREY/WOOLDRIDGE TEST: This indicates the presence of serial correlation in the panel data model. 
# Serial correlation suggests that the errors are correlated over time, violating the assumption of independence of observations.

# PERSARAN CD TEST: This confirms the presence of cross-sectional dependence in the panel data model. 
# The test statistic is significant, suggesting that the cross-sectional units are correlated.

# BREUSCH-PAGAN LM TEST: This suggests that there is evidence of cross-sectional dependence in the panel data model. 
# In other words, there are likely correlations among the cross-sectional units (entities) in your data.

### Controlling time effects______________________________________________________
model1 <- lm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1) + factor(Date) - 1 + factor(Name), data = datanew)
summary(model1)

# Two-way effects
fixed_two <- plm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1), data = data_pd, model = "within", effect = "twoways")
summary(fixed_two)

# Within estimator
within <- plm(log(tvl + 1) ~ log(fees + 1) + log(expenses + 1) + log(incentives + 1) + log(core_dev + 1) + log(trading_volume + 1) + factor(Date), data = datanew, index = "Name", model = "within")
summary(within)

# Test for bias
pdwtest(fixed_two) # We got serial autocorrelation
bptest(fixed_two, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fixed_two, vcovHC) # Only trading_volume is significant
coeftest(fixed_two, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fixed_two, test = "lm") # There is cross-sectional dependence
pcdtest(fixed_two, test = "cd") # There is cross-sectional dependence
pbgtest(fixed_two)

# If we still got heteroskedasticity and autocorrelation, we need to use the standard errors also cluster can be used
# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
  )

print(summary_table)

#test
model_summary <- summary(fixed_two)

# Extract R-squared and Adjusted R-squared
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

# Add R-squared and Adjusted R-squared to the summary table
summary_table <- rbind(
  summary_table,
  c("R-squared", r_squared, NA, NA, NA),
  c("Adjusted R-squared", adj_r_squared, NA, NA, NA)
)

print(summary_table)













#data infra_____________________________________________________________________________________
data <- read_excel("C:\\Users\\jerem\\Documents\\test_python\\data_infra.xls")
head(data)


datanew <- data %>% mutate(
  log_fees = log(fees + 1),
  log_treasury = log(treasury + 1),
  log_core_dev = log(core_dev + 1),
  log_earnings = log(earnings + 1),
  log_trading_volume = log(trading_volume + 1),
  log_tvl = log(tvl + 1)
)
head(datanew)

data_pd <- pdata.frame(data, index = c("Name", "Date"), drop.index = TRUE)
head(data_pd)
data_pd <- data_pd[!duplicated(index(data_pd)), ]
head(data_pd)

#test stationarity for each variables (LLC)_____________________
data[is.na(data)] <- 0


data <- data[order(data$Name, data$Date), ]
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
pdata <- pdata.frame(data, index = c("Name", "Date"))

llc_test_tvl <- purtest(pdata$tvl, test = "levinlin")
llc_test_trading_vol <- purtest(pdata$trading_volume, test = "levinlin")
llc_test_earnings <- purtest(pdata$earnings, test = "levinlin")
llc_test_fees <- purtest(pdata$fees, test = "levinlin")
llc_test_treasury <- purtest(pdata$treasury, test = "levinlin")
llc_test_core_dev <- purtest(pdata$core_dev, test = "levinlin")

# Summarize the results
summary(llc_test_tvl)
summary(llc_test_trading_vol)
summary(llc_test_treasury)
summary(llc_test_fees)
summary(llc_test_earnings)
summary(llc_test_core_dev)


# Plot the data to get an overview _______________________________________________
plotmeans(log_tvl ~ Name, main = "Heterogeneity across protocols", data = datanew)
plotmeans(log_tvl ~ Date, main = "Heterogeneity across protocols", data = datanew)

datanew %>% filter(log_fees != "0") %>% ggplot(mapping = aes(x = log_fees, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Fees")

datanew %>% filter(log_trading_volume != "0") %>% ggplot(mapping = aes(x = log_trading_volume, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Trading Volume")

datanew %>% filter(log_treasury != "0") %>% ggplot(mapping = aes(x = log_treasury, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Treasury")

datanew %>% filter(log_earnings != "0") %>% ggplot(mapping = aes(x = log_earnings, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Earnings")

datanew %>% filter(log_core_dev != "0") %>% ggplot(mapping = aes(x = log_core_dev, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Core Dev")

# Which model is best suited? POLS / FEM / REM ____________________________________
data <- na.omit(datanew)

## OLS model
summary(data)
ols <- lm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd)
summary(ols)

## Panel view
panelview(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"))

## FE-model
fe <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"), model = "within")
summary(fe)
Pe <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"), model = "pooling")
summary(Pe)
# Checking if the pooling model is suitable BREUSCH-PAGAN
pooltest(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd, index = c("Name", "Date"), model = "within")

## FE model statistics
fixef(fe)
pFtest(fe, ols) # H0: pooled OLS model is consistent / H1: fixed effect model is consistent / Here H0 is rejected
plmtest(fe, c = "time", type = "bp")

## Random model
datanew_aggregated <- data %>%
  group_by(Name, Date) %>%
  summarise_all(.funs = mean, na.rm = TRUE)
table(index(datanew_aggregated), useNA = "ifany")
re <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd, model = "random")
summary(re)

# FEM or REM is suitable? HAUSMAN TEST _____________________________________
# Hausman test => which model from the fixed or random is best suited
phtest(fe, re) # Fixed model is best suited

pdwtest(fe) # We got serial autocorrelation
bptest(fe, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fe, vcovHC) # Only trading_volume is significant
coeftest(fe, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fe, test = "lm") # There is cross-sectional dependence
pcdtest(fe, test = "cd") # There is cross-sectional dependence
pbgtest(fe) # There is autocorrelation or serial correlation in error term

# The deal here is that the fixed model isn't perfect and modification is necessary

# BREUSCH-GODFREY/WOOLDRIDGE TEST: This indicates the presence of serial correlation in the panel data model. 
# Serial correlation suggests that the errors are correlated over time, violating the assumption of independence of observations.

# PERSARAN CD TEST: This confirms the presence of cross-sectional dependence in the panel data model. 
# The test statistic is significant, suggesting that the cross-sectional units are correlated.

# BREUSCH-PAGAN LM TEST: This suggests that there is evidence of cross-sectional dependence in the panel data model. 
# In other words, there are likely correlations among the cross-sectional units (entities) in your data.

### Controlling time effects______________________________________________________
model1 <- lm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1) + factor(Date) - 1 + factor(Name), data = datanew)
summary(model1)

# Two-way effects
fixed_two <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1), data = data_pd, model = "within", effect = "twoways")
summary(fixed_two)

# Within estimator
within <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(core_dev + 1) + log(earnings + 1) + log(trading_volume + 1) + factor(Date), data = datanew, index = "Name", model = "within")
summary(within)

# Test for bias
pdwtest(fixed_two) # We got serial autocorrelation
bptest(fixed_two, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fixed_two, vcovHC) # Only trading_volume is significant
coeftest(fixed_two, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fixed_two, test = "lm") # There is cross-sectional dependence
pcdtest(fixed_two, test = "cd") # There is cross-sectional dependence
pbgtest(fixed_two)

# If we still got heteroskedasticity and autocorrelation, we need to use the standard errors also cluster can be used
# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

print(summary_table)

#test
model_summary <- summary
# Extract R-squared and Adjusted R-squared
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

# Add R-squared and Adjusted R-squared to the summary table
summary_table <- rbind(
  summary_table,
  c("R-squared", r_squared, NA, NA, NA),
  c("Adjusted R-squared", adj_r_squared, NA, NA, NA)
)

print(summary_table)










#lst data____________________________________________________________________________________________
data <- read_excel("C:\\Users\\jerem\\Documents\\test_python\\lst.xlsx")
head(data)



datanew <- data %>% mutate(
  log_fees = log(fees + 1),
  log_active_users = log(active_users + 1),
  log_core_dev = log(core_dev + 1),
  log_tvl = log(tvl + 1)
)
head(datanew)


#test stationarity for each variables (LLC)_____________________
data[is.na(data)] <- 0


data <- data[order(data$Name, data$Date), ]
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
pdata <- pdata.frame(data, index = c("Name", "Date"))

llc_test_tvl <- purtest(pdata$tvl, test = "levinlin")
llc_test_trading_vol <- purtest(pdata$trading_volume, test = "levinlin")
llc_test_fees <- purtest(pdata$fees, test = "levinlin")
llc_test_active_users <- purtest(pdata$active_users, test = "levinlin")

# Summarize the results
summary(llc_test_tvl)
summary(llc_test_trading_vol)
summary(llc_test_fees)
summary(llc_test_active_users)


# Create a panel data frame
data_pd <- pdata.frame(datanew, index = c("Name", "Date"), drop.index = TRUE)
head(data_pd)
data_pd <- data_pd[!duplicated(index(data_pd)), ]
head(data_pd)

# Plot the data to get an overview
plotmeans(log_tvl ~ Name, main = "Heterogeneity across protocols", data = datanew)
plotmeans(log_tvl ~ Date, main = "Heterogeneity across protocols", data = datanew)

# Scatter plots
datanew %>% filter(log_fees != "0") %>% ggplot(mapping = aes(x = log_fees, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Fees")

datanew %>% filter(log_active_users != "0") %>% ggplot(mapping = aes(x = log_active_users, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Active Users")

datanew %>% filter(log_core_dev != "0") %>% ggplot(mapping = aes(x = log_core_dev, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Core Dev")

# Which model is best suited? POLS / FEM / REM
data <- na.omit(datanew)

## OLS model
summary(data)
ols <- lm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd)
summary(ols)

## Panel view
panelview(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd, index = c("Name", "Date"))

## FE-model
fe <- plm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd, index = c("Name", "Date"), model = "within")
summary(fe)
Pe <- plm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd, index = c("Name", "Date"), model = "pooling")
summary(Pe)
# Checking if the pooling model is suitable BREUSCH-PAGAN
pooltest(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd, index = c("Name", "Date"), model = "within")

## FE model statistics
fixef(fe)
pFtest(fe, ols) # H0: pooled OLS model is consistent / H1: fixed effect model is consistent / Here H0 is rejected
plmtest(fe, c = "time", type = "bp")

## Random model
datanew_aggregated <- data %>%
  group_by(Name, Date) %>%
  summarise_all(.funs = mean, na.rm = TRUE)
table(index(datanew_aggregated), useNA = "ifany")
re <- plm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd, model = "random")
summary(re)

# FEM or REM is suitable? HAUSMAN TEST
phtest(fe, re) # Fixed model is best suited

pdwtest(fe) # We got serial autocorrelation
bptest(fe, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fe, vcovHC) # Only trading_volume is significant
coeftest(fe, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fe, test = "lm") # There is cross-sectional dependence
pcdtest(fe, test = "cd") # There is cross-sectional dependence
pbgtest(fe) # There is autocorrelation or serial correlation in error term

# The deal here is that the fixed model isn't perfect and modification is necessary

# BREUSCH-GODFREY/WOOLDRIDGE TEST: This indicates the presence of serial correlation in the panel data model. 
# Serial correlation suggests that the errors are correlated over time, violating the assumption of independence of observations.

# PERSARAN CD TEST: This confirms the presence of cross-sectional dependence in the panel data model. 
# The test statistic is significant, suggesting that the cross-sectional units are correlated.

# BREUSCH-PAGAN LM TEST: This suggests that there is evidence of cross-sectional dependence in the panel data model. 
# In other words, there are likely correlations among the cross-sectional units (entities) in your data.

### Controlling time effects
model1 <- lm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1) + factor(Date) - 1 + factor(Name), data = datanew)
summary(model1)

# Two-way effects
fixed_two <- plm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1), data = data_pd, model = "within", effect = "twoways")
summary(fixed_two)

# Within estimator
within <- plm(log(tvl + 1) ~ log(fees + 1) + log(active_users + 1) + log(core_dev + 1) + factor(Date), data = datanew, index = "Name", model = "within")
summary(within)

# Test for bias
pdwtest(fixed_two) # We got serial autocorrelation
bptest(fixed_two, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fixed_two, vcovHC) # Only trading_volume is significant
coeftest(fixed_two, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fixed_two, test = "lm") # There is cross-sectional dependence
pcdtest(fixed_two, test = "cd") # There is cross-sectional dependence
pbgtest(fixed_two)

# If we still got heteroskedasticity and autocorrelation, we need to use the standard errors also cluster can be used
# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

print(summary_table)

#test
model_summary <- summary(fixed_two)
# Extract R-squared and Adjusted R-squared
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

# Add R-squared and Adjusted R-squared to the summary table
summary_table <- rbind(
  summary_table,
  c("R-squared", r_squared, NA, NA, NA),
  c("Adjusted R-squared", adj_r_squared, NA, NA, NA)
)

print(summary_table)










#stablecoin____________________________________________________________________________________
data <- read_excel("C:\\Users\\jerem\\Documents\\test_python\\stablecoins.xls")
head(data)

datanew <- data %>% mutate(
  log_fees = log(fees + 1),
  log_incentives = log(token_incentives + 1),
  log_core_dev = log(core_dev + 1),
  log_active_user = log(active_user + 1),
  log_earnings = log(earnings + 1),
  log_tvl = log(tvl + 1)
)
head(datanew)

# Create a panel data frame
data_pd <- pdata.frame(datanew, index = c("Name", "Date"), drop.index = TRUE)
head(data_pd)
data_pd <- data_pd[!duplicated(index(data_pd)), ]
head(data_pd)


#test stationarity for each variables (LLC)_____________________
data[is.na(data)] <- 0


data <- data[order(data$Name, data$Date), ]
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
pdata <- pdata.frame(data, index = c("Name", "Date"))

llc_test_tvl <- purtest(pdata$tvl, test = "levinlin")
llc_test_trading_vol <- purtest(pdata$trading_volume, test = "levinlin")
llc_test_token_incentives <- purtest(pdata$token_incentives, test = "levinlin")
llc_test_fees <- purtest(pdata$fees, test = "levinlin")
llc_test_core_dev <- purtest(pdata$core_dev, test = "levinlin")
llc_test_active_users <- purtest(pdata$active_user, test = "levinlin")

# Summarize the results
summary(llc_test_tvl)
summary(llc_test_trading_vol)
summary(llc_test_token_incentives)
summary(llc_test_fees)
summary(llc_test_core_dev)
summary(llc_test_active_user)


# Plot the data to get an overview
plotmeans(log_tvl ~ Name, main = "Heterogeneity across protocols", data = datanew)
plotmeans(log_tvl ~ Date, main = "Heterogeneity across protocols", data = datanew)

datanew %>% filter(log_fees != 0) %>% ggplot(mapping = aes(x = log_fees, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Fees")

datanew %>% filter(log_incentives != 0) %>% ggplot(mapping = aes(x = log_incentives, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Token Incentives")

datanew %>% filter(log_core_dev != 0) %>% ggplot(mapping = aes(x = log_core_dev, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Core Dev")

datanew %>% filter(log_active_user != 0) %>% ggplot(mapping = aes(x = log_active_user, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Active User")

datanew %>% filter(log_earnings != 0) %>% ggplot(mapping = aes(x = log_earnings, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Earnings")

# Determine the best suited model: POLS / FEM / REM
data <- na.omit(datanew)

# OLS model
summary(data)
ols <- lm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd)
summary(ols)

# Panel view
panelview(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd, index = c("Name", "Date"))

# FE-model
fe <- plm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd, index = c("Name", "Date"), model = "within")
summary(fe)
Pe <- plm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd, index = c("Name", "Date"), model = "pooling")
summary(Pe)
# Checking if the pooling model is suitable BREUSCH-PAGAN
pooltest(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd, index = c("Name", "Date"), model = "within")

# FE model statistics
fixef(fe)
pFtest(fe, ols) # H0: pooled OLS model is consistent / H1: fixed effect model is consistent / Here H0 is rejected
plmtest(fe, c = "time", type = "bp")

# Random model
datanew_aggregated <- data %>%
  group_by(Name, Date) %>%
  summarise_all(.funs = mean, na.rm = TRUE)
table(index(datanew_aggregated), useNA = "ifany")
re <- plm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd, model = "random")
summary(re)

# FEM or REM is suitable? HAUSMAN TEST
# Hausman test => which model from the fixed or random is best suited
phtest(fe, re) # Fixed model is best suited

pdwtest(fe) # We got serial autocorrelation
bptest(fe, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fe, vcovHC) # Only trading_volume is significant
coeftest(fe, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fe, test = "lm") # There is cross-sectional dependence
pcdtest(fe, test = "cd") # There is cross-sectional dependence
pbgtest(fe) # There is autocorrelation or serial correlation in error term

# The deal here is that the fixed model isn't perfect and modification is necessary

# BREUSCH-GODFREY/WOOLDRIDGE TEST: This indicates the presence of serial correlation in the panel data model. 
# Serial correlation suggests that the errors are correlated over time, violating the assumption of independence of observations.

# PERSARAN CD TEST: This confirms the presence of cross-sectional dependence in the panel data model. 
# The test statistic is significant, suggesting that the cross-sectional units are correlated.

# BREUSCH-PAGAN LM TEST: This suggests that there is evidence of cross-sectional dependence in the panel data model. 
# In other words, there are likely correlations among the cross-sectional units (entities) in your data.

# Controlling time effects
model1 <- lm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1) + factor(Date) - 1 + factor(Name), data = datanew)
summary(model1)

# Two-way effects
fixed_two <- plm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1), data = data_pd, model = "within", effect = "twoways")
summary(fixed_two)

# Within estimator
within <- plm(log(tvl + 1) ~ log(fees + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(active_user + 1) + log(earnings + 1) + factor(Date), data = datanew, index = "Name", model = "within")
summary(within)

# Test for bias
pdwtest(fixed_two) # We got serial autocorrelation
bptest(fixed_two, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fixed_two, vcovHC) # Only trading_volume is significant
coeftest(fixed_two, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fixed_two, test = "lm") # There is cross-sectional dependence
pcdtest(fixed_two, test = "cd") # There is cross-sectional dependence
pbgtest(fixed_two)

# If we still got heteroskedasticity and autocorrelation, we need to use the standard errors also cluster can be used
# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

print(summary_table)

#test
model_summary <- summary(fixed_two)
# Extract R-squared and Adjusted R-squared
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

# Add R-squared and Adjusted R-squared to the summary table
summary_table <- rbind(
  summary_table,
  c("R-squared", r_squared, NA, NA, NA),
  c("Adjusted R-squared", adj_r_squared, NA, NA, NA)
)

print(summary_table)







#dex_____________________________________________________________________________________
data <- read_excel("C:\\Users\\jerem\\Documents\\test_python\\dex.xls")
head(data)



# Data transformation
datanew <- data %>% mutate(
  log_fees = log(fees + 1),
  log_treasury = log(treasury + 1),
  log_tradable_pairs = log(tradable_pairs + 1),
  log_incentives = log(incentives + 1),
  log_trading_vol = log(trading_vol + 1),
  log_expenses = log(expenses + 1),
  log_active_users = log(active_users + 1),
  log_tvl = log(tvl + 1)
)
head(datanew)

data_pd <- pdata.frame(datanew, index = c("Name", "Date"), drop.index = TRUE)
head(data_pd)
data_pd <- data_pd[!duplicated(index(data_pd)), ]
head(data_pd)


#test stationarity for each variables (LLC)_____________________
data[is.na(data)] <- 0


data <- data[order(data$Name, data$Date), ]
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
pdata <- pdata.frame(data, index = c("Name", "Date"))

llc_test_tvl <- purtest(pdata$tvl, test = "levinlin")
llc_test_trading_pairs <- purtest(pdata$tradable_pairs, test = "levinlin")
llc_test_incentives <- purtest(pdata$incentives, test = "levinlin")
llc_test_revenue <- purtest(pdata$revenue, test = "levinlin")
llc_test_fees <- purtest(pdata$fees, test = "levinlin")
llc_test_expenses <- purtest(pdata$expenses, test = "levinlin")
llc_test_treasury <- purtest(pdata$treasury, test = "levinlin")
llc_test_active_users <- purtest(pdata$active_users, test = "levinlin")

# Summarize the results
summary(llc_test_tvl)
summary(llc_test_trading_pairs)
summary(llc_test_incentives)
summary(llc_test_revenue)
summary(llc_test_fees)
summary(llc_test_expenses)
summary(llc_test_treasury)
summary(llc_test_active_users)


# Plot the data to get an overview
plotmeans(log_tvl ~ Name, main = "Heterogeneity across protocols", data = datanew)
plotmeans(log_tvl ~ Date, main = "Heterogeneity across protocols", data = datanew)

datanew %>% filter(log_fees != "0") %>% ggplot(mapping = aes(x = log_fees, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Fees")

datanew %>% filter(log_trading_vol != "0") %>% ggplot(mapping = aes(x = log_trading_vol, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Trading Volume")

datanew %>% filter(log_treasury != "0") %>% ggplot(mapping = aes(x = log_treasury, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Treasury")

datanew %>% filter(log_tradable_pairs != "0") %>% ggplot(mapping = aes(x = log_tradable_pairs, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Tradable Pairs")

datanew %>% filter(log_incentives != "0") %>% ggplot(mapping = aes(x = log_incentives, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Incentives")

datanew %>% filter(log_expenses != "0") %>% ggplot(mapping = aes(x = log_expenses, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Expenses")

datanew %>% filter(log_active_users != "0") %>% ggplot(mapping = aes(x = log_active_users, y = log_tvl, color = Name)) +
  geom_point() + theme_bw() + theme(legend.position = "bottom") + labs(title = "TVL against Active Users")

# Which model is best suited? POLS / FEM / REM
data <- na.omit(datanew)

## OLS model
summary(data)
ols <- lm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1) + log(active_users + 1), data = data_pd)
summary(ols)

## Panel view
panelview(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1) + log(active_users + 1), data = data_pd, index = c("Name", "Date"))

## FE-model
fe <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1)  + log(active_users + 1), data = data_pd, index = c("Name", "Date"), model = "within")
summary(fe)
Pe <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1) + log(active_users + 1), data = data_pd, index = c("Name", "Date"), model = "pooling")
summary(Pe)
# Checking if the pooling model is suitable BREUSCH-PAGAN
pooltest(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1)  + log(active_users + 1), data = data_pd, index = c("Name", "Date"), model = "within")

## FE model statistics
fixef(fe)
pFtest(fe, ols) # H0: pooled OLS model is consistent / H1: fixed effect model is consistent / Here H0 is rejected
plmtest(fe, c = "time", type = "bp")

## Random model
datanew_aggregated <- datanew %>%
  group_by(Name, Date) %>%
  summarise_all(.funs = mean, na.rm = TRUE)
table(index(datanew_aggregated), useNA = "ifany")
re <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1)  + log(active_users + 1), data = data_pd, model = "random")
summary(re)

# FEM or REM is suitable? HAUSMAN TEST
# Hausman test => which model from the fixed or random is best suited
phtest(fe, re) # Fixed model is best suited

pdwtest(fe) # We got serial autocorrelation
bptest(fe, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fe, vcovHC) # Only trading_volume is significant
coeftest(fe, vcovHC(fe, method = "arellano")) # Only trading_volume is significant
pcdtest(fe, test = "lm") # There is cross-sectional dependence
pcdtest(fe, test = "cd") # There is cross-sectional dependence
pbgtest(fe) # There is autocorrelation or serial correlation in error term

# The deal here is that the fixed model isn't perfect and modification is necessary

# BREUSCH-GODFREY/WOOLDRIDGE TEST: This indicates the presence of serial correlation in the panel data model. 
# Serial correlation suggests that the errors are correlated over time, violating the assumption of independence of observations.

# PERSARAN CD TEST: This confirms the presence of cross-sectional dependence in the panel data model. 
# The test statistic is significant, suggesting that the cross-sectional units are correlated.

# BREUSCH-PAGAN LM TEST: This suggests that there is evidence of cross-sectional dependence in the panel data model. 
# In other words, there are likely correlations among the cross-sectional units (entities) in your data.

### Controlling time effects
model1 <- lm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1)  + log(active_users + 1) + factor(Date) - 1 + factor(Name), data = datanew)
summary(model1)

# Two-way effects
fixed_two <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1)  + log(active_users + 1), data = data_pd, model = "within", effect = "twoways")
summary(fixed_two)

# Within estimator
within <- plm(log(tvl + 1) ~ log(fees + 1) + log(treasury + 1) + log(tradable_pairs + 1) + log(incentives + 1) + log(trading_vol + 1)+ log(active_users + 1) + factor(Date), data = datanew, index = "Name", model = "within")
summary(within)

# Test for bias
pdwtest(fixed_two) # We got serial autocorrelation
bptest(fixed_two, data = data_pd, studentize = FALSE) # We got heteroscedasticity 
coeftest(fixed_two, vcovHC) # Only trading_vol is significant
coeftest(fixed_two, vcovHC(fe, method = "arellano")) # Only trading_vol is significant
pcdtest(fixed_two, test = "lm") # There is cross-sectional dependence
pcdtest(fixed_two, test = "cd") # There is cross-sectional dependence
pbgtest(fixed_two)

# If we still got heteroskedasticity and autocorrelation, we need to use the standard errors also cluster can be used
# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

print(summary_table)

# Test
model_summary <- summary
# Extract R-squared and Adjusted R-squared
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

# Robust standard errors
robust_fixed_model <- vcovHC(fixed_two, type = "HC1")
coefficients <- coef(fixed_two)
robust_stdr <- sqrt(diag(robust_fixed_model))
robust_t_values <- coefficients / robust_stdr

# Degrees of freedom for the model
df <- df.residual(fixed_two)

# Calculate the p-values
robust_p_values <- 2 * pt(abs(robust_t_values), df = df, lower.tail = FALSE)

# Create a summary table
summary_table <- data.frame(
  Coefficients = coefficients,
  Robust_Std_Errors = robust_stdr,
  t_values = robust_t_values,
  p_values = robust_p_values
)

# Add R-squared and Adjusted R-squared to the summary table
summary_table <- rbind(
  summary_table,
  c("R-squared", r_squared, NA, NA, NA),
  c("Adjusted R-squared", adj_r_squared, NA, NA, NA)
)

print(summary_table)