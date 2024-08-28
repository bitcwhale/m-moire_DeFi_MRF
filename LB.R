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

library(readxl)
library(plm)
library(tidyverse)
library(gplots)
library(foreign)
library(panelView)
library(dplyr)
library(nlme)
library(lmtest)

# data parametrage ________________________________________________________
data <- read_excel("C:\\Users\\jerem\\Documents\\test_python\\thesis-lending_borrowing.xlsx")
head(data)

datanew <- data %>% mutate (log_fees = log (Fees), log_OPexp = log(operating_expenses), log_tokinc = log(token_incentives), log_coredev = log(core_dev), log_treasury = log (treasury), log_tvl = log(TVL))
head(datanew)

data_pd <- pdata.frame (data, index = c("Name","Date"), drop.index=TRUE)
head(data_pd)
data_pd <- data_pd[!duplicated(index(data_pd)), ]
head(data_pd)


#test stationarity for each variables (LLC)_____________________


data <- data[order(data$Name, data$Date), ]
data$Date <- as.Date(data$Date, format="%m/%d/%Y")
pdata <- pdata.frame(data, index = c("Name", "Date"))

llc_test_tvl <- purtest(pdata$tvl, test = "levinlin")
llc_test_incentives <- purtest(pdata$token_incentives, test = "levinlin")
llc_test_treasury <- purtest(pdata$treasury, test = "levinlin")
llc_test_fees <- purtest(pdata$Fees, test = "levinlin")
llc_test_expenses <- purtest(pdata$operating_expenses, test = "levinlin")
llc_test_core_dev <- purtest(pdata$core_dev, test = "levinlin")
llc_test_active_users <- purtest(pdata$active_users, test = "levinlin")

# Summarize the results
summary(llc_test_tvl)
summary(llc_test_incentives)
summary(llc_test_treasury)
summary(llc_test_fees)
summary(llc_test_expenses)
summary(llc_test_core_dev)
summary(llc_test_active_users)


#plot the data to get an overview _______________________________________________
plotmeans(log_tvl ~ Name, main = "heterogeineity across protocols", data = datanew)
plotmeans(log_tvl ~ Date, main = "heterogeineity across protocols", data = datanew)

datanew %>% filter (log_fees != "0") %>% ggplot(mapping= aes(x=log_fees, y= log_tvl, color = Name))+
  geom_point()+theme_bw()+theme(legend.position="bottom")+labs(title="tvl against fees")

datanew %>% filter (log_treasury != "0") %>% ggplot(mapping= aes(x=log_treasury, y= log_tvl, color = Name))+
  geom_point()+theme_bw()+theme(legend.position="bottom")+labs(title="tvl against treasury")

datanew %>%filter (log_OPexp != "0") %>% ggplot(mapping= aes(x=log_OPexp, y= log_tvl, color = Name))+
  geom_point()+theme_bw()+theme(legend.position="bottom")+labs(title="tvl against operating income")

datanew %>%filter (log_tokinc != "0") %>% ggplot(mapping= aes(x=log_tokinc, y= log_tvl, color = Name))+
  geom_point()+theme_bw()+theme(legend.position="bottom")+labs(title="tvl against token incentives")

datanew %>%filter (log_coredev != "0") %>% ggplot(mapping= aes(x=log_coredev, y= log_tvl, color = Name))+
  geom_point()+theme_bw()+theme(legend.position="bottom")+labs(title="tvl against core dev")

#which model is best suited ? POLS / FEM  / REM_____________________________________
data <- na.omit(datanew)

## OLS model
summary(data)
ols <- lm(log(TVL+1) ~ log(Fees+1) + log(operating_expenses+1) + log(token_incentives+1) + log(core_dev+1) + log(treasury+1), data = data_pd)
summary(ols)

##panel view
panelview(log(TVL+1) ~ log(Fees) + log(operating_expenses) + log(token_incentives) + log(core_dev) + log(treasury), data = data_pd, index= c("Name","Date"))

## fe-model
fe <- plm(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data_pd, index= c("Name","Date"), model="within")
summary(fe)
Pe <- plm(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data_pd, index= c("Name","Date"), model="pooling")
summary(Pe)
#checking if the pooling model is suistable BREUCSH pagan 
pooltest(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data_pd, index= c("Name","Date"), model="within")

## fe model statistic
fixef(fe)
pFtest(fe, ols) # ho pooled ols model is consistent / h1 : fixed effect model is consistent / here h0 is rejected
plmtest(fe, c="time", type="bp")

## random model 

datanew_aggregated <- data %>%
  group_by(Name, Date) %>%
  summarise_all(.funs = mean, na.rm = TRUE)
table(index(datanew_aggregated), useNA = "ifany")
re <- plm(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1)+log(token_incentives + 1) + log(treasury + 1), data = data_pd, model = "random")
summary(re)

#FEM or REM is suistable HAUSSEMAN TEST _____________________________________
#hausseman test => wich model from the fixed or random is best suited knowing I dropped one value "core_dev"
phtest(fe,re) #fixed model is best suited

pdwtest(fe)  #we got serial autocorrelation
bptest(fe,data = data_pd, studentize = F) # we got heteroscedasticity 
coeftest(fe,vcovHC)# only treasury is significant
coeftest(fe,vcovHC(fe,method="arellano"))# only treasury is significant
pcdtest(fe, test=("lm"))#there is cross sectional depencies
pcdtest(fe, test=("cd"))##there is cross sectional depencies
pbgtest(fe) #there is autocorrelation or serial correlation in error term

##the deal here is that the fixed model isn't perfect and modification is necessary

# BREUCH GODRRFEY/WOORL TEST This indicates the presence of serial correlation in the panel data model. 
#Serial correlation suggests that the errors are correlated over time, 
#violating the assumption of independence of observations.

#PERSAN CD TEST This confirms the presence of cross-sectional dependence in the panel data model. 
#The test statistic is significant, suggesting that the cross-sectional units are correlated.

#BREUCH PAGAN LM TEST This suggests that there is evidence of cross-sectional dependence in the panel data model. 
#In other words, there are likely correlations among the cross-sectional units (entities) in your data.

### controlling time effects______________________________________________________
model1<-lm(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1)+factor(Date)-1+factor(Name), data = datanew)
summary(model1)

#two ways affect
fixed_two <- plm(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data_pd, model="within",effect="twoways")
summary(fixed_two)

#within estimator
within <- plm(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1)+factor(Date), data = datanew, index= "Name", model="within")
summary(within)

# test for bias
pdwtest(fixed_two)  #we got serial autocorrelation
bptest(fixed_two,data = data_pd, studentize = F) # we got heteroscedasticity 
coeftest(fixed_two,vcovHC)# only treasury is significant
coeftest(fixed_two,vcovHC(fe,method="arellano"))# only treasury is significant
pcdtest(fixed_two, test=("lm"))#there is cross sectional depencies
pcdtest(fixed_two, test=("cd"))##there is cross sectional depencies
pbgtest(fixed_two)


#if we still got heteroskedasticity and autocorrelation, we need to use the standard errors also cluster can be used
# robust standard errors
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

# Print the summary table
print(summary_table)

#________________________________________________________________________________
##Panel Generalized Least Squares 
#In summary, the pggls function is used to estimate a panel data model 
#with heteroscedasticity and autocorrelation robust standard errors

fgls <- pggls(log(TVL+1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data_pd,index= c("Name","Date"), model = "within")
summary(fgls)
pcdtest(fgls, test=("lm"))
pcdtest(fgls, test=("cd"))
pbgtest(fgls)


## correction of heteroscedasticity ##

data <- na.omit(data) ## check for NA value
sum(is.na(data$treasury + 1))## check for NA value
# WLS strat
model <- lm(log(TVL + 1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data)
print(model)
residuals <- residuals(model)
weights <- 1 / residuals^2
length(weights)

model_weights <- lm(log(TVL + 1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data, weights = weights)
summary(model_weights)
bptest(model_weights)

#GLS strat
model_gls <- gls(log(TVL + 1) ~ log(Fees + 1) + log(operating_expenses + 1) + log(token_incentives + 1) + log(core_dev + 1) + log(treasury + 1), data = data, correlation = corSymm(form = ~ 1 | Date))
summary(model_gls)
bptest(model_gls)