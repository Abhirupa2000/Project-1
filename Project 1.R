# loading packages
library(haven)
library(tidyverse)
library(stargazer)
install.packages("ggrepel")
library(ggrepel)
installed.packages("ggshowtext")
# library(ggshowtext)
# installed.packages("ggtext")
library(ggtext)
library(sandwich)
library(lmtest)
install.packages("fixest")
library(fixest)

## Cross-sectional relationship between agriculture's share in output or employment versus GDP per capita


# Filtering out rows to create new dataset
wdi_dtast <- wdi_struc_change_assignment
wdi_dtast15 <- wdi_dtast %>% filter(year == 2015 & not_high_inc== 1 & not_tiny==1)
wdi_dtast15 <- wdi_dtast15 %>% mutate(log_gdp_pc_ppp=log(gdp_pc_ppp))

# Scatterplot for agri gdp share (1.1)

graph1.1 <- wdi_dtast15 %>% ggplot(aes(log_gdp_pc_ppp,agri_gdp_sh, label = CountryCode)) + geom_point()
graph1.1

# Regression for agri gdp share (1.2)

lm1 <- lm((agri_gdp_sh)~(log_gdp_pc_ppp), data = wdi_dtast15)
stargazer(lm1, type = "html", 
          title = "Regression of agricultural share of GDP on log GDP per capita (PPP)", 
          out = "ass2reg1.html")

# Scatterplot and reg line for agri gdp share (1.3)
graph1.3 <- wdi_dtast15 %>% ggplot(aes(log_gdp_pc_ppp,agri_gdp_sh, label = CountryCode)) +
  geom_text_repel(point.size = NA, max.overlaps = 50) +
  geom_point() + 
  geom_point(data = ~filter(.x, CountryCode == "IND"), colour = "red") +
  geom_smooth(method = lm, se=F)
graph1.3

# scatterplot for agri emp share (1.4.1)
graph1.4 <- wdi_dtast15 %>% ggplot(aes(log_gdp_pc_ppp,agri_emp_sh, label = CountryCode))+
  geom_point()
graph1.4

# Regression for agri emp share (1.4.2)

lm2 <- lm((agri_emp_sh)~(log_gdp_pc_ppp), data = wdi_dtast15)
stargazer(lm2, type = "html", 
          title = "Regression of agricultural share of employment on log GDP per capita (PPP)", 
          out = "ass2reg2.html")

graph1.4.2 <- wdi_dtast15 %>% ggplot(aes(log_gdp_pc_ppp,agri_emp_sh, label = CountryCode)) +
  geom_text_repel(point.size = NA, max.overlaps = 50) +
  geom_point() + 
  geom_point(data = ~filter(.x, CountryCode == "IND"), colour = "red") +
  geom_smooth(method = lm, se=F)
graph1.4.2



## Cross-sectional relationship between self-employment share in total employment and GDP per capita


graph2.1 <- wdi_dtast15 %>% ggplot(aes(log_gdp_pc_ppp,self_emp_sh, label = CountryCode)) + geom_point()
graph2.1

lm3 <- lm((self_emp_sh)~(log_gdp_pc_ppp), data = wdi_dtast15)
stargazer(lm3, type = "html", 
          title = "Regression of self employment share on log GDP per capita (PPP)", 
          out = "ass2reg3.html")

graph2.2 <- wdi_dtast15 %>% ggplot(aes(log_gdp_pc_ppp,self_emp_sh, label = CountryCode)) +
  geom_text_repel(point.size = NA, max.overlaps = 50) +
  geom_point() + 
  geom_point(data = ~filter(.x, CountryCode == "IND"), colour = "red") +
  geom_smooth(method = lm, se=F)
graph2.2


## Changes in agri and self-employment shares over time for selected countries

wdi_dtast_emp <- wdi_dtast %>% filter(CountryCode %in% c("IND",'VNM'))

linept1 <- wdi_dtast_emp %>% ggplot(aes(x=year, y= agri_emp_sh, color = CountryCode)) +
  geom_line()
linept1

linept2 <-  wdi_dtast_emp %>% ggplot(aes(x=year, y= self_emp_sh, color = CountryCode)) +
  geom_line() 
linept2



## Panel Regression

wdi_pan <- wdi_dtast %>%  filter(not_tiny==1 & not_high_inc==1) %>%  mutate(log_gdp_pc_ppp=log(gdp_pc_ppp))

lm4 <- feols((agri_emp_sh)~(log_gdp_pc_ppp)|CountryCode, data = wdi_pan)
summary(lm4)

lm5 <- feols((self_emp_sh)~(log_gdp_pc_ppp)|CountryCode, data = wdi_pan)
lm5