# ECON 5253
# Ethan Maddy
# Final Project Code

rm(list = ls())

# load packages
library(tidyverse)
library(dplyr)
library(magrittr)
library(readxl)
library(broom) 
library(clubSandwich)
library(modelsummary)
library(estimatr)
library(plm)
library(fixest)

# load data from World Bank Health Nutrition and Population Statistics DataBank
WBdata <- read_excel("WBdata2.xlsx")
WBdata

# view variable names
names <- tibble(name = names(WBdata))
names

# rename
WBdata %<>% rename(
  HealthSpending = `Current health expenditure per capita (current US$) [SH.XPD.CHEX.PC.CD]`,
  GNI = `GNI per capita, Atlas method (current US$) [NY.GNP.PCAP.CD]`,
  DeathRate = `Death rate, crude (per 1,000 people) [SP.DYN.CDRT.IN]`,
  HIV = `Incidence of HIV, all (per 1,000 uninfected population) [SH.HIV.INCD.TL.P3]`,
  LifeExpect = `Life expectancy at birth, total (years) [SP.DYN.LE00.IN]`,
  Malnutrition = `Malnutrition prevalence, weight for age (% of children under 5) [SH.STA.MALN.ZS]`,
  Mortality = `Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%) [SH.DYN.NCOM.ZS]`,
  InfantDeaths = `Number of infant deaths [SH.DTH.IMRT]`, 
  MaternalDeaths = `Number of maternal deaths [SH.MMR.DTHS]`, 
  Undernourished = `Number of people who are undernourished [SN.ITK.DEFC]`, 
  Sanitation = `People using at least basic sanitation services (% of population) [SH.STA.BASS.ZS]`
  )

# mutate so variables are numeric 
WBdata %<>% mutate(Year = as.factor(Year),
                   GNI = as.numeric(GNI),
                   HealthSpending = as.numeric(HealthSpending),
                   log.DeathRate = log(as.numeric(DeathRate)),
                   log.HIV = log(as.numeric(HIV)),
                   log.LifeExpect = log(as.numeric(LifeExpect)),
                   log.Malnutrition = log(as.numeric(Malnutrition)),
                   Mortality = as.numeric(Mortality),
                   log.InfantDeaths = log(as.numeric(InfantDeaths)),
                   log.MaternalDeaths = log(as.numeric(MaternalDeaths)),
                   log.Undernourished = log(as.numeric(Undernourished)),
                   Sanitation = as.numeric(Sanitation))

# mutate to make new column - Health Spending / GNI = Health Spending percent of GNI
WBdata %<>% mutate(HealthSpendPCNT = HealthSpending / GNI * 100)


# estimate FE: Death Rate ~ Current % Health Expenditure
est.DeathRate <- feols(log.DeathRate ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.DeathRate, output = "markdown") %>% print

# estimate FE: HIV ~ Current Health % Expenditure
est.HIV <- feols(log.HIV ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.HIV, output = "markdown") %>% print

# estimate FE: Life Expectancy ~ Current % Health Expenditure
est.LifeExpect <- feols(log.LifeExpect ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.LifeExpect, output = "markdown") %>% print

# estimate FE: Malnutrition ~ Current % Health Expenditure
est.Malnutrition <- feols(log.Malnutrition ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.Malnutrition, output = "markdown") %>% print

# estimate FE: Mortality ~ Current % Health Expenditure
est.Mortality <- feols(Mortality ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.Mortality, output = "markdown") %>% print

# estimate FE: Infant Deaths ~ Current % Health Expenditure
est.InfantDeaths <- feols(log.InfantDeaths ~ HealthSpendPCNT + Year| `Country Name`, data = WBdata)
modelsummary(est.InfantDeaths, output = "markdown") %>% print

# estimate FE: Maternal Deaths ~ Current % Health Expenditure
est.MaternalDeaths <- feols(log.MaternalDeaths ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.MaternalDeaths, output = "markdown") %>% print

# estimate FE: Undernourished ~ Current % Health Expenditure
est.Undernourished <- feols(log.Undernourished ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.Undernourished, output = "markdown") %>% print

# estimate FE: Sanitation ~ Current % Health Expenditure
est.Sanitation <- feols(Sanitation ~ HealthSpendPCNT + Year | `Country Name`, data = WBdata)
modelsummary(est.Sanitation, output = "markdown") %>% print



# model summary of % of GNI health spending on health outcomes
modelsummary(list(est.DeathRate,est.HIV,est.LifeExpect,est.Malnutrition,est.Mortality,
                    est.InfantDeaths,est.MaternalDeaths,est.Undernourished,est.Sanitation),
                      output = "kableExtra") %>% print
modelsummary(list(est.DeathRate,est.HIV,est.LifeExpect,est.Malnutrition,est.Mortality,
                    est.InfantDeaths,est.MaternalDeaths,est.Undernourished,est.Sanitation),
                      output = "latex") %>% print
  
# data summary skim - summary statistics
datasummary_skim(
  WBdata %>% select(GNI, HealthSpending, log.DeathRate, log.HIV,log.LifeExpect, log.Malnutrition, 
                      Mortality, log.InfantDeaths, log.MaternalDeaths, log.Undernourished, Sanitation), 
                        histogram = F, output = "kableExtra")
datasummary_skim(
  WBdata %>% select(GNI, HealthSpending, log.DeathRate, log.HIV,log.LifeExpect, log.Malnutrition, 
                      Mortality, log.InfantDeaths, log.MaternalDeaths, log.Undernourished, Sanitation), 
                        histogram = F, output = "latex")