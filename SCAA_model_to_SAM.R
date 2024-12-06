## ---------------------------
##
## Script name: A SAM draft model for beaked redfish
##
## Purpose of script: A quick model hacked from the SCAA model data. Imitates the approach presented by Olav Nikolai Breivik in NSS Herring example during the SAM course. Some of the code is his (or he has borrowed it from somewhere).
##
## Date Created: 2024-12-03
##
## ---------------------------

### Clear workspace (comment to )

rm(list = ls())

### Load packages ####

library(tidyverse)
library(stockassessment)
library(cowplot)

### Source or list custom functions used within the script ####

## ggplot theme

theme_cust <- theme_classic(base_size = 8) %+replace%
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        plot.margin = margin(c(5.5, 10, 5.5, 5.5)))

theme_set(theme_cust) # Set default theme globally for the entire project


# Read and manipulate data ####

### Catches ####

cn <- 
  list("demersal" = 
         read_table("data/from scaa model/CatchAtAge.txt") %>% 
         column_to_rownames("Year") %>% 
         setNames(gsub("\\+", "", colnames(.))) %>% 
         as.matrix(),
       "pelagic" = 
         read_table("data/from scaa model/PelagicCatchAtAge.txt") %>% 
         column_to_rownames("Year") %>% 
         setNames(gsub("\\+", "", colnames(.))) %>% 
         as.matrix()
  )

## ####

# lapply(seq_along(cn), function(i) {
#   as_tibble(cn[[i]], rownames = "year") %>% 
#     pivot_longer(-year) %>% 
#     add_column(type = names(cn)[i], .before = 1)
# }) %>% bind_rows() %>% 
#   mutate(
#     name = 
#       factor(name, 
#              rev(as.integer(unique(name)))),
#     year = as.integer(year)) %>% 
#   ggplot(aes(x = year, y = value/1e3, fill = name)) +
#   geom_col() +
#   facet_wrap(~type, ncol = 1) +
#   scale_fill_viridis_d() +
#   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#   labs(x = "Year", y = "Catch-at-age (millions?)", fill = "Age")

## ####


### Surveys ####

survey_names <- c("EcosystemSurvey", "WinterSurvey", "RussianSurvey", "WGIDEEPS")

surveys <- setNames(
  lapply(survey_names, function(k){
    # print(k)
    
    out <- read_table(paste0("data/from scaa model/", k, ".txt"),
                      show_col_types = FALSE) %>%
      filter(Year > 1991) %>% 
      setNames(gsub("\\+", "", colnames(.))) %>% 
      mutate(across(everything(), as.numeric)) %>%
      mutate(across(everything(), ~if_else(is.nan(.), NA, .))) %>% 
      suppressWarnings()
    
    
    test <- suppressWarnings(as.integer(colnames(out)))
    test <- test[!is.na(test)]
    
    if(any(test > 19)) {
      
      out <- out %>% 
        pivot_longer(cols = -Year) %>% 
        mutate(name = as.integer(name)) %>% 
        mutate(name = if_else(name > 19, 19, name)) %>% 
        pivot_wider(values_fn = ~ sum(.x, na.rm = TRUE))  %>% 
        # mutate(across(everything(), ~if_else(. == 0, NA, .))) # remove 0s
        column_to_rownames("Year") %>% 
        select(where(~!all(is.na(.x)))) %>% 
        as.matrix()
      
      
    } else {
      out <- out %>% column_to_rownames("Year") %>% 
        select(where(~!all(is.na(.x)))) %>% 
        as.matrix()
    }
    
    attr(out, "time") <- 
      case_when(k == "EcosystemSurvey" ~ rep(9/12, 2),
                k == "WinterSurvey" ~ rep(2/12, 2),
                k == "WGIDEEPS" ~ rep(7/12, 2),
                k == "RussianSurvey" ~ rep(9/12, 2) # find correct time here
      )
    
    attr(out, "twofirst") <- c(1,1)
    
    return(out)
  }), survey_names)

## ####

# lapply(seq_along(surveys), function(i) {
#   as_tibble(surveys[[i]], rownames = "year") %>% 
#     pivot_longer(-year) %>% 
#     add_column(type = names(surveys)[i], .before = 1)
# }) %>% bind_rows() %>% 
#   mutate(
#     name = 
#       factor(name, 
#              rev(as.integer(unique(name)))),
#     year = as.integer(year)) %>% 
#   ggplot(aes(x = year, y = value/1e3, fill = name)) +
#   geom_col() +
#   facet_wrap(~type, ncol = 1, scales = "free_y") +
#   scale_fill_viridis_d() +
#   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#   labs(x = "Year", y = "Index", fill = "Age")

## ####


### Maturity proportion ####

mo <- read_table("data/from scaa model/MaturityAtAge.txt",
                 show_col_types = FALSE) %>% 
  column_to_rownames("Year") %>% 
  setNames(gsub("\\+", "", colnames(.))) %>% 
  as.matrix()

## ####

# mo %>% 
#   as_tibble(rownames = "year") %>% 
#   pivot_longer(-year) %>% 
#   mutate(name = as.integer(name),
#          year = as.integer(year)) %>% 
#   ggplot(aes(name, value, color = year, group = year)) +
#   geom_path() +
#   scale_color_viridis_c(direction = -1) +
#   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#   labs(x = "Age", y = "Maturity proportion", color = "Age") +
#   theme(legend.position = "bottom")

## ####

### Weight at age ####

sw <- read_table("data/from scaa model/WeightAtAge.txt",
                 show_col_types = FALSE) %>% 
  column_to_rownames("Year") %>% 
  setNames(gsub("\\+", "", colnames(.))) %>% 
  as.matrix()

## ####

# sw %>%
#   as_tibble(rownames = "year") %>%
#   pivot_longer(-year) %>%
#   mutate(name = as.integer(name),
#          year = as.integer(year)) %>%
#   ggplot(aes(name, value, color = year, group = year)) +
#   geom_path() +
#   scale_color_viridis_c(direction = -1) +
#   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#   scale_x_continuous(
#     limits = c(0,round(as.integer(colnames(sw)[length(colnames(sw))])/10)*10),
#     expand = c(0,0)) +
#   labs(x = "Age", y = "Weight (kg)", color = "Age") +
#   theme(legend.position = "bottom")

## ####

### Dummy datasets ####

dummy0 <- mo
dummy0[dummy0 != 0] <- 0

dummy1 <- dummy0
dummy1[dummy1 == 0] <- 1

### Natural mortality ####

nm <- dummy0
nm[nm == 0] <- 0.07 # modify as needed

# SAM ####

### Setup SAM ####

dat <- setup.sam.data(
  surveys = surveys,
  residual.fleet = cn, 
  prop.mature = mo, 
  stock.mean.weight = sw, 
  catch.mean.weight = sw, 
  dis.mean.weight = sw, 
  land.mean.weight = sw,
  prop.f = dummy0, 
  prop.m = dummy0, 
  natural.mortality=nm, 
  land.frac = dummy1)

conf <- defcon(dat)

par = defpar(dat,conf)

### Fit SAM ####

fit = sam.fit(dat,conf,par)

# Code copied from the Olav Nikolai example ####

#Usefull commands
AIC(fit) #Calcualte AIC
res = residuals(fit) #Caclulate osa-residuals
plot(res)
residDiagPlot(fit,resid = res) #Tests for patterns in residuals
jj = jit(fit) #Jitter analyuss
sim = simstudy(fit) #Simstudy
ll = leaveout(fit) #Leave-out analysis
plot(ll)
ret = retro(fit,year = 5) #Retrospective analysis
plot(ret)
mohn(ret)
parplot(ret)
yearMat =  matrix(c(2023, 2024, 2024,2024, 
                    2022, 2023, 2023,2023, 
                    2021, 2022, 2022,2022,
                    2020, 2021, 2021,2021,
                    2019, 2020, 2020,2020), 
                  nrow = 5, ncol = 4, byrow = TRUE) #year to remove in retor analysis
retManual = retro(fit,year = yearMat) #Retrospective analysis, manually provide years to remove from each fleet

#Plotting functionality
ssbplot(fit)
fbarplot(fit)
recplot(fit)
tsbplot(fit)
fselectivityplot(fit)
predstdplot(fit)#If prediciton-variance link is included
fitplot(fit) #Prediction vs observations
corplot(fit) #Estimated observation correlation 
catchplot(fit) #plot aggregated catch and estimate in each year
dataplot(fit) #Illustrate years with observations
srplot(fit) #Stock-recruitment plot

#Useful tables
partable(fit)
faytable(fit) #Fishing mortality at age
fbartable(fit) 
caytable(fit) #Esimated catch per age
ntable(fit) #Estimated abundance at age
rectable(fit) #Estimated recruitment
