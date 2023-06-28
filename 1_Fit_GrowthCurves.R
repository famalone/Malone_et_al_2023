if ("magrittr" %in% rownames(installed.packages()) == FALSE) {install.packages("magrittr")}
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if ("minpack.lm" %in% rownames(installed.packages()) == FALSE) {install.packages("minpack.lm")}
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
if ("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if ("xlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("xlsx")}
if ("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}

###### Tree Growth Curves and Cessation Day (Author: Fin Malone)
library(magrittr)
library(dplyr)
library(minpack.lm)
library(lubridate)
library(readxl)
library(xlsx)
library(stringr)

############## DATA ORGANIZATION ################

work.dir <- 'D:/Documents/1_MASTERS/Malone_et_al_2023' # Folder Directory

# Dendrometer data and environmental variables grouped and summarized by Day and ID
dendro_data <- readxl::read_excel(paste0(work.dir,'/_2_lubrecht_data_thesis.xlsx'),
                                  sheet = 'time_series_30min', na = 'NA', col_names = T) %>%
  dplyr::select(dendro, day, Temp_Ave, TDR1_W, TDR2_W, TDR1_T, TDR2_T, VPD_Ave, ID) %>%
  group_by(day,ID) %>%
  dplyr::summarise(max_dendro = max(dendro, na.rm = T))   # daily maximum value

dendro_data = left_join(dendro_data, readxl::read_excel(paste0(work.dir,'/_2_lubrecht_data_thesis.xlsx'),
                                                        sheet = 'time_series_30min', na = 'NA', col_names = T) %>%
                          filter(month(time) %in% c(5,6,7,8)) %>% # Standardizes the time period of environmental data to include only months where all sites had
                                                                  # beginning and ending measurements (equipment was installed and taken down at different times)
                          dplyr::select(dendro, day, Temp_Ave, TDR1_W, TDR2_W, TDR1_T, TDR2_T, VPD_Ave, ID) %>%
                          group_by(day,ID) %>%
                          dplyr::summarise(mean_temp = mean(Temp_Ave, na.rm = T), # average of daily values
                                    mean_TDR1 = mean(TDR1_W, na.rm = T),   # average of daily values
                                    mean_TDR1_T = mean(TDR1_T, na.rm = T), # average of daily values
                                    mean_TDR2 = mean(TDR2_W, na.rm = T),   # average of daily values
                                    mean_TDR2_T = mean(TDR2_T, na.rm = T), # average of daily values
                                    mean_VPD = mean(VPD_Ave, na.rm = T)))  # average of daily values

# Geospatial Data
site_data <- readxl::read_excel(paste0(work.dir,'/_2_lubrecht_data_thesis.xlsx'),
                                sheet = 'pre_site_data', col_names = T) %>%
  dplyr::select(ID, TPI, Elevation) %>%
  mutate(topo = as.numeric(stringr::str_sub(ID,-1)), # looks at site ID to determine topographic position,
         topo = case_when(                           # aspect, and elevation types.
           topo %in% c(1, 3, 5) ~ 'Hollow',
           T ~ 'Upslope'),
         aspect = stringr::str_sub(ID,-3,-3),
         aspect = case_when(
           aspect %in% 'N' ~ 'North', T ~ 'South'
         ),
         elev = stringr::str_sub(ID,-5,-5),
         elev = case_when(
           elev %in% 'L' ~ 'Low', elev %in% 'M' ~ 'Middle', T ~ 'High'
         )
  )


########################### GROWTH FUNCTIONS ############################

# Gompertz Growth Curve Equation

Gompertz <- function(x, y0, ymax, k, lag) {                     # This form of the Gompertz equation accounts for y and x axis values
  result <- y0 + (ymax -y0)*exp(-exp(k*(lag-x)/(ymax-y0) + 1))  # starting above or below zero. Useful for point dendrometer timeseries.
  return(result)                                                # Got this equation from:
}                                                               # http://www.ipb.pt/~vcadavez/websiteVC/tutorial/rcode/2019-04-28-gompertzmodel/

# Fit Curve Function

fit_curve = function(data) { # data = daily dendro_data for individual site
  
  Gomp <- tryCatch({nlsLM(data = data,
                          data$max_dendro ~ Gompertz(data$day, y0, ymax, k, lag),
                          control = nls.lm.control(maxiter=1000),
                          start = list(y0 = min(data$max_dendro, na.rm = T),
                                       ymax = max(data$max_dendro, na.rm = T), k = 5, lag = 20))},
                   error = function(e) # Most curves fit with k = 5. This catches a fitting error and retries it with k = 20.
                   {nlsLM(data = data,
                          data$max_dendro ~ Gompertz(data$day, y0, ymax, k, lag),
                          control = nls.lm.control(maxiter=1000),
                          start = list(y0 = min(data$max_dendro, na.rm = T),
                                       ymax = max(data$max_dendro, na.rm = T), k = 50, lag = 20))}) 
  
  coefs <- coef(Gomp)
  print(coefs) # displays final parameters of fitted curve
  
  y0   <- coefs[1]  # store final parameters
  ymax <- coefs[2]
  k   <- coefs[3]
  lag <- coefs[4]
  
  days <- seq(min(data$day),max(data$day)) # Julian days within measurement period
  
  gompvals <- Gompertz(days, # values of Gompertz curve for every Julian day
                       y0 = y0, ymax = ymax, k = k, lag=lag)
  
  gompvals = tibble(
    day = days,
    gompvals = gompvals,
    ID = data$ID[1]
  )
  
  return(left_join(gompvals, data, by = c('day', 'ID')))
}


###################### FIT CURVES #########################

# list of sites for fitting curves
sitelist = unique(dendro_data$ID)

for(i in 1:length(sitelist)){ # Fit curves for all sites in sitelist. Displays site name and fitted parameters.
  print(sitelist[i])
  if(i == 1){
    dendro_data_gomp = filter(dendro_data,ID == sitelist[i]) %>% fit_curve()
  } else {
    dendro_data_gomp = full_join(dendro_data_gomp, filter(dendro_data, ID == sitelist[i]) %>% fit_curve(),
                                 by = c("day", "ID", "max_dendro", "mean_temp", "mean_TDR1",
                                        "mean_TDR1_T","mean_TDR2_T", "mean_TDR2", "mean_VPD", "gompvals"))
  }
}


####################### CESSATION DAY ########################

percent_growth <- 0.90 # percent of total growth curve at which cessation day occurs.

# returns a list of the rate of growth change from day to day.
rate <- function(data) # data = dendro_data_gomp for individual site
{
  chng <- rep(0, length(data$day))
  
  for(i in 1:length(data$day)) {
    if (i == 1) {
      chng[i] = 0 # beginning of growth has a growth rate of zero
    }
    else {
      chng[i] = data$gompvals[i] - data$gompvals[i-1] # rate of change from day to day
    }
  }
  return(chng)
}

# returns the cessation day for a single growth curve based on the percent_growth value.

cessation_gomp <- function(data,percent_growth) # data = dendro_data_gomp for individual site
{
  numdays <- max(data$day) - min(data$day) # number of days
  
  total_growth = max(data$gompvals) - min(data$gompvals) # total growth during the measurement period
  
  for (i in 1:(numdays+1)) {
    if (data$gompvals[i] - min(data$gompvals) >= percent_growth*total_growth) {
      
      cesday = min(data$day) + i # day of cessation (growth reaches percent_growth)
      break
    }
    if (max(rate(data), na.rm = T) < 1) {
      cesday = min(data$day) + i # day of cessation (growth is negligible; less than 1 micrometer per day)
      break
    }
  }
  return(cesday)
}

# Find cessation day for all sites in sitelist and add to site_data.

for(i in 1:length(sitelist)){
  print(sitelist[i])
  if(i == 1){
    site_data_ces = tibble(ID = sitelist[i],
                                cessation_day_gomp = filter(dendro_data_gomp,
                                                            ID == sitelist[i]) %>% cessation_gomp(percent_growth))
  } else {
    site_data_ces = full_join(site_data_ces,                           # adds onto dataframe above.
                              tibble(ID = sitelist[i],
                                     cessation_day_gomp = filter(dendro_data_gomp,
                                                                 ID == sitelist[i]) %>% cessation_gomp(percent_growth)),
                              by = c("ID", "cessation_day_gomp"))
  }
}

site_data <- left_join(site_data, site_data_ces, by = 'ID')

# Transform dendrometer readings to growth change from minimum gompertz value

for(i in 1:length(sitelist)) { # creates comparable curves starting at 0 micrometers)
  
  if(i == 1){
    start_at_zero <- dendro_data_gomp %>% # starts a dataframe with dendrometer readings starting at zero.
      filter(ID == sitelist[i]) %>%
      mutate(max_dendro = max_dendro - min(gompvals),
             gompvals = gompvals - min(gompvals))
  } else {
    start_at_zero <- full_join(start_at_zero, dendro_data_gomp %>% # adds onto dataframe above.
                            filter(ID == sitelist[i]) %>%
                            mutate(max_dendro = max_dendro - min(gompvals),
                                   gompvals = gompvals - min(gompvals)))
  }
}
dendro_data_gomp = start_at_zero

##################### EXPORT DATA ############################

write.xlsx(as.data.frame(site_data), paste0(work.dir,'/_3_processed_data_thesis.xlsx'), sheetName = "post_site_data",
           col.names = T, row.names = F, append = F)
write.xlsx(as.data.frame(dendro_data_gomp), paste0(work.dir,'/_3_processed_data_thesis.xlsx'), sheetName = "time_series_daily_and_curves",
           col.names = T, row.names = F, append = T)
