if ("magrittr" %in% rownames(installed.packages()) == FALSE) {install.packages("magrittr")}
if ("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
if ("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
if ("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if ("xlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("xlsx")}
if ("viridis" %in% rownames(installed.packages()) == FALSE) {install.packages("viridis")}
if ("Rmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("Rmisc")}
if ("ggsignif" %in% rownames(installed.packages()) == FALSE) {install.packages("ggsignif")}
if ("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
if ("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart")}
if ("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot")}
if ("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}

library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)
library(readxl)
library(xlsx)
library(viridis)
library(Rmisc)
library(ggsignif)
library(MASS)
library(rpart)
library(rpart.plot)
library(stringr)
library(visreg)
library(gridExtra)
library(patchwork)
library(splines)


###################################### ALL DATA INPUT AND TRANSFORMATIONS ################################################

work.dir = 'D:/Documents/1_MASTERS/Malone_et_al_2023' # Folder Directory

# Precipitation data from the Lubrecht Flume SNOTEL station.
Lflume <- readxl::read_excel(paste0(work.dir,'/_2_lubrecht_data_thesis.xlsx'),
                             sheet = 'lubrecht_precip_daily', col_names = T) %>%
  mutate(Day = as.Date(Day,    # Convert Julian day to date
                       origin = as.Date("2016-01-01")))

# Daily environmental data and growth curves.
dendro_data <- readxl::read_excel(paste0(work.dir,'/_3_processed_data_thesis.xlsx'),
                                  sheet = 'time_series_daily_and_curves', col_names = T) %>%
  mutate(topo = case_when(as.numeric(stringr::str_sub(ID,-1)) %in% c(1, 3, 5) ~ 'Hollow', # looks at site ID to determine topographic position,
                                            T ~ 'Upslope'),                               # aspect, and elevation types.
         elev = case_when(stringr::str_sub(ID,-5,-5) %in% 'L' ~ 'Low',
                               stringr::str_sub(ID,-5,-5) %in% 'M' ~ 'Middle', T ~ 'High'),
         aspect = case_when(stringr::str_sub(ID,-3,-3) %in% 'N' ~ 'North', T ~ 'South'),
         TPI = NA,
         cessation_day_gomp = NA,
         mean_TDR = (mean_TDR1 + mean_TDR2) / 2, mean_TDR_T = (mean_TDR1_T + mean_TDR2_T) / 2, # average soil temp and moisture using both depths.
         day = as.Date(day,    # Convert Julian day to date
                       origin = as.Date("2016-01-01")))


# Average site conditions for entire season
averages <- dendro_data %>% filter(month(day) %in% c(5,6,7,8)) %>% # Standardizes the time period of environmental data to include only months where all sites had
                                                                   # beginning and ending measurements (equipment was installed and taken down at different times)
  group_by(ID) %>% dplyr::summarise(avg_temp = mean(mean_temp, na.rm = T),
                                                       avg_soil_temp05 = mean(mean_TDR1_T, na.rm = T),
                                                       avg_soil_temp50 = mean(mean_TDR2_T, na.rm = T),
                                                       avg_VPD = mean(mean_VPD, na.rm = T),
                                                       avg_TDR1 = mean(mean_TDR1, na.rm = T),
                                                       avg_TDR2 = mean(mean_TDR2, na.rm = T))

# Add average conditions to site data
site_data <- left_join(readxl::read_excel(paste0(work.dir,'/_3_processed_data_thesis.xlsx'),
                                sheet = 'post_site_data', col_names = T), averages, by = 'ID')

# Average for two soil depths
site_data = site_data %>% mutate(avg_soil_temp = (avg_soil_temp05 + avg_soil_temp50)/2,
                                 avg_TDR = (avg_TDR1 + avg_TDR2)/2)

# Add TPI and cessation day to the daily data for groupings.
for(i in 1:length(dendro_data$TPI)) {
  dendro_data$TPI[i] = site_data$TPI[which(dendro_data$ID[i] == site_data$ID)]
  dendro_data$cessation_day_gomp[i] = site_data$cessation_day_gomp[which(dendro_data$ID[i] == site_data$ID)]
}

dendro_data = dendro_data %>% arrange(TPI) # reorder by TPI for time series overlap.

###################### Figure Parameters (Color and Text)

# scale_color_viridis
alpha = 1
begin = 0.1
end = .8
# line thickness
linesize = 1.2
# font sizes
numsize = 11
labsize = 12
tagsize = 14
# geom_point
pointsize = 2

######################### APPENDIX A FIGURES ########################


# lists of sites by elevation
low <- c('LENA1','LENA2','LENA3','LENA7',
         'LESA1','LESA2','LESA3','LESA5','LESA6')

middle <- c('MENA1','MENA2','MENA3','MENA6',
            'MESA1','MESA2','MESA4','MESA8','MESA9')

high <- c('HENA1','HENA3','HENA5','HENA8','HENA9',
          'HESA2','HESA3','HESA4','HESA5','HESA9')

# Daily Growth Curves

App_A1 <- ggplot(data = dendro_data %>% filter(ID %in% low)) +
  geom_line(aes(x = day, y = max_dendro), color = '#999999', size = 1) +
  geom_point(aes(x = day, y = max_dendro), color = '#009E73', size = 1.5) +
  geom_line(aes(x = day, y = gompvals), color = 'black', size = 1) +
  geom_vline(data = site_data %>% filter(ID %in% low),
             aes(xintercept = as.Date(cessation_day_gomp,origin = as.Date("2016-01-01"))), size = 1,
             linetype = 'dashed', color = '#D55E00') +
  xlab('Time') +
  scale_y_continuous(expression(paste('Daily Maximum Radial Growth (  ', mu ,'m )'))) +
  facet_wrap(~ ID, scales = 'free_y', ncol = 3)
App_A1

ggsave(filename = paste0(work.dir,'/Figures/Appendix_A1.tiff'),
       plot = App_A1,
       dpi = 800, units = "in", width = 7.5, height = 4)


App_A2 <- ggplot(data = dendro_data %>% filter(ID %in% middle)) +
  geom_line(aes(x = day, y = max_dendro), color = '#999999', size = 1) +
  geom_point(aes(x = day, y = max_dendro), color = '#009E73', size = 1.5) +
  geom_line(aes(x = day, y = gompvals), color = 'black', size = 1) +
  geom_vline(data = site_data %>% filter(ID %in% middle),
             aes(xintercept = as.Date(cessation_day_gomp,origin = as.Date("2016-01-01"))), size = 1,
             linetype = 'dashed', color = '#D55E00') +
  xlab('Time') +
  scale_y_continuous(expression(paste('Daily Maximum Radial Growth (  ', mu ,'m )'))) +
  facet_wrap(~ ID, scales = 'free_y', ncol = 3)
App_A2

ggsave(filename = paste0(work.dir,'/Figures/Appendix_A2.tiff'),
       plot = App_A2,
       dpi = 800, units = "in", width = 7.5, height = 4)


App_A3 <- ggplot(data = dendro_data %>% filter(ID %in% high)) +
  geom_line(aes(x = day, y = max_dendro), color = '#999999', size = 1) +
  geom_point(aes(x = day, y = max_dendro), color = '#009E73', size = 1.5) +
  geom_line(aes(x = day, y = gompvals), color = 'black', size = 1) +
  geom_vline(data = site_data %>% filter(ID %in% high),
             aes(xintercept = as.Date(cessation_day_gomp,origin = as.Date("2016-01-01"))), size = 1,
             linetype = 'dashed', color = '#D55E00') +
  xlab('Time') +
  scale_y_continuous(expression(paste('Daily Maximum Radial Growth (  ', mu ,'m )'))) +
  facet_wrap(~ ID, scales = 'free_y', ncol = 3)
App_A3

ggsave(filename = paste0(work.dir,'/Figures/Appendix_A3.tiff'),
       plot = App_A3,
       dpi = 800, units = "in", width = 7.5, height = 5)



############################### Figure 2 ###################################

# Precipitation Hyetograph
p0 <- ggplot() +
  geom_bar(data = Lflume, aes(x = Day, y = rain),stat = 'identity', fill = '#0072B2') +
  geom_bar(data = Lflume, aes(x = Day, y = melt),stat = 'identity', fill = 'lightblue') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=4))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_reverse('Precipitation (mm)', expand = expansion(mult = c(0.05,0)), breaks = c(0,10,20)) +
  guides(color = FALSE)


# Air Temperature
p1 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_temp, group = TPI, color = TPI), size = 1) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=5))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Air Temperature (\u00B0C)',expand = expansion(mult = c(0,0.05))) +
  guides(color = FALSE)


# Vapor Pressure Deficit
p2 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_VPD*-1, group = TPI, color = TPI), size = 1) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=2))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('VPD (-kPa)',expand = expansion(mult = c(0,0.05))) +
  guides(color = FALSE)


# Soil Temperature
p3 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_TDR_T, group = TPI, color = TPI), size = 1) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=5))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Soil Temperature (\u00B0C)',expand = expansion(mult = c(0,0.05)))


# Soil Moisture
p4 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_TDR*100, group = TPI, color = TPI), size = 1) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=1.5))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Soil Moisture (%)', expand = expansion(mult = c(0,0.05))) +
  guides(color = FALSE)


# Tree Growth
p5 <- ggplot() +
  geom_line(data = dendro_data,
            aes(x = day, y = gompvals/1000, group = TPI, color = TPI), size = 1) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=4))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Radial Growth (mm)',expand = expansion(mult = c(0.02,0.05))) +
  guides(color = FALSE)


# Growth Cessation
p6 <- ggplot() +
  geom_vline(data = site_data, aes(xintercept = as.Date(cessation_day_gomp,    # Convert Julian day to date
                                                        origin = as.Date("2016-01-01")),
                                   group = TPI, color = TPI), size = 2.5) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8), axis.title.y = element_text(margin=margin(r=18.5))) +
  scale_x_date(name = 'Time', limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  guides(color = FALSE) +
  scale_y_continuous('Cessation Day')


# Combine all time series

timeseries <- plot_grid(p0,p1,p2,p3,p4,p5,p6, nrow = 7, rel_heights = c(.9, 1, 1, 1.15, 1, 1, 1),
                        axis = 'rl', align = 'v')
timeseries

ggsave(filename = paste0(work.dir,'/Figures/Figure_2.tiff'),
       plot = timeseries,
       dpi = 800, units = "in", width = 8.5, height = 8)



############################### FIGURE 3 #################################
                                                                          ##### SM #####
LM1 = lm(formula = avg_TDR*100 ~ TPI + Elevation + aspect, data = site_data )
summary(LM1)
# Stepwise AIC indicates elevation is not important here
stepAIC(LM1, trace = T)
# Therefore:
LM1 = lm(formula = avg_TDR*100 ~ TPI + aspect, data = site_data )
capture.output(summary(LM1), file = paste0(work.dir,"/Stat_Output/1_Fig3_SM.txt")) #.txt file with summary output.

yhat1 = predict(LM1)

p1 <- ggplot(data = site_data , aes(x = yhat1, y = avg_TDR*100)) +
  geom_point(size = pointsize, shape = 21, stroke = 1.5, color = '#0072B2') +
  geom_abline(color = '#014a75', slope = 1, size = 1, linetype = 'dashed') +
  scale_y_continuous('Observed Soil Moisture (%)', limits = c(8,36), breaks = seq(0,100,by = 10)) +
  scale_x_continuous('Predicted Soil Moisture (%)', limits = c(8,36),  breaks = seq(0,100,by = 10)) +
  geom_label(label = paste('R2 = ', signif(summary(LM1)$adj.r.squared,2)),
             x = 8 + (36-8)*(1/6), y = 36 - (36-8)*(1/6), label.size = NA) +
  geom_label(label = 'TPI\nAspect',
             x = 36 - (36-8)*(1/6), y = 8 + (36-8)*(1/6), label.size = NA) +
  labs(tag = 'B') +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))


                                                                          ##### VPD #####
LM2 = lm(formula = avg_VPD ~ TPI + Elevation + aspect, data = site_data )
summary(LM2)
# Stepwise AIC indicates all three are important
stepAIC(LM2)
capture.output(summary(LM2), file = paste0(work.dir,"/Stat_Output/2_Fig3_VPD.txt")) #.txt file with summary output.

yhat2 = predict(LM2)

p2 <- ggplot(data = site_data , aes(x = yhat2*-1, y = avg_VPD*-1)) +
  geom_point(size = pointsize, shape = 21, stroke = 1.5, color = '#D55E00') +
  geom_abline(slope = 1, size = 1, linetype = 'dashed', color = '#A04800') +
  scale_y_continuous('Observed VPD (-kPa)', limits = c(0.4, 1.3), breaks = seq(0,2, by = 0.2)) +
  scale_x_continuous('Predicted VPD (-kPa)', limits = c(0.4, 1.3), breaks = seq(0,2, by = 0.2)) +
  geom_label(label = paste('R2 = ', signif(summary(LM2)$adj.r.squared,2)),
             x = 0.4 + (1.3-0.4)*(1/6), y = 1.3 - (1.3-0.4)*(1/6), label.size = NA) +
  geom_label(label = 'Elevation\nTPI\nAspect',
             x = 1.3 - (1.3-0.4)*(1/6), y = 0.4 + (1.3-0.4)*(1/6), label.size = NA) +
  labs(tag = 'A') +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

                                                                        ##### Air Temp #####
LM3 = lm(formula = avg_temp ~ TPI + Elevation + aspect, data = site_data )
summary(LM3)
# Stepwise AIC indicates all three are important
stepAIC(LM3)
capture.output(summary(LM3), file = paste0(work.dir,"/Stat_Output/3_Fig3_airtemp.txt")) #.txt file with summary output.

yhat3 = predict(LM3)

p3 <- ggplot(data = site_data , aes(x = yhat3, y = avg_temp)) +
  geom_point(size = pointsize, shape = 21, stroke = 1.5, color = '#D55E00') +
  geom_abline(slope = 1, size = 1, linetype = 'dashed', color = '#A04800') +
  scale_y_continuous('Observed Air Temp. (\u00B0C)', limits = c(10.5, 15), breaks = c(1:15)) +
  scale_x_continuous('Predicted Air Temp. (\u00B0C)', limits = c(10.5, 15), breaks = c(1:15)) +
  geom_label(label = paste('R2 = ', signif(summary(LM3)$adj.r.squared, 2)),
             x = 10.5 + (15-10.5)*(1/6), y = 15 - (15-10.5)*(1/6), label.size = NA) +
  geom_label(label = 'Elevation\nTPI\nAspect',
             x = 15 - (15-10.5)*(1/6), y = 10.5 + (15-10.5)*(1/6), label.size = NA) +
  labs(tag = 'C') +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

                                                                          ##### Soil Temp #####
LM4 = lm(formula = avg_soil_temp ~ TPI + Elevation + aspect, data = site_data )
summary(LM4)
# Stepwise AIC indicates all three are important
stepAIC(LM4)
capture.output(summary(LM4), file = paste0(work.dir,"/Stat_Output/4_Fig3_soiltemp.txt")) #.txt file with summary output.

yhat4 = predict(LM4)

p4 <- ggplot(data = site_data , aes(x = yhat4, y = avg_soil_temp)) +
  geom_point(size = pointsize, shape = 21, stroke = 1.5, color = '#0072B2') +
  geom_abline(slope = 1, size = 1, linetype = 'dashed', color = '#014a75') +
  scale_y_continuous('Observed Soil Temp. (\u00B0C)', limits = c(6.5, 13), breaks = c(1:15)) +
  scale_x_continuous('Predicted Soil Temp. (\u00B0C)', limits = c(6.5, 13), breaks = c(1:15)) +
  geom_label(label = paste('R2 = ', signif(summary(LM4)$adj.r.squared,2)),
             x = 6.5 + (13-6.5)*(1/6), y = 13 - (13-6.5)*(1/6), label.size = NA) +
  geom_label(label = 'Elevation\nTPI\nAspect',
             x = 13 - (13-6.5)*(1/6), y = 6.5 + (13-6.5)*(1/6), label.size = NA) +
  labs(tag = 'D') +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

TPI_regressions <- plot_grid(p2,p1,p3,p4, align = "hv")
TPI_regressions

ggsave(filename = paste0(work.dir,'/Figures/Figure_3.tiff'),
       plot = TPI_regressions,
       dpi = 800, units = "in", width = 6, height = 5.5)



############################## Results: t-tests of cessation day by group #################################

# Cessation day differs by topographic position
(test1 = t.test(cessation_day_gomp ~ topo, data = site_data))
capture.output(print(test1), file = paste0(work.dir,"/Stat_Output/5_TopoPosition_Compare.txt")) #.txt file with t-test results.

# Cessations separated into June and after June:

(test2 = t.test(TPI ~ cessation_day_gomp > 181, data = site_data))
capture.output(print(test2), file = paste0(work.dir,"/Stat_Output/6_June_NonJune_Compare.txt")) #.txt file with t-test results.

# Categorical Site Cessation Comparisons:

(test3 = t.test(cessation_day_gomp ~ elev == 'High', data = site_data)) # Differs by elevation between high and middle + low.
(test4 = t.test(cessation_day_gomp ~ aspect, data = site_data)) # Does not differ by Aspect
capture.output(print(test3),print(test4), file = paste0(work.dir,"/Stat_Output/7_Elev_Aspect_Compare.txt")) #.txt file with t-test results.



############################## REGRESSION TREE ANALYSIS ####################################

tree1 <- rpart(cessation_day_gomp ~ TPI + avg_VPD + avg_TDR,
              data = site_data , method = 'anova',
              control = rpart.control(cp = 0.01, minsplit = 10, maxdepth = 3))

summary(tree1)


png(file = paste0(work.dir,'/Figures/Figure_4.jpg'), width = 600, height = 500)
rpart.plot(tree1, faclen = 0, extra = 1 , roundint = F, digits = 3)
dev.off()

# separate leaves

leaves <- tibble(ID = site_data  %$% ID, node = tree1$where)
leaves <- tibble(node = unique(tree1$where), sites = c(list(leaves %>% filter(node == 3) %$% ID),
                                                      list(leaves %>% filter(node == 4) %$% ID),
                                                      list(leaves %>% filter(node == 6) %$% ID),
                                                      list(leaves %>% filter(node == 8) %$% ID),
                                                      list(leaves %>% filter(node == 9) %$% ID)))
# shows which sites are in which leaves
leaves$sites[1]
leaves$sites[2]
leaves$sites[3]
leaves$sites[4]
leaves$sites[5]

# ranked importance of variables in model
tree1$variable.importance

# STATISTICAL SIGNIFICANCE OF REGRESSION SPLITS
# 1st Split
(test5 = t.test(cessation_day_gomp ~ TPI >= -5.19, data = site_data ))
# 2nd Split TPI > 5.2
(test6 = t.test(cessation_day_gomp ~ avg_TDR >= 0.142, data = site_data %>% filter(TPI >= -5.19) ))
# 3rd Split TPI < 5.2
(test7 = t.test(cessation_day_gomp ~ avg_TDR >= 0.263, data = site_data %>% filter(TPI < -5.19) ))
# 4th Split TPI < 5.2 and avg_TDR < 0.26
(test8 = t.test(cessation_day_gomp ~ avg_VPD < -0.944, data = site_data %>% filter(TPI < -5.19 & avg_TDR < 0.263) ))
capture.output('RANKED MODEL VARIABLES:', tree1$variable.importance, print(test5), print(test6), print(test7), print(test8),
               file = paste0(work.dir,"/Stat_Output/8_RegressionTree_Nodes.txt")) #.txt file with t-test results.


######################################## FIGURE 5 ###################################################
# Cessation Predicted by Landscape Position Under Moderate Conditions #
site_exclude <- site_data %>% filter(!(ID %in% c('HENA1','LENA3','MENA3'))) # Excludes hollow sites with saturated soils

LM5 = lm(formula = cessation_day_gomp ~ TPI + Elevation + aspect,
         data = site_exclude)
summary(LM5)

# Stepwise AIC identifies Aspect, TPI, and Elevation variables as statistically important
LM5 <- stepAIC(LM5)

capture.output(summary(LM5), file = paste0(work.dir,"/Stat_Output/9_Fig5_Landscape_Regression.txt")) #.txt file with summary output.

# Use the fitted model to predict cessation
yhat5 = predict(LM5)

# Three saturation-limited trees
VWC_excluded <- as.data.frame(site_data %>% filter(ID %in% c('HENA1','LENA3','MENA3')))
# Use the fitted model to predict cessation for the excluded trees
yhat6 = predict(LM5, newdata = VWC_excluded)     # To clarify: these three data points do not influence the model.
                                                 # Their predictions are displayed to show their deviance from the model.

############# Observed vs. Predicted
ces_pos <- ggplot(data = site_exclude, aes(x = yhat5, y = cessation_day_gomp)) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed', color = '#004f3a') +
  geom_point(size = pointsize, shape = 21, stroke = 1.5, color = '#009E73') +
  geom_point(data = VWC_excluded, aes(x = yhat6, y = cessation_day_gomp),
             size = pointsize, shape = 24, stroke = 1.5, color = '#0072B2') +
  scale_y_continuous('Observed Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  scale_x_continuous('Predicted Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  geom_label(label = str_glue('R² = {signif(summary(LM5)$adj.r.squared,2)}'),
             x = 155 + (230-155)*(1/12), y = 230 - (230-155)*(1/12), label.size = NA) +
  theme_classic() +
  guides(color = FALSE) +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))
ces_pos

################# Partial Effects Plots

response_var2 <- visreg(LM5, "TPI", scale = "linear", rug = TRUE,
                        xlab = 'TPI (m)', line.par = list(col = '#A04800'),
                        points.par = list(col = '#009E73'), gg = T) +
  # geom_point(data = VWC_excluded, aes(y = yhat6, x = TPI),
  #            size = 0.5, shape = 24, stroke = 1, color = '#0072B2') +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

response_var1 <- visreg(LM5, "Elevation", scale = "linear", rug = TRUE,
                        xlab = 'Elevation (m)', line.par = list(col = '#A04800'),
                        points.par = list(col = '#009E73'), gg = T) +
  # geom_point(data = VWC_excluded, aes(y = yhat6, x = Elevation),
  #            size = 0.5, shape = 24, stroke = 1, color = '#0072B2') +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

response_var3 <- visreg(LM5, "aspect", scale = "linear", rug = TRUE,
                        xlab = 'Aspect', line.par = list(col = '#A04800'),
                        points.par = list(col = '#009E73'), gg = T) +
  # geom_point(data = VWC_excluded, aes(y = yhat6, x = c(0.1,0.2,0.3)),
  #            size = 0.5, shape = 24, stroke = 1, color = '#0072B2') +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

ces_pos_vars <- grid.arrange(response_var1, response_var2, response_var3, ces_pos,
                             layout_matrix = cbind(c(4, 4, 4), c(1, 2, 3)),
                             nrow = 3, ncol = 2, widths = c(1, .5))

ggsave(filename = paste0(work.dir,'/Figures/Figure_5.tiff'),
       plot = ces_pos_vars,
       dpi = 800, units = "in", width = 8, height = 5)

################################### Figure 6 ##############################################

# Cessation Predicted by Microclimate #
site_data = site_data %>% mutate(HDI = avg_VPD/avg_TDR*-1, avg_VPD = avg_VPD*-1, avg_TDR = avg_TDR*100)

LM7 = lm(formula = cessation_day_gomp ~ poly(avg_TDR,2) + avg_VPD + avg_temp + avg_soil_temp,
         data = site_data)
summary(LM7)


# Stepwise AIC identifies all 3 variables as statistically important
LM7 <- stepAIC(LM7)
summary(LM7)
capture.output(summary(LM7), file = paste0(work.dir,"/Stat_Output/10_Fig6_Microclimate_Regression.txt")) #.txt file with summary output.

# Use the fitted model to predict cessation
yhat7 = predict(LM7)

################ Observed vs. Predicted
ces_env <- ggplot() +
  geom_abline(slope = 1, size = 1, linetype = 'dashed', color = '#004f3a') +
  geom_point(data = site_data, aes(x = yhat7, y = cessation_day_gomp),
             size = pointsize, shape = 21, stroke = 1.5, color = '#009E73') +
  scale_y_continuous('Observed Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  scale_x_continuous('Predicted Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  geom_label(aes(label = str_glue('R² = {signif(summary(LM7)$adj.r.squared,2)}'),
                 x = 155 + (230-155)*(1/12), y = 230 - (230-155)*(1/12)), label.size = NA) +
  # scale_color_viridis_c(option = 'H', begin = 0.05, end = 0.85, alpha = alpha) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))
ces_env

############# Partial Effects Plots
response_var1 <- visreg(LM7, "avg_TDR", scale = "linear", rug = TRUE,
                        xlab = 'Soil Moisture (%)', line.par = list(col = '#A04800'),
                        points.par = list(col = '#009E73'), gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

response_var2 <- visreg(LM7, "avg_VPD", scale = "linear", rug = TRUE,
                        xlab = 'VPD (-kPa)', line.par = list(col = '#A04800'),
                        points.par = list(col = '#009E73'), gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20), limits = c(150,240)) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

response_var3 <- visreg(LM7, "avg_temp", scale = "linear", rug = TRUE,
                        xlab = 'Air Temperature (\u00B0C)', line.par = list(col = '#A04800'),
                        points.par = list(col = '#009E73'), gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20), limits = c(150,240)) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "sans"),
        axis.text =  element_text(size = numsize,  family = "sans"),
        legend.text = element_text(size = numsize,  family = "sans"),
        legend.title = element_text(size = numsize,  family = "sans"),
        plot.tag = element_text(size = tagsize,  family = "sans"))

ces_env_vars <- grid.arrange(response_var1, response_var2, response_var3, ces_env,
                             layout_matrix = cbind(c(4, 4, 4), c(1, 2, 3)),
                             nrow = 3, ncol = 2, widths = c(1, .5))

ggsave(filename = paste0(work.dir,'/Figures/Figure_6.tiff'),
       plot = ces_env_vars,
       dpi = 800, units = "in", width = 8, height = 5)







