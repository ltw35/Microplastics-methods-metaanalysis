#Analysis of full lit review
# Code written by Lisa Watkins, ltw35@cornell.edu

## load necessary libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(ggalluvial)
library(GGally)
library(ggpubr) #combine plots into one
library(car)

### READ & CLEAN: Read in and clean literature summary spreadsheet--------------

voldata = read.csv("corestudies.csv")%>% 
  filter(omit==0) %>% 
  # edit data format for columns with numeric entries
  mutate(Cavg.L = as.numeric(Cavg.L),vol_L = as.numeric(vol_L),lowestsize_mm=as.numeric(lowestsize_mm)) %>% 
  mutate(vol_bin = ifelse(vol_L>100,">100L","0-100L")) %>% 
  mutate(fresh_marine = ifelse(river_ocean == "wetland"|river_ocean == "river"|river_ocean=="reservoir"|river_ocean == "pond"|river_ocean == "lake","freshwater","marine")) %>% 
  mutate(top_shape=ifelse(top_shape =="bead","pellet",top_shape)) %>% 
  mutate(volume_measure_simple = ifelse(volume_measure =="digital flowmeter"|volume_measure =="mechanical flowmeter","flowmeter",ifelse(volume_measure =="current meter"|volume_measure== "river velocity","flowmeter",ifelse(volume_measure == "time x speed","time",volume_measure)))) %>% 
mutate(vol_source_simple = ifelse(vol_source =="given","given","calculated")) %>% 
  mutate(secondary_id_simple = ifelse(secondary_id == "none", "no","yes")) %>% 
mutate(chem_separation = ifelse(separation=="none", "no","yes"))
# The ggplot colorblind palette with grey:
cbPalette <- c("#E69F00", "#CC79A7","#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#999999")
shapevec <- c(16,2,15,3,5)
#### MAIN PLOT-----------------

### Linear Model Concentration ~ Volume =====


## Check colinearity of numeric variables
pairs(~ log10(Cavg.L) + log10(vol_L)+latestyear_sampled+lowestsize_mm+topsize_upper_mm, data = voldata, row1attop=FALSE,
      subset = method=="net", main = "lit review data, net method")
pairs(~ log10(Cavg.L) + log10(vol_L)+latestyear_sampled+lowestsize_mm+topsize_upper_mm, data = voldata, row1attop=FALSE,
      subset = method=="grab", main = "lit review data, grab method")
pairs(~ log10(Cavg.L) + log10(vol_L)+latestyear_sampled+lowestsize_mm+topsize_upper_mm, data = voldata, row1attop=FALSE,
      subset = method=="pump", main = "lit review data, pump method")
## Check histogram of concentration values
hist(log10(voldata$Cavg.L))
hist(log10(voldata$vol_L))
##Check residuals
qqPlot(lm_voldata)

## histograms
voldata %>% 
  ggplot()+
  #geom_histogram(aes(x = latestyear_sampled))+
  #geom_histogram(aes(x = lowestsize_mm))+
  #geom_bar(aes(x = fresh_marine))+
  #geom_bar(aes(x = secondary_id))+
  geom_bar(aes(x = volume_measure))+
  facet_grid(~method, scale = "free_x")

# grab only
lm_grabdata = lm(data=subset(voldata,voldata$method =="grab"),log10(Cavg.L)~log10(vol_L)+lowestsize_mm+fresh_marine+chem_separation)
summary(lm_grabdata)
anova(lm_grabdata)
AIC(lm_voldata)

# net only
lm_netdata = lm(data=subset(voldata,voldata$method =="net"),log10(Cavg.L)~log10(vol_L)+lowestsize_mm+fresh_marine+volume_measure_simple+chem_separation)
summary(lm_netdata)
anova(lm_netdata)

#check assumptions
plot(lm_netdata)

voldata %>% 
  select(Cavg.L,vol_L,lowestsize_mm,fresh_marine,volume_measure, method) %>% 
ggpairs()

# pump only
lm_pumpdata = lm(data=subset(voldata,voldata$method =="pump"),log10(Cavg.L)~log10(vol_L)+lowestsize_mm+fresh_marine+volume_measure_simple)
summary(lm_pumpdata)
anova(lm_pumpdata)



### Concentration vs. volume plot====
#figure 4
voldata %>% 
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  ggplot(aes(x = vol_L,y=Cavg.L))+
  #stat_smooth(method = "lm", col = "gray", formula = log10(voldata$Cavg.L)~log10(voldata$vol_L)+voldata$method*voldata$lowestsize_mm+voldata$location)+
  geom_point(aes(color = method, shape=location), size = 1.5)+
  labs(shape = "location")+#,
      # subtitle = paste("Adj R2 = ",signif(summary(lm_voldata)$adj.r.squared, 5),
       #                 "Intercept =",signif(lm_voldata$coef[[1]],5 ),
        #                " Slope =",signif(lm_voldata$coef[[2]], 5),
         #               " P =",signif(summary(lm_voldata)$coef[2,4], 5)))+
  xlab("Volume Sampled, L")+
  ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_colour_manual(values = cbPalette)+
  scale_shape_manual(values = shapevec)+
  theme_bw()+ theme(text = element_text(size = 14))
#ggsave("volume_concentration.png",width = 7, height = 4, units = "in", dpi = 300)



#### FULL LINEAR REGRESSION------------------

  voldata = voldata %>% 
    mutate(blanks_subtracted_simple = ifelse(blanks_subtracted =="yes"|blanks_subtracted == "none","yes","no")) %>% 
    mutate(id_tech = ifelse(secondary_id == "none"|secondary_id =="hot needle", "no","yes"))
    
lm_voldata_full = lm(data = voldata,log10(Cavg.L)~log10(vol_L)+method+lowestsize_mm+fresh_marine+id_tech+blanks_subtracted_simple)
summary(lm_voldata_full)
anova(lm_voldata_full)

ggplot(data = voldata)+
  geom_col(aes(x = highest_id, y=Cavg.L))+
  facet_grid(.~fresh_marine)
#AIC(lm_voldata_full)
# Check assumptions
par(mfrow = c(2, 2))
plot(lm_voldata_full)
cor.test(voldata$vol_L, voldata$lowestsize_mm)


# check correlation (used sig. 0.05, but probably should Bonferroni correct!!!***)
  methvmesh = table(voldata$method, voldata$lowestsize_mm)
chisq.test(methvmesh) #p-value <0.05 says method & mesh size are dependent
methvwaterbody = table(voldata$method, voldata$fresh_marine)
chisq.test(methvwaterbody) #p-value >0.05 says method & waterbody type are independent
meshvwaterbody = table(voldata$lowestsize_mm, voldata$fresh_marine)
chisq.test(meshvwaterbody) #p-value >0.05 says waterbody type & mesh size are independent
methvvol = table(voldata$method, voldata$vol_L)
chisq.test(methvvol) #p-value >0.05 says method & volume are independent
meshvvol = table(voldata$lowestsize_mm, voldata$vol_L)
chisq.test(meshvvol) #p-value >0.05 says mesh size & volume are independent
volvwaterbody = table(voldata$vol_L, voldata$fresh_marine)
chisq.test(volvwaterbody) #p-value >0.05 says volume & waterbody type are independent
visvol= table(voldata$vol_L, voldata$secondary_id_simple)
chisq.test(visvol) #p-value <0.05 says volume & visual vs. advanced id are dependent
viswaterbody = table(voldata$fresh_marine, voldata$secondary_id_simple)
chisq.test(viswaterbody) #p-value <0.05 says waterbody type & visual vs. advanced id are dependent
vismeth = table(voldata$method, voldata$secondary_id_simple)
chisq.test(vismeth) #p-value <0.05 says method & visual vs. advanced id are dependent
vismesh = table(voldata$lowestsize_mm, voldata$secondary_id_simple)
chisq.test(vismesh) #p-value <0.05 says mesh size & visual vs. advanced id are dependent
vischem = table(voldata$chem_separation, voldata$secondary_id_simple)
chisq.test(vischem) #p-value <0.05 says chemical separation or not & visual vs. advanced id are dependent
chemvol = table(voldata$chem_separation, voldata$vol_L)
chisq.test(chemvol) #p-value <0.05 says chemical separation or not & volume are independent
chemwaterbody = table(voldata$chem_separation, voldata$fresh_marine)
chisq.test(chemwaterbody) #p-value <0.05 says chemical separation or not & waterbody type are dependent
chemmeth  = table(voldata$chem_separation, voldata$method)
chisq.test(chemmeth) #p-value <0.05 says chemical separation or not & method are independent
chemmesh = table(voldata$chem_separation, voldata$lowestsize_mm)
chisq.test(chemmesh) #p-value <0.05 says chemical separation or not & mesh size are independent





## check why chem separation & waterbody type are correlated
voldata %>% 
  ggplot(aes(x = latestyear_sampled , fill = fresh_marine))+
  geom_bar()+
  facet_grid(~chem_separation)

bigmesh =voldata %>% 
  filter(Cavg.L<10^-1)
  lm_bigmesh =lm(data=bigmesh,log10(Cavg.L)~lowestsize_mm)
  summary(lm_bigmesh)
smallmesh =voldata %>% 
    filter(Cavg.L>10^-1)
  lm_smallmesh =lm(data=smallmesh,log10(Cavg.L)~lowestsize_mm)
  summary(lm_smallmesh)
  lm_nomethod = lm(data=voldata,log10(Cavg.L)~lowestsize_mm)
  summary(lm_nomethod)

#### STATS FROM LITERATURE REVIEW-----------------

### Sankey chart of the kinds of studies included
##OG
voldata %>% 
 ggplot(aes(axis3= fresh_marine, axis2=method, axis1=vol_bin))+
  geom_alluvium(aes(fill = method))+
  stat_stratum()+
  geom_stratum()+
  geom_text(stat="stratum",aes(label=after_stat(stratum)))+
  scale_x_discrete(limits=c("Sample Volume","Method","Sampled waterbody"))+
  labs(y = "Number of studies")+
  scale_fill_manual(values = cbPalette)+
  theme_bw()
## Graphical Abstract
  voldata %>% 
    group_by(method) %>% 
    mutate(conc_binned = ifelse(Cavg.L>10^2, ">100/L",ifelse(Cavg.L<=10^2&Cavg.L>1,"1-100/L",ifelse(Cavg.L<=10^0&Cavg.L>10^-2,"0.01-1/L",ifelse(Cavg.L<=10^-2&Cavg.L>10^-4,"0.0001-0.01/L","<0.0001/L"))))) %>% 
  mutate(conc_binned = factor(conc_binned,c("<0.0001/L","0.0001-0.01/L","0.01-1/L","1-100/L",">100/L"))) %>% 
    ggplot(aes(axis1= factor(method,c("net","pump","grab")), axis3=conc_binned, axis2=vol_bin,fill = method))+
    geom_alluvium()+
    stat_stratum()+
    geom_stratum()+
   #v1 geom_text(stat="stratum",aes(label=after_stat(stratum)), size = 15)+
    geom_text(stat="stratum", aes(label = "      "))+
    scale_x_discrete(limits=c("Method","Sample volume (L)","Concentration (particles/L)"))+
    labs(y = "Number of studies")+
    scale_fill_manual(values = cbPalette)+
    theme_bw()+
    theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")
  #ggsave("graphicalabstract_v2.png",width = 7, height = 4, units = "in", dpi = 300)
### Alternative stacked bar plots
## Fresh vs marine
loc_plot = voldata %>% 
  ggplot(aes(x = fresh_marine, fill = method))+
  geom_bar()+
  scale_fill_manual(values = cbPalette)+
  ylim(0, 90)+
  ylab("")+
  xlab("Sampled waterbody type")+
  theme_bw()
## Volume
vol_plot = voldata %>% 
  mutate(vol_bin = factor(vol_bin, levels = c("0-100L",">100L"))) %>% 
  ggplot(aes(x = vol_bin, fill = method))+
  geom_bar()+
  scale_fill_manual(values = cbPalette)+
  ylim(0, 90)+
  ylab("")+
  xlab("Sample volume (binned)")+
  theme_bw()
## Method
meth_plot = voldata %>% 
  ggplot(aes(x = method, fill = method))+
  geom_bar()+
  scale_fill_manual(values = cbPalette)+
  ylim(0, 90)+
  ylab("Number of included studies")+
  xlab("Sampling method used")+
  theme_bw()
## combine all three into one plot
ggarrange(meth_plot, vol_plot, loc_plot, 
          labels = c("A", "B", "C"),
          common.legend = TRUE, legend = "right",
          ncol = 3, nrow = 1)
#ggsave("lit_summary.png",width = 7, height = 4, units = "in", dpi = 300)

  ### General ====

## Number of studies included
n_distinct(voldata$doi)

## Number of unique entries (method-watersystem combinations)
nrow(voldata)

## Breakdown of studies by method used
voldata %>% 
  group_by(method) %>% 
  summarize(n())

## Breakdown of studies by system sampled
voldata %>% 
  group_by(fresh_marine,river_ocean) %>% 
  summarize(n())

## do our own calculations of volume (when not given) have undue influence on trends we're finding?
# how many studies do we calculate volume for?
voldata %>% 
  group_by(vol_source) %>% 
  summarize(n())

# Plot: 
voldata %>% 
  mutate(vol_calc = ifelse(vol_source =="averaged" | vol_source =="given","given","calculated")) %>% 
  filter(method =="net") %>% 
  ggplot(aes(x= log10(vol_L),y = log10(Cavg.L), group=vol_calc, shape = vol_calc))+
  geom_point()+
  scale_shape_manual(values = c(16, 1))+
  xlab("log(Volume Sampled, L)")+
  ylab("log(Avg. Concentration, particles/L)")+
  geom_smooth(method = "lm", se = FALSE, color = "black",aes(linetype = vol_calc))+
  theme_bw()+
  labs(shape="Sample Volume:\ncalculated post-hoc\nor given in paper?",linetype="Sample Volume:\ncalculated post-hoc\nor given in paper?")
#ggsave("posthoc_volume.png",width = 7, height = 4, units = "in", dpi = 300)

  # Linear model:?
lm_voldata_calculatedvolume = lm(data=subset(voldata,voldata$method=="net"),log10(Cavg.L)~log10(vol_L)+vol_source_simple)
lm_voldata_givenvolume = lm(data)
anova(lm_voldata_calculatedvolume)
summary(lm_voldata_calculatedvolume)
net=subset(voldata,voldata$method=="net")
t.test(net$vol_source_simple)

# does including post-hoc volume samples change slope of graph?
voldata=voldata %>% 
  mutate(vol_calc = ifelse(vol_source =="averaged" | vol_source =="given","given","calculated"))
  lm_voldata_calculatedvolume = lm(data=subset(voldata,voldata$vol_calc =="calculated"&voldata$method=="net"),log10(Cavg.L)~log10(vol_L))
summary(lm_voldata_calculatedvolume)
lm_voldata_givenvolume = lm(data=subset(voldata,voldata$vol_calc =="given"),log10(Cavg.L)~log10(vol_L)+method+lowestsize_mm)
summary(lm_voldata_givenvolume)

anova(lm_voldata_full)


## Histogram of articles by study year
voldata %>% 
  ggplot(aes(x =ifelse(is.na(earliestyear_sampled),latestyear_sampled,earliestyear_sampled)))+
  geom_bar(aes(fill = method))+
  theme_bw()+
  scale_fill_manual(values = cbPalette)+
  ylab("Number of Studies")+
  xlab("Year of Collection")
#ggsave("histogram_year.png",width = 7, height = 4, units = "in", dpi = 300)
  
  

## Does average concentration decrease by year (hypothesis: methods getting tighter/better with time) 
  # **ANSWER: NO.**
  #Plot looking only at ocean, net samples since ppl have been interested in this method/location throughout time:
voldata %>% 
  filter(method == "net",river_ocean == "marine") %>%
  #filter(latestyear_sampled>2005) %>% 
  #plot earliest year sampled, unless not available, in which case sub in latest year estimate
  ggplot(aes(x= ifelse(is.na(earliestyear_sampled),latestyear_sampled,earliestyear_sampled),y = log10(Cavg.L)))+
  # consider coloring points by volume_measure
  geom_point()+
  xlab("Year sampled")+
  ylab("log(Avg. Concentration, particles/L)")+
  theme_bw()

## Is there a difference between the affects of method in marine and freshwater systems?
# **ANSWER: NOT AT ALL**
# plot: 
voldata %>% 
  ggplot(aes(x = log10(vol_L),y = log10(Cavg.L)))+
           geom_point(aes(color = method))+
           facet_grid(fresh_marine~.)+
           theme_bw()

## Ranges of concentration values for each method
# Table 2
voldata %>% 
  group_by(method) %>% 
  summarize(min(Cavg.L), max(Cavg.L))
## Ranges of volume values for each method
voldata %>% 
  group_by(method) %>% 
  summarize(min(vol_L), max(vol_L))

### Map of studies ====
world = ne_countries(scale = "medium",returnclass = "sf")
ggplot(data=world)+
  geom_sf()+
  geom_point(data = voldata, size = 1.5,aes(x = long, y=lat, color = method))+
  theme_bw()+
  xlab("")+
  ylab("")+
  scale_colour_manual(values = cbPalette)
#ggsave("studies_map.png",width = 7, height = 4, units = "in", dpi = 300)


## Freshwater vs. Marine studies, histogram
voldata %>% 
  group_by(method, fresh_marine) %>% 
  summarize(count=n())
  ggplot(aes(x = fresh_marine))+
  geom_histogram(stat="percent")+
  facet_grid(~method)




 ### Contamination ====


## What percent of studies include mention of blanks being performed?

blanks =voldata %>% 
  mutate(waterblanks = ifelse(is.na(waterblanks),0,ifelse(waterblanks == ">1",1,as.numeric(waterblanks))),airblanks = ifelse(is.na(airblanks),0,ifelse(airblanks == ">1", 1,as.numeric(airblanks)))) %>% 
    mutate(blanks = ifelse(waterblanks+airblanks>0, "yes","no")) %>% 
             group_by(blanks) %>% 
             summarize(studies = n()) %>% 
          mutate(percentofstudies = studies/n_distinct(voldata))

# Plot: studies w/ vs. studies w/o blanks by year
# ** IMPROVE: make y-axis % of studies from that year


## Measure the difference in 1 particle in a 1L sample vs 1 particle in a 10000L sample
# Assume there is 1 additional particle for each of the liters sampled and plot that relationship
voldata %>% 
  mutate(conc_var = Cavg.L+vol_L) %>% 
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  ggplot(aes(x = vol_L,y=conc_var))+
  #stat_smooth(method = "lm", col = "gray", formula = log10(voldata$Cavg.L)~log10(voldata$vol_L)+voldata$method*voldata$lowestsize_mm+voldata$location)+
  geom_point(aes(color = method, shape=location))+
  labs(shape = "location",
       subtitle = paste("Adj R2 = ",signif(summary(lm_voldata)$adj.r.squared, 5),
                        "Intercept =",signif(lm_voldata$coef[[1]],5 ),
                        " Slope =",signif(lm_voldata$coef[[2]], 5),
                        " P =",signif(summary(lm_voldata)$coef[2,4], 5)))+
  xlab("Volume Sampled, L")+
  ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw()

## Does secondary ID method affect concentration found?
voldata %>% 
  ggplot(aes(x = secondary_id, y = log10(Cavg.L)))+
  geom_boxplot()+
  facet_wrap(~method,scales="free_y")

## Does separation technique affect concentration found?
voldata %>% 
  mutate(id_tech = ifelse(secondary_id == "none"|secondary_id =="hot needle", "approx.","polymer id")) %>% 
  ggplot(aes(x = separation, y = log10(Cavg.L)))+
  geom_point(aes(color = id_tech))+
  facet_wrap(~method,scales="free_y")

voldata %>% 
  mutate(id_tech = ifelse(secondary_id == "none"|secondary_id =="hot needle", "no","yes")) %>% 
  ggplot(aes(x = vol_L, y = Cavg.L, group=id_tech))+
  geom_point(aes(color = id_tech))+
  geom_smooth(aes(color = id_tech), se = FALSE, method = "lm")+
  labs(color = "Material Confirmation")+
  xlab("Volume Sampled, L")+
  ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw()

voldata %>% 
mutate(id_tech = ifelse(secondary_id == "none"|secondary_id =="hot needle", "approx.","polymer id")) %>% 
  group_by(id_tech) %>% 
  summarize(count=n())
  


  ### Net volumes underestimated ====

## Percent of studies we had to calculate our own volume estimate for

voldata %>% 
  group_by(vol_source) %>% 
  summarize(count = n())

## Graph: Breakdown of net studies by method of measuring volume sampled
  
voldata %>% 
  filter(method == "net") %>% 
  group_by(method_specific,volume_measure) %>% 
  summarize(studies = n()) %>% 
  mutate(measuretype = ifelse(is.na(volume_measure),"other",ifelse(str_detect(volume_measure, "flowmeter"),"flowmeter",ifelse(volume_measure =="not mentioned","other","speed and distance")))) %>% 
  print() %>% 
  ggplot(aes(x = as.factor(method_specific), y = studies)) +
  geom_bar(aes(fill = measuretype), position = "dodge", stat = "identity")+
  theme_bw()+
  xlab("net method")+
  labs(fill = "volume measurement approach")


## linear model of net data only. Does specific method & volume measurement choice affect concentration measured?
  ##NO, it appears even among net samples, the only thing affecting concentration is volume, not net-type or volume measurement
voldata_net = voldata %>% 
  filter(method =="net") %>% 
  #group various volume measure techniques into fewer categories
    mutate(volume_measure = ifelse(volume_measure == "current meter"|volume_measure == "digital flowmeter"|volume_measure == "flowmeter"|volume_measure == "mechanical flowmeter","flowmeter",ifelse(volume_measure=="speed"|volume_measure =="time x speed"|volume_measure =="river velocity","system speed",volume_measure))) 
  

  lm_voldata_net = lm(data = voldata_net,log10(Cavg.L)~log10(vol_L)+method_specific+volume_measure+river_ocean)
summary(lm_voldata_net)
anova(lm_voldata_net)
#AIC(lm_voldata_net)
  
## Plot of net data only (vol. vs conc). Does specific method & volume measurement choice affect concentration measured?
voldata_net %>% 
     ggplot(aes(x = log10(vol_L), y = log10(Cavg.L) ))+
  geom_point(aes(color = volume_measure))#+
#geom_point(aes(color = method_specific))

### Intersample variability -----
## Do small volume samples have higher variability?
voldata %>% 
  filter(!is.na(Csd.L)) %>% 
  ggplot(aes(x = log10(vol_L), y = as.numeric(Csd.L)))+
  geom_point(aes(colour = method))+
  xlab("Volume Sampled, L")+
  ylab("Reported Standard Deviation of Concentrations \n(particles/L)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_colour_manual(values = cbPalette)+
  theme_bw()

  ### Mesh size affects samples differently ====
### Volume vs. Mesh correlations -----------
voldata %>% 
  ggplot(aes(x = lowestsize_mm))+
  geom_histogram()
voldata %>% 
  ggplot(aes(x = vol_L))+
  geom_histogram()
voldata %>% 
  filter(vol_L<100) %>% 
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  #ggplot(aes(x = log10(vol_L), y = lowestsize_mm))+
  ggplot(aes(x = lowestsize_mm, y = vol_L))+
  geom_point(aes(colour= method, shape=location))+
  scale_colour_manual(values = cbPalette)+
 # facet_grid(~method, scales="free")+
  ylab("Volume Sampled, L")+
  xlab("Mesh or Filtration Size, mm")+
  theme_bw()

# Kendall's Tau because data isn't necessarily linear or normal
cor(x = voldata$lowestsize_mm, y = voldata$vol_L, method = "kendall")
grab = voldata %>% 
  filter(method =="grab")
net = voldata %>% 
  filter(method=="net")
pump = voldata %>% 
  filter(method=="pump")
cor(x = net$lowestsize_mm, y = net$vol_L, method = "kendall")
cor(x = grab$lowestsize_mm, y = grab$vol_L, method = "kendall")
cor(x = pump$lowestsize_mm, y = pump$vol_L, method = "kendall")

# looking only at mesh size = 0.3-0.35, is there a relationship between volume and concentration?
smallmesh = subset(voldata, voldata$lowestsize_mm>0.04999&voldata$lowestsize_mm<0.071)
lm_smallmesh = lm(data = smallmesh, log10(Cavg.L)~log10(vol_L)+method+river_ocean)
bigmesh = subset(voldata, voldata$lowestsize_mm>0.299&voldata$lowestsize_mm<0.36)
lm_bigmesh= lm(data = bigmesh, log10(Cavg.L)~log10(vol_L)+method+river_ocean)
#Figure S4
voldata %>% 
  mutate(meshclass = ifelse(lowestsize_mm>0.299&lowestsize_mm<0.36,"0.3-0.35mm", ifelse(lowestsize_mm>0.04&lowestsize_mm<0.076,"0.05-0.07mm","exclude"))) %>% 
           filter(meshclass!="exclude") %>% 
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  ggplot(aes(x = vol_L,y=Cavg.L))+
  #stat_smooth(method = "lm", col = "gray", formula = log10(voldata$Cavg.L)~log10(voldata$vol_L)+voldata$method*voldata$lowestsize_mm+voldata$location)+
  geom_point(aes(color = method, shape=location), size = 1.5)+
  labs(shape = "location")+
  xlab("Volume Sampled, L")+
  ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_colour_manual(values = cbPalette)+
  scale_shape_manual(values = shapevec)+
  theme_bw()+
  #geom_smooth(method=lm, color = "black", linetype = "dashed", se = FALSE)+
  #labs(subtitle=paste("Adj R2 = ",signif(summary(lm_smallmesh)$adj.r.squared, 5),
  #               "Intercept =",signif(lm_smallmesh$coef[[1]],5 ),
  #              " Slope =",signif(lm_smallmesh$coef[[2]], 5),
  #           " P =",signif(summary(lm_smallmesh)$coef[2,4], 5)),
  #title = paste("Adj R2 = ",signif(summary(lm_bigmesh)$adj.r.squared, 5),
  #              "Intercept =",signif(lm_bigmesh$coef[[1]],5 ),
  #              " Slope =",signif(lm_bigmesh$coef[[2]], 5),
  #              " P =",signif(summary(lm_bigmesh)$coef[2,4], 5)))+
  facet_grid(~meshclass)
#ggsave("meshcontrolled_volvsconc.png",width = 7, height = 4, units = "in", dpi = 300)
# looking only at volume = 10^2, is there a relationship between mesh size and concentration?
smallvol = subset(voldata, voldata$vol_L>0.9 & voldata$vol_L<2)
lm_smallvol = lm(data = smallvol, log10(Cavg.L)~lowestsize_mm+river_ocean)
midvol = subset(voldata, voldata$vol_L==20)
lm_midvol = lm(data = midvol, log10(Cavg.L)~lowestsize_mm+method+river_ocean)
bigvol = subset(voldata, voldata$vol_L>10^1.9&voldata$vol_L<10^2.1)
lm_bigvol= lm(data = bigvol, log10(Cavg.L)~lowestsize_mm+method+river_ocean)
#Figure S5
voldata %>% 
  mutate(volclass = ifelse(vol_L>10^1.9&vol_L<10^2.1,"100L",ifelse(vol_L==20,"20L", ifelse(vol_L>0.9 & vol_L<2,"1L","exclude")))) %>% 
  filter(volclass!="exclude") %>% 
  mutate(volclass = factor(volclass,c("1L","20L","100L"))) %>% 
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  ggplot(aes(x = lowestsize_mm,y=Cavg.L))+
  #stat_smooth(method = "lm", col = "gray", formula = log10(voldata$Cavg.L)~log10(voldata$vol_L)+voldata$method*voldata$lowestsize_mm+voldata$location)+
  geom_point(aes(color = method, shape=location), size = 1.5)+
  labs(shape = "location")+
  xlab("Mesh or Filtration size, mm")+
  ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  #geom_smooth(method=lm, color = "black", linetype = "dashed", se = FALSE)+
  scale_colour_manual(values = c("#E69F00", "#009E73"))+
  scale_shape_manual(values = c(16,2,15,3))+
  theme_bw()+
  facet_grid(~volclass)
#ggsave("volcontrolled_meshvsconc.png",width = 7, height = 4, units = "in", dpi = 300)

# looking only at mesh size = 
# volume vs mesh size plot
# Figure S3
voldata %>% 
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  ggplot(aes(x = vol_L, y = lowestsize_mm))+
  geom_point(aes(color = method, shape = location))+
  xlab("Volume Sampled, L")+
  ylab("Mesh or filtration size (mm)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_colour_manual(values = cbPalette)+
  scale_shape_manual(values = shapevec)+
  theme_bw()
##ggsave("vol_mesh.png",width = 7, height = 4, units = "in", dpi = 300)

## Plot: Concentration vs. mesh size
#Figure S2
voldata %>%
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  ggplot(aes(x= lowestsize_mm,y = Cavg.L))+
    geom_point(aes(shape=location,color = method), size =1.5)+
    xlab("Mesh or Filtration size (mm)")+
    ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
               labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x)))+
    theme_bw()+
    labs(shape = "location")+
  scale_colour_manual(values = cbPalette)+
  scale_shape_manual(values = shapevec)
  facet_grid(~method)
#ggsave("logconcentration_logmeshsize.png",width = 7, height = 4, units = "in", dpi = 300)

voldata %>%
  mutate(location = ifelse(river_ocean=="lagoon","lake",ifelse(river_ocean=="lake and river","lake",ifelse(river_ocean =="reservoir","lake",ifelse(river_ocean =="pond","lake",river_ocean))))) %>% 
  mutate(vol_bin2 = ifelse(vol_L<0.01, "tiny", ifelse(vol_L>=0.01&vol_L<0.1, "wee", ifelse(vol_L>=0.1&vol_L<1, "small", ifelse(vol_L>=1&vol_L<10, "mid", ifelse(vol_L>=10&vol_L<100, "large", ifelse(vol_L>=100&vol_L<1000, "larger", ifelse(vol_L>=1000,"largest","other")))))))) %>% 
ggplot(aes(x= lowestsize_mm,y = Cavg.L))+
  geom_point(aes(shape=method,color = vol_bin2), size =2.5)+
  xlab("Mesh or Filtration size (mm)")+
  ylab("Avg. Concentration, particles/L")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw()+
  labs(shape = "location")
  
## Plot: Mesh size correlated with method?
voldata %>% 
  ggplot(aes(x = method, y = lowestsize_mm)) +
  geom_boxplot()
## Plot: Mesh size correlated with volume?
voldata %>% 
  ggplot(aes(x = lowestsize_mm, y = log10(vol_L))) +
  geom_point(aes(color = method))

## Does smallest filtration size method affect concentration?
voldata %>%
  ggplot(aes(x= lowestsize_mm,y = log10(Cavg.L)))+
  geom_point(aes(color = lowest_mesh, shape = lowest_mesh))+
  # note: sieve = metal, filter = filter paper, mesh = plastic net-like mesh
  xlab("Mesh or Filtration size (mm)")+
  ylab("log(Avg. Concentration, particles/L)")+
  theme_bw()+
  facet_grid(~method)+
  labs(color = "Method of enforcing lowest size bound", shape = "Method of enforcing lowest size bound")

## Breakdown of method of enforcing lowest size bound
voldata %>% 
  #filter(method =="grab") %>% 
  group_by(lowest_mesh) %>% 
  summarize(n())

# max & min mesh size by category
voldata %>% 
  group_by(method) %>% 
  summarize(minmesh = min(lowestsize_mm),maxmesh = max(lowestsize_mm), meanmesh = mean(lowestsize_mm), medianmesh = median(lowestsize_mm))

#historgram mesh sizes
voldata %>% 
 # filter(method != "pump") %>% 
  ggplot(aes(x = lowestsize_mm))+
  geom_histogram(aes(fill = method))+
theme_bw()+
  xlab("Mesh or Filtration Size (mm)")+
  ylab("Number of studies")+
  scale_fill_manual(values = cbPalette)+
  facet_grid(method~.)
  

## Summary of particle types by method and mesh size
voldata %>% 
  ggplot(aes(x = method))+
  geom_bar(aes(fill = top_shape),position = "fill")+
  facet_grid(~river_ocean)

## Most prominant particle type by method
# table
voldata %>% 
  filter(!is.na(top_shape)) %>% 
  group_by(top_shape) %>% 
  summarize(n())
# graph
voldata %>% 
  mutate(top_shape = ifelse(is.na(top_shape), "not listed",top_shape)) %>% 
  mutate(top_shape=factor(top_shape,levels = c("fiber", "fragment", "film","pellet", "foam", "not listed"))) %>% 
  mutate(method=factor(method, levels=c("grab","pump","net"))) %>% 
  ggplot(aes(x = top_shape))+
  geom_bar(position = position_dodge(preserve = 'single'))+
  facet_grid(~method)+
  theme_bw()+
  ylab("Number of studies")+
  xlab("Dominant Particle Type")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
#ggsave("dominantparticle.png",width = 7, height = 4, units = "in", dpi = 300)

### Correcting for differences with contamination----
## Using k values from paired studies
voldata %>% 
  mutate(Cavg.L_corrected = ifelse(method == "grab",Cavg.L-1000/vol_L,ifelse(method =="pump",Cavg.L-1000/vol_L,Cavg.L-mean(1000,1000)/vol_L))) %>% 
  #mutate(Cavg.L_corrected = ifelse(Cavg.L_corrected<0,0,Cavg.L_corrected)) %>% 
  filter(vol_L<50) %>% 
  #ggplot(aes(x = log10(vol_L),y =log10(Cavg.L_corrected)))+
  ggplot(aes(x = vol_L,y =Cavg.L_corrected))+
  geom_point(aes(color = method))
  ### 




# Does sample mesh size have a greater influence on some methods or some locations than others?
voldata %>% 
  mutate(mesh_bin = ifelse(lowestsize_mm<0.33, "mesh_under.33", "mesh_over.33")) %>% 
 #filter(method =="net") %>% 
  filter(lowestsize_mm<0.33&lowestsize_mm>0.04) %>% 
ggplot(aes(x = river_ocean, y = log10(Cavg.L)))+
  geom_boxplot()+
  facet_grid(rows=vars(lowestsize_mm), cols = vars(method))

#bin volumes "low" vs. "high"
lm_voldata_bins = lm(data = voldata, log10(Cavg.L)~vol_bin+method*lowestsize_mm+river_ocean)
summary(lm_voldata_bins)
anova(lm_voldata_bins)
AIC(lm_voldata_bins)





  
#does measuring volume with a meter help explain differences?
voldata %>% 
 # filter(!is.na(volume_measure)) %>% 
  filter(volume_measure != "GPS distance") %>% 
  ggplot(aes(x= as.numeric(lowestsize_mm),y = log(Cavg.L)))+
  geom_point(aes(color=volume_measure,shape = method))+
  xlab("Mesh or Filtration size (mm)")+
  ylab("log(Avg. Concentration, particles/L)")+
  theme_bw()+
  labs(shape = "volume_measure")
  
  ## Alter all estimated volumes based on idea that net measurement error = 1%
  error = .1
voldata %>% 
  mutate(calculated = ifelse(grepl('calculated',vol_source),'True','False')) %>% 
  mutate(Cavg.Lup=ifelse(calculated == 'True',
                        Cavg.L*vol_L/(vol_L*(1+error)),
                        Cavg.L), 
         vol_Lup = ifelse(calculated == 'True',
                        vol_L*(1+error),
                        vol_L),
         Cavg.Ldown=ifelse(calculated == 'True',
                          Cavg.L*vol_L/(vol_L*(1-error)),
                          Cavg.L), 
         vol_Ldown = ifelse(calculated == 'True',
                          vol_L*(1-error),
                          vol_L)) %>% 
  ggplot()+
  geom_point(aes(x = log10(vol_L),y = log10(Cavg.L),color = method, shape = 19))+ 
  geom_point(aes(x = log10(vol_Lup),y = log10(Cavg.Lup), color = method, shape = 1))+
  geom_point(aes(x = log10(vol_Ldown),y = log10(Cavg.Ldown), color = method, shape = 8))+
  scale_shape_identity()
  ##no difference is seen at 1% error

lm_voldata_bins = lm(data = voldata, log10(Cavg.Ldown)~log10(vol_Ldown)+method*lowestsize_mm+river_ocean)
summary(lm_voldata_bins)
anova(lm_voldata_bins)
AIC(lm_voldata_bins)
