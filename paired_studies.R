# Analysis of Paired studies
# Code written by Lisa Watkins, ltw35@cornell.edu

## load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Ensure this script, code, and .Rproj file are all in same local directory

### READ & CLEAN: Read in csv summarizing studies that collected paired samples--------------

paired = read.csv("pairedstudies.csv") %>% 
  filter(omit == "0")

# The ggplot colorblind palette with grey:
cbPalette <- c("#E69F00", "#CC79A7","#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#999999")

### MAIN PLOT--------
## Volume
#cobweb
avgpaired=paired %>% 
  filter(study_type != "mesh_compare") %>% 
  mutate(samemesh = ifelse(is.na(conc_L_samemeshsize),"no","yes")) %>% 
  filter(samemesh =="yes") %>% 
  mutate(study.sample =paste(study,sample_num)) %>% 
  group_by(study, method, study_type) %>% 
  summarise(Csd = sd(Cavg.L), Cavg.L = mean(Cavg.L),vol_L = mean(vol_L))

  #figure 5            
paired %>% 
  mutate(study.sample = paste(study,sample_num)) %>%
  filter(study_type != "mesh_compare") %>% 
  mutate(samemesh = ifelse(is.na(conc_L_samemeshsize),"different","same")) %>%
  #filter(Cavg.L>0) %>% 
  ggplot(aes(x = vol_L, y = ifelse(samemesh=="yes",conc_L_samemeshsize,Cavg.L)))+
  geom_line(aes(group = study.sample, color = samemesh), size = 0.25) +
  #geom_line(data = avgpaired,aes(x = vol_L, y = Cavg.L, group = study.sample),color = "black")+
  geom_point(aes(fill = method),size = 2.5, shape = 21, stroke = 0,color = "white")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab("Volume (L)")+
  ylab("Concentration (particles/L)")+
  theme_bw()+
  scale_fill_manual(values = cbPalette)+
    scale_colour_manual(values = c("gray","black"))+
  labs(title="Field studies that sample paired methods at a single time & location", colour = "mesh size")
#ggsave("logconcentration_logvolume_pairedstudies.png",dpi=300,height = 4,width = 7, units = "in" )






### Average differences -----
temp=paired %>% 
  filter(study_type != "mesh_compare", study_type != "grab-grab") %>% 
  filter(!is.na(conc_L_samemeshsize)) %>% 
  pivot_wider(id_cols = c(study,sample_num),names_from = method, values_from = Cavg.L) %>% 
  mutate(perc_diff_grab = (net-grab)/grab, perc_diff_pump = (net-pump)/pump, grabovernet = grab/net, pumpovernet = pump/net) %>% 
  filter_if(~is.numeric(.),all_vars(!is.infinite(.))) %>% 
  filter(sample_num!="-1", sample_num!="0") %>% 
  #group_by(study) %>% 
  summarize(min(!is.na(perc_diff_grab)), max(!is.na(perc_diff_grab)), mean(!is.na(perc_diff_grab)),min(!is.na(perc_diff_pump)), max(!is.na(perc_diff_pump)), mean(!is.na(perc_diff_pump)),min(!is.na(grabovernet)), max(!is.na(grabovernet)), mean(!is.na(grabovernet)),min(!is.na(pumpovernet)), max(!is.na(pumpovernet)), mean(!is.na(pumpovernet)))


### 
#averages & errorbars
paired %>% 
  #filter(study != "Schönlau 2020") %>% 
  #mutate(study.sample = paste(study,sample_num)) %>%
  filter(study_type != "mesh_compare") %>% 
  #filter(Cavg.L>0) %>% 
  group_by(study, method, study_type) %>% 
  summarise(Csd = sd(Cavg.L), Cavg.L = mean(Cavg.L),vol_L = mean(vol_L)) %>% 
  mutate(ymin = log10(Cavg.L-Csd), ymax = log10(Cavg.L+Csd)) %>% 
  ggplot(aes(x = log10(vol_L), y = log10(Cavg.L)))+
  geom_line(aes(group = study, color = study))+
  geom_errorbar(aes(ymin = ymin, ymax=ymax, color = method), width=.3)+
  geom_point(aes(color = study))+
  xlab("log10 of Volume (L)")+
  ylab("log10 of Concentration (particles/L)")+
  theme_bw()+
  facet_grid(.~study_type)+
  labs(title="Field studies that sample paired methods at a single time & location")


## Meshsize
# cobweb
paired %>% 
  #filter(study != "Garcia 2020") %>% 
  mutate(study.sample = paste(study,sample_num)) %>%
  filter(study_type == "mesh_compare") %>% 
  #filter(Cavg.L>0) %>% 
  ggplot(aes(x = mesh_mm, y = Cavg.L))+
  geom_point(aes(color = location, shape = study))+
  geom_line(aes(color = method, group = study.sample)) +
  xlab("Mesh size (mm)")+
  ylab("Concentration (particles/L)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw()+
  labs(title="Field studies that sample paired methods at a single time & location")

#plotting same, but in aggregate
paired %>% 
  filter(study_type == "mesh_compare") %>% 
  group_by(study, mesh_mm) %>% 
  summarise(Csd = sd(Cavg.L), Cavg.L = mean(Cavg.L),mesh_mm = mean(mesh_mm)) %>% 
  ggplot(aes(x = mesh_mm, y = Cavg.L))+
  geom_line(aes(group = study, color = study))+
  geom_errorbar(aes(ymin = Cavg.L-Csd, ymax=Cavg.L+Csd, color = study))+
  geom_point(aes(color = study))+
  xlab("Mesh size (mm)")+
  ylab("Concentration (particles/L)")+
  theme_bw()+
  labs(title="Field studies that sample paired methods at a single time & location")

### Average grab conc. vs average net conc.
paired %>% 
  filter(study_type == "grab-net"|study_type =="net-pump") %>% 
  group_by(study, method, vol_L) %>% 
  summarize(Cavg.L=mean(Cavg.L)) 
  #add 1 particle and see how it changes
  
paired %>% 
  filter(study_type !="grab-grab", study_type != "mesh_compare") %>% 
  mutate(newC.L = ((Cavg.L*vol_L)+5)/vol_L) %>% 
  #mutate(newC.L = ((Cavg.L*vol_L)+(Cavg.L*vol_L)*.1)/vol_L) %>% 
  ggplot(aes(x = Cavg.L, y = newC.L))+
  geom_point(aes(color = method))+
  geom_abline(intercept = 0)+
  scale_colour_manual(values = cbPalette)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab("Reported Concentration (particles/L)")+
  ylab("New Concentration if 1 additional particle were added")
  
### Particles lost to net----
## X out of 100 particles that enter net, left behind in net and not included in sample
#for pump-net pairs
paired %>% 
  filter(study_type =="net-pump") %>% 
  mutate(count = Cavg.L*vol_L) %>% 
  mutate(study.sample = paste(study,sample_num)) %>% 
  pivot_wider(id_cols = c(study.sample, study),names_from=method,values_from = c(Cavg.L,vol_L, count)) %>% 
  mutate(escapedparticles = Cavg.L_pump*vol_L_net-Cavg.L_net*vol_L_net, ratio_escapedtocaptured=escapedparticles/count_net) %>% 
  filter(count_net!=0) %>% 
  select(study,escapedparticles,ratio_escapedtocaptured) %>% 
  rbind(temp) %>% 
 # group_by(study) %>% 
  summarize(mean(escapedparticles), mean(ratio_escapedtocaptured), sd(ratio_escapedtocaptured))
#for grab-net pairs
paired %>% 
  filter(study_type =="grab-net") %>% 
  mutate(count = Cavg.L*vol_L) %>% 
  mutate(study.sample = paste(study,sample_num)) %>% 
  pivot_wider(id_cols = c(study.sample, study),names_from=method,values_from = c(Cavg.L,vol_L, count)) %>% 
  mutate(escapedparticles = Cavg.L_grab*vol_L_net-Cavg.L_net*vol_L_net, ratio_escapedtocaptured=escapedparticles/count_net) %>% 
  filter(count_net!=0) %>% 
  select(study,escapedparticles,ratio_escapedtocaptured) %>% 
  #group_by(study) %>% 
  summarize(mean(escapedparticles), mean(ratio_escapedtocaptured))

### Contamination ----
## particles of contamination needed to explain differences observed
    paired %>% 
      filter(study_type =="grab-net", study !="Hung 2020", study !="This study") %>% 
      mutate(study.sample = paste(study,sample_num)) %>% 
      mutate(count = Cavg.L*vol_L, percdiff_contam_count= (count-contam_numpersample_avg)/count) %>% 
      pivot_wider(id_cols = c(study.sample,contam_numpersample_avg),names_from=method,values_from = c(Cavg.L,vol_L, count)) %>% 
      mutate(bigger=ifelse(Cavg.L_net>Cavg.L_grab,"net",ifelse(Cavg.L_net==Cavg.L_grab,"same","grab"))) %>% 
      group_by(bigger) %>% 
      summarize(n())

    
#grab vs. net: For reasonable levels of k (i.e. not greater than or equal to sample count), what amount of concentration difference can it explain?
paired %>% 
    filter(study_type =="net-pump") %>% 
    mutate(study.sample = paste(study,sample_num)) %>% 
    pivot_wider(id_cols = c(study.sample,contam_numpersample_avg),names_from=method,values_from = c(Cavg.L,vol_L)) %>% 
    mutate(particles_needed = (Cavg.L_pump-Cavg.L_net)/(1/vol_L_pump-1/vol_L_net)) %>% 
  mutate(diff_contam_k = particles_needed-contam_numpersample_avg) %>% 
    summarize(avg_particles_needed=mean(!is.na(particles_needed)), sd_particles_needed=sd(!is.na(particles_needed)), se_particles_needed = sd_particles_needed/sqrt(n()),avg_diff_contam_k =mean(!is.na(diff_contam_k)))


## Comparison of percent fiber by method
paired %>% 
  filter(!is.na(perc_fib)) %>% 
  ggplot(aes(x= study))+
  #geom_point(aes(color = method))
  geom_bar(aes(fill = perc_fib))+
  facet_grid(~method)

paired %>%
  filter(!is.na(perc_fib)) %>% 
  ggplot(aes(x = sample_num, y=perc_fib))+
  geom_col(position = "dodge",aes(fill = method, group = study))+
  facet_grid(study~method, space = "free", scales = "free")

## Are fibers less prevalent in one method's sample than another?
#no
temp =paired %>% 
  filter(!is.na(perc_fib)) %>% 
  mutate(perc_frag = 1-perc_fib, dompart = ifelse(perc_frag>perc_fib,"frag", ifelse(perc_fib>perc_frag,"fib","broken"))) %>% 
  ggplot() +
  geom_bar(aes(x = dompart))+
  facet_grid(method~.)
  geom_histogram(aes(x = perc_fib), fill = "red", alpha = 0.5)+
  geom_histogram(aes(x = perc_frag), alpha = 0.5)+
  facet_grid(method~.)

paired %>% 
  t.test(perc_fib~method,paired = TRUE)

wilcox.test(perc_fib~method, data = paired, exact = FALSE)

## Number of studies included
# 18 unique studies, but Schonlau included twice: once as pump-net pair, once as pump-pump mesh-size pair
paired %>% 
  filter(study_type !="mesh_compare", study_type != "grab-grab") %>% 
  summarize(unique(study))

## Number of samples included
nrow(paired)
nrow(subset(paired,paired$study_type!="mesh_compare"))
## Number of samples where concentration = 0
nrow(subset(paired,paired$Cavg.L==0))
## Percent of samples where concentration = 0
nrow(subset(paired,paired$Cavg.L==0))/nrow(paired)


### Intersample variability----
##Do grab samples show more skewedness (right tails)
# Pearsons mode skewness
paired %>% 
  filter(study_type !="mesh_compare") %>% 
  group_by( method, study, location) %>% 
  summarize(mean=mean(Cavg.L), median = median(Cavg.L)) %>% 
  mutate(meanovermedian = mean/median) %>% 
  ggplot(aes(x = method, y = meanovermedian, group = study, color = location))+
  geom_line()+
  geom_point()
paired %>% 
  filter(study_type !="mesh_compare", location == "ocean") %>% 
  #filter(study == "McEacherna 2019") %>% 
  ggplot(aes(x= Cavg.L)) +
  geom_histogram(fill = "darkgreen")+
  facet_grid(study~method, scales = "free")
  #ggsave("concentration_histogram_ocean.png",dpi=300,height = 12,width = 4, units = "in" )
paired %>% 
  filter(study_type !="mesh_compare", location == "river") %>% 
  #filter(study == "McEacherna 2019") %>% 
  ggplot(aes(x= Cavg.L)) +
  geom_histogram(fill = "darkblue")+
  facet_grid(study~method, scales = "free")
#ggsave("concentration_histogram_river.png",dpi=300,height = 12,width = 4, units = "in" )
paired %>% 
  filter(study_type !="mesh_compare", location == "lake") %>% 
  #filter(study == "McEacherna 2019") %>% 
  ggplot(aes(x= Cavg.L)) +
  geom_histogram(fill = "darkred")+
  facet_grid(study~method, scales = "free")
#ggsave("concentration_histogram_lake.png",dpi=300,height = 12,width = 4, units = "in" )

