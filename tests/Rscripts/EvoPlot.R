library(tidyverse)
rm(list=ls())

output_dir = "pspm_output"
prefix = "p50NEWW"

solver = "p50" #_old_params"
setwd(paste0("~/PlantFate/root/Plant-FATE/",output_dir,"/",prefix))#,"_",solver))


#add_band = function(){
#  polygon(x=c(2000,5000,5000,2000), y=c(-1e20,-1e20,1e20,1e20), border = NA, col=scales::alpha("yellow2",0.2))
#}

#add_hband = function(ylim, col=scales::alpha("grey30",0.2), xlim=c(-1e20,2000)){
#  polygon(y=c(ylim[1],ylim[2],ylim[2],ylim[1]), x=c(xlim[1],xlim[1],xlim[2],xlim[2]), border = NA, col=col)
#}

# seeds1 = read.delim("seeds.txt", header=F, col.names = paste0("V", 1:(n_species+2)))
Zp = read.delim("z_star.txt", header=F, col.names = paste0("V", 1:50))
# BA1 = read.delim("basal_area.txt", header=F, col.names = paste0("V", 1:(n_species+2)))
co = read.delim("canopy_openness.txt", header=F, col.names = paste0("V", 1:50))
lai_v = read.delim("lai_profile.txt", header=F, col.names = paste0("V", 1:27))
traits = read.delim("traits_ELE_HD.txt")
dat = read.delim("AmzFACE_D_PFATE_ELE_HD.txt")
dat2 = read.delim("AmzFACE_Y_PFATE_ELE_HD.txt")
dist = read.delim("size_distributions.txt", header=F)
dist = dist[,-ncol(dist)]
x = exp(seq(log(0.01), log(10), length.out=100))

# To get avg size distribution, sum over species and average over years
names(dist)[1:2] = c("YEAR", "SPP")
dist_amb = dist %>% filter(YEAR>1100 & YEAR<2000) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)
dist_ele = dist %>% filter(YEAR>2100 & YEAR<3000) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)

# dist_amb = dist %>% filter(YEAR == 1100) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)
# dist_ele = dist %>% filter(YEAR == 1101) %>% pivot_longer(cols=-(YEAR:SPP), names_to="size_class") %>% group_by(YEAR,size_class) %>% summarize(de = sum(value, na.rm=T)) %>% pivot_wider(names_from = size_class, values_from = de) %>% colMeans(na.rm=T)

n_species = length(unique(dat2$PID))
n_year = length(unique(dat2$YEAR))

p1 = traits %>% select(YEAR, p50) %>% 
      # filter(YEAR > 1050) %>% 
      # filter(RES==T) %>% 
     ggplot(aes(y=P50, x=YEAR))+
      theme_classic(base_size = 12)+
      geom_point(aes(col=YEAR, size=RES), alpha=0.4)+
       scale_color_viridis_c(direction = -1)+
      scale_size("size_RES", range = c(0, 1.5))

p3 = dat2 %>% select(YEAR, PID, BA) %>% 
  left_join(traits, by = c("PID"="SPP", "YEAR"="YEAR")) %>% 
 # filter(YEAR > 2120 & YEAR < 3000) %>% 
 #  filter(RES==T) %>% 
  ggplot(aes(y=P50, x=YEAR))+
  theme_classic(base_size = 12)+
  geom_point(aes(col=BA*1e4, size=RES), alpha=0.7)+
  scale_color_viridis_c(direction = -1)+
  scale_size("size_RES", range = c(0, 1.5), guide = F)+
  labs(col="BA")+
  ggtitle("Elevated")


if (plot_to_file) png("traitspace.png", width=1618*1.5, height = 1196*1.5, res=300)
print(
cowplot::plot_grid(p1,p3)
)