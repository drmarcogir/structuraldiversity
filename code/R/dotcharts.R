library(tidyverse)

# read in data file
read_csv("./data/CzechiaPCAs1s2.csv")->dat

dat %>%
  dplyr::select(matches('VH|VV|B2|B3|B4|B5|B6|B7|B8|B8A|B11|B12|ndvi|evi|savi|ndwi|Ratio')) %>%
  mutate(id = 1:length(B2)) %>%
  pivot_longer(names_to = "variable",values_to = "value",cols =-c(id))->dat1


varl<-unique(dat1$variable)

for (i in 1:length(varl)){
  dat1 %>%
    filter(variable == varl[i])->dat2
  ggplot(data = dat2,aes(x = value))+geom_dotplot(method="histodot",dotsize = 0.5,
                                                  fill = "green")+theme_bw()+
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          strip.text = element_text(size = 15))+xlab(paste0("Variable name -> ", varl[i]))->p1
  fileout<-paste0("./results/dotcharts/geom_dotchart_variable_",varl[i],".png")
  ggsave(p1,filename=fileout,width = 8, height =8 ,dpi = 400) 
}
