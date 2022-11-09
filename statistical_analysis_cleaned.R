library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(cowplot)
library(lubridate)
library(readxl)
library(gridExtra)
library(grid)

#select data and sort
dat <- read.csv('input_data.csv')
#subsampleing data
new <- select(dat,1,5,10:12,17:22)

#subsample for only colombo district and main delta sub(AY.28, AY.104 and B.1.617.2)
lin <- dat %>% filter(district=="Colombo") %>% 
  filter(pangolin_lineage %in% c('AY.28','AY.104','B.1.617.2'))%>% select(1,3)

#add fortnight column to lineage data
lin = lin %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))
lin$half <- ifelse(lin$day>=15,  'f2',  'f1' )
lin$month <- format(as.Date(lin$date, "%m/%d/%Y"),format= "%b" )
lin$half <- paste(lin$month,lin$half)

#month data
lin$month <- factor(lin$month, levels=c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
lin$half <- factor(lin$half, levels = 
                     c('May f2','Jun f1','Jun f2','Jul f1','Jul f2','Aug f1','Aug f2','Sep f1','Sep f2','Oct f1','Oct f2'))
#week data
lin$week <- format(as.Date(lin$date, "%Y/%m/%d" ), format="%U%n%Y")

#plot all 3
fortnight_col_plot <- ggplot(lin, aes(half, fill=pangolin_lineage))+ geom_bar() + 
  xlab('Fortnight')+ theme(legend.position = 'none')+ scale_fill_brewer(palette = "Set2")
month_col_plot <- ggplot(lin, aes(x=month, fill=pangolin_lineage)) + geom_bar() + scale_fill_brewer(palette = "Set2")+
  xlab('Month')+theme(axis.title.y = element_blank(),axis.title = element_text(size=10))+ labs(fill="Pango Lineage")
week_col_plot <- ggplot(lin, aes(x=week, fill=pangolin_lineage)) + geom_bar() + theme(legend.position = 'none')+ 
  scale_fill_brewer(palette = "Set2")+xlab('Week')+ylab('No of Sequences')
#combine fort and month plots
comb_plot <- plot_grid(fortnight_col_plot, month_col_plot, rel_widths  = 6:4)
#plot all 3
grid.arrange(comb_plot,week_col_plot)

##Additional revised plots for the paper
#fortnight only
one_plot <- ggplot(lin, aes(half, fill=pangolin_lineage))+ geom_bar() + ylab('No of sequences')+
  xlab('Fortnight (2021)')+ scale_fill_brewer(palette = "Set2")+ labs(fill='Pango Lineage')
#fort_relative freq
two_plot <- ggplot(lin, aes(half, fill=pangolin_lineage))+ geom_bar(position = 'fill') + ylab('Relative frequency')+
  xlab('Fortnight (2021)')+ scale_fill_brewer(palette = "Set2")+ labs(fill='Pango Lineage')

grid.arrange(one_plot,two_plot)


##### statistics ####

library(plyr)
library(reshape2)
library(psych)
library(ggpubr)

##age
#islandwide
age <-select(new,2,3,5)
n_age=length(na.omit(age$age))

#ggplot(age, aes(x=pos_701, y= as.numeric(age), fill=pos_701)) + geom_boxplot()+ labs(title = paste('Age islandwide. n = ',n_age)) + stat_compare_means(method = "t.test")

island_age_plot <- ggboxplot(age, x='pos_701', y= 'age', add = 'jitter', color = 'pos_701', palette = "jco")+ stat_compare_means(method = "t.test")+
  labs(title = paste('Age islandwide. n = ',n_age)) + stat_compare_means(method = "t.test")

#col only age
age_col <- age %>% filter(location=="Colombo")
n_age_col=length(na.omit(age_col$age))
#ggplot(age_col, aes(x=pos_701, y= as.numeric(age), fill=pos_701)) + 
#  geom_boxplot()+ labs(title = paste('Age colombo only. n = ',n_age_col))

col_age_plot <- ggboxplot(age_col, x='pos_701', y= 'age', add = 'jitter', color = 'pos_701', palette = "jco")+ stat_compare_means(method = "t.test")+
  labs(title = paste('Age colombo only. n = ',n_age_col))

#both age plots
grid.arrange(island_age_plot, col_age_plot, ncol=2)

#group_by(age, pos_701) %>% dplyr::summarise(count = n(), mean = mean(age, na.rm = TRUE),sd = sd(age, na.rm = TRUE))

#working stats
describeBy(age, age$pos_701, mat = T) 
write.csv(stat, 'age_stats.csv')

#SD of age data
by(age$age,age$pos_701, sd, na.rm = T)

##vaccination status
vac <- select(new, 2, 3, 6:8)
#filter only vac data
vac <- vac %>% filter(vaccinated %in% c('TRUE', 'FALSE'))

#vac or unvac plot
vac_plot <- ggplot(vac, aes(x=as.logical(vaccinated), fill=pos_701)) + geom_bar()+labs(title = paste ('Vaccination status. n =', nrow(vac)))

#dose
vac1 <- vac %>% filter(dose_updated!='na')
vac_dose_plot <- ggplot(vac1, aes(x=as.character(dose_updated), fill=pos_701)) + geom_bar()+labs(title = paste ('Vaccination dose n =', nrow(vac1)))

#vaccine_type
terms <- c('Covisheild', 'Sinopharm', 'Pfizer')
vac2 <- vac %>% filter(vac_combined %in% terms)

vac_type_plot <- ggplot(vac2, aes(x=as.character(vac_combined), fill=pos_701)) + geom_bar() +labs(title = paste ('Vaccine. n =', nrow(vac2)))

#vac$Vaccine <- factor(vac2$Vaccine, levels = c('Sinopharm', 'Covisheild', 'Vaccinated', 'Not vaccinated'))

#all vac plots
vac_comb_plot <- grid.arrange(vac_plot,vac_dose_plot, ncol=2)
grid.arrange(vac_comb_plot, vac_type_plot)


one1 <- ggplot(vac, aes(x=as.character(dose_updated), fill=pos_701)) + geom_bar()+ facet_grid(~vac_combined)
two <- ggplot(vac, aes(x=vac_combined, fill=pos_701)) + geom_bar()+ facet_grid()
three <- ggplot(vac, aes(x=as.character(dose_updated), fill=pos_701)) + geom_bar()

plot <- plot_grid(two,three)
plot_grid(plot, one1, nrow= 2)

#vac stats

vac_stat <- describeBy(vac ~dose_updated, mat = T)
write.csv(vac_stat, 'vac_stats.csv')

#period days stats
period <- select(new, 2,3,10)
n_period=length(na.omit(period$period))
ggboxplot(period, x='pos_701', y= 'period', add = 'jitter', color = 'pos_701', palette = "jco")+ stat_compare_means(method = "t.test")+
  labs(title = paste('Period (days). n = ',n_period))

#severity
sev <- select(new, 2,11)
sev <- sev %>% filter(severity!='')
describeBy(sev, sev$severity, mat = T) 

ggplot(sev, aes(x=severity, fill=pos_701)) + geom_bar()

#all_stats 
all_stats<- describeBy(new, new$pos_701, mat = T)
write.table(all_stats, 'all_stats.csv', row.names = T , sep = ",")
