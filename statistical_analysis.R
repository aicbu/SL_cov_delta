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
dat <- read_xlsx('outcome_merged_final.xlsx')

#subsampleing data
new <- select(dat,1,4,10:12,17:21)

#fortnight data
new = new %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

new$half <- ifelse(new$day>=15,  '02q',  '01q' )
new$month <- format(as.Date(new$date, "%m/%d/%Y"),format= "%b" )
new$half <- paste(new$month,new$half)
#month data
new$month <- factor(new$month, levels=c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
#week data
new$week <- format(as.Date(new$date, "%m/%d/%Y" ), format="%U%n%Y")

##plot islandwide data
#make as factor
new$half <- factor(new$half, levels=unique(new$half))
new$month <- factor(new$month, levels=c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
#plot
fortnight_is <- ggplot(new, aes(half, fill=pos_701))+ geom_bar() + xlab('fortnight')+ theme(legend.position = 'none')
month_is <- ggplot(new, aes(x=month, fill=pos_701)) + geom_bar() 
week_is <- ggplot(new, aes(x=week, fill=pos_701)) + geom_bar() + theme(legend.position = 'none')

#plot_grid islandwide data
n=nrow(new)
one <- plot_grid(fortnight_is, month_is, rel_widths  = 3:2)
grid.arrange(one,week_is, top=textGrob(paste('Island wide. N =',n)))

##plot col only data
#subset col data
col <- new %>% filter(district=="Colombo")
#make as factor
col$half <- factor(col$half, levels=unique(col$half))
col$month <- factor(col$month, levels=c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'))
#plot
fortnight_col <- ggplot(col, aes(half, fill=pos_701))+ geom_bar() + xlab('fortnight')+ theme(legend.position = 'none')
month_col <- ggplot(col, aes(x=month, fill=pos_701)) + geom_bar() 
week_col <- ggplot(col, aes(x=week, fill=pos_701)) + geom_bar() + theme(legend.position = 'none')

#plot_grid col data
n=nrow(col)
two <- plot_grid(fortnight_col, month_col, rel_widths  = 3:2)
grid.arrange(two,week_col, top=textGrob(paste('Colombo Only. N = ',n )))

#sum <- select(sum,2,7)
#write.table(table(sum), file = 'col_data_table.tsv', sep = '\t', quote = T)
#table(sum)


## statistics

library(plyr)
library(reshape2)
library(psych)
library(ggpubr)

##age
#islandwide
age <-select(new,2,3,4)
n_age=length(na.omit(age$age))
#ggplot(age, aes(x=pos_701, y= as.numeric(age), fill=pos_701)) + 
 # geom_boxplot()+ labs(title = paste('Age islandwide. n = ',n_age)) + stat_compare_means(method = "t.test")

island_age <- ggboxplot(age, x='pos_701', y= 'age', add = 'jitter', color = 'pos_701', palette = "jco")+ stat_compare_means(method = "t.test")+
  labs(title = paste('Age islandwide. n = ',n_age)) + stat_compare_means(method = "t.test")

#col only
age_col <- age %>% filter(district=="Colombo")
n_age_col=length(na.omit(age_col$age))
#ggplot(age_col, aes(x=pos_701, y= as.numeric(age), fill=pos_701)) + 
#  geom_boxplot()+ labs(title = paste('Age colombo only. n = ',n_age_col))

col_age <- ggboxplot(age_col, x='pos_701', y= 'age', add = 'jitter', color = 'pos_701', palette = "jco")+ stat_compare_means(method = "t.test")+
  labs(title = paste('Age colombo only. n = ',n_age_col))

grid.arrange(island_age, col_age, ncol=2)

#group_by(age, pos_701) %>% dplyr::summarise(count = n(), mean = mean(age, na.rm = TRUE),sd = sd(age, na.rm = TRUE))

#working stats
describeBy(age, age$pos_701, mat = T) 
write.csv(stat, 'age_stats.csv')

age %>% group_by(pos_701) %>% sd(age$age, na.rm = T)

#by(age$age,age$pos_701, sd, na.rm = T)

##vaccine
vac <- select(new, 2, 3, 6:8)
#filter only vac data
vac <- vac %>% filter(vaccinated %in% c('T','F'))

#vac or not plot
vac_one <- ggplot(vac, aes(x=as.logical(vaccinated), fill=pos_701)) + geom_bar()+labs(title = paste ('Vaccination status. n =', nrow(vac)))

#dose
vac1 <- vac %>% filter(dose_comb!='na')
vac_two <- ggplot(vac1, aes(x=as.character(dose_comb), fill=pos_701)) + geom_bar()+labs(title = paste ('Vaccination dose n =', nrow(vac1)))

#vaccine
terms <- c('Covisheild', 'Sinopharm', 'Pfizer')
vac2 <- vac %>% filter(vac_combined %in% terms)

vac_three <- ggplot(vac2, aes(x=as.character(vac_combined), fill=pos_701)) + geom_bar() +labs(title = paste ('Vaccine. n =', nrow(vac2)))

#vac$Vaccine <- factor(vac2$Vaccine, levels = c('Sinopharm', 'Covisheild', 'Vaccinated', 'Not vaccinated'))

#all vac plots
four <- grid.arrange(vac_one,vac_two, ncol=2)
grid.arrange(four, vac_three)

one1 <- ggplot(vac, aes(x=as.character(dose), fill=pos_701)) + geom_bar()+ facet_grid(~Vaccine)
two <- ggplot(vac, aes(x=Vaccine, fill=pos_701)) + geom_bar()+ facet_grid()
three <- ggplot(vac, aes(x=as.character(dose), fill=pos_701)) + geom_bar()

plot <- plot_grid(two,three)
plot_grid(plot, one1, nrow= 2)

#vac stats

vac_stat <- describeBy(vac ~dose, mat = T)
write.csv(vac_stat, 'vac_stats.csv')

#period days stats
period <- select(new, 2,3,9)
n_period=length(na.omit(period$period))
ggboxplot(period, x='pos_701', y= 'period', add = 'jitter', color = 'pos_701', palette = "jco")+ stat_compare_means(method = "t.test")+
  labs(title = paste('Period (days). n = ',n_period))

#severity

sev <- select(new, 2,10)
describeBy(sev, sev$pos_701, mat = T) 

sev <- sev %>% filter(severity!='na')
ggplot(sev, aes(x=severity, fill=pos_701)) + geom_bar()

#all_stats 
all_stats<- describeBy(new, new$pos_701, mat = T)
write.table(all_stats, 'all_stats.csv', row.names = T , sep = ",")
