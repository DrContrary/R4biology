
library(dplyr)
library(RCurl)
library(ggplot2)

#read in the files from github
urlBase <-'https://raw.githubusercontent.com/DrContrary/R4biology/master/Data%20Files/' 
mkCon <- function(nm) { 	
  textConnection(getURL(paste(urlBase,nm,sep='/')))
}
sytox_data_full <- read.table(mkCon('sytox_data_full.csv'), sep=',',header=T,comment.char='', row.names = 1)
sytox_data_full$glucose = as.character(sytox_data_full$glucose)

#generate the table of statisitcs 
mean_stats_full = sytox_data_full %>%
  group_by(glucose, mye.treatment) %>%
  summarise(percent_dead_mean = mean(percent_dead), sd_percent = sd(percent_dead)) %>%
  arrange(mye.treatment)
mean_stats_full

#box plot to show data and outliers
ggplot(sytox_data_full, 
       aes(x= glucose, y = percent_dead, fill = mye.treatment)) +
  geom_boxplot()


install.packages("outliers")
library(outliers)

otest = sytox_data_full %>%
  filter(glucose ==3) %>%
  filter(mye.treatment == "+_Mye") 
grubbs.test(otest$percent_dead)

library(cowplot)
#box plot to show data and outliers
nooutlier =ggplot(sytox_data_full, 
       aes(x= glucose, y = percent_dead, fill = mye.treatment)) +
  geom_boxplot(outlier.shape = NA)+
  ylab("Percentage of Dead Cells") +
  xlab("Glucose Concentration (g/L)") +
  labs(title = "Outliers Hidden")+
  scale_fill_discrete(name="Experimental Condition",
                    breaks=c("+_Mye", "No_Mye"),
                    labels=c("Treated", "Untreated"))+
  theme(legend.position="bottom")
woutlier =ggplot(sytox_data_full, 
                  aes(x= glucose, y = percent_dead, fill = mye.treatment)) +
  geom_boxplot()+
  ylab("Percentage of Dead Cells") +
  xlab("Glucose Concentration (g/L)") +
  labs(title = "Outliers Shown")+
  scale_fill_discrete(name="Experimental Condition",
                      breaks=c("+_Mye", "No_Mye"),
                      labels=c("Treated", "Untreated"))+
  theme(legend.position="bottom")
plot_grid(woutlier, nooutlier, labels = "AUTO")


#high impact journal formatting
ggplot(mean_stats_full, 
       aes(x= glucose, y = percent_dead_mean, fill = mye.treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(data = sytox_data_full,
             aes(x= glucose, y = percent_dead, fill = mye.treatment),
             position = position_dodge(width = 0.9),
             shape = 21,    #shape with outline
             size = 3)+     #size of point
  geom_errorbar(aes(ymin=percent_dead_mean-sd_percent, ymax=percent_dead_mean+sd_percent),
                size=.3,    # Thinner lines
                width=.2,
                position = position_dodge(width = 0.9)) +
  
  ylab("Percentage of Dead Cells") +
  xlab("Glucose Concentration (g/L)") +
  labs(title = "Barplot of Data using ggplot2") +
  scale_fill_discrete(name="Experimental \n Condition",
                      breaks=c("+_Mye", "No_Mye"),
                      labels=c("Treated", "Untreated"))+
  theme_bw()

