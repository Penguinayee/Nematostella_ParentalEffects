##set up the environment
if(!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)

if(!require(devtools)) 
  install.packages("devtools", repos = "https://github.com/r-lib/devtools") #for statistic analysis
if(!require(ggpubr)) 
  install.packages("ggpubr")
library(ggpubr)

if(!require(dplyr))
  install.packages("dplyr", repos = "https://github.com/tidyverse/dplyr")
library(dplyr)

if(!require(readr))
  install.packages("readr")
library("readr")

setwd("")


#read table
oc <- read.csv("oc.csv", header=TRUE, sep=",", dec=".")

##plot histogram
oc.histo <- ggplot(oc, aes(size, fill = group)) +
  geom_histogram(data=subset(oc,Treatment == 'Before spawn'), aes(fill = "B", colour = "B"), alpha = 0.4, stat = "bin") + #set as 30 bins
  geom_histogram(data=subset(oc,Treatment == 'After spawn'), aes(fill = "A", colour = "A"), alpha = 0.4, stat = "bin") + 
  #geom_histogram(data=subset(oc,Treatment == 'Before spawn'), aes(fill = "B"), alpha = 0.4, binwidth = 10000) + #set bin size
  #geom_histogram(data=subset(oc,Treatment == 'After spawn'), aes(fill = "A"), alpha = 0.4, binwidth = 10000) +
  labs(x = "size(µm^2)") +
  scale_colour_manual(name="Spawn", values=c("B" = "red", "A"="blue"), labels=c("B"="Before", "A"="After")) +
  scale_fill_manual(name="Spawn", values=c("B" = "red", "A"="blue"), labels=c("B"="Before", "A"="After"))
oc.histo

#normalized histogram
oc.histo2 <- ggplot(oc, aes(size, fill = group)) +
  geom_histogram(data=subset(oc,Treatment == 'After spawn'), aes(y=..count../sum(..count..), fill = "A", colour = "A"), alpha = 0.4, stat = "bin") +
  geom_histogram(data=subset(oc,Treatment == 'Before spawn'), aes(y=..count../sum(..count..), fill = "B", colour = "B"), alpha = 0.4, stat = "bin") + 
  #geom_histogram(data=subset(oc,Treatment == 'Before spawn'), aes(fill = "B"), alpha = 0.4, binwidth = 10000) + 
  #geom_histogram(data=subset(oc,Treatment == 'After spawn'), aes(fill = "A"), alpha = 0.4, binwidth = 10000) +
  scale_colour_manual(name="Spawn", values=c("B" = "red", "A"="blue"), labels=c("B"="Before", "A"="After")) +
  scale_fill_manual(name="Spawn", values=c("B" = "red", "A"="blue"), labels=c("B"="Before", "A"="After"))+
  labs(x = "size(µm^2)", y = "oocyte count/total count") +
  theme(panel.grid.minor = element_blank(), legend.position = c(.87, .9),
      legend.background = element_rect(colour="gray"),
      legend.text = element_text(size=20, colour="black", face="bold"),
      legend.title = element_text(size=20, colour="black", face="bold"),
      axis.text.x = element_text(size=16, colour="black", face="bold"),
      axis.text.y = element_text(size=16, colour="black", face="bold"),
      axis.title.x = element_text(size=24, colour="black", face="bold"),
      axis.title.y = element_text(size=24, colour="black", face="bold"))

oc.histo2

ggsave(filename= "normalized histogram.png", 
       width=8, height=8, units="in", dpi=300)

#normalized histogram2


#The package plyr is used to calculate the average weight of each group :
library(plyr)
me <- ddply(oc, "Treatment", summarise, grp.mean=mean(size))
head(me)

#statistic analysis
# Shapiro-Wilk normality test for Before spawn:
with(oc, shapiro.test(size[Treatment == "Before spawn"])) # p < 2.2e-16
with(oc, shapiro.test(size[Treatment == "After spawn"])) # p < 2.2e-16
## The distribution of the data are significantly different from the normal distribution.
## So use non parametric two-samples Wilcoxon rank test in stead.

## t <- wilcox.test(oc$size[which(oc$Treatment == "Before spawn")], oc$size[which(oc$Treatment == "After spawn")], exact = FALSE)
## t$p.value # Print the p-value = 3.07427e-22

t <- compare_means(size ~ Treatment, oc, method = "wilcox.test", paired = FALSE)
t$p # Print the p-value = 3.07427e-22

#create levels to the Treatments
oc$level <- factor(oc$Treatment, levels=c("Before spawn", "After spawn"))

oc.histo3 <- ggplot(oc, aes(size, fill = Treatment, colour = Treatment)) +
  geom_histogram(aes(y=..count../sum(..count..)), alpha = 0.4, stat = "bin") +
  geom_vline(data=me, aes(xintercept=grp.mean, color=Treatment), linetype="dashed") + #add mean line
  facet_grid(level ~ .) + #Use facets to split the plot into multiple panels
  scale_colour_manual("Spawn",breaks=c("Before spawn", "After spawn"),
                      values = c("red", "blue"),
                      labels = c("Before", "After")) +
  scale_fill_manual("Spawn",breaks=c("Before spawn", "After spawn"),
                    values = c("red", "blue"),
                    labels = c("Before", "After")) +
  labs(x = "oocyte size(µm^2)", y = "oocyte count/total count")+
  theme(panel.grid.minor = element_blank(), legend.position = "none", #legend.position = c(.87, .9),
      legend.background = element_rect(colour="gray"),
      legend.text = element_text(size=20, colour="black", face="bold"),
      legend.title = element_text(size=20, colour="black", face="bold"),
      axis.text.x = element_text(size=16, colour="black", face="bold"),
      axis.text.y = element_text(size=16, colour="black", face="bold"),
      axis.title.x = element_text(size=24, colour="black", face="bold"),
      axis.title.y = element_text(size=24, colour="black", face="bold"),
      strip.text = element_text(face="bold", size=20), #modify facet labels
      strip.background = element_rect(colour="black",size=1))
oc.histo3



ggsave(filename= "normalized histogram2.png", 
       width=8, height=8, units="in", dpi=300)

##box plot
oc.box <- ggplot(oc, aes(x = Treatment, y = size, fill = Treatment)) + 
  geom_boxplot(alpha = 0.4, colour = c("red", "blue")) +
  scale_x_discrete(limits=c("Before spawn", "After spawn")) +
  scale_colour_manual("Spawn",breaks=c("Before spawn", "After spawn"),
                    values = c("red", "blue"),
                    labels = c("Before", "After")) +
  scale_fill_manual("Spawn",breaks=c("Before spawn", "After spawn"),
                    values = c("red", "blue"),
                      labels = c("Before", "After")) +
  labs(y = "oocyte size(µm^2)") +
  theme(panel.grid.minor = element_blank(), legend.position = c(.87, .9),
        legend.background = element_rect(colour="gray"),
        legend.text = element_text(size=20, colour="black", face="bold"),
        legend.title = element_text(size=20, colour="black", face="bold"),
        axis.text.x = element_text(size=16, colour="black", face="bold"),
        axis.text.y = element_text(size=16, colour="black", face="bold"),
        axis.title.x = element_text(size=24, colour="black", face="bold"),
        axis.title.y = element_text(size=24, colour="black", face="bold"))
oc.box

ggsave(filename= "oocyte-size-box.png", 
       width=8, height=8, units="in", dpi=300)

##boxplot with stat
oc.box2 <- ggplot(oc, aes(x = Treatment, y = size, fill = Treatment)) + 
  geom_boxplot(alpha = 0.4, colour = c("red", "blue")) +
  stat_compare_means(data=oc,
                     mapping=aes(x=Treatment, y=size),
                     label = "p.signif", 
                     method= "wilcox.test")
oc.box2

##plot density
oc.dens <- ggplot(oc, aes(size)) +
  geom_density(data=subset(oc,Treatment == 'Before spawn'), fill = "magenta", alpha = 0.5, kernel = "gaussian") + 
  geom_density(data=subset(oc,Treatment == 'After spawn'), fill = "green", alpha = 0.5, kernel = "gaussian")

oc.dens

