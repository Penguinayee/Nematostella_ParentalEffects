knitr::opts_chunk$set(echo = TRUE)
setwd(" ")

#load libraries
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


#read data
oc <- read.csv("OC_raw.csv", header=TRUE, sep=",", dec=".")

#Analyze individual oocytes
##statistic analysis
##Shapiro-Wilk normality test for normality:
with(oc, shapiro.test(Size[Time == "Before"])) # p < 2.2e-16
with(oc, shapiro.test(Size[Time == "After"])) # p < 2.2e-16
## The distribution of the data are significantly different from the normal distribution.
## So use non parametric two-samples Wilcoxon rank test in stead.

wil <- compare_means(Size ~ Time, oc, method = "wilcox.test", paired = FALSE)
wil$p # Print the p-value = 3.07427e-22, oocyte size is significant different


# Reorder the groups order : change the order of the factor data$names
oc$Time <- factor(oc$Time , levels=c("Before", "After"), ordered = TRUE)


##boxplot of individual oocyte with stat
oc.box <- ggplot(oc, aes(Time, Size, fill = Time)) + 
  geom_boxplot(alpha = 0.4, colour = c("red", "blue")) +
  scale_fill_manual(values=c("red", "blue")) +
  stat_compare_means(data=oc,
                     mapping=aes(x=Time, y=Size),
                     label = "p", 
                     method= "wilcox.test",
                     label.x.npc = "center")
oc.box
ggsave(filename= "oocyte-size-box.png", 
       width=8, height=8, units="in", dpi=300)




#use average egg size for each female and comparing those pre and post spawn.

b1.iv <- which(oc$Time == "Before" & oc$Animal == "1")
b2.iv <- which(oc$Time == "Before" & oc$Animal == "2")
b3.iv <- which(oc$Time == "Before" & oc$Animal == "3")
b4.iv <- which(oc$Time == "Before" & oc$Animal == "4")
b5.iv <- which(oc$Time == "Before" & oc$Animal == "5")
a1.iv <- which(oc$Time == "After" & oc$Animal == "1")
a2.iv <- which(oc$Time == "After" & oc$Animal == "2")
a3.iv <- which(oc$Time == "After" & oc$Animal == "3")
a4.iv <- which(oc$Time == "After" & oc$Animal == "4")
a5.iv <- which(oc$Time == "After" & oc$Animal == "5")

mean_per_ani <- c(mean(oc$Size[b1.iv]), mean(oc$Size[b2.iv]), mean(oc$Size[b3.iv]), mean(oc$Size[b4.iv]), mean(oc$Size[b5.iv]),
                  mean(oc$Size[a1.iv]), mean(oc$Size[a2.iv]), mean(oc$Size[a3.iv]), mean(oc$Size[a4.iv]), mean(oc$Size[a5.iv]))
Time_2 <- c("Before", "Before","Before", "Before", "Before", "After", "After", "After", "After", "After")
#Or use cat(paste(shQuote(rep("Before", 5), type="cmd"), collapse=", ")) to create five repettive characters

#create data frame
oc_mean <- data.frame(Time_2, mean_per_ani)

# Shapiro-Wilk normality test for normality:
with(oc_mean, shapiro.test(mean_per_ani)) # p = 0.1513
with(oc_mean, shapiro.test(mean_per_ani[Time_2 == "Before"])) # p = 0.6801
with(oc_mean, shapiro.test(mean_per_ani[Time_2 == "After"])) # p = 0.3259
## The distribution of the data are no different from the normal distribution.
## So use independent t-test.

# Reorder the groups order : change the order of the factor data$names
oc_mean$Time_2 <- factor(oc_mean$Time_2 , levels=c("Before", "After"), ordered = TRUE)


##boxplot of individual oocyte with stat
oc.box_2 <- ggplot(oc_mean, aes(Time_2, mean_per_ani, fill = Time_2)) + 
  geom_boxplot(alpha = 0.4, colour = c("red", "blue")) +
  scale_fill_manual(values=c("red", "blue")) +
  xlab("Spawn") +
  ylab("Mean oocyte size per-animal (µm^2)") +
  theme(panel.grid.minor = element_blank(), legend.position = "none",
        #legend.background = element_rect(colour="gray"),
        #legend.text = element_text(size=20, colour="black", face="bold"),
        #legend.title = element_text(size=20, colour="black", face="bold"),
        axis.text.x = element_text(size=16, colour="black", face="bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.x = element_text(size=24, colour="black", face="bold"),
        axis.title.y = element_text(size=24, colour="black", face="bold")) +
  stat_compare_means(data=oc_mean,
                     mapping=aes(x=Time_2, y=mean_per_ani),
                     label = "p", 
                     method= "t.test",
                     label.x.npc = c(.35,0),
                     size=6)
oc.box_2
ggsave(filename= "oocyte-size-box_per_animal.png", 
       width=8, height=8, units="in", dpi=300)
