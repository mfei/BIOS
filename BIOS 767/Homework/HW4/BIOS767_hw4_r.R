## BIOS767_hw4_Eunchoing Kang
# Library set-up
library(tidyverse)
library(psych)

# Data Import
# setwd("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 767/hw/hw4")
monkey <- read.table("monkey.dat", header=TRUE, na.strings="na")

# Data Transformation
monkey_long <- pivot_longer(monkey, starts_with("carnt"),
                            names_to="week", values_to="carnt",
                            names_prefix="carnt")
monkey_long$id <- factor(monkey_long$id, levels=c(1:14))
monkey_long$group <- as.character(monkey_long$group)
monkey_long$week <- as.numeric(monkey_long$week)

# Finding # of observations and missing values
nrow(monkey_long)
sum(is.na(monkey_long))


# Plotting a Spaghetti plot
ggplot(monkey_long, aes(week, carnt, color=id, group=id)) +
        geom_line(size=0.8)+
        facet_grid(. ~ group) + 
        theme(legend.position = "none") +
        coord_cartesian(xlim=c(0,30), ylim=c(0,100)) +
        labs(x= "Week", y="Total Carnitine Level (nmol/ml)") +
        scale_x_continuous(breaks=c(0,2,4,6,8, 16,18,20,22,24,26,28,30)) +
        scale_y_continuous(breaks=seq(0, 100, 20))
        #theme(axis.text.x=element_text(angle=45, hjust=1))


# Making Summay Table
monkey_summary <- monkey_long %>% group_by(group, week) %>% 
        summarise(mean=mean(carnt, na.rm=TRUE), sd=sd(carnt, na.rm=TRUE), min=min(carnt, na.rm=TRUE), max=max(carnt, na.rm=TRUE))


# Plotting Mean Values
ggplot(monkey_summary, aes(week, mean,color=group, group=group)) +
        geom_line(size=1) +
        geom_point(size=2) +
        coord_cartesian(xlim=c(0,30), ylim=c(0,100)) +
        labs(x= "Week", y="Mean Carnitine Level (nmol/ml)") +
        scale_x_continuous(breaks=c(0:9, 16:30)) +
        scale_y_continuous(breaks=seq(0, 100, 20))
        #geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
        #             position=position_dodge(0.5))


# Correlation matrix
# Data Transformation
# Group1
monkey_cor1 <- monkey %>% 
                        filter(group==1) %>%
                        select(-id, -group)
colnames(monkey_cor1) <- c(0:9,16:30)
rcor_g1 <- cor(monkey_cor1)

# Group2
monkey_cor2 <- monkey %>% 
                        filter(group==2) %>%
                        select(-id, -group)
colnames(monkey_cor2) <- c(0:9,16:30)
rcor_g2 <- cor(monkey_cor2)

# Plotting correlation heatmap
# Exporting size 6.5X7
corPlot(rcor_g1, upper=FALSE, alpha=0.8, mar=TRUE, cex=0.45, main="Group 1")
corPlot(rcor_g2, upper=FALSE, alpha=0.8, mar=TRUE, cex=0.45, main="Group 2")