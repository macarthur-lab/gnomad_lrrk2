library(ggplot2)
library(dplyr)

data_23andMe <- read.table("../data_files/23andMeEthinicityData.txt", header=TRUE)

total_carriers=554+30+518

data_23andMe <- data_23andMe %>%
        mutate(proportion=count/total_carriers)

data_23andMe$population <- factor(data_23andMe$population, c("AFR","AMR","NFE","SAS","Other"))
data_23andMe$protein_variant <- factor(data_23andMe$protein_variant, c("p.Cys1313Ter","p.Leu2063Ter","p.Arg1334Ter"))

png("Ethnicity_distribution_23andMe.png",width=800,height=1000,res=150)
ggplot(data_23andMe, aes(x=population, y=proportion, fill=protein_variant)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values=c("#C6878F","#729EA1","#4D7EA8")) +
        theme_bw() +
        ylab("Proportion of LRRK2 variant carriers") +
        scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_blank(),text = element_text(size=16),legend.tit
dev.off()

