library(ggplot2)
library(dplyr)
library(ggpubr)

## FIGURE 3
### Figure 3a - age distributions

data <- read.table("../data_files/gnomAD_age_distributions.txt", header=TRUE)

age_data<-data %>%
        mutate(percent=Count/Total)

age_data$Cohort <- gsub("Entire_cohort", "Non-carriers", age_data$Cohort)

age_data$Cohort <- gsub("_", " ", age_data$Cohort)
age_data$Cohort <- factor(age_data$Cohort, c("Non-carriers","LRRK2 carriers"))
age_data$age_bin <- factor(age_data$age_bin, c("<30","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))

gnomAD<-ggplot(age_data, aes(x=age_bin,y=percent,fill=Cohort)) +
        geom_bar(stat="identity",position="dodge",colour="black") +
        scale_fill_manual(values=c("darkgrey","#00BFC4")) +
        xlab("age bin") +
        ylab("proportion") +
        ggtitle("gnomAD") +
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colo

data2 <- read.table("../data_files/23andMe_age_distributions.txt", header=TRUE)

data2$Cohort <- gsub("Entire_cohort", "Non-carriers", data2$Cohort)

data2$Cohort <- gsub("_", " ", data2$Cohort)
data2$Cohort <- factor(data2$Cohort, c("Non-carriers","LRRK2 carriers"))
data2$age_bin <- factor(data2$age_bin, c("<30","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))

tw3andMe<-ggplot(data2, aes(x=age_bin,y=Proportion,fill=Cohort)) +
        geom_bar(stat="identity",position="dodge",colour="black") +
        scale_fill_manual(values=c("darkgrey","#00BFC4")) +
        xlab("age bin") +
        ylab("proportion") +
        ggtitle("23andMe") +
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colo

png("Figure3a.png",width=2800,height=1000,res=150)
        age<-ggarrange(gnomAD, tw3andMe, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()

### Figure 3b - 23andMe pheWAS

phewas_data <- read.table("../data_files/Supplementary_Table_7.tsv", header=TRUE) # Supplementary Table 7
data_new <- na.omit(phewas_data)

gw_threshold=-log10(0.05/nrow(data_new))
                                                                                                                                              
data_new$ID_num <- seq.int(nrow(data_new))

colours<-rep(c("black","grey"), (length(unique(data_new$tag))/2))
colours<-c(colours,"black")
groups<-unique(data_new$tag)

position_list=numeric(0)

for (i in 1:length(groups))
{
        name=groups[i]
        max=max(data_new$ID_num[data_new$tag==name])
        min=min(data_new$ID_num[data_new$tag==name])
        position=min+((max-min)/2)
        position_list[i]=position
}

xmax=max(data_new$ID_num)+1

groups <- gsub("_", " ", groups)

png("Figure3b.png",width=2600,height=1200,res=150)
ggplot(data_new, aes(x=number,y=-log10(P_value), colour=tag)) +
        geom_point() +
        geom_hline(yintercept=gw_threshold, linetype="dashed") +
        ylim(0,5) +
        scale_colour_manual(values=colours) +
        ylab("-log10(Pvalue)") +
        theme_bw() +
        scale_x_continuous(breaks=position_list, labels=groups, limits=c(0,xmax), expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),panel.border = element_blank(),, panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), axis.title.x = element_blank(),text = element_text(size=16),axis.text.y=element_text(size=20),legend.position="none")
dev.off()


