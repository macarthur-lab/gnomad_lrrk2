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

age<-ggarrange(gnomAD, tw3andMe, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

### Figure 3b - biomarkers

sampleData <- read.table("../data_files/BIOMARKERS_perSample_selected.txt", header=TRUE)
cohortData <- read.table("../data_files/BIOMARKERS_cohort_stats.txt", header=TRUE)

carrierSummaryData<-data.frame(variable=character(), mean=numeric(0), sd=numeric(0))
to_add_creatine=data.frame(variable="creatinine", mean=mean(sampleData$creatinine, na.rm=TRUE), sd=sd(sampleData$creatinine, na.rm=TRUE))
carrierSummaryData=rbind(carrierSummaryData, to_add_creatine)
to_add_CPeptide=data.frame(variable="CPeptide", mean=mean(sampleData$CPeptide, na.rm=TRUE), sd=sd(sampleData$CPeptide, na.rm=TRUE))
carrierSummaryData=rbind(carrierSummaryData, to_add_CPeptide)
to_add_VEGF=data.frame(variable="VEGF", mean=mean(sampleData$VEGF, na.rm=TRUE), sd=sd(sampleData$VEGF, na.rm=TRUE))
carrierSummaryData=rbind(carrierSummaryData, to_add_VEGF)
to_add_cholesterol=data.frame(variable="cholesterol", mean=mean(sampleData$cholesterol, na.rm=TRUE), sd=sd(sampleData$cholesterol, na.rm=TRUE))
carrierSummaryData=rbind(carrierSummaryData, to_add_cholesterol)
to_add_LDL=data.frame(variable="LDL", mean=mean(sampleData$LDL, na.rm=TRUE), sd=sd(sampleData$LDL, na.rm=TRUE))
carrierSummaryData=rbind(carrierSummaryData, to_add_LDL)
to_add_HDL=data.frame(variable="HDL", mean=mean(sampleData$HDL, na.rm=TRUE), sd=sd(sampleData$HDL, na.rm=TRUE))
carrierSummaryData=rbind(carrierSummaryData, to_add_HDL)

carrierSummaryData<-carrierSummaryData %>%
        mutate(y=2)

sampleData<-sampleData %>%
        mutate(y=2)

cohortData<-cohortData %>%
        mutate(y=1)

creatine<-ggplot(subset(cohortData, variable=='creatinine'), aes(x=y,y=mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
        xlab(paste("creatinine \n(", cohortData$unit[cohortData$variable=='creatinine'], ")")) +
        ylab("") +
        geom_point(data=subset(carrierSummaryData, variable=='creatinine'), aes(x=y, y=mean), colour="grey", size=3) +
        geom_errorbar(data=subset(carrierSummaryData, variable=='creatinine'), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
        geom_point(data=sampleData, aes(x=y, y=creatinine), colour="#00BFC4", shape=18, size=4) +
        theme_bw() +
        xlim(-1,4) +
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

CPeptide<-ggplot(subset(cohortData, variable=='CPeptide'), aes(x=y,y=mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
        xlab(paste("CPeptide \n(", cohortData$unit[cohortData$variable=='CPeptide'], ")")) +
        ylab("") +
        geom_point(data=subset(carrierSummaryData, variable=='CPeptide'), aes(x=y, y=mean), colour="grey", size=3) +
        geom_errorbar(data=subset(carrierSummaryData, variable=='CPeptide'), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
        geom_point(data=sampleData, aes(x=y, y=CPeptide), colour="#00BFC4", shape=18, size=4) +
        theme_bw() +
        xlim(-1,4) +
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

VEGF<-ggplot(subset(cohortData, variable=='VEGF'), aes(x=y,y=mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
        xlab(paste("VEGF \n", cohortData$unit[cohortData$variable=='VEGF'])) +
        ylab("") +
        geom_point(data=subset(carrierSummaryData, variable=='VEGF'), aes(x=y, y=mean), colour="grey", size=3) +
        geom_errorbar(data=subset(carrierSummaryData, variable=='VEGF'), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
        geom_point(data=sampleData, aes(x=y, y=VEGF), colour="#00BFC4", shape=18, size=4) +
        theme_bw() +
        xlim(-1,4) +
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

cholesterol<-ggplot(subset(cohortData, variable=='cholesterol'), aes(x=y,y=mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
        xlab(paste("cholesterol \n(", cohortData$unit[cohortData$variable=='cholesterol'], ")")) +
        ylab("") +
        geom_point(data=subset(carrierSummaryData, variable=='cholesterol'), aes(x=y, y=mean), colour="grey", size=3) +
        geom_errorbar(data=subset(carrierSummaryData, variable=='cholesterol'), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
        geom_point(data=sampleData, aes(x=y, y=cholesterol), colour="#00BFC4", shape=18, size=4) +
        theme_bw() +
        xlim(-1,4) +
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

LDL<-ggplot(subset(cohortData, variable=='LDL'), aes(x=y,y=mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
        xlab(paste("LDL \n(", cohortData$unit[cohortData$variable=='LDL'], ")")) +
        ylab("") +
        geom_point(data=subset(carrierSummaryData, variable=='LDL'), aes(x=y, y=mean), colour="grey", size=3) +
        geom_errorbar(data=subset(carrierSummaryData, variable=='LDL'), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
        geom_point(data=sampleData, aes(x=y, y=LDL), colour="#00BFC4", shape=18, size=4) +
        theme_bw() +
        xlim(-1,4) +
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

HDL<-ggplot(subset(cohortData, variable=='HDL'), aes(x=y,y=mean)) +
        geom_point(size=3) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
        xlab(paste("HDL \n(", cohortData$unit[cohortData$variable=='HDL'], ")")) +
        ylab("") +
        geom_point(data=subset(carrierSummaryData, variable=='HDL'), aes(x=y, y=mean), colour="grey", size=3) +
        geom_errorbar(data=subset(carrierSummaryData, variable=='HDL'), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
        geom_point(data=sampleData, aes(x=y, y=HDL), colour="#00BFC4", shape=18, size=4) +
        theme_bw() +
        xlim(-1,4) +
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

biomarkers<-ggarrange(creatine, CPeptide, VEGF, cholesterol, LDL, HDL, ncol=6, nrow = 1)

### Figure 3c - 23andMe pheWAS

phewas_data <- read.table("../data_files/23andMe_phewas_statistics.tsv", header=TRUE)
data_new <- na.omit(phewas_data)

gw_threshold=-log10(0.05/nrow(data_new)) # 3.976808

colours<-rep(c("black","grey"), (length(unique(data_new$tag))/2))
colours<-c(colours,"black")
groups<-unique(data_new$tag)

position_list=numeric(0)

for (i in 1:length(groups))
{
        name=groups[i]
        max=max(data_new$number[data_new$tag==name])
        min=min(data_new$number[data_new$tag==name])
        position=min+((max-min)/2)
        position_list[i]=position
}

xmax=max(data_new$number)+1

groups <- gsub("_", " ", groups)

pheWAS<-ggplot(data_new, aes(x=number,y=-log10(P_value), colour=tag)) +
        geom_point() +
        geom_hline(yintercept=gw_threshold, linetype="dashed") +
        ylim(0,5) +
        scale_colour_manual(values=colours) +
        ylab("-log10(Pvalue)") +
        theme_bw() +
        scale_x_continuous(breaks=position_list, labels=groups, limits=c(0,xmax), expand = c(0, 0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),panel.border = element_blank(),, panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), axis.title.x = element_blank(),text = element_text(size=16),axis.text.y=element_text(size=20),legend.position="none")

### Combine plots

png("Figure3.png",width=2000,height=1500,res=150)
        ggarrange(ggarrange(age, biomarkers, ncol = 2, labels = c("(a)", "(b)")), pheWAS, nrow = 2, labels = "(c)")
dev.off()

