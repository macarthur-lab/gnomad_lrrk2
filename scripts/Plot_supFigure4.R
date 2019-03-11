library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

sampleData <- read.table("../data_files/BIOMARKERS_all_LRRK2_carriers.txt", header=TRUE, sep="\t")
cohortData <- read.table("../data_files/BIOMARKERS_all_normal_ranges.txt", header=TRUE)

biomarkers = cohortData$variable

carrierData <- gather(sampleData, key="biomarker", value="value", -promis_id)

carrierSummaryData<-data.frame(variable=character(), mean=numeric(0), sd=numeric(0))

for(i in 1:nrow(cohortData))
{
        bio=biomarkers[i]
        to_add=data.frame(variable=bio, mean=mean(carrierData$value[carrierData$biomarker==bio], na.rm=TRUE), sd=sd(carrierData$value[carrierData$biomarker==bio], na.rm=TRUE))
        carrierSummaryData=rbind(carrierSummaryData, to_add)
}

carrierSummaryData<-carrierSummaryData %>%
        mutate(y=2)

carrierData<-carrierData %>%
        mutate(y=2)

cohortData<-cohortData %>%
        mutate(y=1)

plot_list=list()
for(i in 1:21)
{
        bio=biomarkers[i]
        plot<-ggplot(subset(cohortData, variable==bio), aes(x=y,y=mean)) +
                geom_point(size=3) +
                geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
                xlab(bio) +
                ylab("") +
                geom_point(data=subset(carrierSummaryData, variable==bio), aes(x=y, y=mean), colour="grey", size=3) +
                geom_errorbar(data=subset(carrierSummaryData, variable==bio), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
                geom_point(data=subset(carrierData, biomarker==bio), aes(x=y, y=value), colour="#00BFC4", shape=18, size=4) +
                theme_bw() +
                xlim(-1,4) +
                theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
        plot_list[[i]]<-plot
}

png("PROMIS_biomarkers_ALL1.png",width=1500,height=2000,res=150)
        grid.arrange(grobs=plot_list, nrow = 3)
dev.off()

plot_list2=list()
j=1
for(i in 22:42)
{
        bio=biomarkers[i]
        plot<-ggplot(subset(cohortData, variable==bio), aes(x=y,y=mean)) +
                geom_point(size=3) +
                geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
                xlab(bio) +
                ylab("") +
                geom_point(data=subset(carrierSummaryData, variable==bio), aes(x=y, y=mean), colour="grey", size=3) +
                geom_errorbar(data=subset(carrierSummaryData, variable==bio), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
                geom_point(data=subset(carrierData, biomarker==bio), aes(x=y, y=value), colour="#00BFC4", shape=18, size=4) +
                theme_bw() +
                xlim(-1,4) +
                theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
        plot_list2[[j]]<-plot
        j=j+1
}

png("PROMIS_biomarkers_ALL2.png",width=1500,height=2000,res=150)
        grid.arrange(grobs=plot_list2, nrow = 3)
dev.off()

plot_list3=list()
j=1
for(i in 42:62)
{
        bio=biomarkers[i]
        plot<-ggplot(subset(cohortData, variable==bio), aes(x=y,y=mean)) +
                geom_point(size=3) +
                geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
                xlab(bio) +
                ylab("") +
                geom_point(data=subset(carrierSummaryData, variable==bio), aes(x=y, y=mean), colour="grey", size=3) +
                geom_errorbar(data=subset(carrierSummaryData, variable==bio), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
                geom_point(data=subset(carrierData, biomarker==bio), aes(x=y, y=value), colour="#00BFC4", shape=18, size=4) +
                theme_bw() +
                xlim(-1,4) +
                theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
        plot_list3[[j]]<-plot
        j=j+1
}

png("PROMIS_biomarkers_ALL3.png",width=1500,height=2000,res=150)
        grid.arrange(grobs=plot_list3, nrow = 3)
dev.off()

plot_list4=list()
j=1
for(i in 62:nrow(cohortData))
{
  bio=biomarkers[i]
  plot<-ggplot(subset(cohortData, variable==bio), aes(x=y,y=mean)) +
                geom_point(size=3) +
                geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0) +
                xlab(bio) +
                ylab("") +
                geom_point(data=subset(carrierSummaryData, variable==bio), aes(x=y, y=mean), colour="grey", size=3) +
                geom_errorbar(data=subset(carrierSummaryData, variable==bio), aes(x=y, ymin=mean-sd, ymax=mean+sd), width=0, colour="grey") +
                geom_point(data=subset(carrierData, biomarker==bio), aes(x=y, y=value), colour="#00BFC4", shape=18, size=4) +
                theme_bw() +
                xlim(-1,4) +
                theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=16),legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
        plot_list4[[j]]<-plot
        j=j+1
}

png("PROMIS_biomarkers_ALL4.png",width=1500,height=2000,res=150)
        grid.arrange(grobs=plot_list4, ncol=5)
dev.off()
