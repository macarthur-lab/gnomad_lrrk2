library(ggplot2)

data <- read.table("../data_files/gnomAD_ethnicities.txt", header=TRUE)

ethnicity_byVar<-data.frame(variant=character(), ethnicity=character(), proportion=numeric(0))

pops<-c("oth","fin","asj","eas","sas","afr","amr","nfe")

vars<-c("other","p.Arg772Ter","p.Cys1313Ter","p.Arg1725Ter","p.Leu2063Ter")

for(i in 1:length(pops))
{
        for (j in 1:length(vars))
        {
                to_add=data.frame(variant=vars[j], ethnicity=pops[i], proportion=(nrow(subset(data,variant==vars[j]&pop==pops[i]))/nrow(data)))
                ethnicity_byVar=rbind(ethnicity_byVar, to_add)
        }
}

ethnicity_byVar$ethnicity <- gsub("oth", "Other", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("fin", "FIN", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("asj", "ASJ", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("eas", "EAS", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("sas", "SAS", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("afr", "AFR", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("amr", "AMR", ethnicity_byVar$ethnicity)
ethnicity_byVar$ethnicity <- gsub("nfe", "NFE", ethnicity_byVar$ethnicity)

ethnicity_byVar$ethnicity <- factor(ethnicity_byVar$ethnicity, c("AFR","AMR","ASJ","EAS","FIN","NFE","SAS","Other"))
ethnicity_byVar$variant <- factor(ethnicity_byVar$variant, c("other","p.Arg772Ter","p.Cys1313Ter","p.Arg1725Ter","p.Leu2063Ter"))


png("Ethnicity_distribution.png",width=1200,height=1000,res=150)
ggplot(ethnicity_byVar, aes(x=ethnicity, y=proportion, fill=variant)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values=c("#7F7F7F","#ADA8B6","#C6878F","#AFD5AA","#729EA1")) +
        theme_bw() +
        ylab("Proportion of LRRK2 variant carriers") +
        scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_blank(),text = element_text(size=20),legend.title = element_blank(),legend.position="bottom",legend.direction = "horizontal",legend.text=element_text(size=15))
dev.off()
