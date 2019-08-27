library(ggplot2)
library(dplyr)
library(ggbeeswarm)
library(gridExtra)

data = read.table("Biomarkers_and_age_forR.txt", header=TRUE, sep="\t") # matrix with a sample per line including a column with genotype ("None" or "pLoF") and another headed "eur" with a "Y" for samples to include (unrelated Europeans) with all biomarkers and lung function tests in different columns
data = subset(data, data$eur=='Y')

data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

### panel (a) kidney markers ###
# ACR (albumin to creatinine ratio)

kidney_data <- data %>%
	mutate(ACR = Microalbumin.in.urine/(Creatinine..enzymatic..in.urine*0.0001131222))

acr<-ggplot(subset(kidney_data, genotype %in% c("pLoF","None")), aes(x=genotype, y=ACR, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Albumin-to-creatinine ratio (ACR)") +
	stat_summary(fun.data=data_summary, colour="black") +
	ylim(-90,6000) +
	coord_cartesian(ylim=c(0,400)) +
	geom_hline(yintercept=30, linetype="dashed") +
	geom_hline(yintercept=300, linetype="dashed") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	annotate("text", x = 1.5, y = 22, label = "Normal", colour="black",size=4) +
	annotate("text", x = 1.5, y = 285, label = "Moderate", colour="black",size=4) +
	annotate("text", x = 1.5, y = 315, label = "Severe", colour="black",size=4) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

# eGFR (Glomerular filtration rate) using the CKD-EPI CREATININE EQUATION (2009) - https://www.kidney.org/content/ckd-epi-creatinine-equation-2009

kidney_data <- kidney_data %>%
	mutate(Creatinine.mg.dl=Creatinine*0.01131222) %>%
	mutate(k=ifelse(Sex<0.5, 0.7, 0.9)) %>%
	mutate(alpha=ifelse(Sex<0.5, -0.329, -0.411)) %>%
	mutate(min_val=ifelse((Creatinine.mg.dl/k)<1, (Creatinine.mg.dl/k), 1)) %>%
	mutate(max_val=ifelse((Creatinine.mg.dl/k)>1, (Creatinine.mg.dl/k), 1)) %>%
	mutate(final_fac=ifelse(Sex<0.5, 1.018, 1)) %>%
	mutate(eGFR=141*(min_val^alpha)*(max_val^(-1.209))*(0.993^Age.at.recruitment)*final_fac)

egfr<-ggplot(subset(kidney_data, genotype %in% c("pLoF","None")), aes(x=genotype, y=eGFR, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Glomerular filtration rate (eGFR)") +
	stat_summary(fun.data=data_summary, colour="black") +
	geom_hline(yintercept=15, linetype="dashed") +
	geom_hline(yintercept=30, linetype="dashed") +
	geom_hline(yintercept=45, linetype="dashed") +
	geom_hline(yintercept=60, linetype="dashed") +
	geom_hline(yintercept=90, linetype="dashed") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	annotate("text", x = 1.5, y = 120, label = "Normal", colour="black",size=4) +
	annotate("text", x = 1.5, y = 75, label = "Mild", colour="black",size=4) +
	annotate("text", x = 1.5, y = 55, label = "Mild-", colour="black",size=4) +
	annotate("text", x = 1.5, y = 50, label = "moderate", colour="black",size=4) +
	annotate("text", x = 1.5, y = 40, label = "Moderate", colour="black",size=4) +
	annotate("text", x = 1.5, y = 35, label = "-severe", colour="black",size=4) +
	annotate("text", x = 1.5, y = 22.5, label = "Severe", colour="black",size=4) +
	annotate("text", x = 1.5, y = 8, label = "Kidney", colour="black",size=4) +
	annotate("text", x = 1.5, y = 3, label = "failure", colour="black",size=4) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

plot5<-grid.arrange(acr,egfr, nrow = 1)

ggsave(file="UKBB_figure_A.png", plot=plot5, width = 150, height = 120, units = "mm")

### panel (b) lung function tests ###
fvc<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=FVC, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Forced vital capacity (FVC) Z-score") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

fev1<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=FEV1, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Forced expiratory volume in 1-second (FEV1) Z-score") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

fefv<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=FEV1.FVC, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("FEV1/FVC ratio Z-score") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

plot4<-grid.arrange(fvc,fev1,fefv, nrow = 1)

ggsave(file="UKBB_figure_B.png", plot=plot4, width = 200, height = 120, units = "mm")

### panel (c) blood biomarkers for liver function ###
al<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=Albumin, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Albumin (g/L)") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

ap<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=Alkaline.phosphatase, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Alkaline phosphatase (U/L)") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	ylim(0,1000) +
	coord_cartesian(ylim=c(0,250)) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

at<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=Alanine.aminotransferase, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Alanine aminotransferase (U/L)") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	ylim(0,450) +
	coord_cartesian(ylim=c(0,150)) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

aat<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=Aspartate.aminotransferase, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Aspartate aminotransferase (U/L)") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	ylim(0,600) +
	coord_cartesian(ylim=c(0,100)) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

db<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=Direct.bilirubin, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Direct bilirubin (umol/L)") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	ylim(0,50) +
	coord_cartesian(ylim=c(0,10)) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

cr<-ggplot(subset(data, genotype %in% c("pLoF","None")), aes(x=genotype, y=Creatinine, fill=genotype, colour=genotype)) +
	geom_violin(alpha=0.2, trim=FALSE) +
	geom_quasirandom() +
	ylab("Creatinine (umol/L)") +
	stat_summary(fun.data=data_summary, colour="black") +
	scale_colour_manual(values=c("dark grey","#00BFC4")) +
	scale_fill_manual(values=c("dark grey","#00BFC4")) +
	ylim(0,1200) +
	coord_cartesian(ylim=c(0,200)) +
	theme_classic() +
	theme(legend.position = "none", axis.title.x = element_blank(), text = element_text(size=16))

plot5<-grid.arrange(al,ap,at,aat,db,cr, nrow = 1)

ggsave(file="UKBB_figure_C.png", plot=plot5, width = 350, height = 120, units = "mm")


