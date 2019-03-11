library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

### test difference in ages (gnomAD)
# equivalent 23andMe analysis performed internally

carriers <- read.table("../data_files/age_gnomAD_carriers.txt", header=TRUE, sep="\t")
all <- read.table("../data_files/gnomAD_all_ages.txt", header=TRUE, sep="\t")

x=carriers$age
y=all$age

ks.test(x, y, alternative="two.sided")

### test for domain enrichment/depletion

variantsDist <- read.table("../data_files/LRRK2_domains_withVarCount.txt", header=TRUE, sep="\t")

lrrk2_length=sum(variantsDist$Size)
total_vars=sum(variantsDist$Variants)

domainData<-variantsDist %>%
        mutate(expected=(Size/lrrk2_length)*total_vars) %>%
        mutate(prop_exp=(Size/lrrk2_length))

chisq.test(domainData$Variants, p=domainData$prop_exp)

### test biomarkers for association

sampleData <- read.table("../data_files/BIOMARKERS_all_LRRK2_carriers.txt", header=TRUE, sep="\t")
cohortData <- read.table("../data_files/BIOMARKERS_all_normal_ranges.txt", header=TRUE)

biomarkers = cohortData$variable

carrierData <- gather(sampleData, key="biomarker", value="value", -promis_id)

carrierSummaryData<-data.frame(variable=character(), mean=numeric(0), sd=numeric(0), N=numeric(0))

for(i in 1:nrow(cohortData))
{
        bio=biomarkers[i]
        to_add=data.frame(variable=bio, mean=mean(carrierData$value[carrierData$biomarker==bio], na.rm=TRUE), sd=sd(carrierData$value[carrierData$biomarker==bio], na.rm=TRUE), N=sum(!is.na(carrierData$value[carrierData$biomarker==bio])))
        carrierSummaryData=rbind(carrierSummaryData, to_add)
}

small_sampleData <- read.table("../data_files/BIOMARKERS_perSample_selected.txt", header=TRUE, sep="\t")
small_cohortData <- read.table("../data_files/BIOMARKERS_cohort_stats.txt", header=TRUE)

new_bios = small_cohortData$variable

small_carrierData <- gather(small_sampleData, key="biomarker", value="value", -promis_id)

small_carrierSummaryData<-data.frame(variable=character(), mean=numeric(0), sd=numeric(0), N=numeric(0))

for(i in 1:nrow(small_cohortData))
{
        bio=new_bios[i]
        to_add=data.frame(variable=bio, mean=mean(small_carrierData$value[small_carrierData$biomarker==bio], na.rm=TRUE), sd=sd(small_carrierData$value[small_carrierData$biomarker==bio], na.rm=TRUE), N=sum(!is.na(small_carrierData$value[small_carrierData$biomarker==bio])))
        small_carrierSummaryData=rbind(small_carrierSummaryData, to_add)
}

testData<-data.frame(variable=character(), t_stat=numeric(0), df=numeric(0), p=numeric(0))

for(i in 1:nrow(small_cohortData))
{
        bio=new_bios[i]
        if (small_carrierSummaryData$N[small_carrierSummaryData$variable==bio]>1)
        {
                noncarrier_dist = rnorm(small_cohortData$N[small_cohortData$variable==bio])
                noncarrier_x = scale(noncarrier_dist)*small_cohortData$sd[small_cohortData$variable==bio]+small_cohortData$mean[small_cohortData$variable==bio]
                carrier_dist = rnorm(small_carrierSummaryData$N[small_carrierSummaryData$variable==bio])
                carrier_x = scale(carrier_dist)*small_carrierSummaryData$sd[small_carrierSummaryData$variable==bio]+small_carrierSummaryData$mean[small_carrierSummaryData$variable==bio]
                to_add=data.frame(variable=bio, t_stat=t.test(carrier_x,noncarrier_x)$statistic, df=t.test(carrier_x,noncarrier_x)$parameter, p=t.test(carrier_x,noncarrier_x)$p.value)
                testData=rbind(testData, to_add)
        }
}

for(i in 1:nrow(cohortData))
{
        bio=biomarkers[i]
        if (carrierSummaryData$N[carrierSummaryData$variable==bio]>1)
        {
                noncarrier_dist = rnorm(cohortData$N[cohortData$variable==bio])
                noncarrier_x = scale(noncarrier_dist)*cohortData$sd[cohortData$variable==bio]+cohortData$mean[cohortData$variable==bio]
                carrier_dist = rnorm(carrierSummaryData$N[carrierSummaryData$variable==bio])
                carrier_x = scale(carrier_dist)*carrierSummaryData$sd[carrierSummaryData$variable==bio]+carrierSummaryData$mean[carrierSummaryData$variable==bio]
                to_add=data.frame(variable=bio, t_stat=t.test(carrier_x,noncarrier_x)$statistic, df=t.test(carrier_x,noncarrier_x)$parameter, p=t.test(carrier_x,noncarrier_x)$p.value)
                testData=rbind(testData, to_add)
        }
}
