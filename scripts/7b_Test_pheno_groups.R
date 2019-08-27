library(ggplot)

data = read.table("Grouped_phenotype_data.txt", header=TRUE, sep="\t") # output from 'Group_phenotypes.pl' script
data = subset(data, data$eur=='Y')

spread_data <- data %>%
	gather(key=variable, value=value, -sample, -genotype, -eur, -Sex, -Age_at_recruitment, -Age_at_death)

phen_list <- unique(spread_data$variable)

grouped_data<-data.frame(phenotype_group=character(), pLoF_affected=numeric(0), pLoF_unaffected=numeric(0), plof_prop=numeric(0), none_affected=numeric(0), none_unaffected=numeric(0), none_prop=numeric(0), fisher_p=numeric(0))

for (i in phen_list)
{
	dat<-spread_data[spread_data$variable==i,]
	af_plof=nrow(dat[dat$genotype=="pLoF" & dat$value=="Y",])
	un_plof=nrow(dat[dat$genotype=="pLoF" & dat$value=="N",])
	af_none=nrow(dat[dat$genotype=="None" & dat$value=="Y",])
	un_none=nrow(dat[dat$genotype=="None" & dat$value=="N",])

	mat=matrix(c(af_plof,un_plof,af_none,un_none),nrow=2)
	p2=fisher.test(mat)$p.value

	to_add=data.frame(phenotype_group=i, pLoF_affected=af_plof, pLoF_unaffected=un_plof, plof_prop=(af_plof/(un_plof+af_plof)) ,none_affected=af_none, none_unaffected=un_none, none_prop=(af_none/(un_none+af_none)), fisher_p=p2)
	grouped_data=rbind(grouped_data, to_add)
}
