rm(list=ls())
library(lmerTest)
MYdata = read.csv("input_file.csv", sep=",", header=T)

MYdata$frequent_exposure_pet <- relevel(MYdata$frequent_exposure_pet, ref = "neither")
MYdata$Feeding <- relevel(MYdata$Feeding, ref = "combination")


even_center <- scale(MYdata$Evenness)[,1]
MYdata <- cbind(MYdata,even_center)
univariate <- c("Age","LGG_abund","LGG_level_1to6","Gender","Feeding", "PD", "mom_antibiotics_during_delivery", "baby_antibio_rule_out_sepsis", "even_center","Richness","child_in_house_nr + siblings","roaches_in_past","currently_indoor_pets", "frequent_exposure_pet", "viral_illness_since_last_interview")


write.table(as.matrix(t(c("Variable","Coeff","p_value"))),file="lme_results.txt",sep="\t",append = TRUE,col.names=FALSE,row.names=FALSE)
for (i in univariate){
	form <- as.formula(paste("community_score ~", i, "+ (1|Participant)"))
	data_ex <- na.exclude(MYdata)
	m1 <- lmer(form,data_ex)
	m1_data <- as.data.frame(summary(m1)$coefficients)
	for (var in row.names(m1_data)){
		vect <- c()
		if (var != "(Intercept)"){
			est <- m1_data[var,"Estimate"]
			pval <- m1_data[var,"Pr(>|t|)"]
			vect <- append(vect,var)
			vect <- append(vect,est)
			vect <- append(vect,pval)
			write.table(as.matrix(t(vect)),file="lme_results.txt",sep="\t",append = TRUE,col.names=FALSE,row.names=FALSE)
			}}
}
