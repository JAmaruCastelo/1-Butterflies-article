# Libraries ---------------------------------------------------------------
library(lme4)
library(car)
library(readxl)
library(lattice)
library(nortest)
library (MASS)
library ("effects")
library(glmmTMB)
library(dplyr)
library(Matrix)
library(MuMIn)
library(emmeans)


# open and set the working directory ------------------------------
path="D:/0. Artculos y congresos/Butterfly article correction (Revision)" # set in the working directory
setwd(path)
archivo="./Online information 2C.xlsx" # call the data used # this is de data after correcting all the issues
data<- read_excel(archivo)

length(unique(data$Date))
data$


# variables management --------------------------------------------
# random effect ------------------------------------------------
data$random <-paste(data$Trap,data$Stratification, sep="_") # with this name we create the random effect column that we are going to use
data$random <- as.factor(data$random) # the union of trampo and Altura
data$Trap<- as.factor(data$Trap) # the union of only tramop

# Habitat or forest
data$`Habitat`<-data$`Disturbance gradient`
data$Habitat<- ordered(data$Habitat, levels=c("CCR", "PCR", "SLR"))
contrasts(data$Habitat)<-"contr.poly"
# Height of the forest
data$Height<- ordered(data$Stratification, levels=c("L", "M", "H"))
contrasts(data$Height)<-"contr.poly" # if this is ordered we have to use the contrast poly

# We grouped each month in a season caracteristic 
data$Temp<- data$Month
data$Temp[data$Temp == 6] <- "dry"
data$Temp[data$Temp == 7] <- "dry"
data$Temp[data$Temp == 8] <- "dry"
data$Temp[data$Temp == 9] <- "dryTrainy"
data$Temp[data$Temp == 10] <- "dryTrainy"
data$Temp[data$Temp == 11] <- "dryTrainy"
data$Temp[data$Temp == 12] <- "rainy"
data$Temp[data$Temp == 1] <- "rainy"
data$Temp[data$Temp == 2] <- "rainy"
data$Temp[data$Temp == 3] <- "rainyTdry"
data$Temp[data$Temp == 4] <- "rainyTdry"
data$Temp[data$Temp == 5] <- "rainyTdry"
data$Temp<- as.factor(data$Temp)
#contrasts(data$Temp)<-"contr.poly"
contrasts(data$Temp)<-"contr.sum"

# sacamos un pequeño summary por cada uno de los valores necesarios a analizar------
sd(data$Richness)
R=data %>%
  group_by(Height) %>%
  summarise(Richness= mean(Richness),sdRi=sd(Richness), Ab=mean(Abundance), sdA=sd(Abundance))
R
# Stratification,Temp
write.csv(R, paste(path,"/summary_val.csv", sep=""))

sd(data[data$Habitat=="SLR",]$Richness)
# Run the glmer with the data  ----------------------------------------
# for the richness
modelo<-glmer(Richness~Temp*Habitat*Height+(1|Trap)+(1|random), 
               data=data, family="poisson")
dispersion <- sum(resid(modelo, type = "pearson")^2) / df.residual(modelo) #12.74606 (overdispersed)
dispersion

modeloB<-glmer(Abundance~Temp*Habitat*Height+(1|Trap)+(1|random), 
              data=data, family="poisson")
dispersionB <- sum(resid(modeloB, type = "pearson")^2) / df.residual(modeloB) #12.74606 (overdispersed)
dispersionB

# prove different alternatives of the glmer to analyze of the inclusion of interaction offer a better adjusment
# for the analysis with the richness
modelo3 <-glmmTMB(Richness~Temp*Habitat*Height+(1|Trap)+(1|random), 
                 family = nbinom2(), data=data) #### this is the better option
modelo4 <-glmmTMB(Richness~Temp+Habitat+Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=data)
modelo6 <-glmmTMB(Richness~Temp+Habitat+Height+Habitat:Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=data)


print(paste(AIC(modelo3), AIC(modelo4), AIC(modelo6)))

# for the analysis with the abundance
modelo3B <-glmmTMB(Abundance~Temp*Habitat*Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=data) 
modelo4B <-glmmTMB(Abundance~Temp+Habitat+Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=data)
modelo6B <-glmmTMB(Abundance~Temp+Habitat+Height+Habitat:Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=data)

print(paste(AIC(modelo3B), AIC(modelo4B),AIC(modelo6B)))

### verifico los supuestos del analisis usando DHARMA--------------
library(DHARMa)

svg(file="Residuals4_richness.svg",7.5,4.5)
resR=simulateResiduals(modelo4)
plot(resR) 
dev.off()

svg(file="Residuals4_Abundance.svg",7.5,4.5)
resR=simulateResiduals(modelo4B)
plot(resR) 
dev.off()

# realize the anova of the model selected ----------------------------------------
result1=Anova(modelo4, type="3")
result2=Anova(modelo4B, type="3")

# plot with emmeans and analysis  ---------------------------------------------------

# For the richness
svg(file="Richness_variables.svg",7,3.5)
emm <- emmeans(modelo4, ~ Habitat*Temp | Height,type = "response")
emm_df <- as.data.frame(emm)
ggplot(emm_df, aes(x = Temp, y = response, color = Habitat, group = Habitat)) +
  geom_line() +
  geom_point(size=1.5) +
  geom_crossbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  facet_wrap(~ Height) +
  theme_minimal() +
  labs(y = "Richness", x = "Seasonality")
dev.off()

# on the abundance ------------------------------------------------------
svg(file="Abundance_variables.svg",7,3.5)
emm <- emmeans(modelo4B, ~ Habitat*Temp | Height,type = "response")
emm_df <- as.data.frame(emm)
ggplot(emm_df, aes(x = Temp, y = response, color = Habitat, group = Habitat)) +
  geom_line() +
  geom_point(size=1.5) +
  geom_crossbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  facet_wrap(~ Height) +
  theme_minimal() +
  labs(y = "Abundance", x = "Seasonality") ### Interaction of Temp vs Habitat on Habitat
dev.off()



# Making the contrast and the pairwise comparisons ----------------------------------
Comp_Temp<-emmeans(modelo4, pairwise ~ Temp, type = "response") 
Comp_Hab<-emmeans(modelo4, pairwise ~ Habitat, type = "response") 
Comp_Heig<-emmeans(modelo4, pairwise ~ Height, type = "response") 

richness_c=rbind(data.frame(Comp_Temp$contrasts),data.frame(Comp_Hab$contrasts),data.frame(Comp_Heig$contrasts))

write.csv(richness_c, "contraste_emmeans_richness.csv")

Comp_Temp<-emmeans(modelo4B, pairwise ~ Temp, type = "response") 
Comp_Hab<-emmeans(modelo4B, pairwise ~ Habitat, type = "response") 
Comp_Heig<-emmeans(modelo4B, pairwise ~ Height, type = "response") 

Abundance_c=rbind(data.frame(Comp_Temp$contrasts),data.frame(Comp_Hab$contrasts),data.frame(Comp_Heig$contrasts))

write.csv(Abundance_c, "contraste_emmeans_abundance.csv")
