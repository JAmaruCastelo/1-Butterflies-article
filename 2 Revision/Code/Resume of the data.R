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
archivo="./especies_seleccionadas.csv" # call the data used # this is de data after correcting all the issues
data<- read.csv(archivo)
length(unique(data$sp_name))
# mejoramos la cantidad de especies -------------------
R=data %>%
  group_by(sp_name,height) %>%
  summarise(N = n())

R2=R %>%
  group_by(height) %>%
  summarise(N = n())

write.csv(R, paste(path,"/resumen_per_species.csv",sep=""))


# corregimos la tabla para que cuenta la riqueza y la abundancia
R=data %>%
  group_by(reference, height, start_week) %>%
  summarise(richness = n_distinct(sp_name), Abundance=n())


R$rer = R$reference

R <- R %>% separate_wider_delim(rer, delim="-",
                               names = c("forest", "height2"))
# variables management --------------------------------------------
# random effect ------------------------------------------------
R$random <- as.factor(R$reference) # the union of tramp and vertical stratif
R$Trap<- as.factor(R$forest) # the union of only tramp

# Habitat or forest
R <- R %>%separate_wider_delim(forest,delim="B",
                       names = c("disturbance", "nn"))
R$Habitat<- ordered(R$disturbance, levels=c("D", "P", "S"))
contrasts(R$Habitat)<-"contr.poly"

# Height of the forest
R$Height<- ordered(R$height, levels=c("L", "M", "H"))
contrasts(R$Height)<-"contr.poly" # if this is ordered we have to use the contrast poly

# We grouped each month in a season caracteristic 
R <- R %>%
  separate_wider_delim(start_week,delim="-",
                       names = c("year", "month","day"))

R$Temp<- R$month
R$Temp[R$Temp == "06"] <- "dry"
R$Temp[R$Temp == "07"] <- "dry"
R$Temp[R$Temp == "08"] <- "dry"
R$Temp[R$Temp == "09"] <- "dryTrainy"
R$Temp[R$Temp == "10"] <- "dryTrainy"
R$Temp[R$Temp == "11"] <- "dryTrainy"
R$Temp[R$Temp == "12"] <- "rainy"
R$Temp[R$Temp == "01"] <- "rainy"
R$Temp[R$Temp == "02"] <- "rainy"
R$Temp[R$Temp == "03"] <- "rainyTdry"
R$Temp[R$Temp == "04"] <- "rainyTdry"
R$Temp[R$Temp == "05"] <- "rainyTdry"
R$Temp<- as.factor(R$Temp)
#contrasts(R$Temp)<-"contr.poly"
contrasts(R$Temp)<-"contr.sum"


# considering a unique data frame scaled
R$richness
R$Abundance

# Run the glmer with the data  ----------------------------------------
# for the richness
modelo<-glmer(richness~Temp*Habitat*Height+(1|Trap)+(1|random), 
               data=R, family="poisson")
dispersion <- sum(resid(modelo, type = "pearson")^2) / df.residual(modelo) 
dispersion  ### 1.3444444

modeloB<-glmer(Abundance~Temp*Habitat*Height+(1|Trap)+(1|random), 
              data=R, family="poisson")
dispersionB <- sum(resid(modeloB, type = "pearson")^2) / df.residual(modeloB) #12.74606 (overdispersed)
dispersionB   ### 1.7959

# Prove different alternatives of the glmer to analyze of the inclusion of interaction offer a better adjusment
# For the analysis with the richness
modelo3 <-glmmTMB(richness~Temp*Habitat*Height+(1|Trap)+(1|random), 
                 family = nbinom2(), data=R) #### this is the better option
modelo4 <-glmmTMB(richness~Temp+Habitat+Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=R)
modelo6 <-glmmTMB(richness~Temp+Habitat+Height+Habitat:Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=R)


print(paste(AIC(modelo3), AIC(modelo4), AIC(modelo6)))

# for the analysis with the abundance
modelo3B <-glmmTMB(Abundance~Temp*Habitat*Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=R) 
modelo4B <-glmmTMB(Abundance~Temp+Habitat+Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=R)
modelo6B <-glmmTMB(Abundance~Temp+Habitat+Height+Habitat:Height+(1|Trap)+(1|random), 
                  family = nbinom2(), data=R)

print(paste(AIC(modelo3B), AIC(modelo4B),AIC(modelo6B)))
# el primer modelo es el mejor

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
result1=Anova(modelo3, type="3")
result2=Anova(modelo3B, type="3")

# plot with emmeans and analysis  ---------------------------------------------------

# For the richness
library(ggplot2)
svg(file="Richness_variables.svg",7,3.5)
emm <- emmeans(modelo3, ~ Habitat*Temp | Height,type = "response")
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
emm <- emmeans(modelo3B, ~ Habitat*Temp | Height,type = "response")
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
