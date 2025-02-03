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
path="D:/Butterfly article correction/reanalisis" # set in the working directory
setwd(path)
archivo="./segunda_version.xlsx" # call the data used
data<- read_excel(archivo)

# variables management --------------------------------------------
# random effect ------------------------------------------------
data$random <-paste(data$Tramp,data$Height, sep="_") # with this name we create the random effect column that we are going to use
data$random <- as.factor(data$random) # the union of trampo and Altura
data$Tramp<- as.factor(data$Tramp) # the union of only tramop
# Habitat or forest
data$`Habitat`<-data$`Forest`
data$Habitat<- ordered(data$Habitat, levels=c("CCR", "PCR", "SLR"))
contrasts(data$Habitat)<-"contr.poly"
# Height of the forest
data$Height<- ordered(data$Height, levels=c("L", "M", "H"))
contrasts(data$Height)<-"contr.poly" # if this is ordered we have to use the contrast poly

# cLiamte in two versions
# second version grouping the sacle in four categories 
data$Temp<- data$Precipitation
data$Temp[data$Temp == 59] <- "dry"
data$Temp[data$Temp == 49] <- "dry"
data$Temp[data$Temp == 68] <- "dry"
data$Temp[data$Temp == 126] <- "dryTrainy"
data$Temp[data$Temp == 233] <- "dryTrainy"
data$Temp[data$Temp == 269] <- "dryTrainy"
data$Temp[data$Temp == 333] <- "rainy"
data$Temp[data$Temp == 373] <- "rainy"
data$Temp[data$Temp == 370] <- "rainy"
data$Temp[data$Temp == 306] <- "rainyTdry"
data$Temp[data$Temp == 177] <- "rainyTdry"
data$Temp[data$Temp == 96] <- "rainyTdry"
data$Temp<- as.factor(data$Temp)
contrasts(data$Temp)<-"contr.poly"

# considering a unique data frame scaled
data$Precipitation2<- scale(data$Precipitation)

# Run the glmer with the data  ----------------------------------------
# for the richness
modelo<-glmer(Richness~Precipitation2*Habitat*Height+ (1 | random), 
               data=data, family="poisson")
dispersion <- sum(resid(modelo, type = "pearson")^2) / df.residual(modelo) #12.74606 (overdispersed)

modeloB<-glmer( Abundance~Precipitation2*Habitat*Height+ (1 | random), 
              data=data, family="poisson")
dispersionB <- sum(resid(modeloB, type = "pearson")^2) / df.residual(modeloB) #12.74606 (overdispersed)

# prove different alternatives of the glmer to analyze of the inclusion of interaction offer a better adjusment
# for the analysis with the richness
modelo2 <- glmmTMB(Richness~Precipitation2*Habitat*Height+ (1 | random), 
                 family = nbinom2(), data=data)
modelo3 <-glmmTMB(Richness~Temp*Habitat*Height+ (1 | random), 
                 family = nbinom2(), data=data) #### this is the better option

modelo4 <-glmmTMB(Richness~Temp+Habitat+Height+ (1 | random), 
                  family = nbinom2(), data=data)
modelo5 <- glmmTMB(Richness~Precipitation2+Habitat+Height+ 
                     (1 | random), 
                   family = nbinom2(), data=data)

modelo6 <-glmmTMB(Richness~Temp+Habitat+Height+
                    Habitat:Height+ (1 | random), 
                  family = nbinom2(), data=data)
modelo7 <- glmmTMB(Richness~Precipitation2+Habitat+Height+
                     Habitat:Height+ (1 | random), 
                   family = nbinom2(), data=data)

print(c(AIC(modelo), AIC(modelo2), AIC(modelo3), AIC(modelo4),AIC(modelo5),
        AIC(modelo6), AIC(modelo7)))

# for the analysis with the abundance
modelo2B <- glmmTMB(Abundance~Precipitation2*Habitat*Height+ (1 | random), 
                   family = nbinom2(), data=data)
modelo3B <-glmmTMB(Abundance~Temp*Habitat*Height+ (1 | random), 
                  family = nbinom2(), data=data) #### this is the better option

modelo4B <-glmmTMB(Abundance~Temp+Habitat+Height+ (1 | random), 
                  family = nbinom2(), data=data)
modelo5B <- glmmTMB(Abundance~Precipitation2+Habitat+Height+ 
                     (1 | random), 
                   family = nbinom2(), data=data)

modelo6B <-glmmTMB(Abundance~Temp+Habitat+Height+
                    Habitat:Height+ (1 | random), 
                  family = nbinom2(), data=data)
modelo7B <- glmmTMB(Abundance~Precipitation2+Habitat+Height+
                     Habitat:Height+ (1 | random), 
                   family = nbinom2(), data=data)


print(c(AIC(modeloB), AIC(modelo2B), AIC(modelo3B), AIC(modelo4B),AIC(modelo5B),
        AIC(modelo6B), AIC(modelo7B)))

# realize the anova of the model selected ----------------------------------------
result1=Anova(modelo3, type="3")
result2=Anova(modelo3B, type="3")

# plot with emmeans and analysis  ---------------------------------------------------
# For the richness
svg(file="1 Interaction of Temp vs Height on Temp Richness.svg",6,5)
emmip(modelo3, Temp~Height|Habitat) ### Interaction of Temp vs Height on Temp
dev.off()

svg(file="2 Interaction of Temp vs Height on Heigh Richness.svg",6,5)
emmip(modelo3, Height~Temp|Habitat) ### Interaction of Temp vs Height on Height
dev.off()

svg(file="3 Interaction of Temp vs Habitat on temp Richness.svg",6,5)
emmip(modelo3, Temp~Habitat|Height) ### Interaction of Temp vs Habitat on Temp
dev.off()

svg(file="4 Interaction of Temp vs Habitat on Habitat Richness.svg",6,5)
emmip(modelo3, Habitat~Temp|Height) ### Interaction of Temp vs Habitat on Habitat
dev.off()

svg(file="5 Interaction of Habitat vs Height on Height Richness.svg",6,5)
emmip(modelo3, Habitat~Height|Temp) ### Interaction of Temp vs Height on Height
dev.off()

svg(file="6 Interaction of Habitat vs Height on Habitat Richness.svg",6,5)
emmip(modelo3, Height~Habitat|Temp) ### Interaction of Temp vs Height on Height
dev.off()

# on the abundance ------------------------------------------------------
svg(file="1 Interaction of Temp vs Height on Temp Abundace.svg",6,5)
emmip(modelo3B, Temp~Height|Habitat) ### Interaction of Temp vs Height on Temp
dev.off()

svg(file="2 Interaction of Temp vs Height on Heigh Abundance.svg",6,5)
emmip(modelo3B, Height~Temp|Habitat) ### Interaction of Temp vs Height on Height
dev.off()

svg(file="3 Interaction of Temp vs Habitat on temp Abundance.svg",6,5)
emmip(modelo3B, Temp~Habitat|Height) ### Interaction of Temp vs Habitat on Temp
dev.off()

svg(file="4 Interaction of Temp vs Habitat on Habitat Abundance.svg",6,5)
emmip(modelo3B, Habitat~Temp|Height) ### Interaction of Temp vs Habitat on Habitat
dev.off()

svg(file="5 Interaction of Habitat vs Height on Height Abundance.svg",6,5)
emmip(modelo3B, Habitat~Height|Temp) ### Interaction of Temp vs Height on Height
dev.off()

svg(file="6 Interaction of Habitat vs Height on Habitat Abundance.svg",6,5)
emmip(modelo3B, Height~Habitat|Temp) ### Interaction of Temp vs Height on Height
dev.off()

# making the contrast to the pairwise comparisons of the analysis
modelo_enm<- emmeans (modelo3, ~Temp*Height*Habitat)
Contraste<- contrast(modelo_enm, "pairwise", simple = "each", combine = TRUE, adjust =
           "mvt")
write.csv(Contraste, "contraste_emmeans_richness.csv")

modelo_enmb<- emmeans (modelo3B, ~Temp*Height*Habitat)
Contrasteb<- contrast(modelo_enmb, "pairwise", simple = "each", combine = TRUE, adjust =
                       "mvt")
write.csv(Contrasteb, "contraste_emmeans_abundance.csv")
