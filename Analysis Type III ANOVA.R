
# Libraries ---------------------------------------------------------------
library(car)
library(readxl)
library(lattice)
library(nortest)
library (MASS)
library ("effects")

# open and set the working directory ------------------------------

path="D:/00022-butterflies reduction of species" # set in the working directory
setwd(path)

archivo="./Material suplementario 2.xlsx" # call the data used
data<- read_excel(archivo)

# add a variable for each group --------------------------------------
data$code<-data$code 


# Create exploratory diagrams ------------------------------------
histogram(~ Abundance|Height, data=data )
boxplot(Abundance~Height, data=data)
data$`Habitat`<-data$`Habitat/Forest`

# Modife the categorical variables to the ANOVA III ----------------
data$Habitat<- ordered(data$Habitat, levels=c("CCR", "PCR", "SLR"))
contrasts(data$Habitat)<-"contr.poly"

data$Height<- ordered(data$Height, levels=c("L", "M", "H"))
contrasts(data$Height)<-"contr.poly"

data$Temporade<- factor(data$Temporade)
contrasts(data$Temporade)<-"contr.sum"


# Run the lineal model without modification and graphic --------------------
windowsFonts("Helvetica" = windowsFont("Helvetica"))

modelo1<-lm(Richness~Temporade*Habitat*Height, data=data)
anova<-Anova(modelo1, type="III")
a<-lillie.test(resid(modelo1)) # see if they are normal
b<- bartlett.test(as.vector(resid(modelo1)), data$code) # see the variance homogeneity

# plot qq plot
svg(file="qqplot_riquezapp.svg",6,5)
qqnorm(resid(modelo1), pch=1, col="black", lwd=0.5, cex=0.2,family = "Helvetica", main="")
qqline(resid(modelo1), col="black", lty=2,family = "Helvetica")
text(-4,30,paste("D. ",round(a$statistic,3)), pos=4, cex=0.8,family = "Helvetica")
text(-4,28,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()

# plot residuals vs Fitted values
svg(file="residualvsfitted_riquezapp.svg",6,5)
plot(modelo1, which=1, caption="", cex=0.3,add.smooth = T, 
     pch=16, sub="")
text(1.5,32,paste("D. ",round(b$statistic,1)), pos=4, cex=0.8,family = "Helvetica")
text(1.5,30,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()

# variables transformation ---------------------------------------------
svg(file="boxcox_riqueza.svg",6,5)
b <- MASS::boxcox(lm(modelo1))
lambda <- b$x[which.max(b$y)]
dev.off()

data$bc_richness<-(data$Richness ^ lambda - 1) / lambda


# create the corrected model ---------------------------

modelo1<-lm(bc_richness~Temporade*Habitat*Height, data=data)
anova<-Anova(modelo1, type="III")
#writexl::write_xlsx(data.frame(anova), "riqueza_anovaIII.xlsx")


a<-lillie.test(resid(modelo1)) # see normality
b<- bartlett.test(resid(modelo1), data$code) # see homosedasticity

# plot qq plot
svg(file="qqplot_riquezafinal.svg",6,5)
qqnorm(resid(modelo1), pch=1, col="black", lwd=0.5, cex=0.2,family = "Helvetica", main="")
qqline(resid(modelo1), col="black", lty=2,family = "Helvetica")
text(-4,30,paste("D. ",round(a$statistic,3)), pos=4, cex=0.8,family = "Helvetica")
text(-4,28,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()

svg(file="boxcox_riqueza.svg",6,5)

# plot residuals vs Fitted values
svg(file="residualvsfitted_riquezafinal.svg",6,5)
plot(modelo1, which=1, caption="", cex=0.3,add.smooth = T, 
     pch=16, sub="")
text(1.5,32,paste("D. ",round(b$statistic,1)), pos=4, cex=0.8,family = "Helvetica")
text(1.5,30,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()


# Interaction  -----------------------------------------------------------

# Plot with lattice
efectos<-predictorEffects(modelo1, c("Habitat", "Height"))
plot(efectos, lines=list(multiline=TRUE),confint=list(style="bands") )

# Plot with interaction plot
svg(file="int_height_habitat.svg",6,5)
interaction.plot(data$Height, data$Habitat, data$bc_richness, type="o", 
                 legend=FALSE, xlab="Height", ylab="",
                 col=c("blue","black", "red"),fun="mean", 
                 pch=c(15,16,17), lwd=2, cex.lab=2, cex.axis=2,
                 family = "Helvetica",bty = "L")
dev.off()



svg(file="int_height_temporada.svg",6,5)
interaction.plot(data$Height, data$Temporade, data$bc_richness, type="o", 
                 legend=F, xlab="Height", ylab="",
                 col=c("green","purple", "yellow"),fun="mean", 
                 pch=c(15,16,17), lwd=2, cex.lab=2, cex.axis=2,
                 family = "Helvetica",bty = "L")
dev.off()

# Probe the termplot function to this analysis
termplot(modelo1, se = TRUE, partial.resid = TRUE) 


# ABUNDANCE ANALYSIS ------------------------------------------------------

# Run the lineal model without modification --------------------
windowsFonts("Helvetica" = windowsFont("Helvetica"))

modelo1<-lm(Abundance~Temporade*Habitat*Height, data=data)
anova<-Anova(modelo1, type="III")
a<-lillie.test(resid(modelo1)) # Normality
b<- bartlett.test(resid(modelo1), data$code) # Homosedasticity

# Plot qqplot 
svg(file="qqplot_abundanciapp.svg",6,5)
qqnorm(resid(modelo1), pch=1, col="black", lwd=0.5, cex=0.2,family = "Helvetica", main="")
qqline(resid(modelo1), col="black", lty=2,family = "Helvetica")
text(-4,60,paste("D. ",round(a$statistic,3)), pos=4, cex=0.8,family = "Helvetica")
text(-4,58,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()

# Plot residuals vs Fitted values
svg(file="residualvsfitted_abundanciapp.svg",6,5)
plot(modelo1, which=1, caption="", cex=0.3,add.smooth = T, 
     pch=16, sub="")
text(2,64,paste("D. ",round(b$statistic,1)), pos=4, cex=0.8,family = "Helvetica")
text(2,61,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()

# Variables transformation ---------------------------------------------
svg(file="residualvsfitted_riquezafinal.svg",6,5)

svg(file="boxcox_abundancia.svg",6,5)
b <- MASS::boxcox(lm(modelo1))
lambda <- b$x[which.max(b$y)]
dev.off()
data$bc_abundace<-(data$Abundance ^ lambda - 1) / lambda


# Created corrected model ---------------------------

modelo1<-lm(bc_abundace~Temporade*Habitat*Height, data=data)
anova<-Anova(modelo1, type="III")
#writexl::write_xlsx(data.frame(anova), "riqueza_anovaIII.xlsx")


a<-lillie.test(resid(modelo1)) # Normality
b<- bartlett.test(resid(modelo1), data$code) # homoscedasticity

# Plot qqplot 
svg(file="qqplot_abundanciafinal.svg",6,5)
qqnorm(resid(modelo1), pch=1, col="black", lwd=0.5, cex=0.2,family = "Helvetica", main="")
qqline(resid(modelo1), col="black", lty=2,family = "Helvetica")
text(-4,30,paste("D. ",round(a$statistic,3)), pos=4, cex=0.8,family = "Helvetica")
text(-4,28,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()



# Plot residuals vs Fitted values

svg(file="residualvsfitted_abundanciafinal.svg",6,5)
plot(modelo1, which=1, caption="", cex=0.3,add.smooth = T, 
     pch=16, sub="")
text(1.5,32,paste("D. ",round(b$statistic,1)), pos=4, cex=0.8,family = "Helvetica")
text(1.5,30,"P < 0.05 ", pos=4, cex=0.8,family = "Helvetica")
dev.off()


# Interactions -----------------------------------------------------------

# plot with lattice
efectos<-predictorEffects(modelo1, c("habitat", "height"))
plot(efectos, lines=list(multiline=TRUE),confint=list(style="bands") )

# Plot with interaction plot
svg(file="int_height_habitat_abund.svg",6,5)
interaction.plot(data$Height, data$Habitat, data$bc_abundace, type="o", 
                 legend=FALSE, xlab="Height", ylab="",
                 col=c("blue","black", "red"),fun="mean", 
                 pch=c(15,16,17), lwd=2, cex.lab=2,cex.axis=2,
                 family = "Helvetica",bty = "L")
dev.off()

svg(file="int_height_temporada_abund.svg",6,5)
interaction.plot(data$Height, data$Temporade, data$bc_abundace, type="o", 
                 legend=F, xlab="Height", ylab="",
                 col=c("green","purple", "yellow"),fun="mean", 
                 pch=c(15,16,17), lwd=2, cex.lab=2, cex.axis=2,
                 family = "Helvetica",bty = "L")
dev.off()


# probe With termplot
termplot(modelo1, se = TRUE, partial.resid = TRUE)

