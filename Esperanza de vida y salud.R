
library(wbstats)
library(readr)
library(dplyr)
library(naniar)
library(ggplot2)
library(MPV)
library(ggpubr)
library(plotly)
library(modelsummary)
library(lmtest)
library(MASS)
library(orcutt)
library(car)
library(readxl)
library(dplyr)
library(reticulate)
require(devtools)

#Ubicar python en el pc:
use_python("C:/Users/darra/anaconda3")

#Ubicar ambiente de conda:
use_condaenv("stats_pro") 

#Importar librerías de python en R

sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')

#datos de api
df <- wb_data(country = "countries_only", indicator = c("SI.POV.DDAY","SH.DYN.AIDS.ZS","SE.PRM.CMPT.ZS","NY.GDP.PCAP.PP.CD",
         "SE.TER.ENRR", "SE.PRM.ENRR","SP.ADO.TFRT","SH.DYN.NMRT","SN.ITK.DEFC.ZS","SM.POP.REFG.OR","SI.POV.GINI", 
         "SP.DYN.LE00.IN","SP.POP.TOTL") , start_date = 2000, end_date = 2019)


#Selección

ds <- filter(df, date == "2019")




ds1 <- ds[c("country", "SH.DYN.AIDS.ZS","NY.GDP.PCAP.PP.CD",
                                            "SE.PRM.ENRR","SP.ADO.TFRT","SH.DYN.NMRT","SN.ITK.DEFC.ZS","SM.POP.REFG.OR", 
                                           "SP.DYN.LE00.IN","SP.POP.TOTL")]


#Depuración
colnames(ds1) <- c("Pais","VIHprev", "GDP", "primaria_enrrol","adolecent_fert","neonat_Mortal", "desnutricion",  "refugiados"  ,"lifeexp","POP")

ds2 <- na.omit(ds1)

#Diagnostico
pairs(subset(ds2, select = -Pais))#sin nulos
pairs(subset(ds1, select = -Pais))# con nulos



###################################################################



#Crear nueva variable
ds3 <- subset(ds2, select = -Pais)

#Graficar expectativa de vida frente a producto interno bruto

#jointplot chart

ax = sns$jointplot(data=ds3, x="GDP", y="lifeexp", kind="scatter")
plt$show()
plt$clf()

#graficos
plt$clf()


plt$scatter(ds2["GDP"], ds2["lifeexp"], 
            c = ds2$POP,
            s=(ds2["POP"]*0.000001)
)

#nombres en los puntos (seleccionados)
plt$text(x=ds2["GDP"][ds2["Pais"]=='India']+5000,
         y=ds2["lifeexp"][ds2["Pais"]=='India'],
         s="India",
         fontdict=dict(color="black",size=10))

plt$text(x=ds2["GDP"][ds2["Pais"]=='United States']+5000,
         y=ds2["lifeexp"][ds2["Pais"]=='United States']+0.3,
         s="USA",
         fontdict=dict(color="black",size=10))

plt$ext(x=ds2["GDP"][ds2["Pais"]=='Brazil'],
         y=ds2["lifeexp"][ds2["Pais"]=='Brazil'],
         s="Brasil",
         fontdict=dict(color="black",size=10))

plt$text(x=ds2["GDP"][ds2["Pais"]=='Luxembourg']-17000,
         y=ds2["lifeexp"][ds2["Pais"]=='Luxembourg']+1,
         s="Luxemburgo",
         fontdict=dict(color="black",size=10))

plt$text(x=ds2["GDP"][ds2["Pais"]=='Chad']+1000,
         y=ds2["lifeexp"][ds2["Pais"]=='Chad']-0.5,
         s="Chad",
         fontdict=dict(color="black",size=10))

#labels y barra de color
plt$colorbar(label="Población")
plt$xlabel("PIB Per Cápita, dólares estadounidenses ajustados a PPA")
plt$ylabel("Esperanza de vida")
plt$show()


#diagrama de cajas expectativa de vida
plt$clf()
sns$boxplot(data=ds3,y="lifeexp")
sns$swarmplot(data=ds3,y="lifeexp", palette='dark:0')
plt$ylabel("Esperanza de vida")
plt$show()

#diagrama de cajas producto interno bruto
plt$clf()
sns$boxplot(data=ds3,y="GDP")
sns$swarmplot(data=ds3,y="GDP", palette='dark:0')
plt$ylabel("GDP PPA")
plt$show()


#regresion linea simple:


mod <- lm(lifeexp ~ GDP, data = ds3)
summary(mod)

#Modelo y ~ x
y <- c(ds3$lifeexp)

x <- c(ds3$GDP)

dd <- data.frame(cbind(y,x))

ggplot(dd,
       aes(x,y)) + 
  geom_point(shape = 1) +  stat_smooth(method = "lm", formula = y~ x , col = "blue") + theme_classic() +
  xlab("PIB per cápita ajustado a PPA") + ylab("Esperanza de vida")


#Modelo y ~ log(x)

y <- c(ds3$lifeexp)

x <- c(ds3$GDP)

dd <- data.frame(cbind(y,x))

ggplot(dd,
       aes(x,y)) + 
  geom_point(shape = 1) +  stat_smooth(method = "lm", formula = y~ log(x) , col = "blue") + theme_classic() +
  xlab("PIB per cápita ajustado a PPA") + ylab("Esperanza de vida")







#Transformaremos los datos de PIB per cápita a escala logaritmica porque es facil interpretarlos como porcentajes..

ds3$GDPLOG <- log10(ds3$GDP)

#Modelo 1 
mod1 <- lm(lifeexp ~ GDPLOG, data =ds3)
  summary(mod1)

y <- c(ds3$lifeexp)
x <- c(ds3$GDPLOG)
dd <- data.frame(cbind(y,x))

ggplot(dd,
       aes(x,y)) + 
  geom_point(shape = 1) +  stat_smooth(method = "lm", formula = y~ x , col = "blue") + theme_classic() +
  xlab("Logaritmo del PIB per cápita ajustado a PPA") + ylab("Esperanza de vida")

###############################################################################################
#INCLUYENDO OTRAS VARIABLES
########################################################################################

#vih prevalence
plt$clf()
sns$boxplot(data=ds3,y="VIHprev")
sns$swarmplot(data=ds3,y="VIHprev", palette='dark:0')
plt$show()

# vih prevalence violin plot
plt$clf()
sns$violinplot(data=ds3,y="VIHprev")
plt$show()

#adolescent fertility
plt$clf()
sns$boxplot(data=ds3,y="adolecent_fert")
sns$swarmplot(data=ds3,y="adolecent_fert", palette='dark:0')
plt$show()

#Neonatal mortality
plt$clf()
sns$boxplot(data=ds3,y="neonat_Mortal")
sns$swarmplot(data=ds3,y="neonat_Mortal", palette='dark:0')
plt$show()















#MODELO CON TODOS LOS DATOS::

ds3 <- subset(ds3, select = -c(GDP))


reg <- lm(lifeexp ~ ., data = ds3)
summary(reg)


#Paso 3: outliers

#Veremos si hay datos atípicos e influyentes: 

#Resumen de análisis
par(mfrow=c(1,1))
outlierTest(reg) #Hay dato atípico
plot(reg, which=4,main="",lwd=3,col="red")#Distancias de cook
plot(reg, which = 5)#Distancias de cook
influencePlot(reg)
summary(influence.measures(reg))

#eliminacion
ds3 <- ds3 %>%  filter(!row_number() %in% c(1,37))


#####################################################################################################


#PROBAR MODELO CON VARIABLES SIGNIFICATIVAS Y SIN LOS DATOS MÁS INFLUYENTES

reg2 <- lm(lifeexp ~ VIHprev + GDPLOG+adolecent_fert +neonat_Mortal, data = ds3)
summary(reg2)



## Paso 1: Prueba de normalidad de los residuales
par(mfrow = c(1, 1))
qqnorm(reg2$residuals)
qqline(reg2$residuals,col="red")
shapiro.test(reg2$residuals) # Residuos normalmente distribuidos


  # Paso 2: Prueba de homocedasticidad


ggplot(data = ds3, aes(reg2$fitted.values, reg2$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()


bptest(reg2)


  ###################################################################################


# Paso 3: Independencia de los errores

plot(reg2$residuals)#El comportamiento de los residuos parece correlacionado 
acf(reg2$residuals)
pacf(reg2$residuals)
dwtest(reg2)
bgtest(reg2)


#Paso 4: Multicolinealidad

vif(reg2) # No multicolinealidad


#Predicciones:
predict(reg2, data.frame(VIHprev = max(ds3$VIHprev), GDPLOG = min(ds3$GDPLOG), 
                         adolecent_fert= max(ds3$adolecent_fert), neonat_Mortal = max(ds3$neonat_Mortal) ))










