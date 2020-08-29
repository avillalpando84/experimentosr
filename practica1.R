# EDUCACIÓN Y DEMOCRACIA
## Ejercicio de comparación entre el Índice de Educación de la ONU
## y la variable Voice and Accountability de la Unidad de Inteligencia de
## The Economist
## Panel: 166 países, observaciones 2008 - 2018

library(tidyverse)
library(pacman)
library("dplyr")
library("readxl")
library ("readr")
library(lmtest)

setwd("~/Mis Documentos/CIDE DPP/3 Tercer semestre/3 Econometría Aplicada/Práctica")
DI_VA <- read_excel("EIU2.xlsx", col_names = TRUE)
DI_VA <- gather(DI_VA, year, DI, c(3:13), factor_key=FALSE)
EI <- read.csv("Education Index.csv", header = T)
EI <- dplyr::transmute(EI, Country, Code, "2008" = X2008, "2009" = X2009,
                           "2010" = X2010, "2011" = X2011, "2012" = X2012, "2013" = X2013,
                           "2014" = X2014, "2015" = X2015, "2016" = X2016,
                       "2017" = X2017, "2018" = X2018)
EI$'2008' <- as.character(EI$'2008')
EI$'2009' <- as.character(EI$'2009')
EI$'2008' <- as.numeric(EI$'2008')
EI$'2009' <- as.numeric(EI$'2009')
EI <- gather(EI, year, EI, c(3:13), factor_key=FALSE)
DI_VA <- as.data.frame(DI_VA)
EI <- as.data.frame(EI)
DI_EI <- merge(DI_VA, EI, by=c("Code", "year"))
DI_EI <- dplyr::transmute(DI_EI, "Pais" = Country.x, "ISO" = Code, "Anio" = year, "Ind_democracia_EIU" = DI,
                          "Ind_educacion_UNDP" = EI)
str(DI_EI)
DI_EI$Pais <- as.factor(DI_EI$Pais)
DI_EI$ISO <- as.factor(DI_EI$ISO)
DI_EI$Anio <- as.factor(DI_EI$Anio)

DI_EI2 <- dplyr::transmute(DI_EI, Ind_democracia_EIU, Ind_educacion_UNDP)

pairs.panels(DI_EI2[,], method = "pearson", hist.col = "#00AFBB", density = TRUE,
             ellipses = TRUE, main="Correlación (Pearson) entre Índice de Ecucación (UNDP) 
             e Índice de Democracia (EIU) de 2008 a 2018. Muestra de 166 países.")

grangertest(Ind_educacion_UNDP ~ Ind_democracia_EIU, order = 1, data = DI_EI)
grangertest(Ind_democracia_EIU ~ Ind_educacion_UNDP, order = 1, data = DI_EI)
