rm(list=ls())
require(pacman)
p_load(tidyverse, rio, skimr, janitor, ggplot2, haven, dplyr, stargazer,broom,xlsx) 


#Bases de datos de elecciones a presidente y alcalde 
elec_pres_1986 <- read_dta("1986_presidencia.dta")
elec_pres_1990 <- read_dta("1990_presidencia.dta")
elec_pres_1994 <- read_dta("1994_Presidencia_Segunda_Vuelta.dta")
elec_pres_1998 <- read_dta("1998_Presidencia_Segunda_Vuelta.dta")
elec_pres_2002 <- read_dta("2002_Presidencia.dta")
elec_pres_2006 <- read_dta("2002_Presidencia.dta")
elec_pres_2010 <- read_dta("2010_Presidencia_Primera_Vuelta.dta")
elec_pres_2014 <- read_dta("2014_Presidencia_Segunda_Vuelta.dta")
elec_pres_2018 <- read_dta("2018_Presidencia_Primera_Vuelta.dta")
elec_alca_1988 <- read_dta("1988_alcaldia.dta")
elec_alca_1992 <- read_dta("1992_alcaldia.dta")
elec_alca_1997 <- read_dta("1997_alcaldia.dta")
elec_alca_2000 <- read_dta("2000_alcaldia.dta")
elec_alca_2003 <- read_dta("2003_alcaldia.dta")
elec_alca_2007 <- read_dta("2007_alcaldia.dta")
elec_alca_2011 <- read_dta("2011_alcaldia.dta")
elec_alca_2015 <- read_dta("2015_alcaldia.dta")
elec_alca_2019 <- read_dta("2019_alcaldia.dta")


#Bases de datos de población 
poblacion_1985_1994 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1985-1994.xlsx")
poblacion_1995_2004 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1995-2004.xlsx")
poblacion_2005_2017 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")
poblacion_2018_2026 <- read_excel("anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")


#Bases de datos violencia
acciones_belicas <- read_excel("CasosAB_202206.xlsx")
asesinatos_selectivos <- read_excel("CasosAS_202206.xlsx")
danos_bienes_civiles <- read_excel("CasosDB_202206.xlsx")
desaparicion_forzada <- read_excel("CasosDF_202206.xlsx")
masacres <- read_excel("CasosMA_202206.xlsx")
minas <- read_excel("CasosMI_202206.xlsx")
reclutamiento_menores <- read_excel("CasosRU_202206.xlsx")
secuestros <- read_excel("CasosSE_202206.xlsx")
violencia_sexual <- read_excel("CasosVS_202206.xlsx")



#Limpieza bases de datos presidencia y alcalde





#Limpieza vases de datos de población 


#Calcular poblacion mayor de edad para 1985 - 1994
poblacion_1985_1994 <- mutate(.data = poblacion_1985_1994, total_mayores18 = Total_18 + Total_19 + 
                               Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                               Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                               Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                               Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                               Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                               Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                               Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                               Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                               Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                               Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                               Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                               Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                               Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                               Total_98 + Total_99 + Total_100_y_más
                               )


poblacion_1985_1994 <- subset(poblacion_1985_1994$ÁREA_GEOGRÁFICA == Total)
poblacion_1985_1994[poblacion_1985_1994$ÁREA_GEOGRÁFICA == Total, ]   

#Limpieza bases de datos violencia









