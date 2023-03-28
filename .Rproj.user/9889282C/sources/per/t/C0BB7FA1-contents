rm(list=ls())
require(pacman)
p_load(tidyverse, rio, skimr, janitor, ggplot2, haven, dplyr, stargazer,broom,xlsx,readxl) 


#Bases de datos de elecciones a presidente y alcalde 
elec_pres_1986 <- read_dta("1986_presidencia.dta")
elec_pres_1990 <- read_dta("1990_presidencia.dta")
elec_pres_1994 <- read_dta("1994_Presidencia_Segunda_Vuelta.dta")
elec_pres_1998 <- read_dta("1998_Presidencia_Segunda_Vuelta.dta")
elec_pres_2002 <- read_dta("2002_Presidencia.dta")
elec_pres_2006 <- read_dta("2006_Presidencia.dta")
elec_pres_2010 <- read_dta("2010_Presidencia_Primera_Vuelta.dta")
elec_pres_2014 <- read_dta("2014_Presidencia_Segunda_Vuelta.dta")
elec_pres_2018 <- read_dta("2018_Presidencia_Primera_Vuelta.dta")


#Bases de datos de población 
poblacion_1985_1994 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1985-1994.xlsx")
poblacion_1995_2004 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1995-2004.xlsx")
poblacion_2005_2017 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")
poblacion_2018_2026 <- read_excel("anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")










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


poblacion_1985_1994 <-poblacion_1985_1994[poblacion_1985_1994$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_1985_1994 <- subset(poblacion_1985_1994, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18 ))
poblacion_1985_1994 <-subset(poblacion_1985_1994, AÑO == 1986 | AÑO == 1988 | AÑO == 1990 | AÑO == 1992 | AÑO == 1994)



poblacion_1995_2004 <- mutate(.data = poblacion_1995_2004, total_mayores18 = Total_18 + Total_19 + 
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


poblacion_1995_2004 <-poblacion_1995_2004[poblacion_1995_2004$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_1995_2004 <- subset(poblacion_1995_2004, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18 ))
poblacion_1995_2004 <-subset(poblacion_1995_2004, AÑO == 1997 | AÑO == 1998 | AÑO == 2000 | AÑO == 2002 | AÑO == 2003)






poblacion_2005_2017 <- mutate(.data = poblacion_2005_2017, total_mayores18 = Total_18 + Total_19 + 
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


poblacion_2005_2017 <-poblacion_2005_2017[poblacion_2005_2017$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_2005_2017 <- subset(poblacion_2005_2017, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18 ))
poblacion_2005_2017 <-subset(poblacion_2005_2017, AÑO == 2006 | AÑO == 2007 | AÑO == 2010 | AÑO == 2011 | AÑO == 2014 | AÑO == 2015)





poblacion_2018_2026 <- mutate(.data = poblacion_2018_2026, total_mayores18 = Total_18 + Total_19 + 
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


poblacion_2018_2026 <-poblacion_2018_2026[poblacion_2018_2026$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_2018_2026 <- subset(poblacion_2018_2026, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18 ))
poblacion_2018_2026 <-subset(poblacion_2018_2026, AÑO == 2018 | AÑO == 2019)



poblacion_1986_2018 <- rbind(poblacion_1985_1994, poblacion_1995_2004, poblacion_2005_2017, poblacion_2018_2026)
poblacion_1986_2018 = arrange(.data=poblacion_1986_2018 , DPMP)

rm(poblacion_1985_1994, poblacion_1995_2004, poblacion_2005_2017, poblacion_2018_2026)

poblacion_1986_2018$codmpio <- poblacion_1986_2018$DPMP
poblacion_1986_2018$ano <- poblacion_1986_2018$AÑO
poblacion_1986_2018$codmpio <- as.numeric(as.character(poblacion_1986_2018$codmpio))
poblacion_1986_2018 <- subset(poblacion_1986_2018, select = c(MPIO,codmpio, ano,total_mayores18 ))


#Limpieza bases de datos presidencia y alcaldías

elec_pres_1986 <- subset(elec_pres_1986, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1990 <- subset(elec_pres_1990, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1994 <- subset(elec_pres_1994, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1998 <- subset(elec_pres_1998, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2002 <- subset(elec_pres_2002, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2006 <- subset(elec_pres_2006, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2010 <- subset(elec_pres_2010, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2014 <- subset(elec_pres_2014, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2018 <- subset(elec_pres_2018, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))



total_votos_pres_1986 <- aggregate(votos ~ codmpio, data = elec_pres_1986, sum)
ano <- rep(1986,nrow(total_votos_pres_1986))
total_votos_pres_1986 <- cbind(total_votos_pres_1986, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1986))
total_votos_pres_1986<- cbind(total_votos_pres_1986, eleccion)
total_votos_pres_1986 <- subset(total_votos_pres_1986, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_1990 <- aggregate(votos ~ codmpio, data = elec_pres_1990, sum)
ano <- rep(1990,nrow(total_votos_pres_1990))
total_votos_pres_1990 <- cbind(total_votos_pres_1990, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1990))
total_votos_pres_1990<- cbind(total_votos_pres_1990, eleccion)
total_votos_pres_1990 <- subset(total_votos_pres_1990, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_1994 <- aggregate(votos ~ codmpio, data = elec_pres_1994, sum)
ano <- rep(1994,nrow(total_votos_pres_1994))
total_votos_pres_1994 <- cbind(total_votos_pres_1994, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1994))
total_votos_pres_1994<- cbind(total_votos_pres_1994, eleccion)
total_votos_pres_1994 <- subset(total_votos_pres_1994, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_1998 <- aggregate(votos ~ codmpio, data = elec_pres_1998, sum)
ano <- rep(1998,nrow(total_votos_pres_1998))
total_votos_pres_1998 <- cbind(total_votos_pres_1998, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1998))
total_votos_pres_1998<- cbind(total_votos_pres_1998, eleccion)
total_votos_pres_1998 <- subset(total_votos_pres_1998, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2002 <- aggregate(votos ~ codmpio, data = elec_pres_2002, sum)
ano <- rep(2002,nrow(total_votos_pres_2002))
total_votos_pres_2002 <- cbind(total_votos_pres_2002, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2002))
total_votos_pres_2002<- cbind(total_votos_pres_2002, eleccion)
total_votos_pres_2002 <- subset(total_votos_pres_2002, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2006 <- aggregate(votos ~ codmpio, data = elec_pres_2006, sum)
ano <- rep(2006,nrow(total_votos_pres_2006))
total_votos_pres_2006 <- cbind(total_votos_pres_2006, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2006))
total_votos_pres_2006<- cbind(total_votos_pres_2006, eleccion)
total_votos_pres_2006 <- subset(total_votos_pres_2006, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2010 <- aggregate(votos ~ codmpio, data = elec_pres_2010, sum)
ano <- rep(2010,nrow(total_votos_pres_2010))
total_votos_pres_2010 <- cbind(total_votos_pres_2010, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2010))
total_votos_pres_2010<- cbind(total_votos_pres_2010, eleccion)
total_votos_pres_2010 <- subset(total_votos_pres_2010, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2014 <- aggregate(votos ~ codmpio, data = elec_pres_2014, sum)
ano <- rep(2014,nrow(total_votos_pres_2014))
total_votos_pres_2014 <- cbind(total_votos_pres_2014, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2014))
total_votos_pres_2014<- cbind(total_votos_pres_2014, eleccion)
total_votos_pres_2014 <- subset(total_votos_pres_2014, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2018 <- aggregate(votos ~ codmpio, data = elec_pres_2018, sum)
ano <- rep(2018,nrow(total_votos_pres_2018))
total_votos_pres_2018 <- cbind(total_votos_pres_2018, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2018))
total_votos_pres_2018<- cbind(total_votos_pres_2018, eleccion)
total_votos_pres_2018 <- subset(total_votos_pres_2018, select = c(ano,codmpio,votos, eleccion))


rm( elec_pres_1986, elec_pres_1990, elec_pres_1994,elec_pres_1998, elec_pres_2002, elec_pres_2006,
    elec_pres_2010, elec_pres_2014, elec_pres_2018)

votos_totales <- rbind(total_votos_pres_1986, total_votos_pres_1990, total_votos_pres_1994, 
                       total_votos_pres_1998, total_votos_pres_2002, total_votos_pres_2006, 
                       total_votos_pres_2010, total_votos_pres_2014, total_votos_pres_2018)



votos_totales <- rbind(total_votos_pres_1986,  total_votos_pres_1990,
                       total_votos_pres_1994, 
                      total_votos_pres_1998, 
                       total_votos_pres_2002, 
                       total_votos_pres_2006, total_votos_pres_2010, 
                       total_votos_pres_2014,
                       total_votos_pres_2018 )


votos_totales <- arrange(.data=votos_totales , codmpio)

rm(total_votos_pres_1986, total_votos_pres_1990,
   total_votos_pres_1994, 
   total_votos_pres_1998, 
   total_votos_pres_2002, 
   total_votos_pres_2006, total_votos_pres_2010, 
  total_votos_pres_2014, 
   total_votos_pres_2018 )

poblacion_y_votos <- left_join(x=poblacion_1986_2018, y=votos_totales, by=c("codmpio","ano"))

rm(votos_totales, poblacion_1986_2018)

poblacion_y_votos <- mutate(.data = poblacion_y_votos, participacion = votos/total_mayores18)
poblacion_y_votos <- na.omit(poblacion_y_votos)

poblacion_y_votos <- subset(poblacion_y_votos, total_mayores18 != 0 &  votos != 0 & participacion < 1)

hist(poblacion_y_votos$participacion)






