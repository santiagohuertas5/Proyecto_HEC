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

elec_alca_1988 <- subset(elec_alca_1988, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_1992 <- subset(elec_alca_1992, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_1997 <- subset(elec_alca_1997, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_2000 <- subset(elec_alca_2000, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_2003 <- subset(elec_alca_2003, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_2007 <- subset(elec_alca_2007, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_2011 <- subset(elec_alca_2011, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_2015 <- subset(elec_alca_2015, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_alca_2019 <- subset(elec_alca_2019, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))

elec_pres_1986 <- subset(elec_pres_1986, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1990 <- subset(elec_pres_1990, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1994 <- subset(elec_pres_1994, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1998 <- subset(elec_pres_1998, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2002 <- subset(elec_pres_2002, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2006 <- subset(elec_pres_2006, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2010 <- subset(elec_pres_2010, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2014 <- subset(elec_pres_2014, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2018 <- subset(elec_pres_2018, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))


total_votos_alca_1988<- aggregate(votos ~ codmpio, data = elec_alca_1988, sum)
ano <- rep(1988,nrow(total_votos_alca_1988))
total_votos_alca_1988 <- cbind(total_votos_alca_1988, ano)
total_votos_alca_1988 <- subset(total_votos_alca_1988, select = c(ano,codmpio,votos))


total_votos_alca_1992<- aggregate(votos ~ codmpio, data = elec_alca_1992, sum)
ano <- rep(1992,nrow(total_votos_alca_1992))
total_votos_alca_1992 <- cbind(total_votos_alca_1992, ano)
total_votos_alca_1992 <- subset(total_votos_alca_1992, select = c(ano,codmpio,votos))


total_votos_alca_1997<- aggregate(votos ~ codmpio, data = elec_alca_1997, sum)
ano <- rep(1997,nrow(total_votos_alca_1997))
total_votos_alca_1997 <- cbind(total_votos_alca_1997, ano)
total_votos_alca_1997 <- subset(total_votos_alca_1997, select = c(ano,codmpio,votos))


total_votos_alca_2000<- aggregate(votos ~ codmpio, data = elec_alca_2000, sum)
ano <- rep(2000,nrow(total_votos_alca_2000))
total_votos_alca_2000 <- cbind(total_votos_alca_2000, ano)
total_votos_alca_2000 <- subset(total_votos_alca_2000, select = c(ano,codmpio,votos))


total_votos_alca_2003<- aggregate(votos ~ codmpio, data = elec_alca_2003, sum)
ano <- rep(2003,nrow(total_votos_alca_2003))
total_votos_alca_2003 <- cbind(total_votos_alca_2003, ano)
total_votos_alca_2003 <- subset(total_votos_alca_2003, select = c(ano,codmpio,votos))


total_votos_alca_2007<- aggregate(votos ~ codmpio, data = elec_alca_2007, sum)
ano <- rep(2007,nrow(total_votos_alca_2007))
total_votos_alca_2007 <- cbind(total_votos_alca_2007, ano)
total_votos_alca_2007 <- subset(total_votos_alca_2007, select = c(ano,codmpio,votos))


total_votos_alca_2011<- aggregate(votos ~ codmpio, data = elec_alca_2011, sum)
ano <- rep(2011,nrow(total_votos_alca_2011))
total_votos_alca_2011 <- cbind(total_votos_alca_2011, ano)
total_votos_alca_2011 <- subset(total_votos_alca_2011, select = c(ano,codmpio,votos))


total_votos_alca_2015<- aggregate(votos ~ codmpio, data = elec_alca_2015, sum)
ano <- rep(2015,nrow(total_votos_alca_2015))
total_votos_alca_2015 <- cbind(total_votos_alca_2015, ano)
total_votos_alca_2015 <- subset(total_votos_alca_2015, select = c(ano,codmpio,votos))


total_votos_alca_2019<- aggregate(votos ~ codmpio, data = elec_alca_2019, sum)
ano <- rep(2019,nrow(total_votos_alca_2019))
total_votos_alca_2019 <- cbind(total_votos_alca_2019, ano)
total_votos_alca_2019 <- subset(total_votos_alca_2019, select = c(ano,codmpio,votos))




total_votos_pres_1986 <- aggregate(votos ~ codmpio, data = elec_pres_1986, sum)
ano <- rep(1986,nrow(total_votos_pres_1986))
total_votos_pres_1986 <- cbind(total_votos_pres_1986, ano)
total_votos_pres_1986 <- subset(total_votos_pres_1986, select = c(ano,codmpio,votos))


total_votos_pres_1990 <- aggregate(votos ~ codmpio, data = elec_pres_1990, sum)
ano <- rep(1990,nrow(total_votos_pres_1990))
total_votos_pres_1990 <- cbind(total_votos_pres_1990, ano)
total_votos_pres_1990 <- subset(total_votos_pres_1990, select = c(ano,codmpio,votos))


total_votos_pres_1994 <- aggregate(votos ~ codmpio, data = elec_pres_1994, sum)
ano <- rep(1994,nrow(total_votos_pres_1994))
total_votos_pres_1994 <- cbind(total_votos_pres_1994, ano)
total_votos_pres_1994 <- subset(total_votos_pres_1994, select = c(ano,codmpio,votos))


total_votos_pres_1998 <- aggregate(votos ~ codmpio, data = elec_pres_1998, sum)
ano <- rep(1998,nrow(total_votos_pres_1998))
total_votos_pres_1998 <- cbind(total_votos_pres_1998, ano)
total_votos_pres_1998 <- subset(total_votos_pres_1998, select = c(ano,codmpio,votos))


total_votos_pres_2002 <- aggregate(votos ~ codmpio, data = elec_pres_2002, sum)
ano <- rep(2002,nrow(total_votos_pres_2002))
total_votos_pres_2002 <- cbind(total_votos_pres_2002, ano)
total_votos_pres_2002 <- subset(total_votos_pres_2002, select = c(ano,codmpio,votos))


total_votos_pres_2006 <- aggregate(votos ~ codmpio, data = elec_pres_2006, sum)
ano <- rep(2006,nrow(total_votos_pres_2006))
total_votos_pres_2006 <- cbind(total_votos_pres_2006, ano)
total_votos_pres_2006 <- subset(total_votos_pres_2006, select = c(ano,codmpio,votos))


total_votos_pres_2010 <- aggregate(votos ~ codmpio, data = elec_pres_2010, sum)
ano <- rep(2010,nrow(total_votos_pres_2010))
total_votos_pres_2010 <- cbind(total_votos_pres_2010, ano)
total_votos_pres_2010 <- subset(total_votos_pres_2010, select = c(ano,codmpio,votos))


total_votos_pres_2014 <- aggregate(votos ~ codmpio, data = elec_pres_2014, sum)
ano <- rep(2014,nrow(total_votos_pres_2014))
total_votos_pres_2014 <- cbind(total_votos_pres_2014, ano)
total_votos_pres_2014 <- subset(total_votos_pres_2014, select = c(ano,codmpio,votos))


total_votos_pres_2018 <- aggregate(votos ~ codmpio, data = elec_pres_2018, sum)
ano <- rep(2018,nrow(total_votos_pres_2018))
total_votos_pres_2018 <- cbind(total_votos_pres_2018, ano)
total_votos_pres_2018 <- subset(total_votos_pres_2018, select = c(ano,codmpio,votos))


rm(elec_alca_1988, elec_alca_1992, elec_alca_1997, elec_alca_2000, elec_alca_2003, elec_alca_2007,
   elec_alca_2011, elec_alca_2015, elec_alca_2019, elec_pres_1986, elec_pres_1990, elec_pres_1994,
   elec_pres_1998, elec_pres_2002, elec_pres_2006, elec_pres_2010, elec_pres_2014, elec_pres_2018)

votos_totales <- rbind(total_votos_alca_1988, total_votos_alca_1992, total_votos_alca_1997,
                       total_votos_alca_2000, total_votos_alca_2003, total_votos_alca_2007,
                       total_votos_alca_2011, total_votos_alca_2015, total_votos_alca_2019,
                       total_votos_pres_1986, total_votos_pres_1990, total_votos_pres_1994,
                       total_votos_pres_1998, total_votos_pres_2002, total_votos_pres_2006,
                       total_votos_pres_2010, total_votos_pres_2014, total_votos_pres_2018)



votos_totales <- rbind(total_votos_pres_1986, total_votos_alca_1988, total_votos_pres_1990,
                       total_votos_alca_1992, total_votos_pres_1994, total_votos_alca_1997,
                       total_votos_pres_1998, total_votos_alca_2000, total_votos_pres_2002,
                       total_votos_alca_2003, total_votos_pres_2006, total_votos_alca_2007,
                       total_votos_pres_2010, total_votos_alca_2011, total_votos_pres_2014,
                       total_votos_alca_2015, total_votos_pres_2018, total_votos_alca_2019)


votos_totales <- arrange(.data=votos_totales , codmpio)

rm(total_votos_pres_1986, total_votos_alca_1988, total_votos_pres_1990,
   total_votos_alca_1992, total_votos_pres_1994, total_votos_alca_1997,
   total_votos_pres_1998, total_votos_alca_2000, total_votos_pres_2002,
   total_votos_alca_2003, total_votos_pres_2006, total_votos_alca_2007,
   total_votos_pres_2010, total_votos_alca_2011, total_votos_pres_2014,
   total_votos_alca_2015, total_votos_pres_2018, total_votos_alca_2019)

poblacion_y_votos <- left_join(x=poblacion_1986_2018, y=votos_totales, by=c("codmpio","ano"))

rm(votos_totales, poblacion_1986_2018)

poblacion_y_votos <- mutate(.data = poblacion_y_votos, participacion = votos/total_mayores18)


poblacion_y_votos <- subset(poblacion_y_votos, total_mayores18 != 0 &  votos != 0 & participacion < 1)


#Limpieza bases de datos violencia

acciones_belicas$id_caso<-acciones_belicas$`ID Caso`
acciones_belicas$ano<-acciones_belicas$Año
acciones_belicas$codmpio <- acciones_belicas$`Código DANE de Municipio`
acciones_belicas$MPIO <- acciones_belicas$Municipio
acciones_belicas$victimas <- acciones_belicas$`Total de Víctimas del Caso`
acciones_belicas$lesionados_civiles <- acciones_belicas$`Lesionados Civiles`
acciones_belicas$lesionados_combatientes <- acciones_belicas$`Lesionados Combatientes`
acciones_belicas <- mutate(.data = acciones_belicas, total_lesionados = lesionados_combatientes + 
                             lesionados_civiles) 
acciones_belicas<- subset(acciones_belicas, select = c(id_caso, ano, codmpio, MPIO,
                                                       lesionados_combatientes, lesionados_civiles,
                                                       total_lesionados, victimas))


asesinatos_selectivos$id_caso<- asesinatos_selectivos$`ID Caso`
asesinatos_selectivos$ano<-asesinatos_selectivos$Año
asesinatos_selectivos$codmpio <- asesinatos_selectivos$`Código DANE de Municipio`
asesinatos_selectivos$MPIO <- asesinatos_selectivos$Municipio
asesinatos_selectivos$lesionados_civiles <- asesinatos_selectivos $`Lesionados Civiles`
asesinatos_selectivos$victimas <- asesinatos_selectivos$`Total de Víctimas del Caso`
asesinatos_selectivos<- subset(asesinatos_selectivos, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))



danos_bienes_civiles$id_caso<- danos_bienes_civiles$`ID Caso`
danos_bienes_civiles$ano<-danos_bienes_civiles$Año
danos_bienes_civiles$codmpio <- danos_bienes_civiles$`Código DANE de Municipio`
danos_bienes_civiles$MPIO <- danos_bienes_civiles$Municipio
danos_bienes_civiles$lesionados_civiles <- danos_bienes_civiles $`Lesionados Civiles`
danos_bienes_civiles$victimas <- danos_bienes_civiles$`Total de Víctimas del Caso`
danos_bienes_civiles<- subset(danos_bienes_civiles, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))



desaparicion_forzada$id_caso<- desaparicion_forzada$`ID Caso`
desaparicion_forzada$ano<-desaparicion_forzada$Año
desaparicion_forzada$codmpio <- desaparicion_forzada$`Código DANE de Municipio`
desaparicion_forzada$MPIO <- desaparicion_forzada$Municipio
desaparicion_forzada$lesionados_civiles <- desaparicion_forzada $`Lesionados Civiles`
desaparicion_forzada$victimas <- desaparicion_forzada$`Total de Víctimas del Caso`
desaparicion_forzada<- subset(desaparicion_forzada, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))




masacres$id_caso<- masacres$`ID Caso`
masacres$ano<-masacres$Año
masacres$codmpio <- masacres$`Código DANE de Municipio`
masacres$MPIO <- masacres$Municipio
masacres$lesionados_civiles <- masacres $`Lesionados Civiles`
masacres$victimas <- masacres$`Total de Víctimas del Caso`
masacres<- subset(masacres, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))



minas$id_caso<- minas$`ID Caso`
minas$ano<-minas$Año
minas$codmpio <- minas$`Código DANE de Municipio`
minas$MPIO <- minas$Municipio
minas$lesionados_civiles <- minas $`Lesionados Civiles`
minas$victimas <- minas$`Total de Víctimas del Caso`
minas<- subset(minas, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))



reclutamiento_menores$id_caso<- reclutamiento_menores$`ID Caso`
reclutamiento_menores$ano<-reclutamiento_menores$Año
reclutamiento_menores$codmpio <- reclutamiento_menores$`Código DANE de Municipio`
reclutamiento_menores$MPIO <- reclutamiento_menores$Municipio
reclutamiento_menores$victimas <- reclutamiento_menores$`Total de Víctimas del Caso`
reclutamiento_menores<- subset(reclutamiento_menores, select = c(id_caso, ano, codmpio, MPIO ,victimas))




secuestros$id_caso<- secuestros$`ID Caso`
secuestros$ano<-secuestros$Año
secuestros$codmpio <- secuestros$`Código DANE de Municipio`
secuestros$MPIO <- secuestros$Municipio
secuestros$lesionados_civiles <- secuestros $`Lesionados Civiles`
secuestros$victimas <- secuestros$`Total de Víctimas del Caso`
secuestros<- subset(secuestros, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))



violencia_sexual$id_caso<- violencia_sexual$`ID Caso`
violencia_sexual$ano<-violencia_sexual$Año
violencia_sexual$codmpio <- violencia_sexual$`Código DANE de Municipio`
violencia_sexual$MPIO <- violencia_sexual$Municipio
violencia_sexual$lesionados_civiles <- violencia_sexual $`Lesionados Civiles`
violencia_sexual$victimas <- violencia_sexual$`Total de Víctimas del Caso`
violencia_sexual<- subset(violencia_sexual, select = c(id_caso, ano, codmpio, MPIO, lesionados_civiles ,victimas))

violencia_sexual <- subset(violencia_sexual, ano != "0000" &  codmpio != "00000" & MPIO != "SIN INFORMACION" )
violencia_sexual$codmpio <- as.numeric(as.character(violencia_sexual$codmpio))
violencia_sexual$ano <- as.numeric(as.character(violencia_sexual$ano))
violencia_sexual <-subset(violencia_sexual, ano >1985)

violencia_sexual <- violencia_sexual %>% 
  group_by(codmpio, ano) %>% 
  mutate(total_victimas = sum(victimas))

hist(violencia_sexual$total_victimas)







