library(readxl)
library(ggplot2)

#Ucitavanje podataka
data <- read_excel("data/data.xlsx")
View(data)

#Selekcija potrebnih kolona
data <- subset(data, select = c(GODINA, ID, BROJ_LAB_PROTOKOLA, POL,
                                GODINA_ROÐENJA, MESTO_PACIJENTA, 
                                USTANOVA_POSILJALAC_UZORKA, TRAJANJE_VAKCINACIJE,
                                RAZLIKA_ZAVRŠETKA_I_BUSTERA, 
                                RAZLIKA_PRIJEMA_I_UZORKOVANJA, 
                                RAZLIKA_PRIJEMA_I_POCETKA,
                                RAZLIKA_UZORKOVANJA_I_POCETKA,
                                UKUPNO_DANA_OD_VADJENJA_KRVI,
                                NAPOMENA,REZULTAT_ANALIZE,USPESNO_IMUNIZOVAN, 
                                DAT_SERUM,BROJ_JEDINICA,SERIJA_SERUMA,
                                LOT_VAKCINE,LOKACIJA_OZLEDA,BROJ_OZLEDA,
                                ŽIVOTINJA))

#Uklanjanje redova sa nedostajucim podacima na zavisnim varijablama

library(tidyr)

paste("Broj nedostajucih u varijabli REZULTAT_ANALIZE:",
      sum(is.na(data$REZULTAT_ANALIZE)))

#Uklanjanje reda sa nedostajucim podatkom u REZULTAT_ANALIZE varijabli
data <- data %>% drop_na(REZULTAT_ANALIZE)

paste("Broj nedostajucih u varijabli REZULTAT_ANALIZE:", 
      sum(is.na(data$REZULTAT_ANALIZE)))

paste("Broj nedostajucih u varijabli USPESNO_IMUNIZOVAN:",
      sum(is.na(data$USPESNO_IMUNIZOVAN)))

# APPLY da primenimo jednu funkciju na sve kolone u DF

apply(X = d,
      FUN = unique,
      MARGIN = 2) #MARGIN = 2, funckija se primenjuje na KOLONE df-a

#Sredjivanje varijable LOT_VAKCINE

unique(data$LOT_VAKCINE) 

# Uzimanje ispitanika koji nemaju odgovarajuci lot vakcine
data_c <- data[grep(",", data$LOT_VAKCINE),]
data_d <- data[grep(";", data$LOT_VAKCINE),]
data_q <-  data[grep("\\?", data$LOT_VAKCINE),]

data_nelot <- rbind(data_c, data_d, data_q)
View(data_nelot)
  
#Uklanjanje pacijenata sa neodgovarajucim lotom vakcina

library(stringr)
library(dplyr)

d <- data[!grepl(',', data$LOT_VAKCINE),]

d <- d[!grepl(';', d$LOT_VAKCINE),]

d <- d[!grepl('\\?', d$LOT_VAKCINE),]

#31 pacijent je primio dva ili vise razilicitih lotova vakcina!

unique(d$LOT_VAKCINE) 

#PRECISCAVANJE NUMERICKIH VARIJABLI KOJE OZNACAVANJU TRAJANJE

#Zamena negativnih sa NA na varijabli TRAJANJE_VAKCINACIJE

d$TRAJANJE_VAKCINACIJE <- replace(d$TRAJANJE_VAKCINACIJE, 
                                  which(d$TRAJANJE_VAKCINACIJE < 0),
                                  NA)
unique(d$TRAJANJE_VAKCINACIJE)
range(d$TRAJANJE_VAKCINACIJE, na.rm=TRUE)

#Zamena negativnih sa NA na varijabli RAZLIKA_ZAVRŠETKA_I_BUSTERA

d$RAZLIKA_ZAVRŠETKA_I_BUSTERA <- replace(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA, 
                                       which(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA < 0),
                                       NA)
unique(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA)

#Zamena negativnih sa NA na varijabli RAZLIKA_PRIJEMA_I_UZORKOVANJA


d$RAZLIKA_PRIJEMA_I_UZORKOVANJA <- replace(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA, 
                                     which(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA < 0),
                                           NA)
unique(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA)

#Zamena negativnih sa NA na varijabli RAZLIKA_PRIJEMA_I_POCETKA

d$RAZLIKA_PRIJEMA_I_POCETKA <- replace(d$RAZLIKA_PRIJEMA_I_POCETKA, 
                                       which(d$RAZLIKA_PRIJEMA_I_POCETKA < 0),
                                       NA)
unique(d$RAZLIKA_PRIJEMA_I_POCETKA)
range(d$RAZLIKA_PRIJEMA_I_POCETKA, na.rm = TRUE)

#Zamena negativnih sa NA na varijabli RAZLIKA_UZORKOVANJA_I_POCETKA

d$RAZLIKA_UZORKOVANJA_I_POCETKA <- replace(d$RAZLIKA_UZORKOVANJA_I_POCETKA, 
                                    which(d$RAZLIKA_UZORKOVANJA_I_POCETKA < 0),
                                       NA)
unique(d$RAZLIKA_UZORKOVANJA_I_POCETKA)

#Zamena neodgovarajucih vrednosti sa NA na varijabli BROJ_JEDINICA
unique(d$BROJ_JEDINICA)

d$BROJ_JEDINICA <- replace(d$BROJ_JEDINICA,
                        which(d$BROJ_JEDINICA == '/'),
                           NA)
unique(d$BROJ_JEDINICA)

#Zamena neodgovarajucih vrednosti sa NA na varijabli SERIJA_SERUMA

unique(d$SERIJA_SERUMA)

d$SERIJA_SERUMA <- replace(d$SERIJA_SERUMA,
                        which(d$SERIJA_SERUMA == '/' | d$SERIJA_SERUMA == '?'),
                                           NA)
unique(d$SERIJA_SERUMA)

#Zamena neodgovarajucih vrednosti sa NA na varijabli LOKACIJA_OZLEDA

unique(d$LOKACIJA_OZLEDA)

d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                         which(d$LOKACIJA_OZLEDA == '/' | 
                                d$LOKACIJA_OZLEDA == 'S019'),
                         NA)
unique(d$LOKACIJA_OZLEDA)

#Zamena neodgovarajucih vrednosti sa NA na varijabli BROJ_OZLEDA

unique(d$BROJ_OZLEDA)

d$BROJ_OZLEDA <- replace(d$BROJ_OZLEDA,
                           which(d$BROJ_OZLEDA == '/' | d$BROJ_OZLEDA == '?'),
                           NA)
unique(d$BROJ_OZLEDA)

#Zamena neodgovarajucih vrednosti sa NA na varijabli ŽIVOTINJA

unique(d$ŽIVOTINJA)

#Pogresne unose macke u ispravne

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'MAÈAK' | 
                    d$ŽIVOTINJA == 'MA;KA'), 'MAÈKA') 

#Neodgovarajuce unose u NA

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'NEPOZNATO' | 
                                            d$ŽIVOTINJA == '/'), NA)

#Pogresan unos slepog misa u ispravan

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'WSLEPI MIŠ'), 
                       'SLEPI MIŠ') 

#Pogresan unos divlje svinje u ispravan

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'DIVLJA SVINA'), 
                       'DIVLJA SVINJA')

#VISUAL EXPLORATORY DATA ANALYSIS

library(ggplot2)
"""
table(d$GODINA)
ggplot(d, aes(x=GODINA)) + geom_histogram(binwidth = 1, color = 'white')

ggplot(d, aes(x=POL)) + geom_bar()
table(d$POL, d$GODINA_ROÐENJA)

table(d$POL)
sum(is.na(d$POL))

ggplot(d, aes(x=ŽIVOTINJA, fill=ŽIVOTINJA)) + geom_bar()
"""

d %>%
  filter(!is.na(POL)) %>%
  ggplot(aes(x= POL)) + 
  geom_bar() 
