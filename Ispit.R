library(readxl)
library(ggplot2)
options(scipen = 999)
#Ucitavanje podataka
data <- read_excel("data/data.xlsx")
View(data)

#Selekcija potrebnih kolona
data <- subset(data, select = c(GODINA, ID, BROJ_LAB_PROTOKOLA, POL,
                                GODINA_ROĐENJA, MESTO_PACIJENTA, 
                                USTANOVA_POSILJALAC_UZORKA, TRAJANJE_VAKCINACIJE,
                                RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA,
                                RAZLIKA_POCETKA_VAK_I_ISPITIVANJA,
                                RAZLIKA_ZAVRŠETKA_I_BUSTERA, 
                                RAZLIKA_PRIJEMA_I_UZORKOVANJA, 
                                RAZLIKA_PRIJEMA_I_POCETKA,
                                RAZLIKA_UZORKOVANJA_I_POCETKA,
                                UKUPNO_DANA_OD_VADJENJA_KRVI,
                                NAPOMENA,REZULTAT_ANALIZE,USPESNO_IMUNIZOVAN, 
                                DAT_SERUM,BROJ_JEDINICA,SERIJA_SERUMA,
                                LOT_VAKCINE,LOKACIJA_OZLEDA,BROJ_OZLEDA,
                                ŽIVOTINJA))

#Uklanjanje pacijenata sa neodgovarajucim lotom vakcina

library(stringr)
library(dplyr)

# Selekcija ispitanikakojima lot vakcine ne sadrzi , ; \\?

unique(data$LOT_VAKCINE) 

d <- data[!grepl(',', data$LOT_VAKCINE),]

d <- d[!grepl(';', d$LOT_VAKCINE),]

d <- d[!grepl('\\?', d$LOT_VAKCINE),]

unique(d$LOT_VAKCINE) 

#31 pacijent je primio dva ili vise razilicitih lotova vakcina!

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

d$LOT_VAKCINE <- replace(d$LOT_VAKCINE,
                             which(d$LOT_VAKCINE == '24081216' | 
                                     d$LOT_VAKCINE == '24082016' |
                                     d$LOT_VAKCINE== '26092016' |
                                     d$LOT_VAKCINE == '972310'),
                             NA)
unique(d$LOT_VAKCINE)

d$LOT_VAKCINE <- replace(d$LOT_VAKCINE,
                         which(d$LOT_VAKCINE == 'M1666' |
                                 d$LOT_VAKCINE == 'M1666PAS'),
                         'M16661V')

d$LOT_VAKCINE <- replace(d$LOT_VAKCINE,
                         which(d$LOT_VAKCINE == 'N1697' | 
                                 d$LOT_VAKCINE == 'N16972' |
                                 d$LOT_VAKCINE== 'N1G67' |
                                 d$LOT_VAKCINE== 'N1G97' |
                                 d$LOT_VAKCINE == 'N1G972' |
                                 d$LOT_VAKCINE == 'N1G972V'),
                         'N16972V')


d$LOT_VAKCINE <- replace(d$LOT_VAKCINE,
                         which(d$LOT_VAKCINE == 'N1J31' | 
                                 d$LOT_VAKCINE == 'N1J313' |
                                 d$LOT_VAKCINE== 'N1J31PAS' |
                                 d$LOT_VAKCINE == 'N1J31V' |
                                 d$LOT_VAKCINE == 'N4A11'),
                         'N1J313V')

d$LOT_VAKCINE <- replace(d$LOT_VAKCINE,
                         which(d$LOT_VAKCINE == 'P1C53' |
                                 d$LOT_VAKCINE == 'P1C530'),
                         'P1C531')

d$LOT_VAKCINE <- replace(d$LOT_VAKCINE,
                         which(d$LOT_VAKCINE == 'P1D27' |
                                 d$LOT_VAKCINE == 'PIO27'),
                         'P1D271')

unique(d$LOT_VAKCINE)

#pravljenje titer ui/dose varijable

unique(d$TITER)
d$TITER <-  replace(d$LOT_VAKCINE,
                    which(d$LOT_VAKCINE == 'P1C531'),
                    10.8)
d$TITER <-  replace(d$TITER,
                    which(d$TITER == 'P1D271'),
                    6.2)
d$TITER <-  replace(d$TITER,
                    which(d$TITER == 'N1J313V'),
                    9.2)
d$TITER <-  replace(d$TITER,
                    which(d$TITER == 'M16661V'),
                    6.5) 
d$TITER <- replace(d$TITER,
                   which(d$TITER == 'L1262-2'),
                   10.5)
d$TITER <- replace(d$TITER,
                   which(d$TITER == 'N1G972V'),
                   9.7)

d$TITER <- replace(d$TITER,
                   which(d$TITER == 'H1931'),
                   NA)
d$TITER <- as.numeric(d$TITER)
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

range(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA, na.rm=TRUE)
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

#Zamena negativnih sa NA na varijabli RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA <- 
            replace(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA, 
            which(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA < 0),NA)
unique(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)
range(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA, na.rm = TRUE)

#Zamena negativnih sa NA na varijabli RAZLIKA_POCETKA_VAK_I_ISPITIVANJA

d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA <- 
                  replace(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA, 
                 which(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA < 0),NA)
unique(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)
range(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA, na.rm = TRUE)

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
                                d$LOKACIJA_OZLEDA == 'S019' |
                                 d$LOKACIJA_OZLEDA == 'PAS'),
                         NA)


d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="USNA"), "USNE")
                         
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA == "NATKOLENICА" |
                                     d$LOKACIJA_OZLEDA == "NATKOLENICA"
                                     ), "NATKOLENICE")

d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="NOGA"), "NOGE")

d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="OBVA"), "OBRVA")

d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="ŠAKA"), "ŠAKE")

d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="PODLAKTICA"), 
                             "PODLAKTICE")


d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="POTKOLENICA"), 
                             "POTKOLENICE")

d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="NADLAKTICA"), 
                             "NADLAKTICE")
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

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'MAČAK' | 
                    d$ŽIVOTINJA == 'MA;KA'), 'MAČKA') 

#Neodgovarajuce unose u NA

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'NEPOZNATO' | 
                                            d$ŽIVOTINJA == '/'), NA)

#Pogresan unos slepog misa u ispravan

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'WSLEPI MIŠ'), 
                       'SLEPI MIŠ') 

#Pogresan unos divlje svinje u ispravan

d$ŽIVOTINJA <- replace(d$ŽIVOTINJA, which(d$ŽIVOTINJA == 'DIVLJA SVINA'), 
                       'DIVLJA SVINJA')


#Uklanjanje redova sa nedostajucim podacima na zavisnim varijablama

library(tidyr)

paste("Broj nedostajucih u varijabli REZULTAT_ANALIZE:",
      sum(is.na(d$REZULTAT_ANALIZE)))

#Uklanjanje reda sa nedostajucim podatkom u REZULTAT_ANALIZE varijabli

d <- d %>% drop_na(REZULTAT_ANALIZE)

paste("Broj nedostajucih u varijabli REZULTAT_ANALIZE:", 
      sum(is.na(d$REZULTAT_ANALIZE)))

paste("Broj nedostajucih u varijabli USPESNO_IMUNIZOVAN:",
      sum(is.na(d$USPESNO_IMUNIZOVAN)))

#Prevodjenje na engleski

#POl
d$POL <- as.factor(d$POL)
levels(d$POL)
levels(d$POL)[levels(d$POL)=="MUSKI"] <- "Muški" 
levels(d$POL)[levels(d$POL)=="ZENSKI"] <- "Ženski" 

#Dat serum
d$DAT_SERUM <- as.factor(d$DAT_SERUM)

levels(d$DAT_SERUM)
levels(d$DAT_SERUM)[levels(d$DAT_SERUM)=="DA"] <- "Da"
levels(d$DAT_SERUM)[levels(d$DAT_SERUM)=="NE"] <- "Ne"

#Lokacija ozleda
d$LOKACIJA_OZLEDA <- as.factor(d$LOKACIJA_OZLEDA)

levels(d$LOKACIJA_OZLEDA)
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="BUTINA, KOLENO, ŠAKA"] <- "Tigh, knee, palm" #1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="GLAVA"] <- "Head" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="GLUTEUS"] <- "Gluteus"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="GLUTEUS, PODLAKTICA"] <- "Gluteus, forearm" #1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="GRUDNI KOŠ"] <- "Chest"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="GRUDNI KOŠ I LICE"] <- "Chest and face"#1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="GRUDNI KOŠ I LICE"] <- "Chest and face"#1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="KOLENO"] <- "Knee"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="KOLENO I POTKOLENICA"] <- "Knee and shin"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="LAKAT"] <- "Elbow" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="LICE I VRAT"] <- "Face and neck" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="NADLAKTICE"] <- "Upper arm" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="NATKOLENICE"] <- "Upper leg"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="NOGA I RUKA"] <- "Leg and arm" #1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="NOGA, ŠAKA"] <- "Leg, palm" #1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="NOGE"] <- "Legs"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="OBRAZ"] <- "Cheek" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="OBRAZ, NOS"] <- "Cheek, nose" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="OBRVA" ] <- "Eyebrow" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="OKO"] <- "Eye"#2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="PALAC"] <- "Thumb" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="PALAC, ŠAKE"] <- "Thumb, palm" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="PETA"] <- "Heel"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="PODLAKTICA, ŠAKA"] <- "Forearm, palm"#3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="PODLAKTICE"] <- "Forearms" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="POTKOLENICE"] <- "Shins"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="RAME"] <- "Shoulder" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="RUKA" ] <- "Arm" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="STOPALO" ] <- "Feet"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="ŠAKA, NATKOLENICA"] <- "Palm, upper leg" #1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="ŠAKA, POTKOLENICA"] <- "Palm, shin"#1
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="ŠAKE"] <- "Palm" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="ŠAKE, NADLAKTICA"] <- "Palm, upper arm"#3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="TRBUH"] <- "Belly"
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="UBOD IGLOM"] <- "Needle piercing" #3
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="UHO"] <- "Ear" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="USNE"] <- "Lips" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="VRAT"] <- "Neck" #2
levels(d$LOKACIJA_OZLEDA)[levels(d$LOKACIJA_OZLEDA)=="VRAT, GLAVA"] <- "Neck, head" #2

unique(d$LOKACIJA_OZLEDA)

d$LOKACIJA_OZLEDA <- as.character(d$LOKACIJA_OZLEDA)
#Visestruke ozlede
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                         which(d$LOKACIJA_OZLEDA == "Tigh, knee, palm" | 
                                 d$LOKACIJA_OZLEDA == "Gluteus, forearm" |
                                 d$LOKACIJA_OZLEDA== "Chest and face" |
                                 d$LOKACIJA_OZLEDA == "Leg and arm" |
                                 d$LOKACIJA_OZLEDA == "Leg, palm" |
                                 d$LOKACIJA_OZLEDA == "Palm, upper leg" |
                                 d$LOKACIJA_OZLEDA == "Palm, shin" |
                               d$LOKACIJA_OZLEDA == "Chest and face"),
                         'Višestruke ozlede')
#Ozlede glave i vrata
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA == "Head" | 
                                     d$LOKACIJA_OZLEDA == "Face and neck" |
                                     d$LOKACIJA_OZLEDA== "Cheek" |
                                     d$LOKACIJA_OZLEDA == "Cheek, nose" |
                                     d$LOKACIJA_OZLEDA == "Eyebrow"  |
                                     d$LOKACIJA_OZLEDA == "Eye" |
                                     d$LOKACIJA_OZLEDA == "Ear" |
                                     d$LOKACIJA_OZLEDA == "Lips" |
                                   d$LOKACIJA_OZLEDA == "Neck" |
                                     d$LOKACIJA_OZLEDA == "Neck, head" ),
                             'Glava i vrat')
#Ozlede ruku
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA == "Elbow" | 
                                     d$LOKACIJA_OZLEDA == "Upper arm" |
                                     d$LOKACIJA_OZLEDA== "Thumb" |
                                     d$LOKACIJA_OZLEDA == "Thumb, palm" |
                                     d$LOKACIJA_OZLEDA == "Forearm, palm"  |
                                     d$LOKACIJA_OZLEDA == "Forearms" |
                                     d$LOKACIJA_OZLEDA == "Shoulder" |
                                     d$LOKACIJA_OZLEDA == "Arm" |
                                     d$LOKACIJA_OZLEDA == "Palm"  |
                                     d$LOKACIJA_OZLEDA == "Palm, upper arm" |
                                     d$LOKACIJA_OZLEDA == "Needle piercing"),
                             'Ruke')
#Ozlede nogu
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA == "Shins" | 
                                     d$LOKACIJA_OZLEDA == "Legs"  |
                                     d$LOKACIJA_OZLEDA== "Feet" |
                                     d$LOKACIJA_OZLEDA == "Upper leg" |
                                     d$LOKACIJA_OZLEDA ==  "Knee"  |
                                     d$LOKACIJA_OZLEDA == "Heel" |
                                     d$LOKACIJA_OZLEDA == "Knee and shin"|
                                     d$LOKACIJA_OZLEDA == "Gluteus"),
                             'Noge')
#Ozlede trupa
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA == "Chest" | 
                                     d$LOKACIJA_OZLEDA == "Belly"),
                             'Trup')


d$LOKACIJA_OZLEDA <- as.factor(d$LOKACIJA_OZLEDA)
unique(d$LOKACIJA_OZLEDA)


#Zivotinja
d$ŽIVOTINJA <- as.factor(d$ŽIVOTINJA)

levels(d$ŽIVOTINJA)
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="DIVLJA MAČKA"] <- "Divlja mačka"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="DIVLJA SVINJA"] <- "Divlja svinja"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="JAZAVAC"] <- "Jazavac"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="KUNA"] <- "Kuna"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="LISICA"] <- "Lisica"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="MAČKA"] <- "Mačka"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="MAJMUN"] <- "Majmun"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="MIŠ"] <- "Miš"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="PACOV"] <- "Pacov"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="PAS"] <- "Pas"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="SLEPI MIŠ"] <- "Slepi miš"
levels(d$ŽIVOTINJA)[levels(d$ŽIVOTINJA)=="SRNA"] <- "Srna"
unique(d$ŽIVOTINJA)

#Uspesno imunizovan
d$USPESNO_IMUNIZOVAN <- as.factor(d$USPESNO_IMUNIZOVAN)
levels(d$USPESNO_IMUNIZOVAN)
levels(d$USPESNO_IMUNIZOVAN)[levels(d$USPESNO_IMUNIZOVAN)=="DA"] <- "Da"
levels(d$USPESNO_IMUNIZOVAN)[levels(d$USPESNO_IMUNIZOVAN)=="NE"] <- "Ne"
unique(d$USPESNO_IMUNIZOVAN)


#Ustanova posiljalac uzorka

#install.packages("stringi")
library('stringi')
typeof(d$USTANOVA_POSILJALAC_UZORKA)
d$USTANOVA_POSILJALAC_UZORKA <-  as.factor(d$USTANOVA_POSILJALAC_UZORKA)

d$USTANOVA_POSILJALAC_UZORKA <-  stri_trans_totitle(d$USTANOVA_POSILJALAC_UZORKA) 
unique(d$USTANOVA_POSILJALAC_UZORKA)

#Funkcija remove_outrliers() koja za cilj ima ukljanjanje aberantnih rezultata

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#Uklanjanje outliera na numeričkim varijablama


sum(is.na(d$TRAJANJE_VAKCINACIJE))
d$TRAJANJE_VAKCINACIJE <- remove_outliers(d$TRAJANJE_VAKCINACIJE)

sum(is.na(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA))
d$RAZLIKA_ZAVRŠETKA_I_BUSTERA <- remove_outliers(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA)

sum(is.na(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA))
d$RAZLIKA_PRIJEMA_I_UZORKOVANJA <- 
  remove_outliers(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA)

sum(is.na(d$RAZLIKA_PRIJEMA_I_POCETKA))
d$RAZLIKA_PRIJEMA_I_POCETKA <- remove_outliers(d$RAZLIKA_PRIJEMA_I_POCETKA)

sum(is.na(d$RAZLIKA_UZORKOVANJA_I_POCETKA))
d$RAZLIKA_UZORKOVANJA_I_POCETKA <- 
  remove_outliers(d$RAZLIKA_UZORKOVANJA_I_POCETKA)

sum(is.na(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA))
d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA <- 
  remove_outliers(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)

sum(is.na(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA))
d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA <- 
  remove_outliers(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)

sum(is.na(d$BROJ_JEDINICA))
d$BROJ_JEDINICA <- as.numeric(d$BROJ_JEDINICA)
d$BROJ_JEDINICA <- remove_outliers(d$BROJ_JEDINICA)

d$STAROST <- 2022 - d$GODINA_ROĐENJA

d4 <- d[is.na(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA),]
d5 <- d[!is.na(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA),]

View(d4)
unique(d4$RAZLIKA_ZAVRŠETKA_I_BUSTERA)
unique(d5$RAZLIKA_ZAVRŠETKA_I_BUSTERA)

#Pretvaranje varijable BROJ_OZLEDA u numeričku

d$BROJ_OZLEDA <- as.numeric(d$BROJ_OZLEDA)

###Vizualizacija varijabli###

library(psych)

deskriptivna <- describe(d4)

deskriptivna

#Racunanje geometrijske aritmeticke sredine 

gm <- function(x) {
  exp(mean(log(x[x>0]), na.rm=TRUE))
}

d4$BROJ_JEDINICA <- as.numeric(d4$BROJ_JEDINICA)
d4$BROJ_OZLEDA <- as.numeric(d4$BROJ_OZLEDA)

gm(d4$TRAJANJE_VAKCINACIJE)
gm(d4$STAROST)
gm(d4$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)
gm(d4$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)
gm(d4$RAZLIKA_PRIJEMA_I_UZORKOVANJA)
gm(d4$RAZLIKA_PRIJEMA_I_POCETKA)
gm(d4$RAZLIKA_UZORKOVANJA_I_POCETKA)
gm(d4$UKUPNO_DANA_OD_VADJENJA_KRVI)
gm(d4$REZULTAT_ANALIZE)
gm(d4$BROJ_JEDINICA)
gm(as.numeric(d4$BROJ_OZLEDA))


#Racunanje intervala poverenja

cint <- function(n, mean, std) {
  error <- qt(0.975,df=n-1)*std/sqrt(n)
  lower <- mean - error
  upper <- mean + error
  paste(lower, '-', upper)
  
}
#4 Vakcine
cint(517, 15.62, 3.19) #TRAJANJE_VAKCINACIJE
cint(581, 48.03, 21.69) #STAROST
cint(508, 24.35, 7.01) #RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA
cint(531, 40.95, 9.54 ) #RAZLIKA_POCETKA_VAK_I_ISPITIVANJA
cint(567, 11.28, 9.66) #RAZLIKA_PRIJEMA_I_UZORKOVANJA
cint(574, 3.29, 1.87) #RAZLIKA_PRIJEMA_I_POCETKA
cint(563, 14.66, 9.99) #RAZLIKA_UZORKOVANJA_I_POCETKA
cint(312, 6.13, 7.64) #UKUPNO_DANA_OD_VADJENJA_KRVI
cint(581, 5.54, 10.60) #REZULTAT_ANALIZE
cint(408, 1370.13, 408.72) #BROJ_JEDINICA
cint(279, 1.70, 1.16) #BROJ_OZLEDA


#Vizuelni prikaz variajble pol na uzorku

prop.table(table(d5$POL))
table(d5$POL)

d4 %>%
  filter(!is.na(POL)) %>%
  ggplot(aes(x= POL)) +
  geom_bar() + ggtitle('Prikaz varijable pol na uzorku') + xlab('Pol') +
  ylab('Broj')

#Prikaz varijable GODINA

prop.table(table(d4$GODINA))
table(d4$GODINA)

ggplot(d, aes(x=GODINA)) + geom_bar() + 
  ggtitle('Prikaz varijable godina na uzorku') + xlab('Godina') + ylab('Broj')

#Prikaz varijable GODINA_ROĐENJA

table(d$STAROST)

d4 %>%
  ggplot(aes(x= STAROST)) + 
  geom_histogram(color = 'white') + 
  ggtitle('Histogram of Age variable') + xlab('Age') + 
  ylab('Frequency')

shapiro.test(d$STAROST)

#Prikaz varijable USTANOVA_POSILJALAC_UZORKA

table(d5$USTANOVA_POSILJALAC_UZORKA)
prop.table(table(d5$USTANOVA_POSILJALAC_UZORKA))
d4 %>%
  filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>%
  ggplot(aes(y= USTANOVA_POSILJALAC_UZORKA)) +
  geom_bar() + ggtitle('Prikaz varijable ustanove pošiljalca uzorka') + xlab('Frekvencija') +
  ylab('Ustanova pošiljalac uzorka')

#Prikaz varijable TRAJANJE_VAKCINACIJE

d4 %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>% 
  ggplot(aes(x= TRAJANJE_VAKCINACIJE)) + 
  geom_histogram(bins=10) + 
  ggtitle('Trajanje vakcinacije') + xlab('Trajanje vakcinacije (dani)') + 
  ylab('Frekvencija')

shapiro.test(d4$TRAJANJE_VAKCINACIJE)


#Prikaz varijable RAZLIKA_PRIJEMA_I_UZORKOVANJA

d4 %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>% 
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_UZORKOVANJA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika prijema i uzorkovanja') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d4$RAZLIKA_PRIJEMA_I_UZORKOVANJA)

#Prikaz varijable RAZLIKA_UZORKOVANJA_I_POCETKA

d4 %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>% 
  ggplot(aes(x= RAZLIKA_UZORKOVANJA_I_POCETKA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika uzorkovanja i početka') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d4$RAZLIKA_UZORKOVANJA_I_POCETKA)

#Prikaz varijable RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

d4 %>%
  filter(!is.na(RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) %>% 
  ggplot(aes(x= RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika završetka vakcinacije i početka analize') + 
  xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d4$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)

#Prikaz varijable RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

d4 %>%
  filter(!is.na(RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) %>% 
  ggplot(aes(x= RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika početka vakcinacije i početka analize') + 
  xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d4$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)

#Prikaz varijable REZULTAT_ANALIZE

d4 %>% 
  filter(REZULTAT_ANALIZE < 50) %>% 
  ggplot(aes(x=REZULTAT_ANALIZE)) + geom_histogram() + 
  ggtitle('Razultat analize') + xlab('Rezultat analize') + 
  ylab('Frekvencija')

describe(d4$REZULTAT_ANALIZE)

#Prikaz varijable USPESNO_IMUNIZOVAN

ggplot(d4, aes(x=USPESNO_IMUNIZOVAN)) + geom_bar() + 
  ggtitle('Uspešnost imunizacije') + xlab('Uspešno imunizovan') + 
  ylab('Frekvencija')

table(d4$USPESNO_IMUNIZOVAN)
prop.table(table(d4$USPESNO_IMUNIZOVAN))

#Prikaz varijable DAT_SERUM

d4 %>%
  filter(!is.na(DAT_SERUM)) %>% 
  ggplot(aes(x= DAT_SERUM)) + 
  geom_bar() + 
  ggtitle('Histogram davanja seruma') + xlab('Dat serum') + 
  ylab('Frekvencija')


table(d4$DAT_SERUM)
prop.table(table(d4$DAT_SERUM))

#Prikaz varijable BROJ_JEDINICA

sum(is.na(d4$BROJ_JEDINICA))

d4 %>%
  filter(!is.na(BROJ_JEDINICA)) %>% 
  ggplot(aes(x= as.numeric(BROJ_JEDINICA))) + 
  geom_histogram(bins=30) + 
  ggtitle('Broj jedinica') + xlab('Broj jedinica') + 
  ylab('Frekvencija')


shapiro.test(d4$BROJ_JEDINICA)

#Prikaz varijable SERIJA_SERUMA

table(d4$SERIJA_SERUMA)

#Prikaz varijable LOKACIJA_OZLEDA

sum(is.na(d4$LOKACIJA_OZLEDA))

table(d4$LOKACIJA_OZLEDA)
prop.table(table(d4$LOKACIJA_OZLEDA))

d4 %>%
  filter(!is.na(LOKACIJA_OZLEDA)) %>%
  ggplot(aes(y= LOKACIJA_OZLEDA)) +
  geom_bar() + ggtitle('Prikaz varijable ustanove pošiljalca uzorka') + 
  xlab('Frekvencija') +
  ylab('Ustanova pošiljalac uzorka')

#Prikaz varijable BROJ_OZLEDA

sum(is.na(d4$BROJ_OZLEDA))

table(d4$BROJ_OZLEDA)

d4 %>%
  filter(!is.na(BROJ_OZLEDA)) %>% 
  ggplot(aes(x= BROJ_OZLEDA)) + 
  geom_bar() + 
  ggtitle('Prikaz broja ozleda') + xlab('Broj ozleda') + 
  ylab('Frekvencija')

#Prikaz varijable ŽIVOTINJA

sum(is.na(d4$ŽIVOTINJA))

table(d4$ŽIVOTINJA)
prop.table(table(d4$ŽIVOTINJA))

d4 %>%
  filter(!is.na(ŽIVOTINJA)) %>% 
  ggplot(aes(y= ŽIVOTINJA)) + 
  geom_bar() + 
  ggtitle('Broj ozleda nastao od određenih životinja') + xlab('Životinja') + 
  ylab('Frekvencija')


#Prikaz varijable LOT_VAKCINE

sum(is.na(d4$LOT_VAKCINE))

table(d4$LOT_VAKCINE)
prop.table(table(d4$LOT_VAKCINE))

d4 %>%
  filter(!is.na(LOT_VAKCINE)) %>% 
  ggplot(aes(y= LOT_VAKCINE)) + 
  geom_bar() + 
  ggtitle('Broj ozleda nastao od određenih životinja') + xlab('Vaccine lot') + 
  ylab('Count')

#Prikaz varijable TITER

table(d4$TITER)

d4 %>%
  filter(!is.na(TITER)) %>%
  ggplot(aes(x= TITER)) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  ggtitle('Histogram godina rođenja pacijenata') + xlab('TITER') + 
  ylab('COUNT')


shapiro.test(d4$TITER)
d4$TITER <- remove_outliers(d4$TITER)

#Analiza pojedinacnih nezavisnih varijabli sa zavisnim

# USPESNO_IMUNIZOVAN i POL

table(d4$USPESNO_IMUNIZOVAN, d4$POL)

chisq.test(d4$POL, d4$USPESNO_IMUNIZOVAN, correct=FALSE)

d4 %>% 
  filter(!is.na(POL)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=POL)) +
  geom_bar() + ggtitle('Odnos pola i uspešnosti imunizacije') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Pol')

# USPESNO_IMUNIZOVAN i STAROST

shapiro.test(d4$STAROST)

wilcox.test(d4$STAROST ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=STAROST)) +
  geom_boxplot(outlier.shape = NA) + ggtitle('Uspešnost imunizacije u odnosu na starost') + 
  xlab('Uspešno imunizovan') +
  ylab('Starost') + 
  annotate(geom="text", label="p < 0.01 **")



# USPESNO_IMUNIZOVAN i USTANOVA_POSILJALAC_UZORKA

table(d4$USPESNO_IMUNIZOVAN, d4$USTANOVA_POSILJALAC_UZORKA)

#install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)
#install.packages("ggstatsplot")
library(ggstatsplot)

chisq.test(d4$USTANOVA_POSILJALAC_UZORKA, d4$USPESNO_IMUNIZOVAN, correct=TRUE)

chisq.posthoc.test(table(d4$USPESNO_IMUNIZOVAN, d4$USTANOVA_POSILJALAC_UZORKA)
                   , method = "none")
chisq.posthoc.test(table(d4$USPESNO_IMUNIZOVAN, d4$USTANOVA_POSILJALAC_UZORKA)
                   , method = "bonferroni")


d4 %>% 
  filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=USTANOVA_POSILJALAC_UZORKA)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na ustanovu') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Ustanova')

# USPESNO_IMUNIZOVAN i TRAJANJE_VAKCINACIJE
shapiro.test(d4$TRAJANJE_VAKCINACIJE)
wilcox.test(d4$TRAJANJE_VAKCINACIJE ~ d4$USPESNO_IMUNIZOVAN)


d4 %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=TRAJANJE_VAKCINACIJE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Vaccination duration and successful immunization boxplot') + 
  xlab('Successful immunization') +
  ylab('Vaccination duration (days)') 


# USPESNO_IMUNIZOVAN i RAZLIKA_PRIJEMA_I_UZORKOVANJA
shapiro.test(d4$RAZLIKA_PRIJEMA_I_UZORKOVANJA)

wilcox.test(d4$RAZLIKA_PRIJEMA_I_UZORKOVANJA ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_PRIJEMA_I_UZORKOVANJA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na vreme proteklo od prijema do uzorkovanja') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika prijema i uzorkovanja uzorka (dani)')


# USPESNO_IMUNIZOVAN i RAZLIKA_PRIJEMA_I_POCETKA
shapiro.test(d4$RAZLIKA_PRIJEMA_I_POCETKA)

wilcox.test(d4$RAZLIKA_PRIJEMA_I_POCETKA ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_POCETKA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_PRIJEMA_I_POCETKA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu na vreme proteklo od prijema do početka analiza') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika prijema i analize uzorka (dani)')


# USPESNO_IMUNIZOVAN i RAZLIKA_UZORKOVANJA_I_POCETKA
shapiro.test(d4$RAZLIKA_UZORKOVANJA_I_POCETKA)

wilcox.test(d4$RAZLIKA_UZORKOVANJA_I_POCETKA ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_UZORKOVANJA_I_POCETKA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na vreme proteklo od uzorkovanja do početka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika uzorkovanja i analize uzorka (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA
shapiro.test(d4$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)

wilcox.test(d4$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na vreme proteklo od završetka vakcinacije do početka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika završetka vakcinacije i početka analize (dani)')


# USPESNO_IMUNIZOVAN i RAZLIKA_POCETKA_VAK_I_ISPITIVANJA
shapiro.test(d4$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)

wilcox.test(d4$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na vreme proteklo od početka vakcinacije do početka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika početka vakcinacije i početka analize (dani)')

# USPESNO_IMUNIZOVAN i UKUPNO_DANA_OD_VADJENJA_KRVI

shapiro.test(d4$UKUPNO_DANA_OD_VADJENJA_KRVI)

wilcox.test(d4$UKUPNO_DANA_OD_VADJENJA_KRVI ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=UKUPNO_DANA_OD_VADJENJA_KRVI)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na vreme proteklo od vađenja krvi') + 
  xlab('Uspešno imunizovan') +
  ylab('Ukupno dana od vađenja krvi') +
  ylim(0,20)


# USPESNO_IMUNIZOVAN i DAT_SERUM

table(d4$USPESNO_IMUNIZOVAN, d4$DAT_SERUM)

chisq.test(d4$DAT_SERUM, d4$USPESNO_IMUNIZOVAN, correct=FALSE)

d4 %>% 
  filter(!is.na(DAT_SERUM)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=DAT_SERUM)) +
  geom_bar() + ggtitle('Uspesnost imunizacije u odnosu na davanje seruma') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Dat serum')


# USPESNO_IMUNIZOVAN i BROJ JEDINICA
shapiro.test(d4$BROJ_JEDINICA)
wilcox.test(d4$BROJ_JEDINICA ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(BROJ_JEDINICA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=BROJ_JEDINICA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na broj jedinica') + 
  xlab('Uspešno imunizovan') +
  ylab('Broj jedinica')

# USPESNO_IMUNIZOVAN i LOKACIJA_OZLEDA

table(d4$USPESNO_IMUNIZOVAN, d4$LOKACIJA_OZLEDA)

chisq.test(d4$USPESNO_IMUNIZOVAN, d4$LOKACIJA_OZLEDA, correct = TRUE)

d4 %>% 
  filter(!is.na(LOKACIJA_OZLEDA)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=LOKACIJA_OZLEDA)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na lokaciju ozlede') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Lokacija ozleda')


# USPESNO_IMUNIZOVAN i ŽIVOTINJA

table(d4$USPESNO_IMUNIZOVAN, d4$ŽIVOTINJA)

chisq.test(d4$USPESNO_IMUNIZOVAN, d4$ŽIVOTINJA, correct = TRUE)

d4 %>% 
  filter(!is.na(ŽIVOTINJA)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=ŽIVOTINJA)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na životinju') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Životinja')

# USPESNO_IMUNIZOVAN i LOT_VAKCINE
table(d4$LOT_VAKCINE)
prop.table(table(d4$LOT_VAKCINE))

table(d4$USPESNO_IMUNIZOVAN, d4$LOT_VAKCINE)

chisq.test(d4$LOT_VAKCINE, d4$USPESNO_IMUNIZOVAN, correct=TRUE)

d4 %>% 
  filter(!is.na(LOT_VAKCINE)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=LOT_VAKCINE)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na lot vakcine') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Lot vakcine')

# USPESNO_IMUNIZOVAN i TITER
shapiro.test(d4$TITER)

wilcox.test(d4$TITER ~ d4$USPESNO_IMUNIZOVAN)

d4 %>%
  filter(!is.na(TITER)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=TITER)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na potenciju vakcine') + 
  xlab('Uspešno imunizovan') +
  ylab('Potencija vakcine')

# REZULTAT_ANALIZE i GODINA

shapiro.test(d4$REZULTAT_ANALIZE)

kruskal.test(d4$REZULTAT_ANALIZE ~ d4$GODINA)

#install.packages('FSA')
library(FSA)

d4 %>%
  filter(!is.na(GODINA)) %>% filter(REZULTAT_ANALIZE < 15) %>%
  ggplot(aes(x= as.factor(GODINA), y=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na godinu') + 
  xlab('Godina') +
  ylab('Rezultat analize') +
  ylim(0, 8)


# REZULTAT_ANALIZE i POL

wilcox.test(d4$REZULTAT_ANALIZE ~ d4$POL)


d4 %>%
  filter(!is.na(POL)) %>%
  ggplot(aes(x= POL, y=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na pol') + 
  xlab('Pol') +
  ylab('Rezultat analize') +
  ylim(0,8)


# REZULTAT_ANALIZE i STAROST

cor.test(d4$REZULTAT_ANALIZE, d4$STAROST, method='pearson')

d4 %>%
  filter(!is.na(GODINA_ROĐENJA)) %>%
 # filter(REZULTAT_ANALIZE < 15) %>% 
  ggplot(aes(x= STAROST, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Skater dijagram Starosti i Rezultata analize') + 
  xlab('Starost') +
  ylab('Rezultat analize') +
  ylim(0,15)

# REZULTAT_ANALIZE i USTANOVA_POSILJALAC_UZORKA

kruskal.test(d4$REZULTAT_ANALIZE ~ d4$USTANOVA_POSILJALAC_UZORKA)

pairwise.wilcox.test(d4$REZULTAT_ANALIZE, d4$USTANOVA_POSILJALAC_UZORKA,
                     p.adjust.method = "BH")



d4 %>%
  #filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>%
  ggplot(aes(y= USTANOVA_POSILJALAC_UZORKA, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na ustanovu') + 
  xlab('Rezultat analize') +
  ylab('Ustanova') + 
  xlim(0, 10)


# REZULTAT_ANALIZE i TRAJANJE_VAKCINACIJE

cor.test(d4$REZULTAT_ANALIZE, d4$TRAJANJE_VAKCINACIJE, method='pearson')

d4 %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>%
  ggplot(aes(x= TRAJANJE_VAKCINACIJE, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i trajanja vakcinacije') + 
  xlab('Trajanje vakcinacije (dani)') +
  ylab('Rezultat analize')


# REZULTAT_ANALIZE i RAZLIKA_PRIJEMA_I_UZORKOVANJA

cor.test(d4$REZULTAT_ANALIZE, d4$RAZLIKA_PRIJEMA_I_UZORKOVANJA, method='pearson')


d4 %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>%
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_UZORKOVANJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike prijema i uzorkovanja') + 
  xlab('Razlika prijema i uzorkovanja (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 15)



# REZULTAT_ANALIZE i RAZLIKA_PRIJEMA_I_POCETKA

cor.test(d4$REZULTAT_ANALIZE, d4$RAZLIKA_PRIJEMA_I_POCETKA, method='pearson')

d4 %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_POCETKA)) %>%
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_POCETKA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike prijema i početka analiza') + 
  xlab('Razlika prijema i početka (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 30)


# REZULTAT_ANALIZE i RAZLIKA_UZORKOVANJA_I_POCETKA

cor.test(d4$REZULTAT_ANALIZE, d4$RAZLIKA_UZORKOVANJA_I_POCETKA, method='pearson')


d4 %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>%
  ggplot(aes(x= RAZLIKA_UZORKOVANJA_I_POCETKA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike uzorkovanja i početka') + 
  xlab('Razlika uzorkovanja i početka (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 15)


# REZULTAT_ANALIZE i RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

cor.test(d4$REZULTAT_ANALIZE, d4$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA, method='pearson')


d4 %>%
  filter(!is.na(RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) %>%
  ggplot(aes(x= RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike završetka vakcinacije i početka analize') + 
  xlab('Razlika završetka vakcinacije i početka analize (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 15)


# REZULTAT_ANALIZE i RAZLIKA_POCETKA_VAK_I_ISPITIVANJA

cor.test(d4$REZULTAT_ANALIZE, d4$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA, method='pearson')

d4 %>%
  filter(!is.na(RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) %>%
  ggplot(aes(x= RAZLIKA_POCETKA_VAK_I_ISPITIVANJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike početka vakcinacije i početka analize') + 
  xlab('Razlika početka vakcinacije i početka analize (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 15)


# REZULTAT_ANALIZE i UKUPNO_DANA_OD_VADJENJA_KRVI

cor.test(d4$REZULTAT_ANALIZE, d4$UKUPNO_DANA_OD_VADJENJA_KRVI, method='pearson')

d4 %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>%
  ggplot(aes(x= UKUPNO_DANA_OD_VADJENJA_KRVI, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i dana protekih od vađenja krvi') + 
  xlab('Ukupno dana od vađenja krvi') +
  ylab('Rezultat analize') +
  ylim(0, 15)

# REZULTAT_ANALIZE i DAT_SERUM

wilcox.test(d4$REZULTAT_ANALIZE ~ d4$DAT_SERUM)


d4 %>%
  filter(!is.na(DAT_SERUM)) %>%
  ggplot(aes(x= DAT_SERUM, y=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na davanje seruma') + 
  xlab('Dat serum') +
  ylab('Rezultat analize') +
  ylim(0, 15)


# REZULTAT_ANALIZE i BROJ_JEDINICA

cor.test(d4$REZULTAT_ANALIZE, d4$BROJ_JEDINICA, method='pearson')

d4 %>%
  filter(!is.na(BROJ_JEDINICA)) %>%
  ggplot(aes(x= BROJ_JEDINICA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize') +
  ylim(0, 15)

# REZULTAT_ANALIZE i LOKACIJA_OZLEDA

kruskal.test(d4$REZULTAT_ANALIZE ~ d4$LOKACIJA_OZLEDA)

d4 %>%
  filter(!is.na(LOKACIJA_OZLEDA)) %>%
  ggplot(aes(y= LOKACIJA_OZLEDA, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u lokaciju ozlede') + 
  xlab('Rezultat analize') +
  ylab('Lokacija ozlede') +
  xlim(0, 11)

# REZULTAT_ANALIZE i BROJ_OZLEDA

cor.test(d4$REZULTAT_ANALIZE, as.numeric(d4$BROJ_OZLEDA), method='pearson')
kruskal.test(d4$REZULTAT_ANALIZE ~ d4$BROJ_OZLEDA)

d4 %>% 
  filter(!is.na(BROJ_OZLEDA)) %>%
  ggplot(aes(x= BROJ_OZLEDA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja ozleda') + 
  xlab('Broj broj ozleda') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i ŽIVOTINJA

kruskal.test(d4$REZULTAT_ANALIZE ~ d4$ŽIVOTINJA)

d4 %>%
  filter(!is.na(ŽIVOTINJA)) %>%
  ggplot(aes(y= ŽIVOTINJA, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na lokaciju ozlede') + 
  xlab('Rezultat analize') + xlim(0, 11) +
  ylab('Lokacija ozlede')

# REZULTAT_ANALIZE i LOT_VAKCINE

kruskal.test(d4$REZULTAT_ANALIZE ~ d4$LOT_VAKCINE)


d4 %>%
  #filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>%
  ggplot(aes(y= LOT_VAKCINE, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na lot vakcine') + 
  xlab('Rezultat analize') +
  ylab('Lot vakcine') + xlim(0, 10)

#REZULTAT ANALIZE i TITER
cor.test(d5$REZULTAT_ANALIZE, d5$TITER, method='pearson')

d4 %>%
  filter(!is.na(TITER)) %>%
  ggplot(aes(x= TITER, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i potencije vakcine') + 
  xlab('Potencija vakcine') +
  ylab('Rezultat analize')


# Analiza rizika za varijablu USPESNO_IMUNIZOVAN

# Provera klase varijabli pre pocetka analize

lapply(d4, class)

#Izmena klase potrebnih varijali
d4$POL <- as.factor(d4$POL)
d4$USPESNO_IMUNIZOVAN <- as.factor(d4$USPESNO_IMUNIZOVAN)
d4$GODINA <- as.factor(d4$GODINA)
d4$MESTO_PACIJENTA <- as.factor(d4$MESTO_PACIJENTA)
d4$USTANOVA_POSILJALAC_UZORKA <- as.factor(d4$USTANOVA_POSILJALAC_UZORKA)
d4$DAT_SERUM <- as.factor(d4$DAT_SERUM)
d4$LOKACIJA_OZLEDA <- as.factor(d4$LOKACIJA_OZLEDA)
d4$ŽIVOTINJA <- as.factor(d4$ŽIVOTINJA)

#Vizualizacija korelacija varijabli razlike radi potencijalnog izbacivanja zbog
# multikolinearnosti

#install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
x <- select(d, contains('RAZLIKA'))
chart.Correlation(x)

#Pravljenje i optimizovanje modela Logisticke regresije

library(MASS)

#Prvi model

model1 <- glm(USPESNO_IMUNIZOVAN ~ STAROST +  
             USTANOVA_POSILJALAC_UZORKA + BROJ_JEDINICA 
             + DAT_SERUM 
             ,family = binomial(link = "logit"), data=d4)
summary(model1)


#Model nakon uklanjanja neznacajnih varijabli
model2 <- glm(USPESNO_IMUNIZOVAN ~ STAROST +  
               USTANOVA_POSILJALAC_UZORKA 
             ,family = binomial(link = "logit"), data=d4)
summary(model2)

#Naredno uklanjanje neznacajnih varijabli
model3 <- glm(USPESNO_IMUNIZOVAN ~ STAROST
             ,family = binomial(link = "logit"), data=d4)
summary(model3)

#Odredjivanje metrike modela

library(caret)

set.seed(123)
train.indices <- createDataPartition(d4$USPESNO_IMUNIZOVAN, p=0.8, list = FALSE)
train.indices

traindata <- d[train.indices,]
testdata <- d[-train.indices,]
traindata$USPESNO_IMUNIZOVAN <- as.character(traindata$USPESNO_IMUNIZOVAN)
traindata$USPESNO_IMUNIZOVAN <- replace(traindata$USPESNO_IMUNIZOVAN, 
                                which(traindata$USPESNO_IMUNIZOVAN == "Da"), 1)
traindata$USPESNO_IMUNIZOVAN <- replace(traindata$USPESNO_IMUNIZOVAN, 
                                which(traindata$USPESNO_IMUNIZOVAN == "Ne"), 0)
traindata$USPESNO_IMUNIZOVAN <- as.numeric(traindata$USPESNO_IMUNIZOVAN)
traindata$USPESNO_IMUNIZOVAN

testdata$USPESNO_IMUNIZOVAN <- as.character(testdata$USPESNO_IMUNIZOVAN)
testdata$USPESNO_IMUNIZOVAN <- replace(testdata$USPESNO_IMUNIZOVAN, 
                               which(testdata$USPESNO_IMUNIZOVAN == "Da"), 1)
testdata$USPESNO_IMUNIZOVAN <- replace(testdata$USPESNO_IMUNIZOVAN, 
                               which(testdata$USPESNO_IMUNIZOVAN == "Ne"), 0)
testdata$USPESNO_IMUNIZOVAN <- as.numeric(testdata$USPESNO_IMUNIZOVAN)
testdata$USPESNO_IMUNIZOVAN

# Funckija compute.eval.metrics za racunanje metrike predikcije logisticke
# regresije

compute.eval.metrics <- function(cm) {
  TP <- cm[2,2]
  TN <- cm[1,1]
  FP <- cm[1,2]
  FN <- cm[2,1]
  acc <- (TP + TN) / (TP + TN + FP + FN)
  prec <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1 <- 2 * (prec * recall) / (prec + recall)
  c(accuracy = acc, precision = prec, recall = recall, f1 = f1)
}

#Racunanje metrike modela



model2 <- glm(USPESNO_IMUNIZOVAN ~ STAROST + USTANOVA_POSILJALAC_UZORKA
              , family = binomial(link = "logit"), data=traindata)
summary(model1)

model3 <- glm(USPESNO_IMUNIZOVAN ~ STAROST
              ,family = binomial(link = "logit"), data=traindata)
summary(model3)

model4<- glm(USPESNO_IMUNIZOVAN ~ USTANOVA_POSILJALAC_UZORKA
              ,family = binomial(link = "logit"), data=traindata)
summary(model4)



#Metrika drugog modela
model2predict <- predict(model2, newdata = testdata)
model2predict <- ifelse(model2predict > 0.5,1,0)
model2predict

model2.cm <- table(prave.vrednosti=testdata$USPESNO_IMUNIZOVAN,
                 predvidjene.vrednosti=model2predict)
model2.cm 
compute.eval.metrics(model2.cm)

#Metrika treceg modela
model3predict <- predict(model3, newdata = testdata)
model3predict <- ifelse(model3predict > 0.5,1,0)
model3predict

model3.cm <- table(prave.vrednosti=testdata$USPESNO_IMUNIZOVAN,
                   predvidjene.vrednosti=model3predict)
model3.cm 
compute.eval.metrics(model3.cm)

#Metrika cetvrtog modela
model4predict <- predict(model4, newdata = testdata)
model4predict <- ifelse(model4predict > 0.5,1,0)
model4predict

model4.cm <- table(prave.vrednosti=testdata$USPESNO_IMUNIZOVAN,
                   predvidjene.vrednosti=model4predict)
model4.cm 
compute.eval.metrics(model4.cm)
#Poboljsavanje modela i konacno racuanje metrike

model3 <- glm(USPESNO_IMUNIZOVAN ~ STAROST +USTANOVA_POSILJALAC_UZORKA 
              ,family = binomial(link = "logit"), data=traindata)
summary(model3)

model3predict <- predict(model3, newdata = testdata)
model3predict <- ifelse(model3predict > 0.5,1,0)
model3predict

model3.cm <- table(prave.vrednosti=testdata$USPESNO_IMUNIZOVAN,
                   predvidjene.vrednosti=model3predict)
model3.cm 
compute.eval.metrics(model3.cm)


model3 <- glm(USPESNO_IMUNIZOVAN ~ USTANOVA_POSILJALAC_UZORKA+ STA
              ,family = binomial(link = "logit"), data=traindata)
summary(model3)

#install.packages("writexl")
library("writexl")
write_xlsx(d4,"baza_d.xlsx")

ll.null <- model3$null.deviance/-2
ll.proposed <- model3$deviance/-2
(ll.null - ll.proposed) / ll.null # McFadden pseudo R2


1 - pchisq(2*(ll.proposed - ll.null), df=(length(model1$coefficients)-1))

#install.packages("rms")
library(rms)
model1lrm <- lrm(USPESNO_IMUNIZOVAN ~ STAROST + USTANOVA_POSILJALAC_UZORKA
              , y=TRUE, x=TRUE, data=traindata)
model1lrm

#install.packages("oddsratio")
library(oddsratio)                              

or_glm(data = traindata, model = model3, incr = list(STAROST = 1))

mar <- glm(USPESNO_IMUNIZOVAN ~ GODINA + STAROST +# USTANOVA_POSILJALAC_UZORKA +
          DAT_SERUM  , family = binomial(link = "logit"), data=d4)
summary(mar)
