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
                                d$LOKACIJA_OZLEDA == 'S019' |
                                 d$LOKACIJA_OZLEDA == 'PAS'),
                         NA)


d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA =="USNA"), "USNE")
                         
d$LOKACIJA_OZLEDA <- replace(d$LOKACIJA_OZLEDA,
                             which(d$LOKACIJA_OZLEDA == "NATKOLENIC??" |
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


###Vizualizacija varijabli###

library(ggplot2)
library(psych)
describe(d)


#Vizuelni prikaz variajble pol na uzorku

table(d$POL)

d %>%
  filter(!is.na(POL)) %>%
  ggplot(aes(x= POL)) +
  geom_bar() + ggtitle('Prikaz varijable pol na uzorku') + xlab('Pol') +
  ylab('Broj')

#Prikaz varijable GODINA

table(d$GODINA)

ggplot(d, aes(x=GODINA)) + geom_bar() + 
  ggtitle('Prikaz varijable godina na uzorku') + xlab('Godina') + ylab('Broj')

#Prikaz varijable GODINA_ROÐENJA

table(d$GODINA_ROÐENJA)

d %>%
  filter(!is.na(GODINA_ROÐENJA)) %>%
  ggplot(aes(x= GODINA_ROÐENJA)) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  ggtitle('Histogram godina roðenja pacijenata') + xlab('Godina') + 
  ylab('Frekvencija')

ks.test(d$GODINA_ROÐENJA, 'pnorm')

#Prikaz varijable USTANOVA_POSILJALAC_UZORKA

table(d$USTANOVA_POSILJALAC_UZORKA)

d %>%
  filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>%
  ggplot(aes(y= USTANOVA_POSILJALAC_UZORKA)) +
  geom_bar() + ggtitle('Prikaz varijable ustanove pošiljalca uzorka') + xlab('Frekvencija') +
  ylab('Ustanova pošiljalac uzorka')

#Prikaz varijable TRAJANJE_VAKCINACIJE

d %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>% 
  #filter(TRAJANJE_VAKCINACIJE < 100) %>% 
  ggplot(aes(x= remove_outliers(TRAJANJE_VAKCINACIJE))) + 
  geom_histogram(bins=10) + 
  ggtitle('Trajanje vakcinacije') + xlab('Trajanje vakcinacije (dani)') + 
  ylab('Frekvencija')

ks.test(d$TRAJANJE_VAKCINACIJE, 'pnorm')

#Prikaz varijable RAZLIKA_ZAVRŠETKA_I_BUSTERA

d %>%
  filter(!is.na(RAZLIKA_ZAVRŠETKA_I_BUSTERA)) %>% 
  ggplot(aes(x= RAZLIKA_ZAVRŠETKA_I_BUSTERA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika zavrsetka vakcinacije i buster doze') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

ks.test(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA, 'pnorm')

#Prikaz varijable RAZLIKA_PRIJEMA_I_UZORKOVANJA

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>% 
  filter(RAZLIKA_PRIJEMA_I_UZORKOVANJA < 100) %>% 
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_UZORKOVANJA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika prijema i uzorkovanja') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

ks.test(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA, 'pnorm')

#Prikaz varijable RAZLIKA_UZORKOVANJA_I_POCETKA

d %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>% 
  filter(RAZLIKA_UZORKOVANJA_I_POCETKA < 100) %>% 
  ggplot(aes(x= RAZLIKA_UZORKOVANJA_I_POCETKA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika uzorkovanja i pocetka') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

ks.test(d$RAZLIKA_UZORKOVANJA_I_POCETKA, 'pnorm')

# Prikaz varijable UKUPNO_DANA_OD_VADJENJA_KRVI

sum(is.na(d$UKUPNO_DANA_OD_VADJENJA_KRVI))

d %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>% 
  ggplot(aes(x= UKUPNO_DANA_OD_VADJENJA_KRVI)) + 
  geom_histogram(bins=10) + 
  ggtitle('Ukupno dana od vaðenja krvi') + xlab('Dani') + 
  ylab('Frekvencija')

ks.test(d$UKUPNO_DANA_OD_VADJENJA_KRVI, 'pnorm')

#Prikaz varijable REZULTAT_ANALIZE
d %>% 
  filter(REZULTAT_ANALIZE < 50) %>% 
  ggplot(aes(x=REZULTAT_ANALIZE)) + geom_histogram() + 
  ggtitle('Razultat analize') + xlab('Rezultat analize') + 
  ylab('Frekvencija')

describe(d$REZULTAT_ANALIZE)

#Prikaz varijable USPESNO_IMUNIZOVAN

ggplot(d, aes(x=USPESNO_IMUNIZOVAN)) + geom_bar() + 
  ggtitle('Uspešnost imunizacije') + xlab('Uspešno imunizovan') + 
  ylab('Frekvencija')

table(d$USPESNO_IMUNIZOVAN)

#Prikaz varijable DAT_SERUM


d %>%
  filter(!is.na(DAT_SERUM)) %>% 
  ggplot(aes(x= DAT_SERUM)) + 
  geom_bar() + 
  ggtitle('Histogram davanja seruma') + xlab('Dat serum') + 
  ylab('Frekvencija')

table(d$DAT_SERUM)

#Prikaz varijable BROJ_JEDINICA

sum(is.na(d$BROJ_JEDINICA))

d %>%
  filter(!is.na(BROJ_JEDINICA)) %>% 
  filter(as.numeric(BROJ_JEDINICA) < 5000) %>% 
  ggplot(aes(x= as.numeric(BROJ_JEDINICA))) + 
  geom_histogram(bins=30) + 
  ggtitle('Broj jedinica') + xlab('Broj jedinica') + 
  ylab('Frekvencija')

ks.test(as.numeric(d$BROJ_JEDINICA), 'pnorm')

#Prikaz varijable SERIJA_SERUMA

table(d$SERIJA_SERUMA)

#Prikaz varijable LOKACIJA_OZLEDA

sum(is.na(d$LOKACIJA_OZLEDA))

table(d$LOKACIJA_OZLEDA)

#Prikaz varijable BROJ_OZLEDA

sum(is.na(d$BROJ_OZLEDA))

table(d$BROJ_OZLEDA)

d %>%
  filter(!is.na(BROJ_OZLEDA)) %>% 
  ggplot(aes(x= BROJ_OZLEDA)) + 
  geom_bar() + 
  ggtitle('Prikaz broja ozleda') + xlab('Broj ozleda') + 
  ylab('Frekvencija')

#Prikaz varijable ŽIVOTINJA

sum(is.na(d$ŽIVOTINJA))

table(d$ŽIVOTINJA)

d %>%
  filter(!is.na(ŽIVOTINJA)) %>% 
  ggplot(aes(x= ŽIVOTINJA)) + 
  geom_bar() + 
  ggtitle('Broj ozleda nastao od odreðenih životinja') + xlab('Životinja') + 
  ylab('Frekvencija')

#Analiza pojedinacnih nezavisnih varijabli sa zavisnim

# USPESNO_IMUNIZOVAN i POL

table(d$USPESNO_IMUNIZOVAN, d$POL)

chisq.test(d$POL, d$USPESNO_IMUNIZOVAN, correct=FALSE)

d %>% 
  filter(!is.na(POL)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=POL)) +
  geom_bar() + ggtitle('Odnos pola i uspešnosti imunizacije') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Pol')

# USPESNO_IMUNIZOVAN i GODINA_ROÐENJA

t.test(d$GODINA_ROÐENJA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=GODINA_ROÐENJA)) +
  geom_boxplot(outlier.shape = NA) + ggtitle('Uspešnost imunizacije u odnosu na godinu roðenja') + 
  xlab('Uspešno imunizovan') +
  ylab('Godina roðenja') + 
  annotate(geom="text", x=1.5, y=2010, label="p < 0.01 **")

# USPESNO_IMUNIZOVAN i USTANOVA_POSILJALAC_UZORKA

table(d$USPESNO_IMUNIZOVAN, d$USTANOVA_POSILJALAC_UZORKA)

chisq.test(d$USTANOVA_POSILJALAC_UZORKA, d$USPESNO_IMUNIZOVAN, correct=TRUE)

d %>% 
  filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=USTANOVA_POSILJALAC_UZORKA)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na ustanovu') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Ustanova')

# USPESNO_IMUNIZOVAN i TRAJANJE_VAKCINACIJE

t.test(d$TRAJANJE_VAKCINACIJE ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>%
  filter(TRAJANJE_VAKCINACIJE < 1000) %>% 
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=TRAJANJE_VAKCINACIJE)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu na trajanje vakcinacije') + 
  xlab('Uspešno imunizovan') +
  ylab('Trajanje vakcinacije (dani)')

 
# USPESNO_IMUNIZOVAN i RAzLIKA_ZAVRSETKA_I_BUSTERA 

t.test(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_ZAVRŠETKA_I_BUSTERA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_ZAVRŠETKA_I_BUSTERA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od završetka vakcinacije do buster doze') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika završetka vakcinacije i buster doze (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_PRIJEMA_I_UZORKOVANJA

t.test(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>%
  filter(RAZLIKA_PRIJEMA_I_UZORKOVANJA < 50) %>% 
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_PRIJEMA_I_UZORKOVANJA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od prijema do uzorkovanja') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika prijema i uzorkovanja uzorka (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_PRIJEMA_I_POCETKA

t.test(d$RAZLIKA_PRIJEMA_I_POCETKA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_POCETKA)) %>%
  filter(RAZLIKA_PRIJEMA_I_POCETKA < 14) %>% 
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_PRIJEMA_I_POCETKA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od prijema do poèetka analiza') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika prijema i analize uzorka (dani)')

# USPENO_IMUNIZOVAN i RAZLIKA_UZORKOVANJA_I_POCETKA

t.test(d$RAZLIKA_UZORKOVANJA_I_POCETKA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>%
  filter(RAZLIKA_UZORKOVANJA_I_POCETKA < 50) %>% 
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_UZORKOVANJA_I_POCETKA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od uzorkovanja do poèetka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika uzorkovanja i analize uzorka (dani)')


# USPESNO_IMUNIZOVAN i UKUPNO_DANA_OD_VADJENJA_KRVI

t.test(d$UKUPNO_DANA_OD_VADJENJA_KRVI ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>%
  filter(UKUPNO_DANA_OD_VADJENJA_KRVI < 20) %>% 
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=UKUPNO_DANA_OD_VADJENJA_KRVI)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od vaðenja krvi') + 
  xlab('Uspešno imunizovan') +
  ylab('Ukupno dana od vaðenja krvi')

# USPESNO_IMUNIZOVAN i DAT_SERUM

table(d$USPESNO_IMUNIZOVAN, d$DAT_SERUM)

chisq.test(d$DAT_SERUM, d$USPESNO_IMUNIZOVAN, correct=TRUE)

d %>% 
  filter(!is.na(DAT_SERUM)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=DAT_SERUM)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na davanje seruma') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Dat serum')


# USPESNO_IMUNIZOVAN i BROJ JEDINICA

d$BROJ_JEDINICA <- as.numeric(d$BROJ_JEDINICA)

t.test(d$BROJ_JEDINICA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(BROJ_JEDINICA)) %>%
  filter(BROJ_JEDINICA < 5000) %>% 
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=BROJ_JEDINICA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu na broj jedinica') + 
  xlab('Uspešno imunizovan') +
  ylab('Broj jedinica')

# USPESNO_IMUNIZOVAN i LOKACIJA_OZLEDA

table(d$USPESNO_IMUNIZOVAN, d$LOKACIJA_OZLEDA)

chisq.test(d$USPESNO_IMUNIZOVAN, d$LOKACIJA_OZLEDA, correct = TRUE)

d %>% 
  filter(!is.na(LOKACIJA_OZLEDA)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=LOKACIJA_OZLEDA)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na lokaciju ozlede') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Lokacija ozleda')


# USPESNO_IMUNIZOVAN i ŽIVOTINJA

table(d$USPESNO_IMUNIZOVAN, d$ŽIVOTINJA)

chisq.test(d$USPESNO_IMUNIZOVAN, d$ŽIVOTINJA, correct = TRUE)

d %>% 
  filter(!is.na(ŽIVOTINJA)) %>% 
  ggplot(aes(x=USPESNO_IMUNIZOVAN, fill=ŽIVOTINJA)) +
  geom_bar() + ggtitle('Uspešnost imunizacije u odnosu na životinju') +
  xlab('Uspešno imunizovan') + ylab('Frekvencija') +
  scale_fill_discrete('Životinja')

# REZULTAT_ANALIZE i GODINA

summary(aov(d$REZULTAT_ANALIZE ~ d$GODINA))

d %>%
  filter(!is.na(GODINA)) %>% filter(REZULTAT_ANALIZE < 15) %>%
  ggplot(aes(x= as.factor(GODINA), y=REZULTAT_ANALIZE)) +
  geom_boxplot() + 
  ggtitle('Rezultat analize u odnosu na godinu') + 
  xlab('Godina') +
  ylab('Rezultat analize')


# REZULTAT_ANALIZE i POL

t.test(d$REZULTAT_ANALIZE ~ d$POL)

d %>%
  filter(!is.na(POL)) %>%
  filter(REZULTAT_ANALIZE < 10) %>% 
  ggplot(aes(x= POL, y=REZULTAT_ANALIZE)) +
  geom_boxplot() + 
  ggtitle('Rezultat analize u odnosu na pol') + 
  xlab('Pol') +
  ylab('Rezultat analize')


# REZULTAT_ANALIZE i GODINA_ROÐENJA

cor.test(d$REZULTAT_ANALIZE, d$GODINA_ROÐENJA, method='pearson')

d %>%
  filter(!is.na(GODINA_ROÐENJA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  ggplot(aes(x= GODINA_ROÐENJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i godine roðenja') + 
  xlab('Godina roðenja') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i USTANOVA_POSILJALAC_UZORKA

kruskal.test(remove_outliers(d$REZULTAT_ANALIZE) ~ d$USTANOVA_POSILJALAC_UZORKA)
kruskal.test(d$REZULTAT_ANALIZE ~ d$USTANOVA_POSILJALAC_UZORKA)

pairwise.wilcox.test(remove_outliers(d$REZULTAT_ANALIZE), d$USTANOVA_POSILJALAC_UZORKA,
                     p.adjust.method = "BH")


d %>%
  filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>%
  filter(REZULTAT_ANALIZE < 10) %>% 
  ggplot(aes(y= USTANOVA_POSILJALAC_UZORKA, x=REZULTAT_ANALIZE)) +
  geom_boxplot() + 
  ggtitle('Rezultat analize u odnosu na ustanovu') + 
  xlab('Rezultat analize') +
  ylab('Ustanova')

# REZULTAT_ANALIZE i TRAJANJE_VAKCINACIJE

cor.test(d$REZULTAT_ANALIZE, d$TRAJANJE_VAKCINACIJE, method='pearson')

d %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% filter(TRAJANJE_VAKCINACIJE < 100) %>% 
  ggplot(aes(x= TRAJANJE_VAKCINACIJE, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i trajanja vakcinacije') + 
  xlab('Trajanje vakcinacije (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i RAZLIKA_ZAVRŠETKA_I_BUSTERA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_ZAVRŠETKA_I_BUSTERA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_ZAVRŠETKA_I_BUSTERA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% #filter(RAZLIKA_ZAVRŠETKA_I_BUSTERA < 100) %>% 
  ggplot(aes(x= RAZLIKA_ZAVRŠETKA_I_BUSTERA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike završetka i buster doze') + 
  xlab('Razlika završetka i bustera (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i RAZLIKA_PRIJEMA_I_UZORKOVANJA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_PRIJEMA_I_UZORKOVANJA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  filter(RAZLIKA_PRIJEMA_I_UZORKOVANJA < 150) %>% 
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_UZORKOVANJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike prijema i uzorkovanja') + 
  xlab('Razlika prijema i uzorkovanja (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i RAZLIKA_PRIJEMA_I_POCETKA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_PRIJEMA_I_POCETKA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_POCETKA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  filter(RAZLIKA_PRIJEMA_I_POCETKA < 150) %>% 
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_POCETKA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike prijema i poèetka') + 
  xlab('Razlika prijema i poèetka (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i RAZLIKA_UZORKOVANJA_I_POCETKA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_UZORKOVANJA_I_POCETKA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  filter(RAZLIKA_UZORKOVANJA_I_POCETKA < 150) %>% 
  ggplot(aes(x= RAZLIKA_UZORKOVANJA_I_POCETKA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike uzorkovanja i poèetka') + 
  xlab('Razlika uzorkovanja i poèetka (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i UKUPNO_DANA_OD_VADJENJA_KRVI

cor.test(d$REZULTAT_ANALIZE, d$UKUPNO_DANA_OD_VADJENJA_KRVI, method='pearson')

d %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  ggplot(aes(x= UKUPNO_DANA_OD_VADJENJA_KRVI, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i dana protekih od vaðenja krvi') + 
  xlab('Ukupno dana od vaðenja krvi') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i DAT_SERUM

t.test(d$REZULTAT_ANALIZE ~ d$DAT_SERUM)

d %>%
  filter(!is.na(DAT_SERUM)) %>%
  filter(REZULTAT_ANALIZE < 10) %>% 
  ggplot(aes(x= DAT_SERUM, y=REZULTAT_ANALIZE)) +
  geom_boxplot() + 
  ggtitle('Rezultat analize u odnosu na pol') + 
  xlab('Dat serum') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i BROJ_JEDINICA

cor.test(d$REZULTAT_ANALIZE, d$BROJ_JEDINICA, method='pearson')

d %>%
  filter(!is.na(BROJ_JEDINICA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  filter(BROJ_JEDINICA < 9000) %>% 
  ggplot(aes(x= BROJ_JEDINICA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i BROJ_JEDINICA

kruskal.test(d$REZULTAT_ANALIZE ~ d$LOKACIJA_OZLEDA)

d %>%
  filter(!is.na(LOKACIJA_OZLEDA)) %>%
  filter(REZULTAT_ANALIZE < 7.5) %>% 
  ggplot(aes(y= LOKACIJA_OZLEDA, x=REZULTAT_ANALIZE)) +
  geom_boxplot() + 
  ggtitle('Rezultat analize u lokaciju ozlede') + 
  xlab('Rezultat analize') +
  ylab('Lokacija ozlede')

# REZULTAT_ANALIZE i BROJ_OZLEDA

cor.test(d$REZULTAT_ANALIZE, as.numeric(d$BROJ_OZLEDA), method='pearson')

d %>% 
  filter(!is.na(BROJ_OZLEDA)) %>%
  #filter(REZULTAT_ANALIZE < 15) %>% 
  #filter(BROJ_OZLEDA < 9000) %>% 
  ggplot(aes(x= BROJ_OZLEDA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i ŽIVOTINJA

kruskal.test(d$REZULTAT_ANALIZE ~ d$ŽIVOTINJA)

d %>%
  filter(!is.na(ŽIVOTINJA)) %>%
  filter(REZULTAT_ANALIZE < 25) %>% 
  ggplot(aes(y= ŽIVOTINJA, x=REZULTAT_ANALIZE)) +
  geom_boxplot() + 
  ggtitle('Rezultat analize u lokaciju ozlede') + 
  xlab('Rezultat analize') +
  ylab('Lokacija ozlede')



"""
d %>% 
  filter(!d$REZULTAT_ANALIZE %in% boxplot.stats(d$REZULTAT_ANALIZE)$out == TRUE) %>% 
  filter(!d$BROJ_OZLEDA %in% boxplot.stats(d$BROJ_OZLEDA)$out == TRUE) %>% 
  cor.test(REZULTAT_ANALIZE, as.numeric(d$BROJ_OZLEDA), method='pearson')
  
  # REZULTAT_ANALIZE i BROJ_OZLEDA
  
  cor.test(d$REZULTAT_ANALIZE, as.numeric(d$BROJ_OZLEDA), method='pearson')

d %>% 
  filter(!is.na(BROJ_OZLEDA)) %>%
  #filter(REZULTAT_ANALIZE < 15) %>% 
  #filter(BROJ_OZLEDA < 9000) %>% 
  ggplot(aes(x= BROJ_OZLEDA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize')
"""

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

remove_outliers(d$REZULTAT_ANALIZE)
  cor.test(d$REZULTAT_ANALIZE, as.numeric(d$BROJ_OZLEDA), method='pearson')

cor.test(remove_outliers(d$REZULTAT_ANALIZE), remove_outliers(as.numeric(d$BROJ_OZLEDA)), method='pearson')

d %>% 
  filter(!is.na(BROJ_OZLEDA)) %>%
  #filter(REZULTAT_ANALIZE < 15) %>% 
  #filter(BROJ_OZLEDA < 9000) %>% 
  ggplot(aes(x= BROJ_OZLEDA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i ŽIVOTINJA

kruskal.test(d$REZULTAT_ANALIZE ~ d$ŽIVOTINJA)

