library(readxl)
library(ggplot2)

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

sum(is.na(d$GODINA_ROĐENJA))
d$GODINA_ROĐENJA <- remove_outliers(d$GODINA_ROĐENJA)

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

#Pretvaranje varijable BROJ_OZLEDA u numeričku

d$BROJ_OZLEDA <- as.numeric(d$BROJ_OZLEDA)

###Vizualizacija varijabli###

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

#Prikaz varijable GODINA_ROĐENJA

table(d$GODINA_ROĐENJA)

d %>%
  filter(!is.na(GODINA_ROĐENJA)) %>%
  ggplot(aes(x= GODINA_ROĐENJA)) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  ggtitle('Histogram godina rođenja pacijenata') + xlab('Godina') + 
  ylab('Frekvencija')


shapiro.test(d$GODINA_ROĐENJA)

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
  ggplot(aes(x= TRAJANJE_VAKCINACIJE)) + 
  geom_histogram(bins=10) + 
  ggtitle('Trajanje vakcinacije') + xlab('Trajanje vakcinacije (dani)') + 
  ylab('Frekvencija')

shapiro.test(d$TRAJANJE_VAKCINACIJE)

#Prikaz varijable RAZLIKA_ZAVRŠETKA_I_BUSTERA

d %>%
  filter(!is.na(RAZLIKA_ZAVRŠETKA_I_BUSTERA)) %>% 
  ggplot(aes(x= RAZLIKA_ZAVRŠETKA_I_BUSTERA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika zavrsetka vakcinacije i buster doze') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA)

#Prikaz varijable RAZLIKA_PRIJEMA_I_UZORKOVANJA

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>% 
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_UZORKOVANJA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika prijema i uzorkovanja') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA)

#Prikaz varijable RAZLIKA_UZORKOVANJA_I_POCETKA

d %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>% 
  ggplot(aes(x= RAZLIKA_UZORKOVANJA_I_POCETKA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika uzorkovanja i početka') + xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d$RAZLIKA_UZORKOVANJA_I_POCETKA)

#Prikaz varijable RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

d %>%
  filter(!is.na(RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) %>% 
  ggplot(aes(x= RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika završetka vakcinacije i početka analize') + 
  xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)

#Prikaz varijable RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

d %>%
  filter(!is.na(RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) %>% 
  ggplot(aes(x= RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) + 
  geom_histogram(bins=10) + 
  ggtitle('Razlika početka vakcinacije i početka analize') + 
  xlab('Razlika (dani)') + 
  ylab('Frekvencija')

shapiro.test(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)

# Prikaz varijable UKUPNO_DANA_OD_VADJENJA_KRVI

sum(is.na(d$UKUPNO_DANA_OD_VADJENJA_KRVI))

d %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>% 
  ggplot(aes(x= UKUPNO_DANA_OD_VADJENJA_KRVI)) + 
  geom_histogram(bins=10) + 
  ggtitle('Ukupno dana od vađenja krvi') + xlab('Dani') + 
  ylab('Frekvencija')

shapiro.test(d$UKUPNO_DANA_OD_VADJENJA_KRVI)

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
  ggplot(aes(x= as.numeric(BROJ_JEDINICA))) + 
  geom_histogram(bins=30) + 
  ggtitle('Broj jedinica') + xlab('Broj jedinica') + 
  ylab('Frekvencija')


shapiro.test(d$BROJ_JEDINICA)

#Prikaz varijable SERIJA_SERUMA

table(d$SERIJA_SERUMA)

#Prikaz varijable LOKACIJA_OZLEDA

sum(is.na(d$LOKACIJA_OZLEDA))

table(d$LOKACIJA_OZLEDA)

d %>%
  filter(!is.na(LOKACIJA_OZLEDA)) %>%
  ggplot(aes(y= LOKACIJA_OZLEDA)) +
  geom_bar() + ggtitle('Prikaz varijable ustanove pošiljalca uzorka') + 
  xlab('Frekvencija') +
  ylab('Ustanova pošiljalac uzorka')

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
  ggplot(aes(y= ŽIVOTINJA)) + 
  geom_bar() + 
  ggtitle('Broj ozleda nastao od određenih životinja') + xlab('Životinja') + 
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

# USPESNO_IMUNIZOVAN i GODINA_ROĐENJA

wilcox.test(d$GODINA_ROĐENJA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=GODINA_ROĐENJA)) +
  geom_boxplot(outlier.shape = NA) + ggtitle('Uspešnost imunizacije u odnosu na godinu rođenja') + 
  xlab('Uspešno imunizovan') +
  ylab('Godina rođenja') + 
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

wilcox.test(d$TRAJANJE_VAKCINACIJE ~ d$USPESNO_IMUNIZOVAN)


d %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=TRAJANJE_VAKCINACIJE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu na trajanje vakcinacije') + 
  xlab('Uspešno imunizovan') +
  ylab('Trajanje vakcinacije (dani)') 

# USPESNO_IMUNIZOVAN i RAzLIKA_ZAVRSETKA_I_BUSTERA 

wilcox.test(d$RAZLIKA_ZAVRŠETKA_I_BUSTERA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_ZAVRŠETKA_I_BUSTERA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_ZAVRŠETKA_I_BUSTERA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od završetka vakcinacije do buster doze') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika završetka vakcinacije i buster doze (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_PRIJEMA_I_UZORKOVANJA

wilcox.test(d$RAZLIKA_PRIJEMA_I_UZORKOVANJA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_PRIJEMA_I_UZORKOVANJA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od prijema do uzorkovanja') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika prijema i uzorkovanja uzorka (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_PRIJEMA_I_POCETKA

wilcox.test(d$RAZLIKA_PRIJEMA_I_POCETKA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_POCETKA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_PRIJEMA_I_POCETKA)) +
  geom_boxplot() + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od prijema do početka analiza') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika prijema i analize uzorka (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_UZORKOVANJA_I_POCETKA

wilcox.test(d$RAZLIKA_UZORKOVANJA_I_POCETKA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_UZORKOVANJA_I_POCETKA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od uzorkovanja do početka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika uzorkovanja i analize uzorka (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

wilcox.test(d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od završetka vakcinacije do početka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika završetka vakcinacije i početka analize (dani)')

# USPESNO_IMUNIZOVAN i RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

wilcox.test(d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od početka vakcinacije do početka analize') + 
  xlab('Uspešno imunizovan') +
  ylab('Razlika početka vakcinacije i početka analize (dani)')


# USPESNO_IMUNIZOVAN i UKUPNO_DANA_OD_VADJENJA_KRVI

wilcox.test(d$UKUPNO_DANA_OD_VADJENJA_KRVI ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=UKUPNO_DANA_OD_VADJENJA_KRVI)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Uspešnost imunizacije u odnosu vreme proteklo od vađenja krvi') + 
  xlab('Uspešno imunizovan') +
  ylab('Ukupno dana od vađenja krvi') +
  ylim(0,20)

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

wilcox.test(d$BROJ_JEDINICA ~ d$USPESNO_IMUNIZOVAN)

d %>%
  filter(!is.na(BROJ_JEDINICA)) %>%
  ggplot(aes(x= USPESNO_IMUNIZOVAN, y=BROJ_JEDINICA)) +
  geom_boxplot(outlier.shape = NA) + 
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
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na godinu') + 
  xlab('Godina') +
  ylab('Rezultat analize') +
  ylim(0, 8)


# REZULTAT_ANALIZE i POL

wilcox.test(d$REZULTAT_ANALIZE ~ d$POL)

d %>%
  filter(!is.na(POL)) %>%
  ggplot(aes(x= POL, y=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na pol') + 
  xlab('Pol') +
  ylab('Rezultat analize') +
  ylim(0,8)


# REZULTAT_ANALIZE i GODINA_ROĐENJA

cor.test(d$REZULTAT_ANALIZE, d$GODINA_ROĐENJA, method='pearson')

d %>%
  filter(!is.na(GODINA_ROĐENJA)) %>%
  filter(REZULTAT_ANALIZE < 15) %>% 
  ggplot(aes(x= GODINA_ROĐENJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i godine rođenja') + 
  xlab('Godina rođenja') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i USTANOVA_POSILJALAC_UZORKA

kruskal.test(d$REZULTAT_ANALIZE ~ d$USTANOVA_POSILJALAC_UZORKA)

pairwise.wilcox.test(d$REZULTAT_ANALIZE, d$USTANOVA_POSILJALAC_UZORKA,
                     p.adjust.method = "BH")


d %>%
  #filter(!is.na(USTANOVA_POSILJALAC_UZORKA)) %>%
  ggplot(aes(y= USTANOVA_POSILJALAC_UZORKA, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na ustanovu') + 
  xlab('Rezultat analize') +
  ylab('Ustanova') + 
  xlim(0, 22.5)

# REZULTAT_ANALIZE i TRAJANJE_VAKCINACIJE


cor.test(d$REZULTAT_ANALIZE, d$TRAJANJE_VAKCINACIJE, method='pearson')

d %>%
  filter(!is.na(TRAJANJE_VAKCINACIJE)) %>%
  ggplot(aes(x= TRAJANJE_VAKCINACIJE, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i trajanja vakcinacije') + 
  xlab('Trajanje vakcinacije (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i RAZLIKA_ZAVRŠETKA_I_BUSTERA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_ZAVRŠETKA_I_BUSTERA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_ZAVRŠETKA_I_BUSTERA)) %>%
  ggplot(aes(x= RAZLIKA_ZAVRŠETKA_I_BUSTERA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike završetka i buster doze') + 
  xlab('Razlika završetka i bustera (dani)') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i RAZLIKA_PRIJEMA_I_UZORKOVANJA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_PRIJEMA_I_UZORKOVANJA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_UZORKOVANJA)) %>%
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_UZORKOVANJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike prijema i uzorkovanja') + 
  xlab('Razlika prijema i uzorkovanja (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 30)

# REZULTAT_ANALIZE i RAZLIKA_PRIJEMA_I_POCETKA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_PRIJEMA_I_POCETKA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_PRIJEMA_I_POCETKA)) %>%
  ggplot(aes(x= RAZLIKA_PRIJEMA_I_POCETKA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike prijema i početka') + 
  xlab('Razlika prijema i početka (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 30)

# REZULTAT_ANALIZE i RAZLIKA_UZORKOVANJA_I_POCETKA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_UZORKOVANJA_I_POCETKA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_UZORKOVANJA_I_POCETKA)) %>%
  ggplot(aes(x= RAZLIKA_UZORKOVANJA_I_POCETKA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike uzorkovanja i početka') + 
  xlab('Razlika uzorkovanja i početka (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 30)

# REZULTAT_ANALIZE i RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA)) %>%
  ggplot(aes(x= RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike završetka vakcinacije i početka analize') + 
  xlab('Razlika završetka vakcinacije i početka analize (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 30)

# REZULTAT_ANALIZE i RAZLIKA_POCETKA_VAK_I_ISPITIVANJA

cor.test(d$REZULTAT_ANALIZE, d$RAZLIKA_POCETKA_VAK_I_ISPITIVANJA, method='pearson')

d %>%
  filter(!is.na(RAZLIKA_POCETKA_VAK_I_ISPITIVANJA)) %>%
  ggplot(aes(x= RAZLIKA_POCETKA_VAK_I_ISPITIVANJA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i razlike početka vakcinacije i početka analize') + 
  xlab('Razlika početka vakcinacije i početka analize (dani)') +
  ylab('Rezultat analize') +
  ylim(0, 30)

# REZULTAT_ANALIZE i UKUPNO_DANA_OD_VADJENJA_KRVI

cor.test(d$REZULTAT_ANALIZE, d$UKUPNO_DANA_OD_VADJENJA_KRVI, method='pearson')

d %>%
  filter(!is.na(UKUPNO_DANA_OD_VADJENJA_KRVI)) %>%
  ggplot(aes(x= UKUPNO_DANA_OD_VADJENJA_KRVI, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i dana protekih od vađenja krvi') + 
  xlab('Ukupno dana od vađenja krvi') +
  ylab('Rezultat analize') +
  ylim(0, 30)


# REZULTAT_ANALIZE i DAT_SERUM

wilcox.test(d$REZULTAT_ANALIZE ~ d$DAT_SERUM)

d %>%
  filter(!is.na(DAT_SERUM)) %>%
  ggplot(aes(x= DAT_SERUM, y=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u odnosu na pol') + 
  xlab('Dat serum') +
  ylab('Rezultat analize') +
  ylim(0, 20)


# REZULTAT_ANALIZE i BROJ_JEDINICA

cor.test(d$REZULTAT_ANALIZE, d$BROJ_JEDINICA, method='pearson')

d %>%
  filter(!is.na(BROJ_JEDINICA)) %>%
  ggplot(aes(x= BROJ_JEDINICA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize') +
  ylim(0, 30)

# REZULTAT_ANALIZE i LOKACIJA_OZLEDA

kruskal.test(d$REZULTAT_ANALIZE ~ d$LOKACIJA_OZLEDA)

d %>%
  filter(!is.na(LOKACIJA_OZLEDA)) %>%
  ggplot(aes(y= LOKACIJA_OZLEDA, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u lokaciju ozlede') + 
  xlab('Rezultat analize') +
  ylab('Lokacija ozlede') +
  xlim(0, 25)

# REZULTAT_ANALIZE i BROJ_OZLEDA

cor.test(d$REZULTAT_ANALIZE, d$BROJ_OZLEDA, method='pearson')
kruskal.test(d$REZULTAT_ANALIZE ~ d$BROJ_OZLEDA)

d %>% 
  filter(!is.na(BROJ_OZLEDA)) %>%
  ggplot(aes(x= BROJ_OZLEDA, y=REZULTAT_ANALIZE)) +
  geom_point() + 
  ggtitle('Korelacija rezultata analize i broja jedinica') + 
  xlab('Broj jedinica') +
  ylab('Rezultat analize')

# REZULTAT_ANALIZE i ŽIVOTINJA

kruskal.test(d$REZULTAT_ANALIZE ~ d$ŽIVOTINJA)

d %>%
  filter(!is.na(ŽIVOTINJA)) %>%
  ggplot(aes(y= ŽIVOTINJA, x=REZULTAT_ANALIZE)) +
  geom_boxplot(outlier.shape = NA) + 
  ggtitle('Rezultat analize u lokaciju ozlede') + 
  xlab('Rezultat analize') + xlim(0, 25) +
  ylab('Lokacija ozlede')


# Analiza rizika za varijablu USPESNO_IMUNIZOVAN

# Provera klase varijabli pre pocetka analize

lapply(d, class)

#Izmena klase potrebnih varijali
d$POL <- as.factor(d$POL)
d$USPESNO_IMUNIZOVAN <- as.factor(d$USPESNO_IMUNIZOVAN)
d$GODINA <- as.factor(d$GODINA)
d$MESTO_PACIJENTA <- as.factor(d$MESTO_PACIJENTA)
d$USTANOVA_POSILJALAC_UZORKA <- as.factor(d$USTANOVA_POSILJALAC_UZORKA)
d$DAT_SERUM <- as.factor(d$DAT_SERUM)
d$LOKACIJA_OZLEDA <- as.factor(d$LOKACIJA_OZLEDA)
d$ŽIVOTINJA <- as.factor(d$ŽIVOTINJA)

#Vizualizacija korelacija varijabli razlike radi potencijalnog izbacivanja zbog
# multikolinearnosti

install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
x <- select(d, contains('RAZLIKA'))
chart.Correlation(x)

#Pravljenje i optimizovanje modela Logisticke regresije

#Prvi model
model1 <- glm(USPESNO_IMUNIZOVAN ~ POL+ GODINA_ROĐENJA +  
             USTANOVA_POSILJALAC_UZORKA + TRAJANJE_VAKCINACIJE + 
             RAZLIKA_ZAVRSETKA_VAK_I_POCETKA_ANA + RAZLIKA_PRIJEMA_I_POCETKA + 
             RAZLIKA_UZORKOVANJA_I_POCETKA + UKUPNO_DANA_OD_VADJENJA_KRVI +
             BROJ_JEDINICA + LOKACIJA_OZLEDA + ŽIVOTINJA + BROJ_OZLEDA
             ,family = binomial(link = "logit"), data=d)
summary(model1)

#Model nakon uklanjanja neznacajnih varijabli
model2 <- glm(USPESNO_IMUNIZOVAN ~ GODINA_ROĐENJA +  
               USTANOVA_POSILJALAC_UZORKA + 
               RAZLIKA_PRIJEMA_I_POCETKA + 
               BROJ_JEDINICA
             ,family = binomial(link = "logit"), data=d)
summary(model2)

#Naredno uklanjanje neznacajnih varijabli
model3 <- glm(USPESNO_IMUNIZOVAN ~ GODINA_ROĐENJA +  
               USTANOVA_POSILJALAC_UZORKA
             ,family = binomial(link = "logit"), data=d)

summary(model3)




       