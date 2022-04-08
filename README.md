# AVP
 
R kod iz ovog githuba sadrži komande potrebne radi analize podataka dobijenih iz Nacionalne referentne laboratorije za besnilo - Pasterovog zavoda iz Novog Sada.
Podaci su stvari a matrica je uvezena iz .xlsx fajla.
Cilj ovog projekta jeste da proveri koje od brojnih nezavisnih varijabli imaju statistički značajan efekat i predstavljaju rizik faktore za neuspeo proces imunizacije postekspoziciono nakon ozlede.
U analizi se koriste dve zavisne varijable: REZULTAT_ANALIZE i USPESNO_IMUNIZOVAN koje ozačavaju da li je osoba uspešno prošla kroz proces imunizacije.

Nezavisne i zavisne varijable se prvo sređuju izbacivanjem neodgovoarajućih vrednosti, nakon toga uklanjanjem outliera i njihovom vizualizacijom stiče se uvid u odnos kategorija ako je varijabla kategorička, a nad numeričkim varijablama 
sprovodi se analiza normalnosti distribucije radi provere distribucije varijable i odabir prikladnog parametrijskog odnosno neparametrijskog testa ako pretpostavka normalnosti nije zadovoljena.

Nad svakom zavisnom varijablom radi se serija prikladnih testova(hi kvadrat ako je nezavisna varijabla kategorička kao i zavisna, a t test i anova(i prikladne neparametrijske alternativne) ako je nezavisna varijabla numerička.
Takođe ako su i zavisna i nezavina varijabla numeričke koristi se pirsonov koeficijent korelacije.

Na kraju analize pravi se prikladan model logističke regresije sa nezavisnim varijablama kao prediktorima i zavisnom USPESNOST_IMUNIZACIJE kao kriterijumom,
kako bi se stekao uvid u rizične faktore neuspešnosti predikcije procesa imunizacije.
