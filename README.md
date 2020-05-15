# COVID-19 BAZA
Aplikacija uporabnikom omogoča pregled nad vsemi testiranci, med katerimi so lahko tudi zdravstveni delavci. Za zdravnike (aktivne in obolele) ter hospitalizirane paciente pokaže tudi v kateri od bolnišnic so locirani. Avtoriziran uporabnik (zdravnik) lahko v evidenco vnaša novotestirane in njihove simptome. Za vsakega od obolelih lahko pogleda tudi katere simptome ima in kako intenzivni so. Na voljo je še nekaj statističnih podatkov, ki prikazujejo koliko ljudi je zbolevalo dnevno, skupno število okuženih ter zasedenost bolnišnic.

Aplikacija uporabnikom omogoča pregled nad testiranci in zdravniki. Za zdravnike in hospitalizirane paciente pokaže tudi v kateri od bolnišnic so locirani. Avtoriziran uporabnik (zdravnik) lahko v evidenco vnaša tudi novotestirane in njihove simptome. Za vsakega od pacientov lahko pogledamo katere simptome ima in kako intenzivni so. Na voljo je še nekaj statističnih podatkov, ki prikazujejo koliko ljudi je zbolevalo dnevno, skupno število okuženih ter zasedenost bolnišnic.


![GitHub Logo](/podatki/ER_diagram.jpg)

## Aplikacija

Aplikacijo zaženemo tako, da v RStudiu poženemo [`server.R`](app/server.R) ali [`ui.R`](app/ui.R) v mapi [`app/`](app/). Tam se nahaja še program [`auth_public.R`](app/auth_public.R) s podatki za prijavo na bazo.


## Binder

Aplikacijo je mogoče poganjati tudi na spletu z orodjem [Binder](https://mybinder.org/). V ta namen sta na repozitoriju še sledeči datoteki:
* [`Dockerfile`](Dockerfile) - nastavitvena datoteka za [Docker](https://www.docker.com/)
* [`install.R`](install.R) - skripta za namestitev dodatnih paketov

Namesto nameščanja paketov z `install.R` je mogoča tudi priprava in uporaba [lastne slike](https://github.com/jaanos/APPR-docker) za Docker.

Poleg zgoraj omenjenih datotek je mogoče nastaviti tudi podatke v datoteki [`gitconfig`](gitconfig) za uporabo git v RStudiu. Če ti podatki niso podani, jih je mogoče spreminjati tudi ročno tekom poganjanja v Binderju.

Zaradi omejitev javne storitve [Binder](https://mybinder.org/) se povezava z bazo vzpostavi na vratih 443 (namesto običajnih 5432), za kar je bila potrebna posebna nastavitev strežnika.
