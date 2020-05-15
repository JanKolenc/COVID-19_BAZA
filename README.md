# Primer spletne aplikacije s Shiny

* [![Shiny](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/JanKolenc/OPB-shiny/master?urlpath=shiny/OPB-shiny/OPB-shiny/app/ui_server.R) Aplikacija Shiny
* [![RStudio](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/JanKolenc/OPB-shiny/master?urlpath=rstudio) RStudio


Ta repozitorij vsebuje minimalen primer spletne aplikacije z ogrodjem [Shiny](https://shiny.rstudio.com/) in povezavo na podatkovno bazo [PostgreSQL](https://www.postgresql.org/) za potrebe predmeta [Osnove podatkovnih baz](https://github.com/jaanos/OPB).

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
