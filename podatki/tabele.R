setwd("~/OPB-shiny/podatki")
library(knitr)
library(dplyr)
library(readr)



#=============================================OSEBA======================================================================
oseba <- read_csv("oseba.csv", col_types = cols(
  Ime_priimek = col_character(),
  davcna_st = col_character(),
  naslov = col_character(),
  datum_testiranja = col_date(format = "%d.%m.%Y"),
  stanje = col_character()
))

colnames(oseba) <- c("ime","davcna_st","naslov","datum_testiranja","stanje")

#=============================================LOKACIJE-1.del======================================================================



lokacije <- read_csv("lokacije.csv",col_types = cols(
  id = col_integer(),
  lokacija = col_character(),
  st_postelj = col_integer(),
  st_zdravnikov = col_integer()
))


#=============================================SIMPTOM======================================================================

simptom <- as.data.frame(c("glavobol","vročina","tekoče blato","šibkost","mialgija","angina pectoris","dispneja","anozmija","agevzija"))
simptom$id <- rownames(simptom)
colnames(simptom) <- c("simptom","id")
simptom <- simptom[c("id","simptom")]

#=============================================ZD_DELAVEC======================================================================

zd_delavec_na_dolznosti_c <- filter(oseba, stanje == "zd_delavec_na_dolznosti")
zd_delavec_na_dolznosti_c <- zd_delavec_na_dolznosti_c$davcna_st
zd_ustanova_id_c <- sample.int(length(rownames(lokacije)),length(zd_delavec_na_dolznosti_c),rep = TRUE)
zd_delavec_na_dolznosti <- as.data.frame(zd_delavec_na_dolznosti_c)
zd_delavec_na_dolznosti$zd_ustanova_id <- zd_ustanova_id_c
colnames(zd_delavec_na_dolznosti)<- c("id","zd_ustanova_id_c")
#=============================================LOKACIJE-2.del======================================================================

st_postelj <- c()
st_zdravnikov <- c()
for (i in 1:length(lokacije$id)){
  st_zdr <- table(zd_ustanova_id_c)[[i]]
  st_zdravnikov <- c(st_zdravnikov,st_zdr)
  st_postelj <- c(st_postelj,st_zdr*5)
} 

lokacije$st_postelj <- st_postelj
lokacije$st_zdravnikov <- st_zdravnikov


#=============================================BOLNIK======================================================================

bolnik_c <- filter(oseba, stanje == "bolnik")
bolnik_c <- bolnik_c$davcna_st

bolnikov_zdravnik <- c()
hospitalizacija <- c()
for (i in 1:length(bolnik_c)){
  bolnikov_zdravnik <- append(bolnikov_zdravnik,levels(droplevels(zd_delavec_na_dolznosti$id[[sample.int(length(zd_delavec_na_dolznosti$id),1)]][1])))
  hospitalizacija <- c(hospitalizacija, sample(0:1,1,prob = c(0.8,0.2)))
}

bolnik <- as.data.frame(bolnik_c)
bolnik$davcna_od_zdravnika <- bolnikov_zdravnik
bolnik$hospitalizacija <- hospitalizacija
colnames(bolnik)<-c("id_bolnika","id_zdravnika","hospitalizacija")

#=============================================IMA======================================================================

ima_c <- c()
simptomi_c <- c()
jakost_c <- c()
datum_pojavitve_c <- c()
for (i in 1:length(rownames(oseba))){
  rnd <- sample.int(length((rownames(simptom))),1)
  ima_c <- c(ima_c,rep(oseba$davcna_st[i],rnd))
  simptomi_c <- c(simptomi_c,sample.int(length((rownames(simptom))),rnd, rep = FALSE))
  jakost_c <- c(jakost_c,sample.int(10,rnd,rep=TRUE))
  datum_pojavitve_c <- append(datum_pojavitve_c,as.Date(sample(seq(as.Date(oseba$datum_testiranja[i])-11, as.Date(oseba$datum_testiranja[i]), by="day"),rnd)))
  
}

ima <- as.data.frame(ima_c)
ima$id_simptomi <- simptomi_c
ima$jakost <- jakost_c
ima$datum_pojavitve <- datum_pojavitve_c
colnames(ima) <- c("id_pacienta","id_simptomi","jakost","datum_pojavitve")


#=============================================JE_ZDRAVLJEN======================================================================

#source(db.R)




