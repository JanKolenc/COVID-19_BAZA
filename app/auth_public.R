
db='sem2020_jank'
host='baza.fmf.uni-lj.si'
user='javnost'
password='javnogeslo'

credentials = data.frame(
  username_id = c("gost"),
  password   = sapply(c("gost"),password_store),
  permission  = c("basic"), 
  stringsAsFactors = F
)
