credentials = data.frame(
  username_id = c("jan", "aljosa","filip","gost"),
  password   = sapply(c("opb", "opb","opb","gost"),password_store),
  permission  = c("advanced", "advanced","advanced","basic"), 
  stringsAsFactors = F
)