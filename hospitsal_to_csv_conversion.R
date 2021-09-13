#hospital data
hospital = readOGR(dsn = "hospital", layer = "doc-point")
hospital = data.frame(hospital)
hospital$Name = as.character(hospital$Name)
hospital$Address = as.character(hospital$Address)


write.csv(hospital, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\hospital.csv", row.names = FALSE)
