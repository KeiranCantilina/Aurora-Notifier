#Sunset data retrieval
sunset_url <- paste("http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&year=",substr(Sys.Date(),1,4),"&task=0&state=MN&place=St.+Paul",sep="")
sunset_dest <- "C://Users//canti021//Documents//sunset_temp.txt"
sunset_csv <- download.file(sunset_url,sunset_dest,"internal",mode = "w")
sunset_csv <- read.fwf(sunset_dest, widths = c(2,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4),header = FALSE,skip = 31,nrows = 31)
sunset_csv_cleaned <- sunset_csv[,c(seq(1,50,2))]
sunrise_table <- sunset_csv_cleaned[,c(1,seq(2,25,2))]
sunset_table <- sunset_csv_cleaned[,c(1,seq(3,25,2))]
colnames(sunrise_table) <- c("Day",sprintf("%02d",1:12))
sunrise_table$Day <- c(sprintf("%02d",1:31))
colnames(sunset_table) <- c("Day",sprintf("%02d",1:12))
sunrise_table$Day <- c(sprintf("%02d",1:31))

export(sunset_table, "C://Users//canti021//Documents//sunset_table.csv")
export(sunrise_table, "C://Users//canti021//Documents//sunrise_table.csv")
