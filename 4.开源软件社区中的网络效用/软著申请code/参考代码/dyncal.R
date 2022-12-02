## load the packages
library(DBI); 
#library(RMySQL); 
library(sna); 
library(network); 

## connect to mysql database
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="lasers")

sql_b = paste( "select * from temp_y_pp_dyn", sep="" );
rs1 <- dbSendQuery(con, sql_b);

#while ((dbHasCompleted(rs1))==FALSE) {

			df1 <- fetch(rs1,n=-1)
			
			for (j in 1:length(df1$pid)) {	 				
					 
					i <- df1$pid[j]
					year <- df1$TP[j]
					dens <- 1000;
					index <- 1;
				  for (month in 1:12) {
		
							  sql = paste( "select * from temp_pp",
									 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
														
								print(sql)
								rs <- dbSendQuery(con, sql);
								while ((dbHasCompleted(rs))==FALSE) {
									df <- fetch(rs,n=-1)
									if ((length(df))!=0) {
											dens[index] <- df$density
											index <- index + 1
									}
								}	
								dbClearResult(rs)											
					}
					## caculate the measures annual
					if (dens[1] !=1000) {
							endstart <- dens[index-1] - dens[1]
							maxstart <- max(dens) - dens[1]
							minstart <- min(dens) - dens[1]
							meanstart <- mean(dens) - dens[1]
							sql1 = paste( "update temp_y_pp_dyn",
											 	" set endstart=", endstart,
											 	" WHERE pid=", i, " and TP=", year, sep="" );
							sql2 = paste( "update temp_y_pp_dyn",
											 	" set maxstart=", maxstart,
											 	" WHERE pid=", i, " and TP=", year, sep="" );		
							sql3 = paste( "update temp_y_pp_dyn",
											 	" set minstart=", minstart,
											 	" WHERE pid=", i, " and TP=", year, sep="" );	
							sql4 = paste( "update temp_y_pp_dyn",
											 	" set meanstart=", meanstart,
											 	" WHERE pid=", i, " and TP=", year, sep="" );	
							print(sql1)
							dbSendQuery(con, statement=sql1)
							dbSendQuery(con, statement=sql2)
							dbSendQuery(con, statement=sql3)
							dbSendQuery(con, statement=sql4)
					}
			}			
#}	


dbDisconnect(con)



