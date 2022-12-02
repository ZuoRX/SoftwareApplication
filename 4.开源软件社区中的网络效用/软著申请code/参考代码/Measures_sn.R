## load the packages
library(DBI); library(RMySQL); library(sna); library(network); 

## connect to mysql database
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="lasers")


for (i in 1:28402) { #28402		
		## read the network files
		for (year in 1999:2005) {
		  for (month in 0:12) {
		      filename <- paste("D:/OSS/finaldatfiles1010/OssPPNet", year, "_", month, "_", i, ".net", sep="")
		
					if (file.exists(filename)) {
						#print(filename);
						cat(paste(filename,"\n"));
						gg <- read.paj(filename, simplify=FALSE, debug=FALSE, verbose=FALSE);
			
						if (!is.null(gg)) {		
						##### caculate the measurments for the network positions
						#bonpow(gg);
						eve <- evcent(gg);
						gra <- graphcent(gg);
						#inf <- infocent(gg);
						pre <- prestige(gg);
						stre <- stresscent(gg);
						
						#### to the graph level
						evem <- mean(eve);
						evev <- var(eve);
						gram <- mean(eve);
						grav <- var(gra);
						prem <- mean(pre);
						prev <- var(pre);
						strem <- mean(stre);
						strev <- var(stre);
						
						##### caculate the measurments for the network
						mut <- mutuality(gg);
						dens <- gden(gg);
						bet <- betweenness(gg);
						cne <- connectedness(gg);
						eff <- efficiency(gg);
						hie <- hierarchy(gg);
						lub <- lubness(gg);
						#rea <- reachability(gg);
						cen <- centralization(gg, g=1, degree);
						#components(gg,connected="weak");
						#components(g,connected="strong");
						#edp <- eval.edgeperturbation(gg,1,2,grecip); #p
						gre <- grecip(gg);
						
						
							
						##### at the level of graph
			      }
			      result = paste ("pid=", i, "::dens=", dens, ";cne=", cne, ";eff=", eff, ";hie=", hie, ";lub=", lub, ";cen=", cen, ";gre=", gre, ";mut=", mut,  
			                       ";evem=", evem, ";evev=", evev, ";gram=", gram, ";grav=", grav, ";prem=", prem, ";prev=", prev, ";strem=", strem, ";strev=", strev, sep="");
			      print(result);      				
							
						## store them into database
						if (dens=='NaN') {dens <- 0}
						if (cne=='NaN') {cne <- 0}
						if (eff=='NaN') {eff <- 0}
						if (hie=='NaN') {hie <- 0}
						if (lub=='NaN') {lub <- 0}
						if (cen=='NaN') {cen <- 0}
						if (gre=='NaN') {gre <- 0}
						
						sql = paste( "Update project_dyn1010 ",
							 	"set density=", dens,
							 	" and connectedness=", cne,
							 	" and efficiency=", eff,
							 	" and hierarchy=", hie,
							 	" and lubness=", lub,
							 	" and centralization=", cen,
							 	" and grecip=", gre,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql2 = paste( "Update project_dyn1010 ",
							 	"set density=", dens,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql3 = paste( "Update project_dyn1010 ",
							 	"set efficiency=", eff,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql4 = paste( "Update project_dyn1010 ",
							 	"set centralization=", cen,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					
					
					  sql5 = paste( "Update project_dyn1010 ",
							 	"set evcentm=", evem,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql6 = paste( "Update project_dyn1010 ",
							 	"set evcentv=", evev,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql7 = paste( "Update project_dyn1010 ",
							 	"set graphcentm=", gram,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql8 = paste( "Update project_dyn1010 ",
							 	"set graphcentv=", grav,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql9 = paste( "Update project_dyn1010 ",
							 	"set prestigem=", prem,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql10 = paste( "Update project_dyn1010 ",
							 	"set prestigev=", prev,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql11 = paste( "Update project_dyn1010 ",
							 	"set stresscentm=", strem,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql12 = paste( "Update project_dyn1010 ",
							 	"set stresscentv=", strev,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql13 = paste( "Update project_dyn1010 ",
							 	"set mutuality=", mut,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );

					  sql14 = paste( "Update project_dyn1010 ",
							 	"set connectedness=", cne,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql15 = paste( "Update project_dyn1010 ",
							 	"set hierarchy=", hie,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
					  sql16 = paste( "Update project_dyn1010 ",
							 	"set lubness=", lub,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
							 										 	
						print(sql2);
						#dbSendQuery(con, statement=sql2)
						#dbSendQuery(con, statement=sql3)
						#dbSendQuery(con, statement=sql4)
						#dbSendQuery(con, statement=sql)
						
						#dbSendQuery(con, statement=sql5)
						#dbSendQuery(con, statement=sql6)
						#dbSendQuery(con, statement=sql7)
						#dbSendQuery(con, statement=sql8)
						#dbSendQuery(con, statement=sql9)
						#dbSendQuery(con, statement=sql10)
						#dbSendQuery(con, statement=sql11)
						#dbSendQuery(con, statement=sql12)
						#dbSendQuery(con, statement=sql13)												
						dbSendQuery(con, statement=sql14)
						dbSendQuery(con, statement=sql15)
						dbSendQuery(con, statement=sql16)		
					}					
			}
		}	
}



