## process the network of person-modules
## load the packages
library(DBI); library(RMySQL); library(igraph);

## connect to mysql database
drv <- dbDriver("MySQL")
con <- dbConnect(drv,group="lasers")

for (i in 1:28402) { #28402		
		## read the network files
		for (year in 1999:2005) {
		  for (month in 0:0) {
		      filename <- paste("D:/OSS/finaldatfiles1010/OssPPNet", year, "_", month, "_", i, ".net", sep="")
		
					if (file.exists(filename)) {
						#print(filename);
						cat(paste(filename,"\n"));
						gig <- read.graph(filename, "pajek");
			
			
						##
						idlist <- V(gig)[1]$id
						lab <-unlist(strsplit(idlist,":"))
						lab[1]
						
						
						## convert the graph type to adjacent matrix type
						gmat <- get.adjacency(gig, attr="weight");
						tempa=1;
						for (vindex in 1:vcount(gig)) {
								tempa[vindex] = sum(gmat[vindex, 1:vcount(gig)]);
						}
						V(gig)$loadve <- tempa; # store the load of every vertex into an attribute of this graph
						
						## caculate the weight suPPary information for a certain graph
						wei <- E(gig)$weight;
						weim <- mean(wei);
						weiv <- var(wei);
						weimx <- max(wei);
						weimi <- min(wei);
						weisum <- sum(wei);
						weiavg <- sum(wei) / vcount(gig);
						
						## caculate the measurments for the network
						##### at the level of entity
						deg <- degree(gig);
						#ind <- closeness(gig,mode="in");
						outd <- closeness(gig,mode="out");
						#alld <- closeness(gig,mode="all");
						pr <- page.rank(gig)$vector;
						be <- betweenness(gig);
						ebe <- edge.betweenness(gig);
						cl <- closeness(gig);
						#efw <- constraint(gig); # can not handle the NaN
						#ac <- alpha.centrality(gig); #eigenvetor
						#sdi <- similarity.dice(gig); # refer to lada
						#sja <- similarity.jaccard(gig); # refer to lada
						
						
						## change individual level to the graph level
						outdm <- mean(outd);
						outdv <- var(outd);
						#acm <- mean(ac);
						#acv <- var(ac);
						prm <- mean(pr);
						prv <- var(pr);
						bem <- mean(be);
						bev <- var(be);
						clm <- mean(cl);
						clv <- var(cl);
						#efwm <- mean(efw);
						#efwv <- var(efw);
						
						
						##### at the level of graph
						tra <- transitivity(gig); # also called clustering coefficient
						cwnum <- clusters(gig,mode=c("weak"))$no;
						csnum <- clusters(gig,mode=c("strong"))$no;
						#dens <- graph.density(gig);
						dia <- diameter(gig,unconnected=TRUE);
						diau <- diameter(gig,unconnected=FALSE);
						
						cli <- clique.number(gig);
						augr <- graph.automorphisms(gig)$group_size;
						aunn <- graph.automorphisms(gig)$nof_nodes;
						aunl <- graph.automorphisms(gig)$nof_leaf_nodes;
						aunb <- graph.automorphisms(gig)$nof_bad_nodes;
						aunc <- graph.automorphisms(gig)$nof_canupdates;
						aumx <- graph.automorphisms(gig)$max_level;
						avpl <- average.path.length(gig);
						
						##### the score of modularity
						#wtc <- walktrap.coPPunity(gig);
						#memb <- coPPunity.to.membership(gig,wtc$merges,steps=12)
						#mods <- modularity(gig,memb$membership)
			      
			      result = paste ("pid=", i, "::dens=", dens, ";tra=", tra, ";cwnum=", cwnum, ";csnum=", csnum, ";dia=", dia, 
			      		";diau=", diau, ";cli=", cli, ";augr=", augr, ";aunn=", aunn, ";aunl=", aunl, ";aunb=", aunb, ";aunc=", aunc, ";aumx=", aumx, ";avpl=", avpl, 
			      		";outdm=", outdm, ";oudv=", outdv, ";prm=", prm, ";prv=", prv, ";bem=", bem, ";bev=", bev, ";clm=", clm, ";clv=", clv,
			      		sep="");
			      print(result);      				
							
						## store them into database
						if (tra=='NaN') {tra <- 0}
						if (cwnum=='NaN') {cwnum <- 0}
						if (csnum=='NaN') {csnum <- 0}
						if (dia=='NaN') {dia <- 0}
						if (diau=='NaN') {diau <- 0}
						if (cli=='NaN') {cli <- 0}
						if (augr=='NaN') {augr <- 0}
						if (aunn=='NaN') {aunn <- 0}
						if (aunl=='NaN') {aunl <- 0}
						if (aunb=='NaN') {aunb <- 0}
						if (aunc=='NaN') {aunc <- 0}
						if (aumx=='NaN') {aumx <- 0}
						if (avpl=='NaN') {avpl <- 0}
						
						if (outdm=='NaN') {outdm <- 0};
						if (outdv=='NaN') {outdv <- 0};
						if (prm=='NaN') {prm <- 0};
						if (prv=='NaN') {prv <- 0};
						if (bem=='NaN') {bem <- 0};
						if (bev=='NaN') {bev <- 0};
						if (clm=='NaN') {clm <- 0};
						if (clv=='NaN') {clv <- 0};
						#if (efwm=='NaN') {efwm <- 0};
						#if (efwv=='NA') {efwv <- 0};
						
						sql1 = paste( "update project_dyn1010 ",
							 	"set transitivity=", tra,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql2 = paste( "update project_dyn1010 ",
							 	"set averagepath=", avpl,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql3 = paste( "update project_dyn1010 ",
							 	"set diameter=", dia,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql4 = paste( "update project_dyn1010 ",
							 	"set clique=", cli,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql5 = paste( "update project_dyn1010 ",
							 	"set outdm=", outdm,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql6 = paste( "update project_dyn1010 ",
							 	"set outdv=", outdv,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql7 = paste( "update project_dyn1010 ",
							 	"set prm=", prm,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql8 = paste( "update project_dyn1010 ",
							 	"set prv=", prv,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql9 = paste( "update project_dyn1010 ",
							 	"set bem=", bem,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql10 = paste( "update project_dyn1010 ",
							 	"set bev=", bev,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql11 = paste( "update project_dyn1010 ",
							 	"set clm=", clm,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql12 = paste( "update project_dyn1010 ",
							 	"set clv=", clv,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						
						sql13 = paste( "update project_dyn1010 ",
							 	"set augr=", augr,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql14 = paste( "update project_dyn1010 ",
							 	"set aunn=", aunn,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql15 = paste( "update project_dyn1010 ",
							 	"set aunb=", aunb,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql16 = paste( "update project_dyn1010 ",
							 	"set aunc=", aunc,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );


						
						sql17 = paste( "update project_dyn1010 ",
							 	"set weim=", weim,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql18 = paste( "update project_dyn1010 ",
							 	"set weiv=", weiv,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql19 = paste( "update project_dyn1010 ",
							 	"set weimx=", weimx,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql20 = paste( "update project_dyn1010 ",
							 	"set weimi=", weimi,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql21 = paste( "update project_dyn1010 ",
							 	"set weisum=", weisum,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
						sql22 = paste( "update project_dyn1010 ",
							 	"set weiavg=", weiavg,
							 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, sep="" );
			 									 	
						#print(sql1);
						#dbSendQuery(con, statement=sql1)
						#dbSendQuery(con, statement=sql2)
						#dbSendQuery(con, statement=sql3)
						#dbSendQuery(con, statement=sql4)
						#dbSendQuery(con, statement=sql5)
						#dbSendQuery(con, statement=sql6)
						#dbSendQuery(con, statement=sql7)
						#dbSendQuery(con, statement=sql8)
						#dbSendQuery(con, statement=sql9)
						#dbSendQuery(con, statement=sql10)
						#dbSendQuery(con, statement=sql11)
						#dbSendQuery(con, statement=sql12)
						#dbSendQuery(con, statement=sql13)
						#dbSendQuery(con, statement=sql14)
						#dbSendQuery(con, statement=sql15)
						#dbSendQuery(con, statement=sql16)


						#dbSendQuery(con, statement=sql17)
						#dbSendQuery(con, statement=sql18)
						#dbSendQuery(con, statement=sql19)
						#dbSendQuery(con, statement=sql20)
						#dbSendQuery(con, statement=sql21)
						#dbSendQuery(con, statement=sql22)

            # store vertex value into database
            for (vi in 1:vcount(gig)) {
								sql23 = paste( "insert vertex1010 (pid, Type, TP, month, nodeid, loadve) values(",
									 i, ", ",  "'PP'", ", ", year, ", ", month, ", ", vi, ", ", V(gig)$loadve[vi], ")", sep="" );
								sql24 = paste( "update vertex1010 ",
									 	"set betweeness=", be[vi],
									 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, " and nodeid=", vi, sep="" );
								sql25 = paste( "update vertex1010 ",
									 	"set closeness=", cl[vi],
									 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, " and nodeid=", vi, sep="" );
								sql26 = paste( "update vertex1010 ",
									 	"set degree=", deg[vi],
									 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, " and nodeid=", vi, sep="" );
								sql27 = paste( "update vertex1010 ",
									 	"set pagerank=", pr[vi],
									 	" WHERE pid=", i, " and TP=", year, " and Type=", "'PP'", " and month=", month, " and nodeid=", vi, sep="" );
									 	
								print(sql23);
								#print(sql24);
							  dbSendQuery(con, statement=sql23)
							  dbSendQuery(con, statement=sql24)
							  dbSendQuery(con, statement=sql25)
							  dbSendQuery(con, statement=sql26)
							  dbSendQuery(con, statement=sql27)
					  }
			 									 							
						
						
					}					
			}
		}	
}
