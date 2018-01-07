# script to read _d2KROUT.csv files and plot droplet x-y paths
# from droplet centroid location data ONLY.
# place all _d2KROUT.csv files in the same folder as this script!

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots

rm(list=ls(all=TRUE))		#clear workspace of all variables
currentDirectory <- getwd()
setwd(currentDirectory)

########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_D2KROUT.csv")

#read experimental parameters for single component droplet experiments
keyfilename <- dir(pattern="single_key")
key <- read.csv(file=keyfilename,head=TRUE,sep=",",
		stringsAsFactors=FALSE)


# read each csv file and save variables of interest
for (i in 1:length(csvfilenames)){
	
	#store experiment name
	expname <- unlist( strsplit(csvfilenames[i],"_") )[1]
	temp <- read.csv(file=csvfilenames[i],head=TRUE,sep=",",
		stringsAsFactors=FALSE)	

	#grab do for current experiment
	keyRow <- which(key$expname == expname[1])


	#extract only variables of interest
	df.temp <- data.frame(temp$time, temp$do, temp$x_loc_fit,
						temp$x_vel_fit, temp$y_loc_fit,
						temp$y_vel_fit)
	expname <- rep(expname,nrow(temp))
	df.temp <- cbind(expname, df.temp )

	#grab fueltype for current experiment
	fuel <- key$fuel[keyRow]
	fuel_type <- rep(fuel,nrow(temp))
	df.temp <- cbind(df.temp, fuel_type)

	#grad diffusivities for applicable experiments
	Dvalue <- key$D[keyRow]	
	Dvalue_factor <- rep(Dvalue,nrow(temp))
	df.temp <- cbind(df.temp, Dvalue_factor)

	#grab xenon molefraction and chamber pressure for
	#current experiment
	xe_mf <- key$Xe[keyRow]
	pressure <- key$p[keyRow]
	xe <- rep( xe_mf,nrow(temp) )
	pressure_chamber <- rep( pressure, nrow(temp) )
	df.temp <- cbind(df.temp, xe, pressure_chamber)

	#create another data frame to contain variables of interest
	#for generating scatter plots

	#calculate initial droplet velocity
	range_index <- seq(1,30)
	Vo <- mean(sqrt(temp$x_vel_fit[range_index]^2 + temp$y_vel_fit[range_index]^2) )

	#calculate Vo_ofc the average velocity from needle retraction
	#to t_ofc the time at which the onset of flame contraction occurs
	if( key$tofc[keyRow] == 0 ){
		Vo_ofc <- 0
	}else{
		temp_partial <- subset(temp, (time>=0) & (time<=key$tofc[keyRow]) )
		Vo_ofc <- mean( sqrt(temp_partial$x_vel_fit^2 + temp_partial$y_vel_fit^2) )
	}


	df.scatter <- data.frame(expname[1],
							temp$do[1], Vo, fuel, Vo_ofc, Dvalue)


	if ( i == 1){
		df.global <- df.temp
		dfscatter.global <- df.scatter
	}else{
		df.global <- rbind(df.global, df.temp)
		dfscatter.global <- rbind(dfscatter.global, df.scatter)
	}
}
df.global <- setNames(df.global, c("expname","time","do",
									"x_loc_fit","x_vel_fit",
									"y_loc_fit","y_vel_fit",
									"fuel","D","Xe","p") )
dfscatter.global <- setNames(dfscatter.global,c("expname","do","Vo","fuel","Vofc","D"))

# create vector grouping do sizes and add to df.global
doSize <- as.character(nrow(df.global))
for (i in 1:nrow(df.global)){
	if ( df.global$do[i] > 3.0 ){
		doSize[i] <- "Large"
	}else{
		doSize[i] <- "Small"
	}
}
df.global <- cbind(df.global, doSize)


# experiments which have similar diameters to experiments
# where droplet is just evaporating
sim_experiments <- c("X136M01", "X144M01", "X144M04", "X168M05", "X182M01")
p_similar <- ggplot(subset(df.global, expname == sim_experiments))
# p_similar <- ggplot(subset(df.global,   ))
p_similar <- p_similar + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
p_similar <- p_similar + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_similar, file="p_similar.pdf", width=size.w, height=size.h, units=un)




p1 <- ggplot(df.global)
p1 <- p1 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
p1 <- p1 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=5)) +
	guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p1, file="dropTraj_all_do.pdf", width=size.w, height=size.h, units=un)

p1v2 <- ggplot(df.global)
p1v2 <- p1v2 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=fuel)) 
p1v2 <- p1v2 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

ggsave(p1v2, file="dropTraj_all_fuel.pdf", width=size.w, height=size.h, units=un)


p1v3 <- ggplot( subset(df.global,df.global$fuel != "Methanol") )
p1v3 <- p1v3 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=fuel)) 
p1v3 <- p1v3 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

ggsave(p1v3, file="dropTraj_all_fuel2.pdf", width=size.w, height=size.h, units=un)


p1v4 <- ggplot( subset(df.global,df.global$fuel != "Pro95/Gly5") )
p1v4 <- p1v4 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=fuel)) 
p1v4 <- p1v4 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

ggsave(p1v4, file="dropTraj_all_fuel3.pdf", width=size.w, height=size.h, units=un)



p1v5 <- ggplot( subset(df.global, D > 0) )
p1v5 <- p1v5 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=D)) 
p1v5 <- p1v5 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

ggsave(p1v5, file="dropTraj_PropGly.pdf", width=size.w, height=size.h, units=un)



ptsize <- 4.0
p2 <- ggplot(dfscatter.global)
p2 <- p2 + aes(x=do, y=Vo)
# p2 <- p2 + geom_point(mapping=aes(x=do, y=Vo, shape=fuel, colour=fuel, size=1.2))
p2 <- p2 + aes(colour=fuel, shape=fuel) 
p2 <- p2 + geom_point(size=ptsize) +
			scale_colour_manual(
				values=c(
					"#e41a1c",  #red (set1)
					"#984ea3",  #purple (set1)
					"#41b6c4",   #teal   (use when plotting EABB vs tdtv)
					"#4daf4a",  #green (set1)
					"#e7298a",  #pink (dark2)
					"#d95f02",  #brown (dark2)
					"#377eb8",  #blue (set1)
					"#66c2a5"))  #army grn (dark2)
p2 <- p2 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("D"[o]*" (mm)") ) +
	ylab(expression("V"[o]*" (mm/s)") ) 	

ggsave(p2, file="dovsVo_all_droplets.pdf", width=size.w, height=size.h, units=un)



ptsize <- 4.0
p3 <- ggplot( subset(dfscatter.global, Vofc > 0 ) )
p3 <- p3 + aes(x=D, y=Vofc)
# p3 <- p3 + geom_point(mapping=aes(x=do, y=Vo, shape=fuel, colour=fuel, size=1.2))
p3 <- p3 + aes(colour=fuel, shape=fuel) 
p3 <- p3 + geom_point(size=ptsize) +
			scale_colour_manual(
				values=c(
					"#e41a1c",  #red (set1)
					"#984ea3",  #purple (set1)
					"#41b6c4",   #teal   (use when plotting EABB vs tdtv)
					"#4daf4a",  #green (set1)
					"#e7298a",  #pink (dark2)
					"#d95f02",  #brown (dark2)
					"#377eb8",  #blue (set1)
					"#66c2a5"))  #army grn (dark2)
p3 <- p3 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("D (m"^2*"/s)") ) +
	ylab(expression("V"[o]*" (mm/s)") ) 	

ggsave(p3, file="DvsVofc_propGly.pdf", width=size.w, height=size.h, units=un)




