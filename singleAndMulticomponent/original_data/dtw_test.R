# This is a test script to attempt to categorize droplet trajectories
# into groups based on how similar they are to one another.
# This similarity measure can be quantified (and automated!) using direct time warping
# techniques available through the R package "dtw".

# Looks like "dtw" is not very good for this.  Some step patterns
# work well for some data sets, and not so well for others, or sometimes,
# does not work at all even when end constraints on query and reference
# time series are OK.  This kind of defeats the purpose of automation --
# the reason why this code was written in the first place.
# Will have to resort to manually categorizing
# droplet trajectories ... sigh

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots
library(dtw)

rm(list=ls(all=TRUE))		#clear workspace of all variables
currentDirectory <- getwd()
setwd(currentDirectory)
graphics.off()


########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_D2KROUT.csv")

#read experimental parameters for single component droplet experiments
keyfilename <- dir(pattern="single_key")
key <- read.csv(file=keyfilename,head=TRUE,sep=",",
		stringsAsFactors=FALSE)

numExperiments <- length(csvfilenames)
reference <- read.csv(file = "X182M01_D2KROUT.csv",head=TRUE, sep=",",
			stringsAsFactors=FALSE)
tmin <- min(reference$time)
tmax <- max(reference$time)
ref_size <- nrow(reference)

inner_it_counter <- 0
# i <- 82
for (i in 1: (numExperiments) ){
	#store experiment name
	expname <- unlist( strsplit(csvfilenames[i],"_") )[1]

	#grab do for current experiment
	keyRow <- which(key$expname == expname[1])

	query <- read.csv(file=csvfilenames[i],head=TRUE,sep=",",
		stringsAsFactors=FALSE)	

	# partial alignment using dtw function. NOTE: partial alignment
	# e.g. open ends require that step.pattern be a type
	# which is normalizable.
	x_alignment <- dtw( reference$x_loc_fit, 
			# subset(query, time >= tmin & time <= tmax)$x_loc_fit, k=TRUE, 
			query$x_loc_fit, k=TRUE, 
			open.end=FALSE,
			open.begin = FALSE,
			step.pattern = symmetricP2)

	y_alignment <- dtw( reference$y_loc_fit, 
			# subset(query, time >= tmin & time <= tmax)$y_loc_fit, 
			query$y_loc_fit, 	k=TRUE, 
			open.end=FALSE, 
			open.begin = FALSE,		
			step.pattern = symmetricP2)

	readline("--press enter--")  

	totalNormalizedDistance <- x_alignment$normalizedDistance +
								y_alignment$normalizedDistance
	totalDistance <- x_alignment$distance +
					y_alignment$distance

	if (totalNormalizedDistance  <= 5 & totalNormalizedDistance >= 3){
		print(totalNormalizedDistance)
		print(totalDistance)
		print(expname)

		plot(dtw( reference$x_loc_fit, 
			subset(query, time >= tmin & time <= tmax)$x_loc_fit, k=TRUE, 
			open.end = FALSE,
			open.begin = FALSE,
			# step.pattern = "itakura",
			# window.size = 0.10*ref_size, 			
			step.pattern = symmetricP2), type="two",
			off=1, match.lty=2, match.indices=20)
		title(main="X position")

		dev.new()
		plot(dtw( reference$y_loc_fit, 
			subset(query, time >= tmin & time <= tmax)$y_loc_fit, k=TRUE, 
			open.end = FALSE, 
			open.begin = FALSE,
			# window.type = "itakura",
			# window.size = 0.10*ref_size, 			
			step.pattern = symmetricP2), type="two",
			off=1, match.lty=2, match.indices=20)
		title(main="Y position")		

		dev.new()
		plot(dtw( reference$y_loc_fit, 
			subset(query, time >= tmin & time <= tmax)$y_loc_fit, k=TRUE, 
			open.end = FALSE, 
			open.begin = FALSE,
			step.pattern = symmetricP2), type="threeway",
			off=1, match.lty=2, match.indices=20)		

		# Sys.sleep(3.0) 
		# readline("press enter")
		readline("--press enter--")
		graphics.off()		

		inner_it_counter <- inner_it_counter + 1

		#extract only variables of interest
		df.temp <- data.frame(query$time, query$do, query$x_loc_fit,
							query$x_vel_fit, query$y_loc_fit,
							query$y_vel_fit)
		expname2 <- rep(expname,nrow(query))
		df.temp <- cbind(expname2, df.temp )

		#grab fueltype for current experiment
		fuel <- key$fuel[keyRow]
		fuel_type <- rep(fuel,nrow(query))
		df.temp <- cbind(df.temp, fuel_type)


		#store distances associate to each experiment
		df.distances <- data.frame(expname, 
						totalNormalizedDistance, 
						totalDistance)

		if ( inner_it_counter == 1){
			df.global <- df.temp
			df.distancesGlobal <- df.distances
		}else{
			df.global <- rbind(df.global, df.temp)
			df.distancesGlobal <- rbind(df.distancesGlobal, df.distances)
		}		

	} # if (totalNormalizedDistance)

} #for (i in ...)
df.global <- setNames(df.global, c("expname","time","do",
									"x_loc_fit","x_vel_fit",
									"y_loc_fit","y_vel_fit",
									"fuel") )
df.distancesGlobal <- setNames(df.distancesGlobal, 
						c("expname","normalized_distance","distance"))


p1 <- ggplot(df.global)
p1 <- p1 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
p1 <- p1 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.8, 0.8),
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
ggsave(p1, file="test.pdf", width=size.w, height=size.h, units=un)




# #read experimental parameters for single component droplet experiments
# keyfilename <- dir(pattern="single_key")
# key <- read.csv(file=keyfilename,head=TRUE,sep=",",
# 		stringsAsFactors=FALSE)


# # read each csv file and save variables of interest
# for (i in 1:length(csvfilenames)){
	
# 	#store experiment name
# 	expname <- unlist( strsplit(csvfilenames[i],"_") )[1]
# 	temp <- read.csv(file=csvfilenames[i],head=TRUE,sep=",",
# 		stringsAsFactors=FALSE)	

# 	#grab do for current experiment
# 	keyRow <- which(key$expname == expname[1])


# 	#extract only variables of interest
# 	df.temp <- data.frame(temp$time, temp$do, temp$x_loc_fit,
# 						temp$x_vel_fit, temp$y_loc_fit,
# 						temp$y_vel_fit)
# 	expname <- rep(expname,nrow(temp))
# 	df.temp <- cbind(expname, df.temp )

# 	#grab fueltype for current experiment
# 	fuel <- key$fuel[keyRow]
# 	fuel_type <- rep(fuel,nrow(temp))
# 	df.temp <- cbind(df.temp, fuel_type)

# 	#grad diffusivities for applicable experiments
# 	Dvalue <- key$D[keyRow]	
# 	Dvalue_factor <- rep(Dvalue,nrow(temp))
# 	df.temp <- cbind(df.temp, Dvalue_factor)

# 	#grab xenon molefraction and chamber pressure for
# 	#current experiment
# 	xe_mf <- key$Xe[keyRow]
# 	pressure <- key$p[keyRow]
# 	xe <- rep( xe_mf,nrow(temp) )
# 	pressure_chamber <- rep( pressure, nrow(temp) )
# 	df.temp <- cbind(df.temp, xe, pressure_chamber)

# 	#create another data frame to contain variables of interest
# 	#for generating scatter plots

# 	#calculate initial droplet velocity
# 	range_index <- seq(1,30)
# 	Vo <- mean(sqrt(temp$x_vel_fit[range_index]^2 + temp$y_vel_fit[range_index]^2) )

# 	#calculate Vo_ofc the average velocity from needle retraction
# 	#to t_ofc the time at which the onset of flame contraction occurs
# 	if( key$tofc[keyRow] == 0 ){
# 		Vo_ofc <- 0
# 	}else{
# 		temp_partial <- subset(temp, (time>=0) & (time<=key$tofc[keyRow]) )
# 		Vo_ofc <- mean( sqrt(temp_partial$x_vel_fit^2 + temp_partial$y_vel_fit^2) )
# 	}


# 	df.scatter <- data.frame(expname[1],
# 							temp$do[1], Vo, fuel, Vo_ofc, Dvalue)


# 	if ( i == 1){
# 		df.global <- df.temp
# 		dfscatter.global <- df.scatter
# 	}else{
# 		df.global <- rbind(df.global, df.temp)
# 		dfscatter.global <- rbind(dfscatter.global, df.scatter)
# 	}
# }
# df.global <- setNames(df.global, c("expname","time","do",
# 									"x_loc_fit","x_vel_fit",
# 									"y_loc_fit","y_vel_fit",
# 									"fuel","D","Xe","p") )
# dfscatter.global <- setNames(dfscatter.global,c("expname","do","Vo","fuel","Vofc","D"))

# # create vector grouping do sizes and add to df.global
# doSize <- as.character(nrow(df.global))
# for (i in 1:nrow(df.global)){
# 	if ( df.global$do[i] > 3.0 ){
# 		doSize[i] <- "Large"
# 	}else{
# 		doSize[i] <- "Small"
# 	}
# }
# df.global <- cbind(df.global, doSize)


# # experiments which have similar diameters to experiments
# # where droplet is just evaporating
# sim_experiments <- c("X136M01", "X144M01", "X144M04", "X168M05", "X182M01")
# do_X182M01 <- 3.2
# p_incr <- 0.10
