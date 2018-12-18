# script to read _d2KROUT.csv files and plot droplet x-y paths
# from droplet centroid location data ONLY.
# place all _d2KROUT.csv files in the same folder as this script!

rm(list=ls(all=TRUE))		#clear workspace of all variables

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots
library(abind)
library(reshape)

currentDirectory <- getwd()
setwd(currentDirectory)

source("/Users/changvang/mygitFiles/droplet_trajectories/singleAndMulticomponent/original_data/dataFrameConstruct.R")


########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_D2KROUT.csv")

#read in key containing ignition and extinction times of experiments
key_time_points <- read.csv(file = "/Users/changvang/mygitFiles/flex_experiment_information/ignitionExtinctionTimes.csv",
			head=TRUE, sep=",",
			stringsAsFactors=FALSE)

#read experimental parameters for single component droplet experiments
keyfilename <- dir(pattern="single_key.csv")
key_exp_parameters <- read.csv(file=keyfilename,head=TRUE,sep=",",
		stringsAsFactors=FALSE)

# index <- as.numeric(nrow(key_time_points))
# for(j in 1:nrow(key_time_points)){
# 	index[j] <- which(key_time_points$FLEX_ID[j]==key_exp_parameters$expname)
# }
# T_ig <- rep(0,nrow(key_exp_parameters))
# T_ig[index] <- key_time_points$T_IGNITION

# igextTimes <- cbind(key_exp_parameters,T_ig)

# index2 <- rep(0,nrow(key_exp_parameters))
# for (j in 1:nrow(dfscatter.global)){
# 	index <- which(dfscatter.global$expname[j] == key_exp_parameters$expname)
# 	if( key_exp_parameters$T_IGNITION[index] == 0 ){
# 		index2[index] <- 1
# 	}
# }
# write.csv(index2,"test.csv")

# read each csv file and save variables of interest
for (i in 1:length(csvfilenames)){
	
	#store experiment name
	expname <- unlist( strsplit(csvfilenames[i],"_") )[1]
	temp <- read.csv(file=csvfilenames[i],head=TRUE,sep=",",
		stringsAsFactors=FALSE)	

	#grab do for current experiment from key_exp_parameters
	keyRow <- which(key_exp_parameters$expname == expname)

	#grab index for current experiment from key_time_points
	keyRow_time_pts <- which(key_time_points$FLEX_ID == expname)
	# print(keyRow_time_pts)

	#extract only variables of interest
	df.temp <- data.frame(temp$time, temp$do, temp$x_loc_fit,
						temp$x_vel_fit, temp$y_loc_fit,
						temp$y_vel_fit)
	expname <- rep(expname,nrow(temp))
	df.temp <- cbind(expname, df.temp )

	#grab fueltype for current experiment
	fuel <- key_exp_parameters$fuel[keyRow]
	fuel_type <- rep(fuel,nrow(temp))
	df.temp <- cbind(df.temp, fuel_type)

	#grab diffusivities for applicable experiments. This requires
	#reading in the single_key file with data columns for 
	#the diffusivities, D
	Dvalue <- key_exp_parameters$D[keyRow]	
	Dvalue_factor <- rep(Dvalue,nrow(temp))
	df.temp <- cbind(df.temp, Dvalue_factor)

	#grab xenon molefraction and chamber pressure for
	#current experiment
	xe_mf <- key_exp_parameters$Xe[keyRow]
	pressure <- key_exp_parameters$p[keyRow]
	xe <- rep( xe_mf,nrow(temp) )
	pressure_chamber <- rep( pressure, nrow(temp) )
	df.temp <- cbind(df.temp, xe, pressure_chamber)

	#create another data frame to contain variables of interest
	#for generating scatter plots

	tmin <- key_time_points$T_IGNITION[keyRow_time_pts]
	tmax <- key_time_points$T_EXTINCTION[keyRow_time_pts]
	
	# check if tmax is empty
	if ( length(tmax) == 0){
		tmax <- NA
	}
	# calculate average burn rate and droplet acceleration during combustion and after extinction
	# if (length(keyRow_time_pts) != 0 ){
		# tmin <- key_time_points$T_IGNITION[keyRow_time_pts]
		# tmax <- key_time_points$T_EXTINCTION[keyRow_time_pts]	
	if ( (length(keyRow_time_pts) != 0) & (is.na(tmax) != 1) ){
		if ( max(temp$time) > tmax ){
			K_comb <- mean( subset(temp, time >= tmin & time <= tmax)$k_bw1 )
			K_ext <- mean( subset(temp, time > tmax )$k_bw1 )
			x_acc_comb <- mean( subset(temp, time >= tmin & time <= tmax)$x_acc_fit )
			y_acc_comb <- mean( subset(temp, time >= tmin & time <= tmax)$y_acc_fit )	
			total_acc_comb <- sqrt( x_acc_comb^2 + y_acc_comb^2 )

			x_acc_ext <- mean( subset(temp, time > tmax )$x_acc_fit )
			y_acc_ext <- mean( subset(temp, time > tmax )$y_acc_fit )		
			total_acc_ext <- sqrt( x_acc_ext^2 + y_acc_ext^2 )				
		} # if max(temp$time)
	}else{
		K_comb <- NA 
		K_ext <- NA
		x_acc_comb <- NA
		y_acc_comb <- NA 
		total_acc_comb <- NA

		x_acc_ext <- NA 
		y_acc_ext <- NA 
		total_acc_ext <- NA
	}


	#calculate initial droplet velocity
	frameVo <- 12 # the first 0.4 seconds
	range_index <- seq(1,frameVo)
	Vo <- mean(sqrt(temp$x_vel_fit[range_index]^2 + temp$y_vel_fit[range_index]^2) )

	#calculate Vo_ofc the average velocity from needle retraction
	#to t_ofc the time at which the onset of flame contraction occurs
	if( key_exp_parameters$tofc[keyRow] == 0 ){
		Vo_ofc <- 0
	}else{
		temp_partial <- subset(temp, (time>=key_exp_parameters$tofc[keyRow]-0.4) & (time<=key_exp_parameters$tofc[keyRow]) )
		Vo_ofc <- mean( sqrt(temp_partial$x_vel_fit^2 + temp_partial$y_vel_fit^2) )
	}

	if( key_exp_parameters$T_IGNITION[keyRow] == 0){
		V_ig <- 0	
	}else{
		temp_partial <- subset(temp, (time>=key_exp_parameters$T_IGNITION[keyRow]-0.4 ) & (time<=key_exp_parameters$T_IGNITION[keyRow])  )
		V_ig <- mean( sqrt(temp_partial$x_vel_fit^2 + temp_partial$y_vel_fit^2) )
	}


	df.scatter <- data.frame(expname[1],
							temp$do[1], Vo, fuel, Vo_ofc, Dvalue,
							K_comb, K_ext, total_acc_comb, total_acc_ext, V_ig)


	if ( i == 1){
		df.global <- df.temp
		dfscatter.global <- df.scatter
	}else{
		df.global <- rbind(df.global, df.temp)
		dfscatter.global <- rbind(dfscatter.global, df.scatter)
	}
}  #for csv.filenames

df.global <- setNames(df.global, c("expname","time","do",
									"x_loc_fit","x_vel_fit",
									"y_loc_fit","y_vel_fit",
									"fuel","D","Xe","p") )
dfscatter.global <- setNames(dfscatter.global,c("expname","do","Vo",
									"fuel","Vofc","D",
									"K_comb","K_ext","total_acc_comb","total_acc_ext","Vig"))

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


# just a plot of all Methanol-Xenon dorplets for visual observation only
do_low <- 0.0 #3.1
do_high <- 6.5 #3.8
metXenon_only <- ggplot(subset(df.global, Xe == 0 &
			fuel== "Heptane" & do > do_low & do <= do_high ) )
metXenon_only <- metXenon_only + 
	geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
metXenon_only <- metXenon_only + 	theme_bw() +
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
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(metXenon_only, file="Heptane_Only.pdf", width=size.w, height=size.h, units=un)

# list of methanol-xenon experiments with similar trajectories (category 1) to
# that of X182M01 (an evaporating droplet)
methXenon_cat1 <- c("X182M01",
					"X136M01",
					"X168M05", 
					# "X175M06",
					# "X119M02",
					# "X119M05",					
					"X119M06",
					"X119M04",
					"X136M03")
dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key_time_points, 
				   				expInterestNames=methXenon_cat1)
globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal

# p_methXenon_cat1 <- ggplot(subset(df.global, expname== methXenon_cat1) )
p_methXenon_cat1 <- ggplot(subset(df.global, df.global$expname %in% methXenon_cat1) )
p_methXenon_cat1 <- p_methXenon_cat1 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
p_methXenon_cat1 <- p_methXenon_cat1 + 	theme_bw() +
	geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
	 mapping=aes(x=x_loc_fit, y=y_loc_fit),
	 shape=17,
	 colour="black", size=3) + 
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
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_methXenon_cat1, file="methanolXenon_cat1.pdf", width=size.w, height=size.h, units=un)


# list of methanol-xenon experiments with similar trajectories (category 2) 
methXenon_cat2 <- c("X175M06",
					"X163M04",
					"X168M07",
					"X144M05",
					"X163M02",
					"X161M05",
					"X163M03",
					"X121M03", 
					"X144M01",
					"X144M06",
					"X175M04",  #--original--#
					"X161M03",
					"X168M05",
					"X168M06",
					"X175M01",
					"X175M02",
					"X175M03",
					"X175M05")
					# "X182M01")
dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key_time_points, 
				   				expInterestNames=methXenon_cat2)
globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal

# colour=sqrt(x_vel_fit^2+y_vel_fit^2)
# p_methXenon_cat2 <- ggplot(subset(df.global, expname== methXenon_cat2) )
p_methXenon_cat2 <- ggplot(subset(df.global, df.global$expname %in% methXenon_cat2) )
p_methXenon_cat2 <- p_methXenon_cat2 + geom_point(mapping=aes(x=x_loc_fit, 
	y=y_loc_fit, colour=sqrt(x_vel_fit^2+y_vel_fit^2) )) +
	geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
	 mapping=aes(x=x_loc_fit, y=y_loc_fit),
	 shape=17,
	 colour="black", size=3)	
p_methXenon_cat2 <- p_methXenon_cat2 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.1, 0.75),
	# legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6			#specifies height of .pdf of plot in units specified by un
un <- "in"			#specifies unit of size.w and size.h
ggsave(p_methXenon_cat2, file="methanolXenon_cat2.pdf", width=size.w, height=size.h, units=un)


# methXenon_cat2_velocity <- ggplot(subset(df.global, expname== methXenon_cat2) )
methXenon_cat2_velocity <- ggplot(subset(df.global, df.global$expname %in% methXenon_cat2) )
methXenon_cat2_velocity <- methXenon_cat2_velocity + geom_point(mapping=aes(x=time, 
	y=sqrt(x_vel_fit^2 + y_vel_fit^2), colour=do)) 
methXenon_cat2_velocity <- methXenon_cat2_velocity + 	theme_bw() +
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
	ylab(expression("Velocity Magnitude (mm/s)") ) +
	xlab(expression("Time (s)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(methXenon_cat2_velocity, file="methanolXenon_cat2_Velocities_doGroup.pdf", width=size.w, height=size.h, units=un)



methXenon_cat5 <- c("X163M07",
					"X163M01",
					"X119M04",
					"X121M01")
# p_methXenon_cat5 <- ggplot(subset(df.global, expname== methXenon_cat5) )
p_methXenon_cat5 <- ggplot(subset(df.global, df.global$expname %in% methXenon_cat5) )
p_methXenon_cat5 <- p_methXenon_cat5 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
p_methXenon_cat5 <- p_methXenon_cat5 + 	theme_bw() +
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
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_methXenon_cat5, file="methanolXenon_cat5.pdf", width=size.w, height=size.h, units=un)



# experiments which have similar diameters to experiments
# where droplet is just evaporating
sim_experiments <- c("X136M01", "X144M01", "X144M04", "X168M05", "X182M01")
do_X182M01 <- 3.2
p_incr <- 0.10
# p_similar <- ggplot(subset(df.global, expname == sim_experiments))
p_similar <- ggplot(subset(df.global, 
			Xe != 0 & fuel== "Methanol" & do <= (1+p_incr)*do_X182M01 & do >= (1-p_incr)*do_X182M01 ) )
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
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_similar, file="methanolXenon_doSimilar.pdf", width=size.w, height=size.h, units=un)




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

ggsave(p1v3, file="dropTraj_all_hepProGly.pdf", width=size.w, height=size.h, units=un)


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

ggsave(p1v4, file="dropTraj_all_hepMethanol.pdf", width=size.w, height=size.h, units=un)



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

ggsave(p1v5, file="dropTraj_PropGly_diffGroup.pdf", width=size.w, height=size.h, units=un)


size.w2 <- 3.25	    #specifies width of .pdf of plot in units specified by un
size.h2 <- 2.8		#specifies height of .pdf of plot in units specified by un
ptsize2 <- 1.5
txtsize2 <- 12
titlsize2 <- 7

ptsize <- 4.0
p2 <- ggplot(dfscatter.global)
p2 <- p2 + aes(x=do, y=Vo)
# p2 <- p2 + geom_point(mapping=aes(x=do, y=Vo, shape=fuel, colour=fuel, size=1.2))
p2 <- p2 + aes(colour=fuel, shape=fuel) 
p2 <- p2 + geom_point(size=ptsize2) +
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
	legend.position=c(0.8, 0.87),
	legend.title = element_blank(),
	legend.text = element_text(size=9), 
	axis.title.x = element_text(size=txtsize2),
	axis.title.y = element_text(size=txtsize2),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(2.5,"mm"),
	legend.key.width = unit(3,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=txtsize2,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("D"[o]*" (mm)") ) +
	ylab(expression("V"[o]*" (mm/s)") ) 	

# ggsave(p2, file="dovsVo_all_droplets.pdf", width=size.w, height=size.h, units=un)
ggsave(p2, file="dovsVo_all_droplets.pdf", width=size.w2, height=size.h2, units=un)



p2 <- ggplot(dfscatter.global)
p2 <- p2 + aes(x=do, y=( (Vig-Vo)/Vo ) )
# p2 <- p2 + geom_point(mapping=aes(x=do, y=Vo, shape=fuel, colour=fuel, size=1.2))
p2 <- p2 + aes(colour=fuel, shape=fuel) 
p2 <- p2 + geom_point(size=ptsize2) +
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
	legend.position=c(0.8, 0.87),
	legend.title = element_blank(),
	legend.text = element_text(size=9), 
	axis.title.x = element_text(size=txtsize2),
	axis.title.y = element_text(size=txtsize2),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(2.5,"mm"),
	legend.key.width = unit(3,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=txtsize2,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("D"[o]*" (mm)") ) +
	ylab(expression( "(V"[ig]*"-V"[o]*")/V"[o]) ) 	

# ggsave(p2, file="dovsVo_all_droplets.pdf", width=size.w, height=size.h, units=un)
ggsave(p2, file="VigVovsDo_all_droplets.pdf", width=size.w2, height=size.h2, units=un)





p2 <- ggplot(dfscatter.global)
p2 <- p2 + aes(x=do, y= (Vig-Vo) )
# p2 <- p2 + geom_point(mapping=aes(x=do, y=Vo, shape=fuel, colour=fuel, size=1.2))
p2 <- p2 + aes(colour=fuel, shape=fuel) 
p2 <- p2 + geom_point(size=ptsize2) +
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
	legend.position=c(0.8, 0.3),
	legend.title = element_blank(),
	legend.text = element_text(size=9), 
	axis.title.x = element_text(size=txtsize2),
	axis.title.y = element_text(size=txtsize2),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(2.5,"mm"),
	legend.key.width = unit(3,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=txtsize2,colour="black") ) +
	# guides(colour=guide_legend(ncol=5)) +
	# guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("D"[o]*" (mm)") ) +
	ylab(expression( "V"[ig]*"-V"[o]) ) 	

# ggsave(p2, file="dovsVo_all_droplets.pdf", width=size.w, height=size.h, units=un)
ggsave(p2, file="VigvsDo_all_droplets.pdf", width=size.w2, height=size.h2, units=un)

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




heptaneXenon_cat1 <- c("X177H12",
					   "X177H06",
					   "X177H07",
					   "X100H07",
					   "X177H04",
					   "X126H07",
					   "X129H02",
					   "X140H05",
					   "X100H01",
					   "X126H01",
					   "X126H02",
					   "X126H06",
					   "X161H01",
					   "X170H03")
dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key_time_points, 
				   				expInterestNames=heptaneXenon_cat1)
globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal

# plot x-y trajectory for heptane/xenon droplets
p_heptaneXenon_cat1 <- ggplot(subset(df.global, df.global$expname %in% heptaneXenon_cat1) )
p_heptaneXenon_cat1 <- p_heptaneXenon_cat1 + geom_point(mapping=aes(x=x_loc_fit, 
	y=y_loc_fit, colour=expname) ) +
	geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
	 mapping=aes(x=x_loc_fit, y=y_loc_fit),
	 shape=17,
	 colour="black", size=3) 
p_heptaneXenon_cat1 <- p_heptaneXenon_cat1 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.8, 0.2),
	# legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_heptaneXenon_cat1, file="heptaneXenon_cat1.pdf", width=size.w, height=size.h, units=un)


heptaneNonXenon_cat1 <- c("H05H103",
						  "H19H201",
						  "H03H201",
						  "H03H202")

heptane_cat2 <- c("H04H201",
				  "H11H101",
				  "H19H101",
				  "H10H101")

heptane_cat3 <- c("H04H101",
				  "H03H101",
				  "H12H101")


propGly_cat4 <- c("C080D05",
				  "C085D03",
				  "C104D05",
				  "C077D02",
				  "C080D01",
				  "C101D05",
				  "C104D06",
				  "C077D06",
				  "C101D06",
				  "C077D04")
dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key_time_points, 
				   				expInterestNames=propGly_cat4)
globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal

# plot x-y trajectory for all prop/gly drplets with paths color coded
# for local velocity magnitude
p_propGly_cat4 <- ggplot(subset(df.global, df.global$expname %in% propGly_cat4) )
p_propGly_cat4 <- p_propGly_cat4 + geom_point(mapping=aes(x=x_loc_fit, 
	y=y_loc_fit, colour=sqrt(x_vel_fit^2+y_vel_fit^2)) ) +
	geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
	 mapping=aes(x=x_loc_fit, y=y_loc_fit),
	 shape=17,
	 colour="black", size=3) 
p_propGly_cat4 <- p_propGly_cat4 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.1, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_propGly_cat4, file="propgly_cat4.pdf", width=size.w, height=size.h, units=un)


# propanol/glycerol droplet names for experiments with category 2 paths
propGly_cat2 <- c("C077D03",
	 			  "C101D02")

# propanol/glycerol droplet names for experiments with category 3 paths
propGly_cat3 <- c("C108D03")

propGly_all <- subset(df.global, fuel == "Pro95/Gly5")
dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key_time_points, 
				   				expInterestNames=unique(propGly_all$expname))
globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal
FC_timeIndexMatchGlobal <- dataTable$FC_timeIndexMatchGlobal

# plot x-y trajectory for all propanol/glycerol droplets
p_propGly_all <- ggplot(subset(df.global, df.global$expname %in% unique(propGly_all$expname) ) )
p_propGly_all <- p_propGly_all + geom_point(mapping=aes(x=x_loc_fit, 
	y=y_loc_fit, colour=expname ) ) +
	geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
	 mapping=aes(x=x_loc_fit, y=y_loc_fit),
	 shape=17,
	 colour="black", size=3) 
p_propGly_all <- p_propGly_all + theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.1, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6			#specifies height of .pdf of plot in units specified by un
un <- "in"			#specifies unit of size.w and size.h
ggsave(p_propGly_all, file="propgly_XYtraj.pdf", width=size.w, height=size.h, units=un)



# generate plots for propanol/glycerol droplets with curved paths.
# generate x-y paths, with markers for onset of flame contraction times
propGly_interest <- c("C108D02",
				  "C108D03",
				  "C101D02",
				  "C085D03",
				  "C104D05",
				  "C101D06")
dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key_time_points, 
				   				expInterestNames=propGly_interest)
globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal
FC_timeIndexMatchGlobal <- dataTable$FC_timeIndexMatchGlobal

p_propGly_all2 <- ggplot(subset(df.global, df.global$expname %in% propGly_interest ))
p_propGly_all2 <- p_propGly_all2 + geom_point(mapping=aes(x=x_loc_fit, 
	y=y_loc_fit, colour=expname ) ) +
	geom_point(data=globalDataframe[FC_timeIndexMatchGlobal, ],
	 mapping=aes(x=x_loc_fit, y=y_loc_fit),
	 shape=17,
	 colour="black", size=3) 
p_propGly_all2 <- p_propGly_all2 + theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.1, 0.65),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p_propGly_all2, file="propgly_XYtraj_FCtimes.pdf", width=size.w, height=size.h, units=un)


# plot acceleration and burning rate time series for propaynol/glycerol droplets
p_propGly_all3 <- ggplot(data = globalDataframe, aes(x=time))
p_propGly_all3 <- p_propGly_all3 + 
	geom_point( aes(y=sqrt(x_acc_fit^2 + y_acc_fit^2), colour=expname))
p_propGly_all3 <- p_propGly_all3 + geom_point(mapping=aes(y=k_bw1*4, colour=expname))
p_propGly_all3 <- p_propGly_all3 +
	scale_y_continuous(sec.axis = sec_axis(~.*5, name=expression("K (mm"^2*"/s)") ) )
p_propGly_all3 <- p_propGly_all3 + theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.1, 0.65),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("Time (s)") ) +
	ylab(expression("Acceleration (mm/s"^2*")") ) 	
ggsave(p_propGly_all3, file="propGly_xyAccBurnRate.pdf", width=size.w, height=size.h, units=un)


# plot average K vs average Acc for all droplets where applicable
ptsize <- 4.0
p_kvAc <- ggplot( subset(dfscatter.global, K_comb != "NA" & fuel != "Heptane") )
p_kvAc <- p_kvAc + aes(y=total_acc_comb, x=K_comb)
p_kvAc <- p_kvAc + aes(colour=fuel, shape=fuel) 
p_kvAc <- p_kvAc + geom_point(size=ptsize) +
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
p_kvAc <- p_kvAc + 	theme_bw() +
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
	ylab(expression("Acceleration (mm/s"^2*")") ) +
	xlab(expression( bar(K)["burn"]*" (mm"^2*"/s)") ) 	

ggsave(p_kvAc, file="KburnvsAcc_allexp.pdf", width=size.w, height=size.h, units=un)


# create scatter plot of average K vs average Acc during combusiton and after extinction
KaccBurn_data <- data.frame(dfscatter.global$expname,
							dfscatter.global$K_comb,
							dfscatter.global$total_acc_comb,
							dfscatter.global$fuel)
KaccBurn_data <- setNames(KaccBurn_data, c("expname","K_comb","total_acc","fuel"))
KaccBurn_data <- subset(KaccBurn_data,K_comb != "NA")
KaccBurn_data <- melt(KaccBurn_data, id=c("expname","fuel", "total_acc"))

KaccExt_data <- data.frame(dfscatter.global$expname,
							dfscatter.global$K_ext,
							dfscatter.global$total_acc_ext,
							dfscatter.global$fuel)							
KaccExt_data <- setNames(KaccExt_data, c("expname","K_ext","total_acc","fuel"))
KaccExt_data <- subset(KaccExt_data,K_ext != "NA")
KaccExt_data <- melt(KaccExt_data, id=c("expname","fuel","total_acc"))

KaccCombined_data <- rbind(KaccBurn_data, KaccExt_data)



KavgAavg <- ggplot() +
# geom_point(data = KaccBurn_data, aes(x=K_comb,y=total_acc_comb, shape=fuel, colour=fuel), size=4.0) + 
geom_point(data = subset(KaccCombined_data,fuel != "Heptane"), aes(x=value,y=total_acc, shape=variable), size=4.0) +
			aes(colour=fuel) + 
			scale_colour_manual(
				values=c(
					"#e41a1c",  #red (set1)
					"#984ea3",  #purple (set1)
					"#41b6c4",   #teal   (use when plotting EABB vs tdtv)
					"#4daf4a",  #green (set1)
					"#e7298a",  #pink (dark2)
					"#d95f02",  #brown (dark2)
					"#377eb8",  #blue (set1)
					"#66c2a5")) +  #army grn (dark2)
theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.8, 0.80),
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
	ylab(expression("Acceleration (mm/s"^2*")") ) +
	xlab(expression( bar(K)*" (mm"^2*"/s)") ) 
ggsave(KavgAavg, file="KavgvsAcc_MethanolPropGly.pdf", width=size.w, height=size.h, units=un)





