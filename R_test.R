
set_cht <- function(){
	if(grepl('mingw',sessionInfo()$R.version$os)){
		Sys.setlocale(category = "LC_ALL", locale = "cht")
	}else if(grepl('linux',sessionInfo()$R.version$os) | grepl('darwin',sessionInfo()$R.version$os)){
		Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
	}
}


make_all_UTF8 <- function(){
	### OS setting
	set_cht()
	options(warn=-1)

	### add yaml  (and path)
	dealRDatapro <-function(x, sep="\t", encoding="UTF-8"){
		if(FALSE %in% c(sapply(x,test_is.UTF8), sapply(names(x),test_is.UTF8) )){x
		}else{rawtsv <- tempfile()
			write.table(x, file=rawtsv, sep=sep)
			result <- read.table(file(rawtsv, encoding=encoding), sep=sep)
			unlink(rawtsv)
			result}}
	test_is.BIG5 <- function(s){!is.na(iconv(iconv(s,"BIG5","UTF-8"),"UTF-8","BIG5"))}
	test_is.UTF8 <- function(s){!is.na(iconv(iconv(s,"UTF-8","BIG5"),"BIG5","UTF-8"))}
	test_and_change <- function(s){if(is.na(s)){NA  
					}else if(test_is.UTF8(s)){s
					}else if(test_is.BIG5(s)){iconv(s,"BIG5","UTF-8")
					}else{s}}
	#######  有時候有 NA 問題

	ls <- objects(envir=.GlobalEnv)
	print(1:length(ls))
	for(i in 1:length(ls)){
		print(ls[i])
		temp <- get(ls[i])
		cla <- tail(class(temp),1)[1]
		if(cla=='numeric' | cla=='integer'){
			# pass
		}else if(cla=='character' ){	
			for(ic in 1:length(temp) ){temp[ic] <- test_and_change(temp[ic])}
			assign(ls[i], temp)
		}else if(cla=='data.frame'){
			if(grepl('mingw',sessionInfo()$R.version$os)){temp<-dealRDatapro(temp)} ## for windows

			if(FALSE %in% c(sapply(names(temp),test_is.UTF8))){
				names(temp) <- sapply(names(temp), test_and_change)}
			if(FALSE %in% c(sapply(temp,test_is.UTF8))){
				assign(ls[i], as.data.frame(sapply(temp, sapply, test_and_change), row.names=1:nrow(temp)))}

		}else if(cla=='list'){  ## yaml here
			names(temp) <- sapply(names(temp), test_and_change)
			assign(ls[i], lapply(temp, test_and_change))

			### for yaml ###
			# for(il in 1:length(temp)){
			# 	for(j in 1:length(temp[[il]])){
			# 		test_and_change(temp[[il]][[j]])}}
			# assign(ls[i], temp)

		}else if(cla=='function'){
			# function pass
		}else{
			print('pau')
			print(cla)
		}
		assign(objects(envir=.GlobalEnv)[i], get(ls[i]), envir=.GlobalEnv)
	}
	print('done!')
}


check_course <- function(){
	set_cht()
	test_is.BIG5 <- function(s){!is.na(iconv(iconv(s,"BIG5","UTF-8"),"UTF-8","BIG5"))}
	test_is.UTF8 <- function(s){!is.na(iconv(iconv(s,"UTF-8","BIG5"),"BIG5","UTF-8"))}

	## for test
	# path1 <- '/Users/thisray/Documents/work/R_swirl/test_ray/My_New_Course/My_First_Lesson/big5_test.yaml'
	# path2 <- '/Users/thisray/Documents/work/R_swirl/big5.yaml'
	# path3 <- '/Library/Frameworks/R.framework/Versions/3.3/Resources/library/swirl/Courses/DataScienceAndR/00-Hello-DataScienceAndR/lesson.yaml'
	# path <- c(path1, path2, path3)

	## yaml path
	path <- c(
		Sys.glob(file.path(.libPaths(), "swirl","*","*.yaml")),
		Sys.glob(file.path(.libPaths(), "swirl","*","*","*.yaml")),
		Sys.glob(file.path(.libPaths(), "swirl","*","*","*","*.yaml")),
		Sys.glob(file.path(.libPaths(), "swirl","*","*","*","*","*.yaml"))	)
	path <- na.omit(path)

	for(ip in 1:length(path)){
	result <- try( test <- yaml.load_file(path[ip]) )  ### need try-catch ####

	if(class(result) == "try-error"){print(paste('(error) not UTF-8 file: ',path[ip],sep=' '))
	}else{
		## check cht words use ##
		# temp_i <- 1
		# temp_ij <- c(1,1)
		# for(i in 1:length(test)){
		# 	for(j in 1:length(test[[i]])){
		# 		if(!test_is.UTF8(test[[i]][[j]])){temp_i<-0;temp_ij<-c(i,j);break}}}
		# if(temp_i==0){print(paste('need to check UTF-8 : ',path[ip],sep=' '))
		# 				print(test[[temp_ij[1]]][[temp_ij[2]]])}
	}}
	print('check over')
}
