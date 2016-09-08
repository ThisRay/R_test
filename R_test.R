

make_all_UTF8 <- function(){
	### OS setting
	if(grepl('mingw',sessionInfo()$R.version$os)){
		Sys.setlocale(category = "LC_ALL", locale = "cht")
	}else if(grepl('linux',sessionInfo()$R.version$os) | grepl('darwin',sessionInfo()$R.version$os)){
		Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
	}

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
	test_and_change <- function(s){if(is.na(s)){NA  #######  有時候有 NA 問題
					}else if(test_is.UTF8(s)){s
					}else if(test_is.BIG5(s)){iconv(s,"BIG5","UTF-8")
					}else{s}}

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
			names(temp) <- sapply(names(temp), test_and_change)
			assign(ls[i], as.data.frame(sapply(temp, sapply, test_and_change), row.names=1:nrow(temp)))
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

