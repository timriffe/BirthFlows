setwd("/home/tim/Desktop/ArtSubmissions/SwedenBirths/")

library(colorspace)
library(spatstat)

show.pal <- function(colorvec, square = TRUE){
	n <- length(colorvec)
	if (square){
		nside   <- ceiling(sqrt(n))
		colorvec <- c(colorvec, rep(NA,nside^2 - n))
		colsmat <- matrix(colorvec,nrow=nside,byrow=TRUE)
		plot(0:nside,0:nside,type= "n",asp=1,axes=FALSE,xlab="",ylab="")
		rect(row(colsmat)-1,
			 col(colsmat)-1,
			 row(colsmat),
			 col(colsmat),
			 col = colsmat,
			 border = "white", asp = 1)
	} else {
		plot(0:n,type= "n",ylim=c(0,1),xlim=c(0,n),axes=FALSE,xlab="",ylab="")
		rect(0:(n-1),0,1:n,1,col=colorvec,border="white")
	}
}

# darken by changing luminance
darkenhex <- function(hexcols,fac=.3){
	# from @Roland:
	#https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values
	require(colorspace)
	zz   <- textConnection(hexcols)
	cols <- readhex(file = zz,	class = "RGB")
	close(zz)
	# transform to hue/lightness/saturation colorspace
	cols <- as(cols, "HLS")
	cols@coords[, "L"] <- pmax(0, cols@coords[, "L"] - fac)
	cols <- as(cols, "RGB")
	hex(cols)
}

# a gives a number from 0 to 1. 0 is black, 1 is white. vectorized
howdark <- function(col){
	suppressMessages(colorspace::hex2RGB(spatstat::to.grey(col))@coords[, 1])
}

howsat <- function(colvec){
	rgb2hsv(col2rgb(colvec))[2, ]
}
howdark2 <- function(colvec){
	rgb2hsv(col2rgb(colvec))[3, ]
}

# satdarkmin <- function(pars, h, starget = .5, darktarget = .5){
# 	col <- hsv(h, pars["s"], pars["v"])
# 	dk  <- howdark(col)
# 	st  <- howsat(col)
# 	sqrt(((st-starget)^2+(dk-darktarget)^2)/2)
# }
eqspace2 <- function(colvec,dfrom,dto,sfrom,sto,show=TRUE){
	darkvals   <- howdark(colvec)
	colvec     <- colvec[order(darkvals)]
	darkvals   <- sort(darkvals)
	n          <- length(darkvals)
	targetdark <- darkvals - lm(darkvals~I(1:n))$res
	
	if (missing(dfrom)){
		dfrom  <- targetdark[1]
	}
	if (missing(dto)){
		dto    <- targetdark[n]
	}
	if (missing(sfrom)){
		sfrom  <- .8
	}
	if (missing(sto)){
		sto    <- .6
	}
	
	
	if (dfrom < 0){
		dfrom <- .01
	}
	if (dto > 1){
		dto <- .99
	}
	
	targets    <- seq(sfrom, sto, length = n)
	targetdark <- seq(dfrom, dto, length = n)
	colvec2 <- colvec
	for (i in 1:n){
		col1  <- suppressMessages(darken.to(colvec[i], targetdark[i]))
		col2 <- to.saturated(colvec[i],targets[i])
		proposal1 <- blend(col1,col2)
		col1  <- suppressMessages(darken.to(proposal1, targetdark[i]))
		col2 <- to.saturated(proposal1,targets[i])
		colvec2[i] <- blend(col1,col2)
	}
	colvec2 <- colvec2[colvec2 != "#000000"]
	
	if (show){
		show.pal(colvec2)
	}
	colvec2
}

blend <- function(col1, col2){
	rgb1   <- col2rgb(col1)
	rgb2   <- col2rgb(col2)
	rgbnew <- sqrt((rgb1^2+rgb2^2)/2)
	rgb2hex(c(rgbnew))
}


# darkens a color to a specific grayscale target darkness. not vectorized
darken.to <- function(hexcol,target=.2){
	require(colortools)
	require(spatstat)
	
	# a function to optimize the ideal darkening factor
	mintarg <- function(fac,col,target){
		hexcol <- darkenhex(hexcol,fac)
		abs(target - howdark(hexcol))
	}
	# get darkening factor
	fac    <- optimize(mintarg,c(-.8,.8),col=hexcol,target=target)$minimum
	# now darken by that much...
	colout <- darkenhex(hexcol,fac)
	colout
}

#to.saturated(colvec2, s=seq(.9,.6,length=n))

# will use this for surface ramps.
my.palette <- function(col,n,from=.2,to=.9){
	fromcol <- darken.to(col,from)
	tocol   <- darken.to(col,to)
	colorRampPalette(c(fromcol,tocol),space="Lab")(n)
}

# we can sort colors by darkness, but what if we want them in equally spaced
# dark levels, with minimal purturbation. Here we go!
eqspace <- function(colvec,from,to,show=TRUE,sateq=TRUE){
	darkvals   <- howdark(colvec)
	colvec     <- colvec[order(darkvals)]
	darkvals   <- sort(darkvals)
	n          <- length(darkvals)
	targetdark <- darkvals - lm(darkvals~I(1:n))$res

    if (missing(from)){
    	from <- targetdark[1]
    }
	if (missing(to)){
		to <- targetdark[n]
	}
	if (from < 0){
		from <- .01
	}
	if (to > 1){
		to <- .99
	}
	targetdark <- seq(from, to, length = n)
	colvec2 <- colvec
	for (i in 1:length(colvec)){
		colvec2[i] <- suppressMessages(darken.to(colvec[i], targetdark[i]))
	}

	colvec2 <- colvec2[colvec2 != "#000000"]
	

	if (show){
		show.pal(colvec2)
	#	show.pal(to.saturated(colvec2, s=seq(.9,.5,length=n)))
	}
	
	colvec2
}




# library(devtools)
# # install_github("andreacirilloac/paletter")
# library(paletter)
# 
# pex <- create_palette("Data/Van_Gogh_Green_Corn.jpg", number_of_colors = 32,
# 						type_of_variable = "categorical")

cols0 <- rev(c("#9D6D2E", "#C1852B", "#BAA53F", "#AD9D3F", "#969A46", "#7B903F", 
					   "#427C45", "#4D8B62", "#5A978F", "#5194A4", "#41727D", "#566B81"
))				
cols3 <-
c("#993532", "#971E17", "#B4463A", "#E4412E", "#B03425", "#FC6041", 
  "#FC592F", "#9D3417", "#8F2E12", "#EA521C", "#E05F29", "#753B22", 
  "#F56E2C", "#D25315", "#C17950", "#DC8B56", "#834C26", "#A15A26", 
  "#9C501A", "#E7863F", "#A6612C", "#C76711", "#95602B", "#AE7132", 
  "#E08E35", "#7A501D", "#7C521F", "#D98E23", "#736230", "#82141B", 
  "#DE4F53", "#961515")
cols4 <- 
c("#7D2827", "#D55E39", "#89320B", "#C6784C", "#A54C09", "#A3560A", 
  "#B46108", "#8F4D01", "#DF8921", "#AF6601", "#F2981B", "#DA9203", 
  "#9F7E20", "#9A7817", "#B68E13", "#6A5913", "#746520", "#807910", 
  "#BCC142", "#81890A", "#D5E124", "#BDE01F", "#6E9518", "#4393F9", 
  "#2879FE", "#3565D1", "#4A75D8", "#022C8E", "#395EB7", "#0C2DBC", 
  "#24368B", "#0A1593")
cols5 <-
c("#9D605E", "#B68B7F", "#D0522A", "#9F705F", "#EC5417", "#B34107", 
  "#CD6429", "#EAA171", "#E47B33", "#EBAC7F", "#A9876F", "#CB6E2C", 
  "#D08550", "#D7925E", "#F4C399", "#F3B580", "#CF9153", "#DBA772", 
  "#CB8A43", "#C59D6D", "#FED8A4", "#B6751B", "#DBB47D", "#A18C6C", 
  "#DBAA5A", "#A6803E", "#DBC7A1", "#FEC74F", "#F7C854", "#F4D37A", 
  "#6B86C0", "#7F7FAD")
cols6<-
c("#760400", "#9B0800", "#790B00", "#903F31", "#630F00", "#731E0C", 
  "#6B2008", "#EA612B", "#7E370D", "#85512B", "#89430F", "#AC570E", 
  "#D26802", "#FA9832", "#DB7207", "#C07425", "#E67C08", "#94662C", 
  "#8E590E", "#9A5C01", "#CA9342", "#D89936", "#D9A43F", "#EAA218", 
  "#E7AD28", "#AC8820", "#AF8F33", "#D1A917", "#E1C413", "#7A6E02", 
  "#949029", "#C3BE29")
cols7 <-
c("#823B36", "#99170A", "#7A2A1F", "#7C1808", "#99412D", "#EC5620", 
  "#B76241", "#793515", "#8F4C29", "#84421E", "#CE6830", "#794425", 
  "#C1662F", "#C45F1D", "#E68543", "#D37A2E", "#7F3B01", "#D7924D", 
  "#9E5B14", "#B48047", "#D8943C", "#CE8615", "#AD7D1F", "#825E0B", 
  "#927633", "#C2A53E", "#80712B", "#776913", "#837928", "#746F24", 
  "#7D040B", "#7E2224")
cols8 <- c("#705C36", "#8E814C", "#8E8840", "#928915", "#D2D295", "#61673C", 
		   "#4D5A2D", "#889862", "#95AD72", "#8EA672", "#5A6D47", "#709063", 
		   "#426243", "#669572", "#46755B", "#275845", "#376051", "#567C6F", 
		   "#456F68", "#2D6560", "#286B6E", "#528487", "#405E60", "#497A7E", 
		   "#346F74", "#1E5D63", "#286369", "#3D7880", "#2B6B76", "#095064", 
		   "#548D9E", "#2D4B5D")

cols9 <- c("#894C49", "#6D3A27", "#6D5541", "#756646", "#958D3C", "#A7AC6B", 
  "#8B9B5E", "#346524", "#4E7144", "#356F36", "#599A6E", "#337152", 
  "#73B79F", "#326A5B", "#3B6A60", "#5F9690", "#2D6164", "#577F89", 
  "#567881", "#5C9AAB", "#387A8E", "#86BFD4", "#367795", "#68A5C8", 
  "#2C5A7B", "#305A7D", "#657E95", "#81A4C4", "#427AB9", "#435972", 
  "#3A5B9B", "#626595")
cols10 <- c("#449612", "#528D3B", "#397E32", "#1D7326", "#318B46", "#39895A", 
			"#2C8C5A", "#368B62", "#328B68", "#32A27C", "#287E64", "#309F87", 
			"#33A9A0", "#45B9C0", "#14757F", "#4EDCF8", "#31A1BC", "#26A0C0", 
			"#217D97", "#107A99", "#1AADDA", "#0797C6", "#3EC6F6", "#4A98BC", 
			"#08AAF6", "#1E9CDD", "#087BC9", "#054674", "#003E7A", "#0A4078", 
			"#054388", "#184181")
cols11 <- c("#802725", "#99493F", "#C54C30", "#E46847", "#C06739", "#BC6335", 
			"#F9894A", "#E48842", "#E58839", "#F3983F", "#F6B061", "#F2A332", 
			"#D09D43", "#C5A56A", "#CDA844", "#F3EA75", "#FBF375", "#FAFC74", 
			"#2C8692", "#137893", "#357589", "#256081", "#115780", "#2C5076", 
			"#1A4779", "#314D80", "#405689", "#814466", "#7A4058", "#C33343", 
			"#872225", "#884042")
cols12 <-c("#967A43", "#B1A377", "#B19C58", "#DCC884", "#585235", "#5A5438", 
		   "#BDB97C", "#C3BE7B", "#A19F70", "#A3A067", "#CAC891", "#5C5B3D", 
		   "#626243", "#464A33", "#87936C", "#96A67C", "#3D4937", "#698D6D", 
		   "#54705B", "#739B82", "#639174", "#4D6959", "#7CAE96", "#78A490", 
		   "#557A6D", "#4C8270", "#447A6A", "#77A195", "#294C47", "#34675F", 
		   "#2F504F", "#384B4E")
cols13 <-c("#A18D4C", "#B4A312", "#C2CA5D", "#6E7F02", "#9CB648", "#709517", 
		   "#668935", "#76A140", "#3A760E", "#3D691F", "#4E8130", "#5AA440", 
		   "#599B4D", "#377630", "#4D8C4E", "#2A7F34", "#4E9C62", "#29754D", 
		   "#609881", "#437561", "#406C5F", "#629588", "#3A6D6B", "#46717B", 
		   "#4B7793", "#0D446E", "#5397CA", "#8DC6FC", "#1A558D", "#5D9EE1", 
		   "#3B89DC", "#344B80")
cols14 <- c("#927B5E", "#C9AD64", "#BDAD67", "#C3A606", "#A99A45", "#A18D1F", 
			"#9F9724", "#F2E92F", "#A19D05", "#F3F4A8", "#BDBF3E", "#888C40", 
			"#999F28", "#CDD667", "#819321", "#8E9C44", "#5A750F", "#5F7929", 
			"#687653", "#7A9755", "#657E55", "#4A7B3D", "#5D8F6C", "#48745B", 
			"#32507A", "#193E82", "#92AFE6", "#6B87D4", "#7A97EB", "#5A689F", 
			"#5F5E93", "#4846A9")

# plot(sort(howdark(cols5)))

warms             <- rev(eqspace2(cols3,.2,.6,.95,.6))
#warms             <- rev(eqspace2(cols3))
#harvest           <- rev(eqspace2(cols0))
mountainrise      <- rev(eqspace2(cols4,.2,.6,.9,.4))
#show.pal(mountainrise)
#turner            <- rev(eqspace2(cols5))
autumn1           <- rev(eqspace2(cols6,.3,.7,.75,.6))
autumn2           <- rev(eqspace2(cols7,.3,.7,.75,.6))

vangoghnight      <- rev(eqspace2(cols8,.2,.7,.8,.6))
monetlillies      <- rev(eqspace2(cols9,.2,.7,.8,.5))
coral             <- rev(eqspace2(cols10,.2,.7,.8,.2))
jellyfish         <- rev(eqspace2(cols11,.2,.7,.8,.3))
#yosemitefog         <- rev(eqspace2(cols12,.2,.7,.8,.3))
vangoghwheat         <- rev(eqspace2(cols13,.3,.7,.75,.2))
vangoghcorn         <- rev(eqspace2(cols14,.3,.7,.8,.4))

# show.pal(warms)
# show.pal(mountainrise)
# show.pal(autumn1)
# show.pal(autumn2)
# show.pal(vangoghnight)
# show.pal(monetlillies)
# show.pal(coral)
# show.pal(jellyfish)
# show.pal(vangoghwheat)
# show.pal(vangoghcorn)
# warms2             <- rev(eqspace(cols3,.2,.6))
# harvest2           <- rev(eqspace(cols0,.2,.6))
# mountainrise2      <- rev(eqspace(cols4,.2,.6))
# turner2            <- rev(eqspace(cols5,.2,.6))
# autumn12           <- rev(eqspace(cols6,.2,.6))
# autumn22           <- rev(eqspace(cols7,.2,.6))
# vangoghnight2      <- rev(eqspace(cols8,.2,.7))
# monetlillies2      <- rev(eqspace(cols9,.2,.6))
# coral2             <- rev(eqspace(cols10,.2,.5))
# jellyfish2         <- rev(eqspace(cols11,.1,.7))
# yosemitefog2       <- rev(eqspace(cols12))
# vangoghwheat2      <- rev(eqspace(cols13,.2,.5))
# vangoghcorn2       <- rev(eqspace(cols14,.2,.7))
# vangoghcombined    <- rev(eqspace(c(vangoghwheat2, vangoghcorn2)))
# vangoghcombined    <- vangoghcombined[-15]
# 
# warms3             <- rev(eqspace2(cols3,.2,.6,.2))
# harvest3           <- rev(eqspace2(cols0,.2,.6,.2))
# mountainrise3      <- rev(eqspace2(cols4,.2,.6,.2))
# turner3            <- rev(eqspace2(cols5,.2,.6,.2))
# autumn13           <- rev(eqspace2(cols6,.2,.6,.2))
# autumn23           <- rev(eqspace2(cols7,.2,.6,.2))
# vangoghnight3      <- rev(eqspace2(cols8,.2,.7,.2))
# monetlillies3      <- rev(eqspace2(cols9,.2,.6,.2))
# coral3             <- rev(eqspace2(cols10,.2,.6,.2))
# jellyfish3         <- rev(eqspace2(cols11,.2,.6,.2))
# yosemitefog3       <- rev(eqspace2(cols12,.2,.6,.2))
# vangoghwheat3      <- rev(eqspace2(cols13,.2,.6,.2))
# vangoghcorn3       <- rev(eqspace2(cols14,.2,.6,.2))

#show.pal(vangoghcombined[-15],F)
# show.pal(c(vangoghwheat2, vangoghcorn2))
# show.pal(c(vangoghwheat2, vangoghcorn2)[order(howdark(c(vangoghwheat2, vangoghcorn2)))])

# rather than equalize colors, how about just darken / lighten minimally
# to the desired ramp level
# colvec <- warms

# png("cols6.png")
# show.pal(cols6)
# dev.off()
# 
# png("cols6dk.png")
# show.pal(cols6[order(howdark(cols6))])
# dev.off()
# 
# png("cols6st.png")
# show.pal(cols6[order(howsat(cols6),decreasing=TRUE)])
# dev.off()
# 
# png("cols6eq.png")
# show.pal(eqspace2(cols6,.3,.7,.75,.6))
# dev.off()
# 
# # library(colorspace)
# # par(mfrow=c(2,1))
# # show.pal(cols2,FALSE)
# # text(.5:31.5,0,1:32,pos=1,cex=.5,xpd=TRUE)
# 
# pick <- c(1,2,5,7,11,12,18,21,25,29,30,32)
# # show.pal(cols2[pick],FALSE)
# # text(.5:(length(pick)-.5),0,pick,pos=1,cex=.5,xpd=TRUE)
# colorsharvest <- cols2[pick]


