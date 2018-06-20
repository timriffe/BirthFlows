
# Author: tim
###############################################################################


Arc <- function(a,b,nvert){
	pivec <- seq(from = 0 , to = pi, length.out=nvert)
	x <- a * cos(pivec) + a
	y <- b * sin(pivec)
	out <- matrix(c(x,y),nrow=nvert,ncol=2)
	return(out)
}

# how many radians does the standard arc need to be rotated, 
radians <- function(p1,p2){
	rise <- p2$y-p1$y
	run <- p2$x-p1$x
	add <- ifelse(p1$x<p2$x,0,pi)
	add+atan(rise/run)
}

# distance formula, used to calculate a (major radius)
pythag <- function(p1,p2){
	((p2$y-p1$y)^2 + (p2$x-p1$x)^2)^.5
}
# we use matrx multiplication to perform the rotation, since it's more efficient in R
rotationMatrix <- function(theta){
	matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),ncol=2,nrow=2)
}

rotatearc <- function(arc,Rmat){
	rotatevec <- function(arc,Rmat){
		Rmat%*%c(arc)
	}
	t(apply(arc,1,rotatevec,Rmat=Rmat))
}

arcpoints <- function(p1,p2,nvert=200,minorratio=.4){
	a <- pythag(p1,p2)*.5
	b <- a*minorratio
	theta <- radians(p1,p2)
	Rmat <- rotationMatrix(theta)
	arc0 <- Arc(a,b,nvert=nvert)
	arc1 <- rotatearc(arc0,Rmat)
	arcout <- arc1
	arcout[,1] <- arcout[,1]+p1$x
	arcout[,2] <- arcout[,2]+p1$y
	arcout
}

arcpoly <- function(p1, p2, nvert = 200, minorratio = .4, maxthick = 1){
	a      <- pythag(p1,p2)*.5 # cuz it's  radius...
	b      <- a * minorratio
	
	b1     <- b + maxthick / 2
	b2     <- b - maxthick / 2
	
	theta  <- radians(p1,p2)
	Rmat   <- rotationMatrix(theta)
	arc01  <- Arc(a,b1,nvert=nvert)
	arc02  <- Arc(a,b2,nvert=nvert)
	
	arc11  <- rotatearc(arc01,Rmat)
	arc12  <- rotatearc(arc02,Rmat)
	
	arc11[, 1]  <- arc11[, 1] + p1$x
	arc12[, 1]  <- arc12[, 1] + p1$x
	arc11[, 2]  <- arc11[, 2] + p1$y
	arc12[, 2]  <- arc12[, 2] + p1$y
	
	arc12  <- arc12[nrow(arc12):1, ]
	
	arcout <- rbind(arc11,arc12)
	
	colnames(arcout) <- c("x","y")
	arcout
}






