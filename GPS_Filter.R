minimum_distance <- .05
largest_angle <- .4
decimal <- 4
b <- 2
while(b<(nrow(pointtable))){
  p12 <- rdist.earth(pointtable[b-1,],pointtable[b,])
  p23 <- rdist.earth(pointtable[b,],pointtable[b+1,])
  p13 <- rdist.earth(pointtable[b-1,],pointtable[b+1,])  
  angle <- (p12^2+p23^2-p13^2)/(2*p12*p23)
  if(is.na(angle)){
    b <- b+1
    next()
  }
  if(angle > 1)
    angle <- 1
  if(angle < -1)
    angle <- -1
  if(acos(angle)<largest_angle){
    pointtable <- pointtable[-(b),]
  }
  else{
    b <- b+1
  }
}
c <- 1
while(c<(nrow(ftable)-1)){
  if(rdist.earth(pointtable[c,],pointtable[c+1,])<minimum_distance){
    pointtable <- pointtable[-(c+1),]
  }
  else{
    c <- c+1
  }
}