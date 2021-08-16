BarrettSummaryStat <- function(x){
  z = dim(x)
  print(z)
  out = matrix(nrow = z[1],ncol =3)

  for(i in 1:z[1]){
    pos = 0
    nint = 0
    out[i,1]=mean(x[i,])
    for(j in 1:z[2]){
      if(x[i,j]>0){pos = pos+1}
      if(x[i,j]==round(x[i,j])){nint = nint+1}
     }
    out[i,2]=pos
    out[i,3]=nint

  }
  return(out)
}
