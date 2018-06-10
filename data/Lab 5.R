pmean<-function(directory,pollutant,id=1:332){
  d<-directory
  setwd(work_dir)
  i<-1
  file_out<-data.frame()
  while (i<=length(id)){
    if (id[i]<=9) file<-read.csv(paste0("00",id[i],".csv"))
    else if (id[i]<=99) file<-read.csv(paste0("0",id[i],".csv"))
    else file<-read.csv(paste0(id[i],".csv"))
    i<-i+1
    file_out<-rbind(file_out,file)
  }
  mean(file_out[(names(file_out)==pollutant)][,1],na.rm = T)
}

completely<-function(directory, id=1:332){
  d<-directory
  setwd(work_dir)
  i<-1
  file_out<-data.frame(id_col=NA,value=NA)
  while (i<=length(id)){
    if (id[i]<=9) file<-read.csv(paste0("00",id[i],".csv"))
    else if (id[i]<=99) file<-read.csv(paste0("0",id[i],".csv"))
    else file<-read.csv(paste0(id[i],".csv"))
    file_out[nrow(file_out)+1,]<-c(id[i],nrow(file[complete.cases(file),]))
    i<-i+1
  }
  file_out[complete.cases(file_out),]
}