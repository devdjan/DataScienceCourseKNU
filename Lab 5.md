Чтобы скрипт нормально работал, сначал нужно определить рабочую директорию, мы ее буем использовать как переменную, для удобства:
```r
> getwd()
[1] "/Users/kirillkarpenok/Files/Education/data-analysys"
```
Задаем рабочую директорию, как переменную:
```r
work_dir <- "/Users/kirillkarpenok/Files/Education/data-analysys"
```

1. Скрипт, который на вход берет все файлы с рабочей директориии, и вычисляет среднее значение "pollutant".
Стандартное значение - это все файлы на промежутке от 1 и до 332

```r
pmean<-function(directory, pollutant, id=1:332){
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

> pmean("data", "sulfate", 1:2)
[1] 4.402199
> pmean("data", "sulfate", 1:10)
[1] 4.064128
> pmean("data", "sulfate", 55)
[1] 3.587319
> pmean("data", "nitrate")
[1] 1.702932
```
2. Функция, выводит количесво полных наблюдений. На вход - директория с файлами, и сам файл (диапазон). Возвращает датафрейм.

```r
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

> complete_func("sulfate", 10:20)
   id_col value
2      10   148
3      11   443
4      12    96
5      13    46
6      14    96
7      15    83
8      16    60
9      17   927
10     18    84
11     19   353
12     20   124

```

3. Функция возвращает вектор значений корреляций. На вход - директория с файлами, пороговое значение,  threshold = 0.

```r
corr <- function(directory, threshold = 0) {
  d<-directory
  setwd(work_dir)
  i<-1
  cr <- c()
  while (i<=332){
    if (i<=9) file<-read.csv(paste0("00",i,".csv"))
    else if (i<=99) file<-read.csv(paste0("0",i,".csv"))
    else file<-read.csv(paste0(i,".csv"))
    file <- file[complete.cases(file),]
    if ( nrow(file) > threshold ) {
      cr <- c(cr, cor(file$sulfate, file$nitrate) )
    }
  }

  cr
}

```
