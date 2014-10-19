library(tm)
#==================================================
#KOFIC(Korean Film Council) Data Loading
#==================================================
#  Data preprocessing 
#  Remove space & special character
#  Factor data transformation
#  아래화일의 관객수는 공식통계수

movie_data <- read.csv("./moviedata/Movie_Ranking_2014_10_16.txt", sep = "\t",header=TRUE, stringsAsFactors=FALSE)
## Title
movie_data$NewTitle <-removePunctuation(movie_data$Movie_Title) #특수문자 제거
movie_data$NewTitle <-gsub(" ", "",movie_data$NewTitle)#공백 제거
## Realase Date
movie_data$Release_Date <-as.POSIXlt(movie_data$Release_Date)
## Movie_Rate
#movie_data$Movie_Rate <- as.factor(movie_data$Movie_Rate)
## Genre
#movie_data$Genre1 <- as.factor(movie_data$Genre1)
#movie_data$Genre2 <- as.factor(movie_data$Genre2)
## Category
#movie_data$Movie_Category <- as.factor(movie_data$Movie_Category)

## Numeric data
## 원본 데이터에 NA가 있음. 사전 처리해야 함.
#movie_data$Seoul_Revenue <- as.numeric(movie_data$Seoul_Revenue)
#movie_data$National_Revenue <- as.numeric(movie_data$National_Revenue)

# Field Extraction
select_movie <- subset (movie_data, select =c(NewTitle, Release_Date,Number,Movie_Title,Movie_Type,National_Spectator_Number,Running_Time))
                                              
## ----------------------------------
## Temp Data Processing
## tidy data로 정리할때에는 melt와 cast가 사용하기 편함.
## 2개년도에 영화 매출/스크린수를 한개로 합산하는 작업이 필요
## 아래 화일의 관객수는 KOBIS 통계, 관객수는 공식 통계 기준으로 정리함.
## 매출은 공식통계기준으로 KOBIS 통계를 보완한다.
## Screen 와 극장수는 KOBIS 통계를 기준으로 한다.

library(reshape2)
movie1_data <- read.csv("./moviedata/Movie_Number_2014_10_16.txt", sep = "\t",header=TRUE, stringsAsFactors=FALSE)
## Data Selection
data_temp<-subset(movie1_data,select=c(Movie_Title,Release_Date,Revenue,Spectator_Number,Screen_Number,Play_Number))
## Data Metling as Title & Realase Data
data_temp<-melt(id=c("Movie_Title","Release_Date"),data_temp)
good_data<-dcast(data_temp, Movie_Title + Release_Date ~ variable, sum)
good_data<-good_data[order(good_data$Spectator_Number,decreasing = TRUE),]

## Title
good_data$NewTitle <-removePunctuation(good_data$Movie_Title) #특수문자 제거
good_data$NewTitle <-gsub(" ", "",good_data$NewTitle)#공백 제거
## Realase Date
good_data$Release_Date <-as.POSIXlt(good_data$Release_Date)

## 확인용
# write.csv (good_data, "./moviedata/Movie_gooddata.csv ")

# Field Extraction
select_goodmovie <- subset (good_data, select =c(NewTitle, Release_Date,Revenue,Spectator_Number,Screen_Number,Play_Number))

# 2개의 data frame 한개로 join
#total_data<-merge(select_movie,good_data,by = c("NewTitle","Release_Date"))
#total_data<-total_data[order(total_data$National_Spectator_Number,decreasing = TRUE),]

total_data<-merge(select_movie,select_goodmovie,by = c("NewTitle","Release_Date"))
total_data<-total_data[order(total_data$National_Spectator_Number,decreasing = TRUE),]


## Divesity Film(다양성 영화) , General Film (일반 영화) 정리
DiversityFilm_List <- read.csv("./moviedata/Diversity_film_list_2014_10_16.txt", sep = "\t",header=TRUE, stringsAsFactors=FALSE)
## Title
DiversityFilm_List$Movie_Title <-removePunctuation(DiversityFilm_List$Movie_Title) #특수문자 제거
DiversityFilm_List$Movie_Title <-gsub(" ", "",DiversityFilm_List$Movie_Title)#공백 제거
## Realase Date
DiversityFilm_List$Release_Date <-as.POSIXlt(DiversityFilm_List$Release_Date)
colnames(DiversityFilm_List) <- c("NewTitle","Release_Date")
DiversityFilm_List$Movie_Category <- "다양성영화"

# 2개의 data frame 한개로 join
total_data<-merge(total_data,DiversityFilm_List,by = c("NewTitle","Release_Date"),all.x=TRUE)
total_data<-total_data[order(total_data$National_Spectator_Number,decreasing = TRUE),]

total_data$Movie_Category[-which(total_data$Movie_Category=="다양성영화")]<-"일반영화"



## Movie Metadata processing
#  Director number, Distributor number, Actor, Genre Numer Extraction
#  Representative director, distributor, Genre, actor 3 extraction

#  Function defines from character string to array number, 3 objects. 
#  a,b,c,d --> number 4, a,b,c extract
Extract_name <- function (df_original,name) {
  temp1<-strsplit(df_original,",") 
  n_length<-length(temp1)
  df<-data.frame(Number=rep(1,n_length))
  for(i in 1:n_length) {
    df$Number[i]<-length(temp1[[i]])
    if(df$Number[i]>=3) {
      df$Name1[i]<-temp1[[i]][1]
      df$Name2[i]<-temp1[[i]][2]
      df$Name3[i]<-temp1[[i]][3]
    }
    else if(df$Number[i]==2){
      df$Name1[i]<-temp1[[i]][1]
      df$Name2[i]<-temp1[[i]][2]
      df$Name3[i]<-c("")
    }
    else if(df$Number[i]==1){
      df$Name1[i]<-temp1[[i]][1]
      df$Name2[i]<-c("")
      df$Name3[i]<-c("")
    }
    else {
      df$Name1[i]<-c("")
      df$Name2[i]<-c("")
      df$Name3[i]<-c("")
    } 
  } 
  colnames(df)<-c(paste(name,"_Name",sep=""),paste(name,"1",sep=""),paste(name,"3",sep=""),paste(name,"3",sep=""))
  return(df)
}

##Metadata 엑셀화일 날짜 수정 필요함. 포맷이 이상함
metadata <- read.csv("./moviedata/Movie_Metadata_2014_09_30.txt", sep = "\t",header=TRUE, stringsAsFactors=FALSE)
## Title
metadata$NewTitle <-removePunctuation(metadata$Movie_Title) #특수문자 제거
metadata$NewTitle <-gsub(" ", "",metadata$NewTitle)#공백 제거
## Realase Date
metadata$Release_Date <-as.POSIXlt(metadata$Release_Date)

Actor_df<-Extract_name(metadata$Actor,"Actor")
Genre_df<-Extract_name(metadata$Genre,"Genre")
Director_df<-Extract_name(metadata$Director,"Director")

# Field Extraction
select_metadata <- subset (metadata, select =c(NewTitle, Release_Date,Nationality_First,Distributor,Movie_Rate))
select_metadata <- cbind(select_metadata,Genre_df)
select_metadata <- cbind(select_metadata,Director_df)
select_metadata <- cbind(select_metadata,Actor_df)

a<-merge(total_data,select_metadata,by = c("NewTitle"))
a<-a[order(a$National_Spectator_Number,decreasing = TRUE),]
a<-merge(total_data,select_metadata,by = c("NewTitle","Release_Date"),all.x=TRUE)
write.csv (a, "./moviedata/Movie_TinyData.csv ")

# 배우 이름 사전 저장하기
temp1<-strsplit(metadata$Actor,",")
temp2<-factor(unlist(temp1))
temp3<-as.data.frame(levels(temp2))
names(temp3)<-c("Actor_Name")
write.csv (temp3, "./moviedata/Actor_Name.csv ")

# 감독 이름 사전 저장하기
temp1<-strsplit(metadata$Director,",")
temp2<-factor(unlist(temp1))
temp3<-as.data.frame(levels(temp2))
names(temp3)<-c("Director_Name")
write.csv (temp3, "./moviedata/Director_Name.csv ")

# 영화 이름 사전 저장하기
temp1<-total_data$Movie_Title
temp2<-factor(temp1)
temp3<-as.data.frame(levels(temp2))
names(temp3)<-c("Movie_Title")
write.csv (temp3, "./moviedata/Movie_Title.csv ")

#save(movie_data,file="movie_data_2014_09_22.RData")



