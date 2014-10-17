library(tm)
#==================================================
#KOFIC(Korean Film Council) Data Loading
#==================================================
#  Data preprocessing 
#  Remove space & special character
#  Factor data transformation
#  아래화일의 관객수는 공식통계수

movie_data <- read.csv("./moviedata/Movie_Ranking_2014_10_17.txt", sep = "\t",header=TRUE, stringsAsFactors=FALSE)
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
#  Function 
metadata <- read.csv("./moviedata/Movie_Metadata_2014_09_30.txt", sep = "\t",header=TRUE, stringsAsFactors=FALSE)

write.csv (total_data, "./moviedata/Movie_totaldata.csv ")


#save(movie_data,file="movie_data_2014_09_22.RData")



