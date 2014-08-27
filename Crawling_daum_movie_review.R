#########################################################
### 실습 2-3 영화사이트에서 감상평 수집하기
#########################################################
##install.packages("XML")
##install.packages("xtable")
##install.packages("tm")
##library(XML)
##library(xtable)
##library(tm)



##다음 영화 페이지에서 감상평 내용 수집하는 모듈
GetDaumMovieData <- function(num){
  url = gsub(" ","",paste("http://movie.daum.net/review/netizen_point/movieNetizenPoint.do?type=after&page=",num))  
  doc = htmlTreeParse(url, useInternalNodes = T, encoding="UTF-8")
  xpathSApply(doc, "//div[@class='commentList']", xmlValue)
  movie_nm <- xpathSApply(doc, "//span[@class='movieTitle fs11']", xmlValue)                              #영화명
  score<-gsub("네티즌별점","",xpathSApply(doc, "//span[@class='star_small']", xmlValue))                  #별점
  reg_date<-xpathSApply(doc, "//span[@class='date']", xmlValue)                                           #작성일
  contents<-gsub("네티즌별점|\r|\t|\n","",xpathSApply(doc, "//span[@class='comment article']", xmlValue)) #내용  
  contents<-removeNumbers(contents)                    ##숫자제거
  contents<-removePunctuation(contents)                ##특수문자제거
  contents<-stripWhitespace(contents)                  ##공백제거
  senti_data <-cbind(movie_nm,score,reg_date,contents) ##데이터 바인드
  return(senti_data)
}


Crawling_MovieData <-function(startpage,endpage) {
  #실제 추출하는 부분 (페이지별로 추출함수 호출)
  DATA<-NULL        #감상평 데이터 변수 초기화
  StartPage <- startpage    #시작페이지
  
  #크롤링에 오랜시간이 걸리므로 여기에서는 10페이지만 샘플로 수집하기로 한다.
  EndPage <- endpage     #마지막페이지 
  #추출하고자 하는 사이트의 함수를 호출해서 데이터를 수집한다.
  cdir<-getwd() ##  current directory
  directory="reviewdata"
  
  for (i in StartPage:EndPage) {
    getData<-GetDaumMovieData(i)  ##다음 크롤링 
    DATA<-rbind(DATA,getData) #데이터 바인드
  }
  #수집된 감상평 데이터를 CSV파일로 저장한다.
  #Default working directory에 지정된 폴더에 
  #SAMPLE_MOVIE_DATA_DAUM_StartPage_EndPage.csv 이름으로 생성된다.
  filename<-paste("SAMPLE_MOVIE_DATA_DAUM_",as.character(StartPage),"_",as.character(EndPage),".csv",sep="")
  write_filename<-file.path(cdir,directory,filename,fsep="\\")
  write.csv(DATA,write_filename)
}