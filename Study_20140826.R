## 문서 사이트
http://www.dbguide.net/knowledge.db?cmd=interview_view&boardUid=173763&boardConfigUid=84&boardStep=0&categoryUid=

#########################################################
### 실습 2-3 영화사이트에서 감상평 수집하기
#########################################################
install.packages("XML")
install.packages("xtable")
install.packages("tm")
library(XML)
library(xtable)
library(tm)



#다음 영화 페이지에서 감상평 내용 수집하는 모듈
GetDaumMovieData <- function(num){
  url = gsub(" ","",paste("http://movie.daum.net/review/netizen_point/movieNetizenPoint.do?type=after&page=",num))  
  doc = htmlTreeParse(url, useInternalNodes = T, encoding="UTF-8")
  xpathSApply(doc, "//div[@class='commentList']", xmlValue)
  movie_nm <- xpathSApply(doc, "//span[@class='movieTitle fs11']", xmlValue)                              #영화명
  score<-gsub("네티즌별점","",xpathSApply(doc, "//span[@class='star_small']", xmlValue))                  #별점
  reg_date<-xpathSApply(doc, "//span[@class='date']", xmlValue)                                           #작성일
  contents<-gsub("네티즌별점|\r|\t|\n","",xpathSApply(doc, "//span[@class='comment article']", xmlValue)) #내용  
  contents<-removeNumbers(contents)                    #숫자제거
  contents<-removePunctuation(contents)                #특수문자제거
  contents<-stripWhitespace(contents)                  #공백제거
  senti_data <-cbind(movie_nm,score,reg_date,contents) #데이터 바인드
  return(senti_data)
}







#네이트 영화 페이지에서 감상평 내용 수집하는 모듈
GetNateMovieData <- function(num){
  url = gsub(" ","",paste("http://movie.nate.com/comm/reply40/?cinema_id=&type=&tab=all&page=",num))  
  doc = htmlTreeParse(url, useInternalNodes = T)    
  #제목
  movie_nm <- xpathSApply(doc, "//table[@class='table_type01']/tbody/tr/td[2]/dl/dt/a", xmlValue)
  movie_nm <- iconv(movie_nm,"UTF-8","EUC-KR","")   
  
  #작성일
  reg_date <- xpathSApply(doc, "//table[@class='table_type01']/tbody/tr/td[1]", xmlValue)
  reg_date <- substr(reg_date, start=1, stop=10) 
  
  #감상평
  contents<- xpathSApply(doc, "//table[@class='table_type01']/tbody/tr/td[3]", xmlValue)
  contents<- iconv(contents,"UTF-8","EUC-KR","")  
  contents<- gsub("\r|\t|\n","",contents)    
  contents<-removeNumbers(contents)     #숫자제거
  contents<-removePunctuation(contents) #특수문자제거
  contents<-stripWhitespace(contents)   #공백제거 
  
  #별점
  score<- xpathSApply(doc, "//table[@class='table_type01']/tbody/tr/td[2]/dl/dd/span[2]", xmlValue)
  score<- strsplit(score, "/")  
  score<- data.frame(matrix(unlist(score), nrow=length(score), byrow=T))
  score<- score[1]  
  
  senti_data <-cbind(movie_nm,score,reg_date,contents) 
  return(senti_data)
}



#실제 추출하는 부분 (페이지별로 추출함수 호출)
DATA<-NULL        #감상평 데이터 변수 초기화
StartPage <- 1    #시작페이지

#크롤링에 오랜시간이 걸리므로 여기에서는 10페이지만 샘플로 수집하기로 한다.
EndPage <- 10     #마지막페이지 


#추출하고자 하는 사이트의 함수를 호출해서 데이터를 수집한다.
#다음만 수집할 것이므로 네이{
getData<-GetDaumMovieData(i)  #다음 크롤링 
#getData<-GetNateMovieData(i)  #네이트 크롤링 
DATA<-rbind(DATA,getData) #데이터 바인드
}


#수집된 감상평 데이터를 CSV파일로 저장한다.
#아래의 파일은 제일 상단메뉴의 Tools -> Options..에서
#Default working directory에 지정된 폴더에 
#SAMPLE_MOVIE_DATA.csv 이름으로 생성된다.
write.csv(DATA,"SAMPLE_MOVIE_DATA.csv")



##감상평 데이터를 KONLP 패키지의 extractNoun() 함수를 이용해 명사 분리
##우리는 R의 extractNoun() 함수를 이용해 감상평 데이터에서 명사 분리를 시작하였다. 하지만 출발과 동시에 에러가 ‘터지기’ 시작했다


##Warning message:
  ##In preprocessing(sentence) :
  ##It's not kind of right sentence : '자연의경고가좀비라는존재로대체된것같네요!'


 
##우리는 구글에서 열심히 검색해 봤지만, 에러 원인이 무엇인지 명확히 설명해 주는 자료를 찾을 수 없었다.
## R이 오픈소스라는 점에 착안하여 소스코드를 검색해 보기로 했다. 
## 역시, 우리는 https://github.com/haven-jeon/KoNLP에 공개된 소스코드에서 
## 에러 메시지에 나온 preprocessing(sentence) 함수를 확인할 수 있었다. 

# if unable to process, this will return FALSE
preprocessing <- function(inputs){
  if(!is.character(inputs)) {
    warning("Input must be legitimate character!")
    return(FALSE)
  }
  newInput <- gsub("[[:space:]]", " ", inputs)
  newInput <- gsub("[[:space:]]+$", "", newInput)
  newInput <- gsub("^[[:space:]]+", "", newInput)
  if((nchar(newInput) == 0) |  
          (nchar(newInput) > 20 & length(strsplit(newInput, " ")[[1]]) <= 1)){ 
    warning(sprintf("It's not kind of right sentence : '%s'", inputs))
    return(FALSE)
  }
  return(newInput)
}
