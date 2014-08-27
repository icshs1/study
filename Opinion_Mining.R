##시나리오 2: 긍부정 단어 카운팅
##match() 함수로 감상평과 긍부정 사전을 매치해 긍부정 단어 수 카운트
##이론과 현실의 데이터
##두 번째 시나리오는 ‘한글 감성 분석 사전’(김경태 교수 제공)으로 수집한 감상평 데이터를 매치하는 것이다.

##좀 더 구체적으로 말하면 다음과 같다.
##1. 감상평 데이터를 extractNoun() 함수로 분리
##2. match() 함수로 사전과 매칭한 후 결과를 카운팅



##따라 해보세요!

  install.packages("KoNLP",dependencies=TRUE)
install.packages("rJava",dependencies=TRUE)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre6') # for 64-bit version
library(rJava)
library(KoNLP)

setwd("~")
DIC<-read.csv("positive-words_kor.csv", header=FALSE, stringsAsFactors=FALSE) #긍정사전 로딩
names(DIC) <- c('word')
DIC<-DIC[,1]
str(DIC)

STR<-extractNoun("정말 봐야됨 이런배우들이 떠야되는데ㅋ잼나게 봤어요 "); #단어 분리
STR
Matches <- match(STR, DIC) #매칭
Matches
sum(!is.na(Matches)) #매칭된 단어의 총 개수

  
##실행 결과
##> STR<-extractNoun("정말 봐야됨 이런배우들이 떠야되는데ㅋ잼나게 봤어요 ");#단어 분리
##> STR
##[1] "이런배우들이" "떠야되는？잼나게"
##> Matches <- match(STR, DIC) #매칭
##> Matches
##[1] NA NA
##> sum(!is.na(Matches)) #매칭된 단어의 총 개수
##[1] 0 

  
  
##  배 과장의 실험
  
##  match() VS grep() 
##  테스트 환경: i7 CPU, 10GB RAM, 윈도우 8
  
  # match() 함수 실행 시간 테스트
  time_chk<-function(){
    for(i in 1:100){
      sum(!is.na(match(extractNoun("분노의 질주 편부터 편까지 보면서 원편만한 속편없다는
속설히 확실히 깨닫고 실망하던차에 주말에 오리지널편보고 너무 재미있어서
시간가는줄 몰랐네여 빠른 스피드와 스케일 영화보는내내 눈과귀도 즐거워여"), DIC)))
    }
  }
  
  system.time(time_chk())
  > system.time(time_chk())
  user system elapsed 
  1.06 0.00 0.99 
  
  # grep() 함수 실행 시간 테스트
  Cnt_Word <- function(str,Dic){# str 문장의 Dic에 있는 내용을 카운트해 개수를 반환 
    TMP<-c(NA)
    for (i in Dic) {
      cnt<-grep(i,str)
      TMP<-cbind(TMP,cnt)
    }
    return(sum(!is.na(TMP)))
  }
  time_chk<-function(){
    for(i in 1:100){ Cnt_Word("분노의 질주 편부터 편까지 보면서 원편만한
속편없다는 속설히 확실히 깨닫고 실망하던차에 주말에 오리지널편보고
너무 재미있어서 시간가는줄 몰랐네여 빠른 스피드와 스케일 영화보는내내 눈과귀도 즐거워여",DIC) } 
  }
  system.time(time_chk())
  > system.time(time_chk())
  user system elapsed 
  4.56 0.00 4.56 
  
##  시나리오 3: 감상평 스코어링
##  카운트된 긍부정 단어 수를 기반으로 감상평 스코어링
  
  
  #=============================================================
  #라이브러리 로딩
  #=============================================================
  library(plyr)
  library(stringr)
  library(party)
  
  #=============================================================
  #디렉터리 세팅
  #=============================================================
  setwd("~")
  
  #=============================================================
  #함수 로딩
  #=============================================================
  Cnt_Word <- function(str,Dic)
  {# str 문장에 Dic에 있는 내용이 있는지 카운트해 개수를 반환
    TMP<-c(NA)
    for (i in Dic) {
      cnt<-grep(i,str)
      TMP<-cbind(TMP,cnt)
    }
    return(sum(!is.na(TMP)))
  }
  
  
  senti_Score <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms)
  {# 4가지 분류 사전을 가지고 문장에 출현 빈도수를 추출
    final_scores <- matrix('', 0, 5)
    scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms)
    {
      initial_sentence <- sentence
      vPosMatches <- Cnt_Word(sentence, vPosTerms)
      posMatches <- Cnt_Word(sentence, posTerms)
      vNegMatches <- Cnt_Word(sentence, vNegTerms)
      negMatches <- Cnt_Word(sentence, negTerms)
      score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
      newrow <- c(initial_sentence, score)
      final_scores <- rbind(final_scores, newrow)
      return(final_scores)
    }, vNegTerms, negTerms, posTerms, vPosTerms)
    return(scores)
  } 
  
  
  #=============================================================
  #사전데이터 로딩
  #=============================================================
  DIC_org<-read.csv("senti_dic_org.csv", header=FALSE)
  names(DIC_org) <- c('word', 'score') 
  head(DIC_org,10)
  > head(DIC_org,10)
  word score
  1      씨바    -5
  2      ㅆㅂ    -5
  3        tt    -1
  4      철회    -3
  5      엄포    -3
  6      ㅠㅠ    -1
  7  판매금지    -5
  8    가처분    -3
  9      침해    -2
  10     부당    -3
  
  #=============================================================
  #사전 데이터 점수별로 4구간으로 분리
  #=============================================================
  vNegTerms_org <- DIC_org$word[DIC_org$score==-5 | DIC_org$score==-4]
  negTerms_org <- DIC_org$word[DIC_org$score==-3 | DIC_org$score==-2 | DIC_org$score==-1]
  posTerms_org <- DIC_org$word[DIC_org$score==3 | DIC_org$score==2 | DIC_org$score==1]
  vPosTerms_org <- DIC_org$word[DIC_org$score==5 | DIC_org$score==4] 
  
  #=============================================================
  #classification에 사용할 긍부정 문장 준비
  #=============================================================
  negText<- read.csv("Senti_msg_POS_0.csv", header=TRUE, stringsAsFactors=FALSE)#부정 문장
  negText <-negText$contents
  negText <-head(negText,1000)
  posText<- read.csv("Senti_msg_POS_10.csv", header=TRUE, stringsAsFactors=FALSE)#긍정 문장
  posText <- posText$contents
  posText <-head(posText,1000)
  head(posText,10)
  
  > head(posText,10)
  [1] "역사적으로 뜻깊은 영화 만들어주세요 "
  [2] "역사적으로도 실존 인물을 소재로한 영화 바우덕이 왕의남자보다 잘 만들어 주세요 "
  [3] "괜찮다 "
  [4] "의미있는 영화 기대합니다 "
  [5] "유쾌상쾌통쾌감성 영화 기대합니다 "
  [6] "영화 제목만으로도 행운의여신이 올것 같은 영화기대합니다 "
  [7] "파리의 아름다움을 만끽할 수 있었던 영화였어요 중간에 스윙댄스도 나와서 
  너무 좋았던 유쾌한 영화 헤밍웨이 매력적임 ㅁ "
  [8] "젊은 놈들 저리가라 여전히 녹슬지 않은 그들의 솜씨ㅋㅋㅋㅋㅋ 달린다 달려 " 
  [9] "네티즌 전문가집단 평점 모두 무시하라이영화 섹시 코미디의 최고봉임 " 
  [10] "보구나오면 남친이 오징어로 보입니다 커플들은 안가시는편이 좋을듯 "
  
  head(negText,10)
  > head(negText,10)
  [1] "세상에 이런 ㅈ ㅓ 질 영화가 된장 요즘일본 영화 어딘가 이상하다 "
  [2] "마이너스는없냐 그냥 얼굴위로들고 구름떠다니는하늘봐도 이거보단재밌겠다 "
  [3] "이걸 영화라고아진짜 시간 아까워서 개인취양을 떠나서 뭐하나 제대로된게 없는 
  영화무섭지도 않고 놀라지도 않고 야하지도 않고물론 재밌지도 않고ㅠㅠ
  기억에 남는건 이종수의 신음소리만 아 짱나 영화보고 이렇게 시간 아깝기는 
  처음이네ㄱㅆㄹ ㄱ "
  [4] "연출도 개판 연기력도 개판 와놔 ㅋㅋㅋㅋㅋㅋ "
  [5] "진짜 이걸보고 다시느낌 다시는 대만영화 안봄 "
  [6] "많이 실망한 영화 끝나고 나서도 아련한 기억조차 안남는정말 실망 그자체 "
  [7] "최악의 시나리오에 최악의 연출 공짜로 보기도 시간이 아까운 영화 "
  [8] "코미디영화인데 스케일도 크고 등장인물도 화려하네요봉구의 대활약이 볼만하네요 "
  [9] "정말 최악 이렇게 재미없게 본 만화 오랜만이었다 주인공 뭔가 하나하나 
  설명하려하는게 졸라 어색하고 밑에님 말처럼 매력이 전혀 없고 운도 너무좋아 
  완전 주인공 아껴주네 다른세계 들어가면 죽을수도 있다더니 개뿔 개나소나 다 
  용자되서 나오겟네 진부하다"
  [10] "재미없는 영화다 "  
  
  #=============================================================
  #스코어링
  #=============================================================
  posResult_org <- as.data.frame(senti_Score(posText, vNegTerms_org, 
                                             negTerms_org, posTerms_org, vPosTerms_org))
  negResult_org <- as.data.frame(senti_Score(negText, vNegTerms_org, 
                                             negTerms_org, posTerms_org, vPosTerms_org))
  posResult_org <- cbind(posResult_org, 'pos')
  colnames(posResult_org) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
  negResult_org <- cbind(negResult_org, 'neg')
  colnames(negResult_org) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
  
  #=============================================================
  #classification 감성 사전 검증
  #=============================================================
  results_org <- rbind(posResult_org, negResult_org)   
  
  #데이터 형 변환
  results_org$vNeg<-as.integer(results_org$vNeg)
  results_org$neg<-as.integer(results_org$neg)
  results_org$pos<-as.integer(results_org$pos)
  results_org$vPos<-as.integer(results_org$vPos)
  
  head(results_org[,2:5],10)
  > head(results_org[,2:5],10)
  vNeg neg pos vPos
  1     1   1   1    1
  2     1   1   1    1
  3     1   1   1    1
  4     2   1   1    4
  5     1   1   1    5
  6     1   1   1    5
  7     2   1   1    5
  8     1   1   1    4
  9     8   1   1    6
  10    2   1   1    1
  
  str(results_org)
  > str(results_org)
  'data.frame':	2000 obs. of  6 variables:
    $ sentence : Factor w/ 1952 levels " "," 
## 같이 나이들고 있네요십년후 모습도 보고 1 8 2 ...
## $ neg      : int  1 1 1 1 1 1 1 1 1 1 ...
## #s      : int  1 1 1 1 1 1 1 1 1 1 ...
 $ vPos     : int  1 1 1 4 5 5 5 4 6 1 ...
 $ sentiment: Factor w/ 2 levels "pos","neg": 1 1 1 1 1 1 1 1 1 1 ...

ctree_classifier_org <- ctree(sentiment~vNeg+neg+pos+vPos,results_org[,2:6])
plot(ctree_classifier_org)

> plot(ctree_classifier_org)

  