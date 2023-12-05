# <파리, 리옹, 마르세유 선거구 거주민에 대한 데이터 분석>
# <데이터 전처리>

## 1. 데이터불러오기 및 데이터 정제
### (1). text 파일 데이터 불러오기
library(readr)
total2018 <- read_delim("BTX_TD_IMG3A_2018.txt", 
                                delim = "\t", escape_double = FALSE, 
                                trim_ws = TRUE)
View(total2018)


### (2). 데이터 프레임으로 정제
total2018<-as.data.frame(total2018) # df형태로 변환
class(total2018)
names(total2018)<- total2018[7,]   # df의 column 명을 지정
total2018<-total2018[-c(1:7),]     # 필요없는 행은 삭제




## 2. 선거구를 도시명으로 통합하기
total2018$LIBGEO[1:16] <- "Marseille" # 전체 마르세유 16개 선거구를 Marseille로 통합
total2018$LIBGEO[17:25] <- "Lyon"     # 전체 리옹 9개 선거구를 Lyon 통합
total2018$LIBGEO[26:45] <- "Paris"    # 전체 파리 20개 선거구를 Paris 통합


### (4) 전체 데이터의 개수 확인하기
total =0
for(i in 3:34){
  total <-total+  sum(as.numeric(total2018[,i])) # 해당 샘플의 데이터 개수를 모두 합해주는 반복문
}
print(total) 

# total = 3,562,482
# 약 356 만명에 대한 데이터





## 3. 데이터 분석에 사용할 데이터 프레임 생성하기
### 3.1. 데이터를 지역별로 묶어 요약하기
#### (1)마르세유 지방 데이터 통합하기
str(total2018) # 3열부터 34열까지가 범주 데이터인것을 확인


mar_sum <- c(0,0) #1,2열에 더미 생성하여 에러를 방지
for(i in 3:34){    # 각 범주의 합계가 순서대로 축적되도록 반복문을 설계
  mar_sum[i] <- mar_sum[i-1] + sum(as.numeric(total2018[1:16,i]))
}

mar_sum<-mar_sum[c(-1,-2)]  #더미데이터를 삭제
mar_sum <- round(mar_sum/1000) # 1000단위로 나누어준 후 반올림해준 값을 변수에 지정
mar_sum


#### (2) 리옹 지방 데이터 통합하기
lyon_sum<-c(0,0)  #더미생성
for(i in 3:34){   #축적 데이터 생성 반목문문
  lyon_sum[i] <-  lyon_sum[i-1]+sum(as.numeric(total2018[17:25,i]))
}

lyon_sum<-lyon_sum[c(-1,-2)] #더미 삭제
lyon_sum <- round(lyon_sum/1000)
lyon_sum



#### (3) 파리 지방 데이터 통합하기기
paris_sum<-c(0,0)

for(i in 3:34){
  paris_sum[i] <-  paris_sum[i-1]+sum(as.numeric(total2018[26:45,i]))
}


paris_sum<-paris_sum[c(-1,-2)]
paris_sum <- round(paris_sum/1000)
paris_sum


#### (4) 전체 데이터 개수 확인
mar_sum[32]+ lyon_sum[32] + paris_sum[32]
                       
# 같은 비율로 샘플링 1:1000
# 총 3563개 데이터 추출



# ---------------------------------------------------
### 3.2. 데이터 프레임 생성
#### (1) Marseille 지역 데이터 
##### (1)-1. 더미 데이터 프레임 생성
data1 <- data.frame(ID =c(1:mar_sum[32]),   # 마르세유 지역의 데이터를 담을 더미 데이터 생성
                   Sexe=c(1:mar_sum[32]),
                   immi=c(1:mar_sum[32]),
                   job=c(1:mar_sum[32]),
                   region=c(1:mar_sum[32]))

# 해당 더미 데이터 프레임을 수정하는 방식으로 데이터를 입력

mar_sum[32] # 마르세유 지방의 데이터 개수확인 
str(data1) # 더미 데이터 생성 개수 확인




##### (1)-2. 데이터 입력
# 마르세유 지방의 각 범주의 개수 확인
mar_sum  # 다섯번째 순서까지 모두 0이므로 정보가 있는 다섯번째 범주부터 입력시작


for(i in 1:mar_sum[5]){   # df의 1번 열부터 4번째까지 해당 인구통계학 자료를 df에 입력
  data1[i,] <- c(ID= i,             #익명화 된 아이디
                 Sexe = 1,          #성별정보
                 immi=1,            # 이민여부
                 job= "commerçants",       #직업
                 region="Mar")           #거주지역
}


View(data1)
mar_sum[5] # 다섯번째 범주의 개수 확인 
# 더미 데이터에 4행까지 입력되었으므로 다음 범주는 5행부터 입력되도록 설계
# 다음의 작업을 mar_sum[32]까지 진행

for(i in 5:mar_sum[6]){   # 5번 행부터 수정
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "commerçants",
                 region="Mar")
}

mar_sum[6]
for(i in 6:mar_sum[7]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "commerçants",
                 region="Mar")
}


mar_sum[7]

for(i in 18:mar_sum[8]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "commerçants",
                 region="Mar")
}


mar_sum[8]
for(i in 23:mar_sum[9]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=1,
                 job= "Cadres",
                 region="Mar")
}


mar_sum[9]

for(i in 27:mar_sum[10]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "Cadres",
                 region="Mar")
}


mar_sum[10]


for(i in 30:mar_sum[11]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=2,
                 job= "Cadres",
                 region="Mar")
}


mar_sum[11]
for(i in 64:mar_sum[12]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "Cadres",
                 region="Mar")
}


mar_sum[12]


for(i in 91:mar_sum[13]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=1,
                 job= "Professions intermédiaire",
                 region="Mar")
}


mar_sum[13]
for(i in 96:mar_sum[14]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "Professions intermédiaire",
                 region="Mar")
}


mar_sum[14]

for(i in 101:mar_sum[15]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=2,
                 job= "Professions intermédiaire",
                 region="Mar")
}


mar_sum[15]

for(i in 137:mar_sum[16]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "Professions intermédiaire",
                 region="Mar")
}


mar_sum[16]


for(i in 186:mar_sum[17]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=1,
                 job= "Employés",
                 region="Mar")
}


mar_sum[17]


for(i in 193:mar_sum[18]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "Employés",
                 region="Mar")
}


mar_sum[18]

for(i in 207:mar_sum[19]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=2,
                 job= "Employés",
                 region="Mar")
}


mar_sum[19]


for(i in 237:mar_sum[20]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "Employés",
                 region="Mar")
}


mar_sum[20]


for(i in 301:mar_sum[21]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=1,
                 job= "Ouvriers",
                 region="Mar")
}


mar_sum[21]


for(i in 316:mar_sum[22]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "Ouvriers",
                 region="Mar")
}


mar_sum[22]

for(i in 320:mar_sum[23]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=2,
                 job= "Ouvriers",
                 region="Mar")
}


mar_sum[23]

for(i in 355:mar_sum[24]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "Ouvriers",
                 region="Mar")
}


mar_sum[24]

for(i in 362:mar_sum[25]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=1,
                 job= "Retraités",
                 region="Mar")
}


mar_sum[25]

for(i in 373:mar_sum[26]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "Retraités",
                 region="Mar")
}


mar_sum[26]

for(i in 381:mar_sum[27]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=2,
                 job= "Retraités",
                 region="Mar")
}


mar_sum[27]

for(i in 445:mar_sum[28]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "Retraités",
                 region="Mar")
}


mar_sum[28]


for(i in 530:mar_sum[29]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=1,
                 job= "Autres",
                 region="Mar")
}


mar_sum[29]

for(i in 550:mar_sum[30]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=1,
                 job= "Autres",
                 region="Mar")
}


mar_sum[30]

for(i in 583:mar_sum[31]){
  data1[i,] <- c(ID= i,
                 Sexe = 1,
                 immi=2,
                 job= "Autres",
                 region="Mar")
}


mar_sum[31]

for(i in 716:mar_sum[32]){
  data1[i,] <- c(ID= i,
                 Sexe = 2,
                 immi=2,
                 job= "Autres",
                 region="Mar")
}


mar_sum[32]
View(data1)

#### (2) Lyon 지역 데이터 
##### (2)-1. 더미 데이터 프레임 생성
data2 <- data.frame(ID =c(1:lyon_sum[32]),
                    Sexe=c(1:lyon_sum[32]),
                    immi=c(1:lyon_sum[32]),
                    job=c(1:lyon_sum[32]),
                    region=c(1:lyon_sum[32]))


lyon_sum[32]
str(data2)
lyon_sum


##### (2)-2. 데이터 입력

for(i in 1:lyon_sum[5]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "commerçants",
                 region="Lyon")
}


lyon_sum[5]
lyon_sum[6]

for(i in 3:lyon_sum[7]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "commerçants",
                 region="Lyon")
}


lyon_sum[7]

for(i in 10:lyon_sum[8]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "commerçants",
                 region="Lyon")
}


lyon_sum[8]


for(i in 14:lyon_sum[9]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "Cadres",
                 region="Lyon")
}


lyon_sum[9]
for(i in 18:lyon_sum[10]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=1,
                 job= "Cadres",
                 region="Lyon")
}


lyon_sum[10]
for(i in 22:lyon_sum[11]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Cadres",
                 region="Lyon")
}


lyon_sum[11]
for(i in 68:lyon_sum[12]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "Cadres",
                 region="Lyon")
}


lyon_sum[12]

for(i in 103:lyon_sum[13]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "Professions intermédiaires",
                 region="Lyon")
}


lyon_sum[13]

for(i in 107:lyon_sum[14]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=1,
                 job= "Professions intermédiaires",
                 region="Lyon")
}


lyon_sum[14]


for(i in 111:lyon_sum[15]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Professions intermédiaires",
                 region="Lyon")
}


lyon_sum[15]


for(i in 140:lyon_sum[16]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "Professions intermédiaires",
                 region="Lyon")
}


lyon_sum[16]

for(i in 179:lyon_sum[17]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "Employés",
                 region="Lyon")
}


lyon_sum[17]


for(i in 182:lyon_sum[18]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=1,
                 job= "Employés",
                 region="Lyon")
}


lyon_sum[18]


for(i in 191:lyon_sum[19]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Employés",
                 region="Lyon")
}


lyon_sum[19]


for(i in 207:lyon_sum[20]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "Employés",
                 region="Lyon")
}


lyon_sum[20]


for(i in 240:lyon_sum[21]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "Ouvriers",
                 region="Lyon")
}


lyon_sum[21]

for(i in 247:lyon_sum[22]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=1,
                 job= "Ouvriers",
                 region="Lyon")
}


lyon_sum[22]

for(i in 248:lyon_sum[23]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Ouvriers",
                 region="Lyon")
}


lyon_sum[23]

for(i in 264:lyon_sum[24]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "Ouvriers",
                 region="Lyon")
}


lyon_sum[24]

for(i in 267:lyon_sum[25]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "Retraités",
                 region="Lyon")
}


lyon_sum[25]

for(i in 272:lyon_sum[26]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Retraités",
                 region="Lyon")
}


lyon_sum[26]


for(i in 277:lyon_sum[27]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Retraités",
                 region="Lyon")
}


lyon_sum[27]

for(i in 303:lyon_sum[28]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "Retraités",
                 region="Lyon")
}


lyon_sum[28]

for(i in 345:lyon_sum[29]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=1,
                 job= "Autres",
                 region="Lyon")
}


lyon_sum[29]

for(i in 352:lyon_sum[30]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=1,
                 job= "Autres",
                 region="Lyon")
}


lyon_sum[30]



for(i in 365:lyon_sum[31]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 1,
                 immi=2,
                 job= "Autres",
                 region="Lyon")
}


lyon_sum[31]

for(i in 437:lyon_sum[32]){
  data2[i,] <- c(ID= i+868,
                 Sexe = 2,
                 immi=2,
                 job= "Autres",
                 region="Lyon")
}


lyon_sum[32]+mar_sum[32]


View(data2)

#### (3) Paris 지역 데이터 
##### (3)-1. 더미 데이터 프레임 생성

data3 <- data.frame(ID =c(1:paris_sum[32]),
                    Sexe=c(1:paris_sum[32]),
                    immi=c(1:paris_sum[32]),
                    job=c(1:paris_sum[32]),
                    region=c(1:paris_sum[32]))

paris_sum[32]
str(data3)
paris_sum
View(data3)

##### (3)-2. 데이터 입력

for(i in 1:paris_sum[5]){   # df의 1번 열부터 4번째까지 해당 인구통계학 자료를 df에 입력
  data3[i,] <- c(ID= i+1387,             #익명화 된 아이디
                 Sexe = 1,          #성별정보
                 immi=1,            # 이민여부
                 job= "commerçants",       #직업범주
                 region="Paris")           #거주지역
}

paris_sum[5]


#### (2) 1
for(i in 14:paris_sum[6]){   # 5번 행에 다음의 자료로 수정
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "commerçants",
                 region="Paris")
}

paris_sum[6]
#### (2) 1
for(i in 20:paris_sum[7]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "commerçants",
                 region="Paris")
}


paris_sum[7]

#### (2) 1
for(i in 54:paris_sum[8]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "commerçants",
                 region="Paris")
}


paris_sum[8]
#### (2) 1
for(i in 70:paris_sum[9]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=1,
                 job= "Cadres",
                 region="Paris")
}


paris_sum[9]
#### (2) 1

for(i in 111:paris_sum[10]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "Cadres",
                 region="Paris")
}


paris_sum[10]
#### (2) 1


for(i in 154:paris_sum[11]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=2,
                 job= "Cadres",
                 region="Paris")
}


paris_sum[11]
#### (2) 1
for(i in 405:paris_sum[12]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "Cadres",
                 region="Paris")
}


paris_sum[12]


#### (2) 1
for(i in 628:paris_sum[13]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=1,
                 job= "Professions intermédiaire",
                 region="Paris")
}


paris_sum[13]

#### (2) 1
for(i in 651:paris_sum[14]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "Professions intermédiaire",
                 region="Paris")
}


paris_sum[14]

#### (2) 1
for(i in 680:paris_sum[15]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=2,
                 job= "Professions intermédiaire",
                 region="Paris")
}


paris_sum[15]

#### (2) 1
for(i in 772:paris_sum[16]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "Professions intermédiaire",
                 region="Paris")
}


paris_sum[16]


#### (2) 1
for(i in 899:paris_sum[17]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=1,
                 job= "Employés",
                 region="Paris")
}


paris_sum[17]


#### (2) 1
for(i in 924:paris_sum[18]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "Employés",
                 region="Paris")
}


paris_sum[18]

#### (2) 1
for(i in 984:paris_sum[19]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=2,
                 job= "Employés",
                 region="Paris")
}


paris_sum[19]


#### (2) 1
for(i in 1039:paris_sum[20]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "Employés",
                 region="Paris")
}


paris_sum[20]


#### (2) 1
for(i in 1126:paris_sum[21]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=1,
                 job= "Ouvriers",
                 region="Paris")
}


paris_sum[21]


#### (2) 1
for(i in 1156:paris_sum[22]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "Ouvriers",
                 region="Paris")
}


paris_sum[22]


#### (2) 1
for(i in 1166:paris_sum[23]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=2,
                 job= "Ouvriers",
                 region="Paris")
}


paris_sum[23]

for(i in 1195:paris_sum[24]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "Ouvriers",
                 region="Paris")
}


paris_sum[24]

for(i in 1205:paris_sum[25]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=1,
                 job= "Retraités",
                 region="Paris")
}


paris_sum[25]

for(i in 1240:paris_sum[26]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "Retraités",
                 region="Paris")
}


paris_sum[26]

for(i in 1272:paris_sum[27]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=2,
                 job= "Retraités",
                 region="Paris")
}


paris_sum[27]

for(i in 1381:paris_sum[28]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "Retraités",
                 region="Paris")
}


paris_sum[28]


for(i in 1551:paris_sum[29]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=1,
                 job= "Autres",
                 region="Paris")
}


paris_sum[29]

for(i in 1585:paris_sum[30]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=1,
                 job= "Autres",
                 region="Paris")
}


paris_sum[30]

for(i in 1646:paris_sum[31]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 1,
                 immi=2,
                 job= "Autres",
                 region="Paris")
}


paris_sum[31]

for(i in 1898:paris_sum[32]){
  data3[i,] <- c(ID= i+1387,
                 Sexe = 2,
                 immi=2,
                 job= "Autres",
                 region="Paris")
}


paris_sum[32]



#### (4) 모든 데이터 프레임 합치기
data4 <- rbind(data1, data2, data3) # 세 지역의 데이터를 하나의 데이터 프레임으로 통합
str(data4)                           # 계략적인 구조 보기



#### (5) 전처리가 끝난 데이터를 파일로 저장
save(data4, file="data4.RData") # 해당 변수를 RData로 저장
write.csv(data4, "demo_sample_2018.csv") # 전처리가 끝난 data4를 csv 파일로 저장






