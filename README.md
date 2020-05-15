# linear Reserve approx.  

1. 필요패키지  
library(data.table)  
library(readxl)  
library(dplyr)  
library(lubridate)  
library(bit64)  
library(devtools)

2. 설치  
install_github("Melooona / linearreserve")  
library(linearreserve)  

3. 사용  
유지데이터의 layout은 내장되어 있음. '.txt' 형태의 데이터는 아래 함수로 불러옴.   
data <- fread('파일명.txt', sep=';', header = FALSE)  

141번째 컬럼이 null 인 경우가 존재하므로 확인하여 삭제.  
data[,141] # null column 삭제  
table(is.na(data[,141]))  
data <- data[,-141]  
  
확정형 적립금 검증 시 아래 함수 사용  
reserve_review <- linear_reserve_ftn(data)  

저장 :  
ck_error <- ftn_test_1 %>% filter(abs(diff_준비금) > 1)  

write.csv(ck_error,'저장위치/ck_error.csv')  
 
