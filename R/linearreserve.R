linear_reserve_ftn <- function(x){
  colnames(x) <- layout_kor 
  shrink_x <- x %>% 
    select(계약보험번호, 계약번호, 보험코드, 상품코드, 준비금적립형태중분류코드, 계약일자, 납입기간개월수, 준비금기준가입금액, 보험가입금액, 준비금적용식연시준비금, 준비금적용식연말준비금, 준비금표준식연시준비금, 준비금표준식연말준비금, 표준책임준비금여부, 표준식순보식적립금, 적용식순보식적립금)
  
  shrink_x_test <- shrink_x %>% filter(준비금적립형태중분류코드 == 'A01')
  remove(shrink_x)
  
  # format 수정
  shrink_x_test$계약일자 <- 
    as.Date(as.character(shrink_x_test$계약일자),'%Y%m%d')
  shrink_x_test$표준식순보식적립금 <- as.numeric(shrink_x_test$표준식순보식적립금)
  shrink_x_test$적용식순보식적립금 <- as.numeric(shrink_x_test$적용식순보식적립금)
  shrink_x_test$보험가입금액 <- as.numeric(shrink_x_test$보험가입금액)
  shrink_x_test$준비금기준가입금액 <- as.numeric(shrink_x_test$준비금기준가입금액)
  
  shrink_x_test$월할일할 <- 
    ifelse(결산일< shrink_x_test$계약일자 %m+% months(shrink_x_test$납입기간개월수), '월할','일할')
  
  shrink_x_test$가입금액승수 <- 
    ifelse(shrink_x_test$준비금기준가입금액 == 0, 1, shrink_x_test$보험가입금액/shrink_x_test$준비금기준가입금액 )
  
  shrink_x_test$적용식_연시준비금 <- 
    shrink_x_test$준비금적용식연시준비금*100000*shrink_x_test$가입금액승수
  
  shrink_x_test$적용식_연말준비금 <- 
    shrink_x_test$준비금적용식연말준비금*100000*shrink_x_test$가입금액승수
  
  shrink_x_test$표준식_연시준비금 <- 
    shrink_x_test$준비금표준식연시준비금*100000*shrink_x_test$가입금액승수
  
  shrink_x_test$표준식_연말준비금 <- 
    shrink_x_test$준비금표준식연말준비금*100000*shrink_x_test$가입금액승수
  
  shrink_x_test$경과년수 <- 
    floor(time_length(interval(shrink_x_test$계약일자,결산일),'years'))
  
  shrink_x_test$경과월수 <- 
    floor(time_length(
      interval(shrink_x_test$계약일자 + years(shrink_x_test$경과년수),결산일), 'months')
    )+1 
  
  shrink_x_test$경과일수 <- 
    time_length(
      interval(shrink_x_test$계약일자 + years(shrink_x_test$경과년수),결산일), 'days')
  
  shrink_x_test$WRA_적용식준비금 <- 
    shrink_x_test$적용식_연시준비금 + 
    (shrink_x_test$적용식_연말준비금-shrink_x_test$적용식_연시준비금) * 
    ifelse(shrink_x_test$월할일할 == '월할', shrink_x_test$경과월수/12, shrink_x_test$경과일수 / 365)
  
  shrink_x_test$WRA_표준식준비금 <- 
    shrink_x_test$표준식_연시준비금 + 
    (shrink_x_test$표준식_연말준비금-shrink_x_test$표준식_연시준비금) * 
    ifelse(shrink_x_test$월할일할 == '월할', shrink_x_test$경과월수/12, shrink_x_test$경과일수 / 365)
  
  shrink_x_test$diff_준비금 <- 
    round(ifelse(shrink_x_test$표준책임준비금여부=='Y',
                 shrink_x_test$표준식순보식적립금 - shrink_x_test$WRA_표준식준비금,
                 shrink_x_test$적용식순보식적립금 - shrink_x_test$WRA_적용식준비금)
    )
  
  shrink_x_test
  
  # print(table(abs(shrink_x_test$diff_준비금)>1)) # 1보다 큰 값 출력
  
}
