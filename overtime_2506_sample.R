# 필요한 패키지 로드 
library(DBI)
library(odbc)
library(readxl)
library(writexl)
library(dplyr)  # 데이터 조인을 위해 사용
library(tidyr)
library(lubridate)  # 날짜 형식 변환을 위해 사용
library(forcats)
library(scales)


# 데이터 프레임 생성
df_2412 <- read_excel("data/overtime_2412.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2501 <- read_excel("data/overtime_2501.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2502 <- read_excel("data/overtime_2502.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2503 <- read_excel("data/overtime_2503.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2503_추가 <- read_excel("data/overtime_2503_추가.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2503_월말 <- read_excel("data/overtime_2503_월말.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2504 <- read_excel("data/overtime_2504.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2505 <- read_excel("data/overtime_2505.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))
df_2506 <- read_excel("data/overtime_2506.xlsx", skip = 1) %>% filter(구분 %in% c("잔업", "특근"))

# 현재 월에 지난 월 포함된 건 조회
df_2501 %>% filter(문서번호 %in% df_2412$문서번호)  # 21건
df_2502 %>% filter(문서번호 %in% df_2501$문서번호)  # 0건
df_2503 %>% filter(문서번호 %in% df_2502$문서번호)  # 0건
df_2504 %>% filter(문서번호 %in% df_2503$문서번호)  # 0건
df_2505 %>% filter(문서번호 %in% df_2504$문서번호)  # 0건
df_2505 %>% filter(문서번호 %in% df_2504$문서번호)  # 0건
df_2506 %>% filter(문서번호 %in% df_2505$문서번호)  # 0건


# # 지난달 청구건 제외
df <- df_2506 %>%
  filter(구분 %in% c("잔업", "특근")) %>% 
  filter(상태 %in% c("완료", "진행중")) %>%
  filter(사번 != "4241043") # 이동훈 매니저 제외

# SQL Server에 연결 (연결 설정을 사용자의 환경에 맞게 수정)
conn <- dbConnect(odbc(),
                  Driver = "SQL Server",   # 또는 다른 드라이버 이름
                  Server = "172.16.220.3",  # 서버 이름
                  Database = "SAG",  # 데이터베이스 이름
                  UID = "seokgyun",   # 사용자 이름
                  PWD = "1q2w3e4r",   # 비밀번호
                  Port = 1433, 
                  encoding = "UTF-8")  # 필요시 포트 번호


query2 <- "SELECT [SABUN], [SOSOK], [JIKB] FROM [dbo].[COM_AKMSTRP] "
COM_AKMSTRP <- dbGetQuery(conn, query2) %>% tibble()

query3 <- "SELECT PDATE, ISHOLIDAY, DAYDESC FROM SAG.dbo.COM_DIC_DATE "
HOLIDAY <- dbGetQuery(conn, query3) %>% tibble()

# Database 연결해제 
dbDisconnect(conn)



# 시간을 시작과 종료 시간으로 구분
df2 <- df %>% select(-현결재자) %>% 
  separate(기준일자, into = c("start_time", "end_time"), sep = " ~ ", , remove = FALSE) %>% 
  mutate(start_time = ymd_hm(start_time),
         end_time = ymd_hm(end_time)) %>% 
  mutate(DATE = as.Date(start_time))

HOLIDAY2 <- HOLIDAY %>% 
  mutate(DATE = ymd(PDATE)) %>% 
  mutate(weekday = wday(DATE, , label = TRUE)) %>% 
  select(DATE, weekday, ISHOLIDAY, DAYDESC)

HOLIDAY2 %>% filter(ISHOLIDAY != 0)

# 잔업, 특근 카운트
df2 %>% count(근태구분, 구분)
df2 %>% left_join(HOLIDAY2, by = "DATE") %>% filter(ISHOLIDAY == 0)  # 평일 조회
df2 %>% left_join(HOLIDAY2, by = "DATE") %>% filter(ISHOLIDAY != 0)  # 주말 조회

# 잘 못 기재한 목록 확인
df2 %>% left_join(HOLIDAY2, by = "DATE") %>% filter(ISHOLIDAY != 0) %>% filter(구분 != "특근")  # 휴일에 특근이 아닌 건 조회
df2 %>% left_join(HOLIDAY2, by = "DATE") %>% filter(ISHOLIDAY == 0) %>% filter(구분 == "특근")  # 평일에 특근인건 조회


df3 <- df2 %>% 
  left_join(HOLIDAY2, by = "DATE") %>%  # 휴일 정보를 가져오기 위함 ISHOLIDAY
  left_join(COM_AKMSTRP, by = c("사번" = "SABUN")) %>%  # 소속과 직급을 가져오기 위함임
  mutate(
    직급 = case_when(
      JIKB == "21" ~ "책임매니저S",
      JIKB == "22" ~ "책임매니저A",
      JIKB == "23" ~ "매니저A",
      JIKB == "24" ~ "기사",
      JIKB == "31" ~ "인턴",
      TRUE ~ JIKB  # 조건에 맞지 않는 값은 그대로 유지
    )
  ) %>% 
  mutate(
    소속 = case_when(
      사번 %in% c("G230008", "G230006", "G240001", "G240006") ~ "관리팀",
      사번 %in% c("G241003", "G240008", "4180135") ~ "품질팀",
      사번 %in% c("G240005", "G241002", "G241004", "G240007", "G251002", "G251003", "G250005", "G250002") ~ "생산팀",
      사번 %in% c("G240004", "2150152") ~ "영업구매팀",
      사번 %in% c("G251001") ~ "HR안전팀",
      사번 %in% c("G240002", "G230003", "G250001", "G250003", "G241001") ~ "생관자재팀",
      TRUE ~ NA  # 사번이 없는 값은 NA처리
    )
  ) %>% 
  mutate(
    대상자_KR = case_when(
      사번 == "G230008" ~ "최효준",
      사번 == "G240002" ~ "최상만",
      사번 == "G240005" ~ "심재룡",
      사번 == "G241002" ~ "양현석",
      사번 == "G241003" ~ "김명진",
      사번 == "G241004" ~ "유대협",
      사번 == "G240004" ~ "이종복",
      사번 == "G230003" ~ "지영민",
      사번 == "G230006" ~ "이선민",
      사번 == "G240001" ~ "강석균",
      사번 == "G240008" ~ "박성전",
      사번 == "4180135" ~ "송서준",
      사번 == "G240006" ~ "이태우",
      사번 == "2150152" ~ "이건민",
      사번 == "G240007" ~ "성정규",
      사번 == "G240007" ~ "성정규",
      사번 == "G250002" ~ "박찬영",
      사번 == "G250001" ~ "백승연",
      사번 == "G250003" ~ "최문기",
      사번 == "G241001" ~ "리키",
      사번 == "G251002" ~ "탁수익",
      사번 == "G251003" ~ "김대광",
      사번 == "G251001" ~ "코리왓슨",
      사번 == "G250005" ~ "신우용",
      TRUE ~ 대상자  # 조건에 맞지 않는 값은 그대로 유지
    )
  ) %>% 
  mutate(소속 = factor(소속, levels = c("관리팀", "품질팀", "생산팀", "영업구매팀", "HR안전팀", "생관자재팀"))) %>%
  mutate(직급 = factor(직급, levels = c("책임매니저A", "매니저A", "기사", "인턴"))) %>% 
  mutate(대상자 = if_else(사번 == "G240007", "Jeong-Gyu Seong", 대상자))

# 직급, 소속, 사번 맵핑이 되지 않은 인원 조회
df3 %>% filter(is.na(소속)) %>% select(사번, 대상자, 소속)  # 소속이 부여되지 않은 직원 확인
df3 %>% filter(is.na(대상자_KR)) %>% select(사번, 대상자, 소속)  # 소속이 부여되지 않은 직원 확인

# 인원별 소속, 직급, 이름 맵핑 결과 확인
df3 %>% count(소속, 직급, 사번, 대상자, 대상자_KR)  # 한국어 이름이 제대로 매칭되었는지 확인

# Overtime 집계 - 시작시간 종료시간 차이 계산, 2시간 이상 시 식대 추가, 책임매니저는 0시간으로 반영 
df4_overtime <- df3 %>%
  filter(ISHOLIDAY == 0) %>% 
  mutate(
    diff = floor(as.numeric(difftime(end_time, start_time, units = "hours")))
  ) %>% 
  mutate(
    식대 = if_else(diff >= 2, 15, 0)
  ) %>% 
  mutate(
    diff = if_else(직급 == "책임매니저A", 0, if_else(diff > 4, 4, diff))
  ) %>% 
  mutate(
    start_time = format(start_time, "%Y-%m-%d %H:%M"),
    end_time = format(end_time, "%Y-%m-%d %H:%M")
  )

# 평일 오버타임
df4_overtime

# 집계 결과 확인 개인별 근무 시간단위 카운트
df4_overtime %>% count(직급, 대상자, 대상자_KR, diff) %>% print(n=50)
df4_overtime %>% count(소속, 직급, 대상자, 대상자_KR)

# 특근 수당 집계 전 근무 시간 확인 (4시간, 8시간 등 종류별 건수)
df3 %>% filter(ISHOLIDAY != 0) %>% 
  mutate(diff = as.numeric(difftime(end_time, start_time, units = "hours"))) %>% 
  count(diff)

# 특근수당 산정 
df4_spwork <- df3 %>%
  filter(ISHOLIDAY != 0) %>%
  mutate(
    diff = as.numeric(difftime(end_time, start_time, units = "hours"))
  ) %>%
  mutate(
    diff = if_else(diff >= 8, 8, diff)  # diff 8시간 이상인 경우 8시간으로 수정
  ) %>%
  mutate(
    diff = if_else(diff < 8, 4, diff)  # diff 8시간 이하인 경우 4시간으로 수정
  ) %>% 
  mutate(
    start_time = format(start_time, "%Y-%m-%d %H:%M"),
    end_time = format(end_time, "%Y-%m-%d %H:%M")
  )

df4_spwork %>% count(구분, 사번, 대상자_KR, diff)
df4_spwork %>% select(구분, 사번, 대상자_KR, 제목, diff)

# 한글 이름 부여
df5_overtime <- df4_overtime %>% 
  select(문서번호, 구분, 사번, 소속, 직급, 대상자_KR, 대상자, DATE, weekday, 기준일자, 제목, start_time, end_time, diff, 식대) %>% 
  rename(`Start Time` = start_time, `End Time` = end_time, `Input Time` = 기준일자) %>%
  rename(`Document No` = 문서번호, `Employee No` = 사번, `Name` = 대상자, `Name_KR` = 대상자_KR, Position = 직급, Department = 소속, Date = DATE, 
         Weekday = weekday, Title = 제목, `Approved Time` = diff, `Meal` = 식대) %>% 
  arrange(`Start Time`) %>% 
  separate(Name, into = c("FirstName", "LastName"), sep = " ")

df5_overtime
df5_overtime %>% colnames()
df5_overtime %>% count(Department)

df5_spwork <- df4_spwork %>% 
  select(문서번호, 구분, 사번, 소속, 직급, 대상자_KR, 대상자, DATE, weekday, 기준일자, 제목, start_time, end_time, diff) %>% 
  #mutate(소속 = fct_relevel(소속, "관리팀", "품질팀", "생산팀", "영업팀")) %>% # 
  mutate(직급 = fct_relevel(직급, "책임매니저A", "기사")) %>% # "매니저A", 
  rename(`Start Time` = start_time, `End Time` = end_time, `Input Time` = 기준일자) %>%
  rename(`Document No` = 문서번호, `Employee No` = 사번, `Name_KR` = 대상자_KR, 
         `Name` = 대상자, Position = 직급, Department = 소속, Date = DATE, Weekday = weekday, Title = 제목, 
         `Approved Time` = diff) %>% 
  arrange(`Start Time`) %>% 
  separate(Name, into = c("FirstName", "LastName"), sep = " ")

df5_spwork
df5_spwork %>% colnames()


# 오버타임 집계 테이블 생성
tbl_base <- df5_overtime
tbl_tot <- df5_overtime %>% 
  group_by(`Employee No`, Position, Department, FirstName, LastName, Name_KR) %>% 
  summarise(
    `Work Days` = sum(!is.na(`Approved Time`)),  # Count non-missing entries
    `Approved Time` = sum(`Approved Time`, na.rm = TRUE),  # Sum with NA handling
    `Total Meal($)` = sum(Meal, na.rm = TRUE),  # Sum with NA handling
    .groups = "drop"
  ) %>%
  arrange(Department, Position, Name_KR)

tbl_sp_base <- df5_spwork

# 특근 집계 테이블 생성
tbl_sp_tot <- df5_spwork %>% 
  group_by(`Employee No`, Position, Department, FirstName, LastName, Name_KR) %>% 
  summarise(
    `Work Days` = sum(!is.na(`Approved Time`)),  # Count non-missing entries
    `Approved Time` = sum(`Approved Time`, na.rm = TRUE),  # Sum with NA handling
    .groups = "drop"
  ) %>%
  arrange(Department, Position, Name_KR)

tbl_base %>% write_xlsx("./output/2506_OT기초데이터.xlsx")
tbl_tot %>% write_xlsx("./output/2506_OT집계데이터.xlsx")
tbl_sp_base %>% write_xlsx("./output/2506_특근기초데이터.xlsx")
tbl_sp_tot %>% write_xlsx("./output/2506_특근집계데이터.xlsx")

tbl_tot %>% print()
tbl_sp_tot %>% print()

