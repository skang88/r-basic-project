# --- 1. 패키지 설치 및 로드 ---
# 스크립트 실행에 필요한 패키지 목록을 정의합니다.
required_packages <- c("DBI", "odbc", "lubridate", "tidyr", "dplyr", "openxlsx")

# 현재 설치되지 않은 패키지를 확인합니다.
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# 설치가 필요한 패키지가 있다면 설치를 진행합니다.
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

# 모든 필수 패키지를 라이브러리에 로드합니다.
lapply(required_packages, library, character.only = TRUE)


# --- 2. 분석 환경 설정 ---
# 분석에 사용할 주요 변수를 설정합니다.
TARGET_MONTH <- 7 # 분석할 월을 숫자로 지정 (예: 7월은 7)


# --- 3. Data Load ----
# 데이터 분석에 필요한 테이블들을 데이터베이스에서 로드합니다.
# 각 테이블의 역할은 쿼리 위에 주석으로 명시합니다.

# MS SQL 연결
# !!! 보안 경고 !!!
# 실제 운영 코드에서는 아래와 같이 접속 정보를 직접 코드에 작성하지 마세요.
# .Renviron 파일이나 외부 설정 파일을 사용하는 것이 안전합니다.
mssql_con <- dbConnect(odbc::odbc(),
                       Driver = "SQL Server",
                       Server = "172.16.220.3",
                       Database = "SAG",
                       UID = "seokgyun",
                       PWD = "1q2w3e4r",
                       Port = 1433,
                       encoding = "UTF-8") # 한글 깨짐 방지를 위해 인코딩 설정

# [INS_ATTEND_EXC]: 근태 신청/결과에 대한 원본 데이터 테이블.
query_exc <- "SELECT * FROM INS_ATTEND_EXC;"
INS_ATTEND_EXC <- dbGetQuery(mssql_con, query_exc) %>% tibble()

# [COM_AKMSTRP]: 사원(직원) 마스터 정보 테이블.
query_mst <- "SELECT SABUN, SOSOK, JIKB, KNAME FROM COM_AKMSTRP;"
COM_AKMSTRP <- dbGetQuery(mssql_con, query_mst) %>% tibble()

# [COM_ASOSKRP]: 부서(소속) 마스터 정보 테이블.
query_sosok <- "SELECT ASOSK, ASILN FROM COM_ASOSKRP;"
COM_ASOSKRP <- dbGetQuery(mssql_con, query_sosok) %>% tibble()

# [COM_DIC_DATE]: 사내 달력 정보 테이블.
query_date <- "SELECT PDATE, ISHOLIDAY, DAYDESC FROM COM_DIC_DATE;"
COM_DIC_DATE <- dbGetQuery(mssql_con, query_date) %>% tibble()

# [COM_CLS_MST]: 전자결재 문서 마스터 테이블.
query_cls <- "SELECT DOCU_CD, DOCU_NO, DOCU_ST FROM COM_CLS_MST WHERE TABLE_CD='GUNT_EXC';"
COM_CLS_MST <- dbGetQuery(mssql_con, query_cls) %>% distinct_all() %>% tibble()

dbDisconnect(mssql_con)


# --- 4. 데이터 전처리 및 통합 ---

# 4-1. 코드-이름 매핑 테이블 정의
jikb_map <- tibble::tribble(
  ~JIKB, ~직급,
  "21", "책임매니저S",
  "22", "책임매니저A",
  "23", "매니저A",
  "24", "기사",
  "31", "인턴"
)

# DB에서 가져온 부서 테이블의 컬럼명을 공통 키(SOSOK)로 변경하고 중복 제거
sosok_info <- COM_ASOSKRP %>%
  distinct(ASOSK, .keep_all = TRUE) %>%
  rename(SOSOK = ASOSK, 소속 = ASILN)

# 4-2. 날짜 형식 변환 및 휴일 정보 결합
HOLIDAY_INFO <- COM_DIC_DATE %>%
  mutate(DATE = ymd(PDATE),
         weekday = wday(DATE, label = TRUE)) %>%
  select(DATE, weekday, ISHOLIDAY, DAYDESC)

# 4-3. 근태 데이터 필터링 및 정제
df_base <- INS_ATTEND_EXC %>%
  filter(KGUBN %in% c("20", "21")) %>%
  mutate(DOCU_CD_KEY = paste0(GUBN, GDATE, SER)) %>%
  left_join(COM_CLS_MST, by = c("DOCU_CD_KEY" = "DOCU_CD", "SUB_SER" = "DOCU_NO")) %>%
  filter(DOCU_ST %in% c("9")) %>%
  arrange(desc(GDATE)) %>%
  mutate(
    start_time = ymd_hms(paste(SDATE, STIME, "00")),
    end_time = ymd_hms(paste(EDATE, ETIME, "00")),
    DATE = as.Date(start_time)
  ) %>%
  filter(month(start_time) == TARGET_MONTH)

# 4-4. 휴일, 사원 정보 결합 및 파생변수 생성
df_final <- df_base %>%
  left_join(HOLIDAY_INFO, by = "DATE") %>%
  left_join(COM_AKMSTRP, by = "SABUN") %>%
  left_join(sosok_info, by = "SOSOK") %>%
  left_join(jikb_map, by = "JIKB") %>%
  mutate(
    구분 = if_else(KGUBN == "20", "잔업", "특근"),
    JNAME = case_when(
      SABUN == "G230008" ~ "최효준", SABUN == "G240002" ~ "최상만",
      SABUN == "G240005" ~ "심재룡", SABUN == "G241002" ~ "양현석",
      SABUN == "G241003" ~ "김명진", SABUN == "G241004" ~ "유대협",
      SABUN == "G240004" ~ "이종복", SABUN == "G230003" ~ "지영민",
      SABUN == "G230006" ~ "이선민", SABUN == "G240001" ~ "강석균",
      SABUN == "G240008" ~ "박성전", SABUN == "4180135" ~ "송서준",
      SABUN == "G240006" ~ "이태우", SABUN == "2150152" ~ "이건민",
      SABUN == "G240007" ~ "성정규", SABUN == "G250002" ~ "박찬영",
      SABUN == "G250001" ~ "백승연", SABUN == "G250003" ~ "최문기",
      SABUN == "G241001" ~ "리키",   SABUN == "G251002" ~ "탁수익",
      SABUN == "G251003" ~ "김대광", SABUN == "G251001" ~ "코리왓슨",
      SABUN == "G250005" ~ "신우용", SABUN == "G250007" ~ "이준혁",
      TRUE ~ ""
    )
  ) %>%
  separate(KNAME, into = c("FirstName", "LastName"), sep = " ", remove = TRUE, extra = "merge", fill = "right") %>%
  replace_na(list(소속 = "미분류", 직급 = "미분류"))


# --- 5. 수당 계산 ---
df_overtime <- df_final %>%
  filter(ISHOLIDAY == 0) %>%
  mutate(
    diff = floor(as.numeric(difftime(end_time, start_time, units = "hours"))),
    식대 = if_else(diff >= 2, 15, 0),
    Approved_Time = if_else(직급 == "책임매니저A", 0, if_else(diff > 4, 4, diff))
  )

df_spwork <- df_final %>%
  filter(ISHOLIDAY != 0) %>%
  mutate(
    diff = as.numeric(difftime(end_time, start_time, units = "hours")),
    Approved_Time = if_else(diff >= 8, 8, 4)
  )


# --- 6. 결과 테이블 포맷팅 및 집계 ---
department_order <- c("관리팀", "품질팀", "생산팀", "영업구매팀", "HR안전팀", "생관자재팀")
position_order <- c("책임매니저A", "매니저A", "기사", "인턴")

tbl_ot_base <- df_overtime %>%
  select(DOCU_CD_KEY, 구분, SABUN, 소속, 직급, JNAME, FirstName, LastName, DATE, weekday, TITLE, start_time, end_time, Approved_Time, 식대) %>%
  rename(Name_KR = JNAME, `Approved Time` = Approved_Time, Meal = 식대)

tbl_ot_total <- tbl_ot_base %>%
  group_by(`Employee No` = SABUN, Position = 직급, Department = 소속, Name_KR, FirstName, LastName) %>%
  summarise(
    `Work Days` = n(),
    `Approved Time` = sum(`Approved Time`, na.rm = TRUE),
    `Total Meal($)` = sum(Meal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Department = factor(Department, levels = department_order),
    Position = factor(Position, levels = position_order)
  ) %>%
  arrange(Department, Position, `Employee No`)

tbl_sp_base <- df_spwork %>%
  select(DOCU_CD_KEY, 구분, SABUN, 소속, 직급, JNAME, FirstName, LastName, DATE, weekday, TITLE, start_time, end_time, Approved_Time) %>%
  rename(Name_KR = JNAME, `Approved Time` = Approved_Time)

tbl_sp_total <- tbl_sp_base %>%
  group_by(`Employee No` = SABUN, Position = 직급, Department = 소속, Name_KR, FirstName, LastName) %>%
  summarise(
    `Work Days` = n(),
    `Approved Time` = sum(`Approved Time`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Department = factor(Department, levels = department_order),
    Position = factor(Position, levels = position_order)
  ) %>%
  arrange(Department, Position, `Employee No`)


# --- 7. 엑셀 파일로 저장 (최종 서식 적용) ---
output_prefix <- paste0(substr(format(Sys.Date(), "%Y"), 3, 4), sprintf("%02d", TARGET_MONTH))
output_dir <- "./output"
if (!dir.exists(output_dir)) { dir.create(output_dir) }

wb <- createWorkbook()

# --- 스타일 정의 ---
# 헤더 스타일
header_style <- createStyle(fgFill = "#F2F2F2", halign = "center", valign = "center", textDecoration = "bold", fontSize = 9, border = "TopBottomLeftRight", borderStyle = "dotted", borderColour = "#D3D3D3")

# 데이터 영역 기본 스타일 (정렬 + 폰트 + 테두리)
center_style <- createStyle(halign = "center", valign = "center", fontSize = 9, border = "TopBottomLeftRight", borderStyle = "dotted", borderColour = "#D3D3D3")
right_style <- createStyle(halign = "right", valign = "center", fontSize = 9, border = "TopBottomLeftRight", borderStyle = "dotted", borderColour = "#D3D3D3")

# 날짜/시간 서식이 포함된 스타일
date_center_style <- createStyle(numFmt = "mm/dd/yy", halign = "center", valign = "center", fontSize = 9, border = "TopBottomLeftRight", borderStyle = "dotted", borderColour = "#D3D3D3")
datetime_right_style <- createStyle(numFmt = "mm/dd/yy hh:mm", halign = "right", valign = "center", fontSize = 9, border = "TopBottomLeftRight", borderStyle = "dotted", borderColour = "#D3D3D3")

# --- 데이터셋 및 컬럼 맵 정의 ---
datasets <- list(
  "OT_집계" = tbl_ot_total, "OT_기초" = tbl_ot_base,
  "특근_집계" = tbl_sp_total, "특근_기초" = tbl_sp_base
)

# 오른쪽 정렬할 컬럼 이름 맵
right_align_cols_map <- list(
  "OT_집계" = c("Work Days", "Approved Time", "Total Meal($)"),
  "OT_기초" = c("Approved Time", "Meal"),
  "특근_집계" = c("Work Days", "Approved Time"),
  "특근_기초" = c("Approved Time")
)

# --- 엑셀 파일 생성 및 서식 적용 ---
for (sheet_name in names(datasets)) {
  df <- datasets[[sheet_name]]
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)

  # 전체 셀 범위 정의
  all_rows <- 1:(nrow(df) + 1)
  data_rows <- 2:max(all_rows)
  all_cols <- 1:ncol(df)
  
  # 헤더 스타일 적용
  addStyle(wb, sheet_name, style = header_style, rows = 1, cols = all_cols, gridExpand = TRUE)
  
  # 컬럼별로 데이터 영역에 스타일 적용
  for (j in all_cols) {
    col_name <- colnames(df)[j]
    
    # 기본 스타일은 가운데 정렬
    style_to_apply <- center_style
    
    # 오른쪽 정렬 컬럼 확인
    if (col_name %in% right_align_cols_map[[sheet_name]]) {
      style_to_apply <- right_style
    }
    
    # 날짜/시간 서식 컬럼 확인 (기초 시트에만 해당)
    if (sheet_name %in% c("OT_기초", "특근_기초")) {
      if (col_name == "DATE") {
        style_to_apply <- date_center_style
      } else if (col_name %in% c("start_time", "end_time")) {
        style_to_apply <- datetime_right_style
      }
    }
    
    addStyle(wb, sheet_name, style = style_to_apply, rows = data_rows, cols = j, gridExpand = TRUE)
  }
  
  # 열 너비 자동 조정
  setColWidths(wb, sheet_name, cols = all_cols, widths = "auto")
}

# 통합 문서 저장
output_path <- file.path(output_dir, paste0(output_prefix, "_근무시간_리포트.xlsx"))
saveWorkbook(wb, output_path, overwrite = TRUE)

cat(paste("\n분석이 완료되었습니다. '", output_dir, "' 폴더에 최종 서식이 적용된 엑셀 파일(", basename(output_path), ")이 저장되었습니다.\n"))
print(tbl_ot_total)
print(tbl_sp_total)
