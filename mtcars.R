# mtcars 데이터셋은 R에 내장되어 있으므로 바로 사용할 수 있습니다.
# 이 스크립트는 자동차의 무게(wt)와 연비(mpg) 사이의 관계를 시각화합니다.

# ggplot2 패키지 로드 (설치되어 있지 않다면 install.packages("ggplot2") 실행 필요)
# require() 함수는 패키지가 없으면 FALSE를 반환하고 경고를 표시하므로,
# 패키지 설치 여부를 확인하고 설치하는 로직에 유용합니다.
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# ggplot2를 사용하여 산점도 생성
# aes()는 aesthetic 매핑을 정의합니다 (x축, y축, 색상 등).
# geom_point()는 점을 추가합니다. 실린더(cyl) 개수에 따라 색상을 다르게 지정했습니다.
# geom_smooth()는 추세선을 추가합니다 (method="lm"은 선형 모델을 의미).
# labs()는 제목, 부제, 축 레이블 등을 설정합니다.
# theme_minimal()은 깔끔한 테마를 적용합니다.
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl)), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "자동차 무게와 연비의 관계",
    subtitle = "실린더 개수별로 색상 구분",
    x = "무게 (1000 lbs)",
    y = "연비 (Miles/Gallon)",
    color = "실린더 개수"
  ) +
  theme_minimal()

# --- 회귀분석 모델링 ---

# 연비(mpg)를 종속변수로, 무게(wt)와 실린더 개수(cyl)를 독립변수로 하는 다중 회귀모델을 생성합니다.
# 실린더 개수(cyl)는 범주형 변수이므로 factor()로 처리합니다.
model <- lm(mpg ~ wt + factor(cyl), data = mtcars)

# 모델의 요약 정보를 출력합니다.
# 이 요약 정보에는 각 변수의 계수(Coefficients), R-squared 값 등 모델에 대한 상세한 정보가 포함됩니다.
cat("\n--- 다중 회귀분석 모델 요약 ---\n")
summary(model)

# --- 실제 데이터와 회귀선 시각화 ---

# ggplot2를 사용하여 실제 데이터 포인트와 모델의 회귀선을 함께 그립니다.
# aes()에 color와 group을 factor(cyl)로 지정하면, ggplot이 자동으로
# 실린더 그룹별로 데이터를 나누어 점을 찍고, 각 그룹에 대한 회귀선을 그려줍니다.
# 이는 우리가 위에서 만든 lm(mpg ~ wt + factor(cyl), ...) 모델과 동일한 결과를 시각화하는 것입니다.

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), group = factor(cyl))) +
  geom_point(size = 3) + # 실제 데이터 포인트
  geom_smooth(method = "lm", se = FALSE) + # 모델의 회귀선 (se=FALSE는 신뢰구간 음영 제거)
  labs(
    title = "실제 데이터와 회귀 모델 예측",
    subtitle = "모델: mpg ~ wt + factor(cyl)",
    x = "무게 (1000 lbs)",
    y = "연비 (Miles/Gallon)",
    color = "실린더 개수"
  ) +
  theme_minimal()

# --- 모델의 평행 회귀선 정확하게 시각화하기 ---

# 1. 기존 데이터프레임에 모델의 예측값을 'predicted'라는 새 열로 추가합니다.
mtcars_with_predictions <- mtcars
mtcars_with_predictions$predicted <- predict(model)

# 2. 실제값은 점(point)으로, 모델의 예측값은 선(line)으로 그립니다.
# geom_line(aes(y = predicted))는 모델이 예측한 평행한 회귀선을 정확히 그려줍니다.
ggplot(mtcars_with_predictions, aes(x = wt, color = factor(cyl))) +
  geom_point(aes(y = mpg), size = 3) + # 실제 데이터 포인트
  geom_line(aes(y = predicted, group = factor(cyl)), linewidth = 1) + # 모델의 예측선 (평행)
  labs(
    title = "실제 데이터와 모델의 평행 회귀선",
    subtitle = "모델 mpg ~ wt + factor(cyl)의 정확한 시각화",
    x = "무게 (1000 lbs)",
    y = "연비 (Miles/Gallon)",
    color = "실린더 개수"
  ) +
  theme_minimal()

# --- 콘솔 창(채팅창) 지우기 ---
# R 콘솔을 깨끗하게 지웁니다.
# RStudio에서 단축키 Ctrl + L을 누르는 것과 동일한 효과입니다.
cat("\014")
# --- 헬로 월드 출력 ---
# 간단한 "Hello, World!" 메시지를 콘솔에 출력합니다.
cat("Hello, World!\n")
# --- 필요한 정보만 출력하도록 수정 ---
# 스크립트 실행 시 불필요한 출력을 줄이고, 핵심 결과만 표시하도록 합니다.

# 기존의 모든 cat() 및 summary() 호출을 주석 처리하거나 제거합니다.
# 예를 들어, summary(model) 대신 필요한 통계량만 추출하여 출력할 수 있습니다.

# 모델 요약 대신, R-squared 값과 각 변수의 계수만 출력합니다.
cat("\n--- 다중 회귀분석 모델 핵심 요약 ---\n")
cat("Adjusted R-squared: ", summary(model)$adj.r.squared, "\n")
cat("Coefficients:\n")
print(summary(model)$coefficients)

# 시각화는 자동으로 플롯 창에 나타나므로 별도의 cat() 호출이 필요 없습니다.
# 따라서, 시각화 관련 cat() 호출은 제거하거나 주석 처리합니다.

# 최종적으로 콘솔에 "스크립트 실행 완료" 메시지를 출력합니다.
cat("\n스크립트 실행 완료: mtcars 데이터 분석 및 시각화\n")
