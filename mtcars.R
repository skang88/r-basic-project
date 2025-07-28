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
