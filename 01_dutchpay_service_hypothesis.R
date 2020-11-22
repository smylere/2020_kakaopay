# ‘더치페이 요청에 대한 응답률이 높을수록 더치페이 서비스를 더 많이 사용한다.’
# 라는 가설을 통계적으로 검정해주세요.
# 
# 즉, 더치페이를 해달라고 요청한 부분에 대해 응답받은 유저가 많고 응답이 
# 많을 수록 더치페이 서비스를 더 자주 사용한다
# 
# 귀무가설 : 더치페이 요청에 대한 응답률은 더치페이 서비스 사용 비율에 영향을 주지 않는다
# 대립가설 : 더이페이 요청에 대한 응답률은 더이페이 서비스 사용 비율에 영향을 준다. 


### 01. 더치페이 요청에 대한 응답률과 더치페이 사용비율에 대한 기준 수립

load("D:/Project/2020_kakaopay_ds/.RData")

library(sqldf)
library(nortest)

# 데이터 추출기준 및 더치패 요청횟수와 응답률 계산 table 구성
query =
"
select 
	claim_user_id,
	sum(total_claim_cnt) as total_cnt,
	sum(send_cnt) as send_cnt,
	count(claim_user_id) as claim_cnt,
	round((sum(send_cnt)+0.00)/(sum(total_claim_cnt)+0.00),4) as recv_rate
from 
	(
	select 
		claim_user_id ,
		count(claim_id) as total_claim_cnt,
		sum(send_cnt) as send_cnt
	from 
		(
		select 
			dc.claim_id 
			,dc.claim_user_id
			,dcd.recv_user_id 
			,dcd.claim_amount 
			,dcd.send_amount 
			,dcd.status
			,case when dcd.status ='SEND' then 1 else 0 end send_cnt
		from 
			dutchpay_claim dc 
		join 
			dutchpay_claim_detail dcd 
			on dc.claim_id =dcd.claim_id
		where
		    1=1
		    and dc.claim_user_id != dcd.recv_user_id
		)
	group by 
		claim_id ,
		claim_user_id
	order by 
		claim_user_id
	)
group by 
	claim_user_id
"
# 데이터 추출
dataset = sqldf(query)

# 데이터 확인
head(dataset,10)

#      claim_user_id gender_cd age foreigner_yn os_type total_cnt send_cnt claim_cnt recv_rate
# 1  0001440cbac4d21         1  26            N       B         3        0         1    0.0000
# 2  0001f7e9c43abf0         2  22            N       B         7        0         1    0.0000
# 3  0002af834baa1a8         1  28            N       A         4        2         1    0.5000
# 4  00043dcb5157e85         1  39            N       B         4        1         1    0.2500
# 5  0005da54f0216fe         1  24            N       A         4        1         2    0.2500
# 6  000802145306c63         1  37            N       A         8        5         3    0.6250
# 7  0008f3d9b258023         1  20            N       A        10        5         3    0.5000
# 8  000aa226f3190a0         1  34            N       B         6        2         1    0.3333
# 9  000aa70c8a627f7         1  28            N       B         3        0         1    0.0000
# 10 000aef4e7b4d3c1         1  26            N       A         1        0         1    0.0000


# 신규 데이터 정의
# total_cnt : 더치페이 요청 유저가 대상자에게 요청한 총 요청횟수
# send_cnt : 더치페이 요청을 받은 유저중 실제로 돈을 보낸 총 횟수 
# claim_cnt : 더치페이 요청 건수
# recv_rate : send_cnt / total_cnt (더치페이 응신 비율) 

### 02. 더치페이 응답률과 사용빈도 검토

# 귀무가설 : recv_rate와 claim_cnt는 관계가 없다
# 대립가설 : recv_rate가 클 수록 claim_cnt가 크다 

cor.test(dataset$claim_cnt,dataset$recv_rate)

# Pearson's product-moment correlation
# 
# data:  dataset$claim_cnt and dataset$recv_rate
# t = 26.108, df = 53525, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1037618 0.1204918
# sample estimates:
#       cor 
# 0.1121348 

# 더치패이 요청횟수와 응답률은 양의 비례관계를 가지나, 그 값이 크지 않다
# 유의미한 수준에서 두 변수간의 관계가 직접적인 영향을 끼친다고 보기 위해선
# 별도의 조건과 가정을 추가로 진행 해야 한다. 

summary(lm(dataset$claim_cnt~dataset$recv_rate,data=dataset))

# Call:
#     lm(formula = dataset$claim_cnt ~ dataset$recv_rate, data = dataset)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.500  -1.524  -1.524   0.257 118.351 
# 
# Coefficients:
#              Estimate Std.      Error t value Pr(>|t|)    
# (Intercept)        2.52385    0.02467  102.31   <2e-16 ***
# dataset$recv_rate  1.97654    0.07571   26.11   <2e-16 ***

# Residual standard error: 4.104 on 53525 degrees of freedom
# Multiple R-squared:  0.01257,	Adjusted R-squared:  0.01256 
# F-statistic: 681.6 on 1 and 53525 DF,  p-value: < 2.2e-16

# y(claim_cnt) ~ x(recv_rate)에 대한 선형회귀 검토 결과 결정계수가 
# 0.01에 수렴하는것으로 확인 되었으며, 두 값에 대한 선형적 회귀모형 관계는 유추하기
# 어렵다고 해석 할 수 있다.

### 03_정의사항 전체 확인

summary(dataset)
#         claim_user_id     total_cnt         send_cnt        claim_cnt         recv_rate     
# 0001440cbac4d21:    1   Min.   :  1.00   Min.   :  0.00   Min.   :  1.000   Min.   :0.0000  
# 0001f7e9c43abf0:    1   1st Qu.:  2.00   1st Qu.:  0.00   1st Qu.:  1.000   1st Qu.:0.0000  
# 0002af834baa1a8:    1   Median :  4.00   Median :  1.00   Median :  2.000   Median :0.2727  
# 00043dcb5157e85:    1   Mean   :  7.88   Mean   :  2.96   Mean   :  3.003   Mean   :0.3265  
# 0005da54f0216fe:    1   3rd Qu.:  9.00   3rd Qu.:  3.00   3rd Qu.:  3.000   3rd Qu.:0.5068  
# 000802145306c63:    1   Max.   :528.00   Max.   :206.00   Max.   :120.000   Max.   :1.0000

# 요청횟수에 따라 두 집단의 차이를 검토 
# 집단 분리 기준은 요청횟수(claim_cnt) 중위값(median)을 기준으로 2, 그리고 2초과 집단 두개로 분리 

# 두 집단에대한 밀도 분포 시각화 확인
plot(density(subset(dataset,claim_cnt<=2)$recv_rate))
lines(density(subset(dataset,claim_cnt>2)$recv_rate),lty=2)

plot(density(subset(dataset,claim_cnt==1)$recv_rate))
lines(density(subset(dataset,claim_cnt==2)$recv_rate),lty=2,col='red')
lines(density(subset(dataset,claim_cnt==3)$recv_rate),lty=3,col='blue')
lines(density(subset(dataset,claim_cnt==4)$recv_rate),lty=4,col='purple')
lines(density(subset(dataset,claim_cnt==5)$recv_rate),lty=5,col='green')
lines(density(subset(dataset,claim_cnt==6)$recv_rate),lty=5,col='orange')
lines(density(subset(dataset,claim_cnt>=7)$recv_rate),lty=5,col='gray')

# 각 데이터집단이 정규성(n>30)을 가진다고 가정하여 t-test실시 

var.test(
    subset(dataset,claim_cnt <=2)$recv_rate,
    subset(dataset,claim_cnt > 2)$recv_rate,
)


t.test(
    subset(dataset,claim_cnt <=2)$recv_rate,
    subset(dataset,claim_cnt > 2)$recv_rate,
    alternative = c("two.sided")
)












#데이터의 분포가 정규분포를 따르지 않으므로 비모수 측정 진행

wilcox.test(
    subset(dataset,claim_cnt<=2)$recv_rate,
    subset(dataset,claim_cnt>2)$recv_rate,
    alternative = c("two.sided"),
    conf.level = 0.95,
    conf.int = FALSE,
    mu=0
)



# 비교대상 데이터 정규성 검정
# 
# 귀무가설 : 자료의 분포는 정규분포 따른다
# 대립가설 : 자료의 분포는 정규분포를 따르지 않는다

# nortest::ad.test(dataset$claim_cnt)
# nortest::ad.test(dataset$recv_rate)


# 비교대상 데이터 등분산 검정
#
# 귀무가설 : 두 집단의 분산은 같다
# 대립가설 : 두 집단의 분산은 같지 않다 

var.test(
    subset(dataset,claim_cnt ==1)$recv_rate,
    subset(dataset,claim_cnt !=1)$recv_rate,
)

t.test(
    subset(dataset,claim_cnt ==1)$recv_rate,
    subset(dataset,claim_cnt !=1)$recv_rate,
)

t.test(
    subset(dataset,claim_cnt ==1)$recv_rate,
    subset(dataset,claim_cnt !=1)$recv_rate,
    alternative = 'two.sided',
    var.equal= TRUE
)    




cor.test(
    subset(dataset,claim_cnt ==5)$claim_cnt,
    subset(dataset,claim_cnt ==5)$recv_rate,
)




cor.test(
    subset(dataset,gender_cd ==1)$claim_cnt,
    subset(dataset,gender_cd ==1)$recv_rate,
)

cor.test(
    subset(dataset,gender_cd ==2)$claim_cnt,
    subset(dataset,gender_cd ==2)$recv_rate,
)


cor.test(
    subset(dataset,os_type =='A')$claim_cnt,
    subset(dataset,os_type =='A')$recv_rate,
)

cor.test(
    subset(dataset,os_type =='B')$claim_cnt,
    subset(dataset,os_type =='B')$recv_rate,
)


cor.test(
    subset(dataset,os_type =='A' & gender_cd ==2)$claim_cnt,
    subset(dataset,os_type =='A' & gender_cd ==2)$recv_rate,
)
