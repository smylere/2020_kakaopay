--explain query plan
select 
	user_id,
	sls_amt
from 
	(
	select 
		user_id,
		sum(amount) as sls_amt
	from 
		(
		select
--			transaction_id ,
--			transacted_at ,
--			payment_action_type ,
			user_id ,
			amount 
		from 
			a_payment_trx apt 
		where
			1=1
			and substr(transacted_at,1,7)='2019-12'
			and user_id in 
				(
				select distinct 
					claim_user_id 
				from 
					dutchpay_claim dc 
				where 
					1=1
					and substr(claim_at,1,7) ='2019-12'
				)
			and payment_action_type = 'PAYMENT'
		union all
		select
--			transaction_id ,
--			transacted_at ,
--			payment_action_type ,
			user_id ,
			-amount as amount 
		from 
			a_payment_trx apt 
		where
			1=1
			and (transacted_at >= '2019-12-01' and transacted_at < '2020-03-01')
			and user_id in 
				(
				select distinct 
					claim_user_id 
				from 
					dutchpay_claim dc 
				where
					1=1
					and substr(claim_at,1,7) ='2019-12'
				)
			and payment_action_type = 'CANCEL'
			)
	group by 
		user_id
	)
where 
	1=1
	and sls_amt >=10000
; --482명 
