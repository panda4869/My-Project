 set @c_user='',@rank=0;
 
 select f1.user_id
,timestampdiff(day,u.created_at,f1.session_time) as sign_up_to_book
,timestampdiff(day,f1.session_time,f2.session_time) as interval_between_bookings
,hours_ahead1
,hours_ahead2
,round(ifnull(ic.median_income,median_city.avg_median))as median_income
,case when customer_zip1<>customer_zip2 then 1 else 0 end as diff_zip
,f1.city_id
,qs1.value as quality_score1
,qs2.value as quality_score2
,qs1.value-qs2.value as quality_score_diff
,revenue1
,revenue2
,ifnull(discount_ratio1,0) as discount_ratio1
,ifnull(discount_ratio2,0) as discount_ratio2
,(case when (booking_hour1>=16 and booking_hour1<20) then 1 else 0 end)  as prime_session1
,(case when (booking_hour2>=16 and booking_hour2<20) then 1 else 0 end) as prime_session2
,(case when (booking_hour1>=20) then 1 else 0 end) as late_session1
,(case when (booking_hour2>=20) then 1 else 0 end) as late_session2
,couple1
,couple2
,case when weekend1 in ('Saturday','Sunday') then 1 else 0 end as weekend1
,case when weekend2 in ('Saturday','Sunday') then 1 else 0 end as weekend2
,case when sp.id is not null  then 1 else 0 end as special_request2
,ifnull(num_hvc.num_hvc_per_zip,0) as num_hvc
,us.status as rank
 from
 (
 select user_id,rank,session_time,city_id,customer_zip as customer_zip1,booking_hour1,hours_ahead1,discount_ratio1,couple1,revenue1,weekend1,ar1_id
 from
 (select a.user_id,if(@c_user=a.user_id,@rank:=@rank+1,@rank:=1) as rank, @c_user:=a.user_id,session_time,city_id,customer_zip,booking_hour1,hours_ahead1,discount_ratio1,couple1,revenue1,weekend1,ar1_id
 from
 (select user_id,session_time,city_id,customer_zip, hour(convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US & Canada)' then 'America/Los_Angeles' else timezone end)) as booking_hour1,
 timestampdiff(hour,created_at,session_time) as hours_ahead1, (ifnull(credit_amount,0)+ifnull(discount_amount,0))/(ifnull(session_total_price,0)+ifnull(gift_amount,0)+ifnull(credit_amount,0)+ifnull(discount_amount,0)) as discount_ratio1,case when appointment_type='couples' then 1 else 0 end as couple1,(ifnull(session_total_price,0)+ifnull(gift_amount,0)) as revenue1,dayname(convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US & Canada)' then 'America/Los_Angeles' else timezone end)) as weekend1,id as ar1_id
 
 
 from appointment_requests
 where status='completed'
 
 order by user_id,session_time)a
  
 )a
 where rank=1 )f1
 
 join
  (
  select user_id,rank,session_time,booking_hour2,customer_zip2,hours_ahead2,discount_ratio2,couple2,revenue2,weekend2,ar2_id,local_hour2
  from
  (select a.user_id,if(@c_user=a.user_id,@rank:=@rank+1,@rank:=1) as rank, @c_user:=a.user_id,session_time,booking_hour2,customer_zip2,hours_ahead2,discount_ratio2,couple2,revenue2,weekend2,ar2_id,local_hour2
 from
 (select user_id,session_time,hour(convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US & Canada)' then 'America/Los_Angeles' else timezone end)) as booking_hour2,customer_zip as customer_zip2,timestampdiff(hour,created_at,session_time) as hours_ahead2,(ifnull(credit_amount,0)+ifnull(discount_amount,0))/(ifnull(session_total_price,0)+ifnull(gift_amount,0)+ifnull(credit_amount,0)+ifnull(discount_amount,0)) as discount_ratio2,case when appointment_type='couples' then 1 else 0 end as couple2,(ifnull(session_total_price,0)+ifnull(gift_amount,0)) as revenue2,dayname(convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US & Canada)' then 'America/Los_Angeles' else timezone end)) as weekend2,ar.id as ar2_id,
convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US & Canada)' then 'America/Los_Angeles' else timezone end) as local_hour2
 from appointment_requests ar
 

 where status='completed'
 
 order by user_id,session_time)a

 )a
 where rank=2)f2 on f1.rank=f2.rank-1 and f1.user_id=f2.user_id
 join (select user_id,status from user_scores where user_score_type_id=4)us on us.user_id=f1.user_id
join users u on u.id=f1.user_id and u.is_test=0
join income_data ic on ic.zip_code=f1.customer_zip1
left join (select ft.customer_zip,count(*) as num_hvc_per_zip

from (select user_id,customer_zip from (select user_id,customer_zip,session_time from appointment_requests ar where status='completed' order by user_id,session_time) a group by user_id) ft 
join (select user_id,status from user_scores where user_score_type_id=4 and status in ('AAA','AA','A')) us on us.user_id=ft.user_id

group by ft.customer_zip) num_hvc on num_hvc.customer_zip=f1.customer_zip1

left join
(select n.city_id,avg(median_income) as avg_median

from income_data ic

join neighborhoods n on ic.zip_code=n.zip_code
 
where n.city_id in (select id from cities where country='US') 

group by n.city_id) median_city on median_city.city_id=f1.city_id

join (
select ar.id, ar.user_id, round(avg(us.score/0.47),2) as value
from appointments a
join appointment_requests ar
on a.appointment_request_id = ar.id
join (select user_id, score from user_scores where user_score_type_id=6 ) us on us.user_id=a.therapist_id
group by ar.id
) qs1 on qs1.id=f1.ar1_id

join (
select ar.id, ar.user_id, round(avg(us.score/0.47),2) as value
from appointments a
join appointment_requests ar
on a.appointment_request_id = ar.id
join (select user_id, score from user_scores where user_score_type_id=6 ) us on us.user_id=a.therapist_id
group by ar.id
) qs2 on qs2.id=f2.ar2_id
left join (select distinct(appointment_request_id)as id from therapist_preferences) sp on sp.id=f2.ar2_id
where (f2.session_time<=now()-interval 3 month or us.status in ('AAA','AA','A')) and f1.city_id is not null