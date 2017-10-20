
select first_booking.user_id,timestampdiff(day,u.created_at,first_booking.created_at) as first_days_since_signup,first_hours_ahead,first_zip,round(ifnull(ic.median_income,median_city.avg_median))as median_income,first_booking.city_id,session_length, first_MT_quality,ifnull(credit_amount,0) as credit_amount, ifnull(gift_amount,0) as gift_amount, case when (booking_hour>=16 and booking_hour<20) then 1 else 0 end as prime_session,case when (booking_hour>=20) then 1 else 0 end as late_session,case when appointment_type <> 'single' then 1 else 0 end as couple_session,case when dayname in ('Sunday','Saturday') then 1 else 0 end as weekend,ifnull(num_hvc.num_hvc_per_zip,0) as num_hvc , user_score.status
from users u
join (
select ar.user_id, TIMESTAMPDIFF(HOUR, ar.created_at, ar.session_time) as first_hours_ahead, ar.created_at,ar.customer_zip as first_zip, ar_reviews.value as first_MT_quality,
ar.credit_amount,ar_min.session_length,ar_min.city_id,gift_amount,hour(convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US &amp; Canada)' then 'America/Los_Angeles' else timezone end)) as booking_hour,dayname(convert_tz(session_time,'UTC',case timezone when 'Pacific Time (US &amp; Canada)' then 'America/Los_Angeles' else timezone end)) as dayname,appointment_type
from appointment_requests ar
join (
select ar_min.user_id, min(created_at) as min_created_at,session_length,city_id
from appointment_requests ar_min
where ar_min.status='completed'
group by ar_min.user_id
) as ar_min
on ar.created_at = ar_min.min_created_at and ar.user_id = ar_min.user_id
left join (
select ar.id, ar.user_id, round(avg(us.score/0.47),2) as value
from appointments a
join appointment_requests ar
on a.appointment_request_id = ar.id
join (select user_id, score from user_scores where user_score_type_id=6 ) us on us.user_id=a.therapist_id
group by ar.id
) as ar_reviews
on ar_reviews.id = ar.id 
where ar.status='completed' 
group by ar.user_id
order by ar.user_id asc
) as first_booking
on u.id = first_booking.user_id
left join
(select n.city_id,avg(median_income) as avg_median

from income_data ic

join neighborhoods n on ic.zip_code=n.zip_code
where n.city_id in (select id from cities where country='US') 

group by n.city_id) median_city on median_city.city_id=first_booking.city_id

join
(
select user_id, status
from user_scores us
where user_score_type_id=4
) as user_score
on u.id = user_score.user_id
join (select * from cities where country='US') c on c.id=first_booking.city_id
join income_data ic on ic.zip_code=first_booking.first_zip

left join (select ft.customer_zip,count(*) as num_hvc_per_zip

from (select user_id,customer_zip from (select user_id,customer_zip,session_time from appointment_requests ar where status='completed' order by user_id,session_time) a group by user_id) ft 
join (select user_id,status from user_scores where user_score_type_id=4 and status in ('AAA','AA','A')) us on us.user_id=ft.user_id

group by ft.customer_zip) num_hvc on num_hvc.customer_zip=first_booking.first_zip

where u.kind='client' and user_score.status is not null and user_score.status <> 'None' and first_MT_quality is not null and session_length is not null and (u.created_at<=now()-interval 90 day or user_score.status in ('AAA','AA','A'))
