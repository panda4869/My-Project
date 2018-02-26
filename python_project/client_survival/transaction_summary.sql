
select user_id,frequency,case when recency>timestampdiff(day,'2013-08-01',now()) then timestampdiff(day,'2013-08-01',now()) else recency end as recency ,
case when T>timestampdiff(day,'2013-08-01',now()) then timestampdiff(day,'2013-08-01',now()) else T end as T

from 
(select user_id,count(*)-1 as frequency, timestampdiff(day,min(created_at),max(created_at)) as recency, timestampdiff(day,min(created_at),now()) as T,count(*) as bookings, date(max(created_at)) as last_booking
from appointment_requests

where status in ('completed','filled','pending')
group by user_id)a

join users u on u.id=a.user_id and u.is_test=0