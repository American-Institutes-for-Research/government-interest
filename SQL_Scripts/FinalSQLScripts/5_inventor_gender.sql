/* Inventor gender data */
/*the tbales you wante are temp_gi_inventor_gender (which has gender and wipo sector etc)
and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors

/* the existing temp_inventor_gender is the results the italians produced, uploaded ot mysql */
create table temp_govt_associated_inventors_clean as
select patent_id, a.inventor_id, g.dumale from
(select * from patent_inventor where patent_id in (select patent_id from patent_20171226.temp_patent_level_gi)) as a left join
temp_inventor_gender g on a.inventor_id = g.id where g.id is not null;

create index patent_ix on temp_govt_associated_inventors_clean (patent_id);

create table temp_gi_inventor_gender as 
select g.patent_id, g.num_inventors, g.year, g.wipo_sector, g.wipo_field, tg.inventor_id, tg.dumale
 from patent_20171226.temp_patent_level_gi g left join temp_govt_associated_inventors_clean tg  on tg.patent_id = g.patent_id
 where tg.dumale is not null; #the not null part excludes the patents from 1226 that are not in the inventor gender analysis

create index patent_ix on temp_gi_inventor_gender(patent_id);

create table temp_gi_has_female_inv
as select patent_id, case when min(dumale) = 0 then 1 else 0 end as has_fem_inv #exploits the fact that the dumale variable is 0 for women, so min will be 0 if there are any women
from temp_gi_inventor_gender group by patent_id; 






3930295	1
3930366	1
3930405	1
3930449	1
3930537	1
3930541	1
3930550	1




show processlist;