/* Inventor gender data */
/*the tbales you wante are temp_gi_inventor_gender (which has gender and wipo sector etc)
and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors

/* the existing temp_inventor_gender is the results the italians produced, uploaded ot mysql */

select * from temp_inventor_gender;

drop table patent_20171226.temp_govt_associated_inventors_clean;
create table patent_20171226.temp_govt_associated_inventors_clean as
select patent_id, a.inventor_id, g.dumale from
(select * from patent_20171226.patent_inventor where patent_id in (select distinct(patent_id) from patent_20171226.government_interest)) as a left join
patent_20170808.temp_inventor_gender g on a.inventor_id = g.id where g.id is not null;

create index patent_ix on patent_20171226.temp_govt_associated_inventors_clean (patent_id);

select count(distinct (patent_id)) from patent_20171226.temp_govt_associated_inventors_clean; -- 128,565
select count(distinct(patent_id)) from patent_20171226.government_interest; -- 130,312
select  count(patent_id) from patent_20171226.temp_patent_level_gi gi where gi.patent_id NOT in
(select patent_id from patent_20171226.government_interest); -- 0

drop table patent_20171226.temp_gi_inventor_gender;
create table patent_20171226.temp_gi_inventor_gender as 
select g.patent_id, g.num_inventors, g.year, g.wipo_sector, g.wipo_field, tg.inventor_id, tg.dumale
 from patent_20171226.temp_patent_level_gi g left join patent_20171226.temp_govt_associated_inventors_clean tg  on tg.patent_id = g.patent_id
 where tg.dumale is not null; #the not null part excludes the patents from 1226 that are not in the inventor gender analysis

create index patent_ix on temp_gi_inventor_gender(patent_id);

select * from patent_20170808.temp_gi_inventor_gender;

drop table patent_20171226.temp_gi_has_female_inv;
create table patent_20171226.temp_gi_has_female_inv
as select patent_id, case when min(dumale) = 0 then 1 else 0 end as has_fem_inv #exploits the fact that the dumale variable is 0 for women, so min will be 0 if there are any women
from temp_gi_inventor_gender group by patent_id; 

select * from patent_20171226.temp_gi_has_female_inv;

select * from patent_20171226.temp_patent_level_gi where patent_id = '6399383';
select * from patent_20170808.temp_gi_inventor_gender where patent_id = '6399383';

show processlist;