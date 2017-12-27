
alter table temp_5yr_citations_all change patent_id patent_id_join varchar(20);
create index patent_ix on temp_5yr_citations_all (patent_id_join);
create index patent_ix on temp_patent_level_nongi (patent_id);

select count(*) from temp_patent_level_nongi2 where patent_id_join is not null;



create table patent_20170808.temp_patent_level_nongi2 as 
select * from patent_20170808.temp_patent_level_nongi t left join temp_5yr_citations_all y on t.patent_id=y.patent_id_join;

set SQL_SAFE_UPDATES=0;
-- the following fields were generated with group bys so are null where there were no inventors, citations within 5 years etc; so replacing null with 0
update patent_20170808.temp_patent_level_nongi2 set num_inventors = 0 where num_inventors is null;
update patent_20170808.temp_patent_level_nongi2 set num_assignees = 0 where num_assignees is null;
update patent_20170808.temp_patent_level_nongi2 set weighted_cites_5yrs = 0 where weighted_cites_5yrs is null;
update patent_20170808.temp_patent_level_nongi2 set num_citations_in_5yrs= 0 where num_citations_in_5yrs is null;


select wipo_sector, sum(num_citations_in_5yrs)/count(num_citations_in_5yrs) from patent_20170808.temp_patent_level_nongi2 group by wipo_sector;

select wipo_sector, sum(num_citations_in_5yrs)/count(num_citations_in_5yrs) from patent_20170808.temp_patent_level_gi group by wipo_sector;

select year, sum(num_citations_in_5yrs)/count(num_citations_in_5yrs) from patent_20170808.temp_patent_level_nongi2 group by year;

select year, sum(num_citations_in_5yrs)/count(num_citations_in_5yrs) from patent_20170808.temp_patent_level_gi group by year;

Semiconductor device and manufacturing method thereof

select * from patent where id = '8275836';

-- citation analysis
select * from 