-- sarora@air.org modified code originally authored by skelley@air.org
-- march 5, 2018

-- table with each government interest patent and any citations within 5 years -- for each year 1 thru 5 (changed by ska)
-- patent_20180528.temp_updated_gi is the table with all the government interest and government assignee patents
drop table patent_20180528.temp_5yr_citations_by_cite_yr5;
create table patent_20180528.temp_5yr_citations_by_cite_yr5 as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, p.num_times_cited_by_us_patents from (
select * from PatentsView_20180528.uspatentcitation  where cited_patent_id in 
(select distinct(patent_id) from patent_20180528.government_interest)) as a
left join PatentsView_20180528.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20180528.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*5 and datediff(c.citing_patent_date, c.cited_patent_date) > 365*4;

-- derivative table with citation counts and weighted citation count

drop table patent_20180528.temp_5yr_citations_yr5;
create table patent_20180528.temp_5yr_citations_yr5 as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_5, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20180528.temp_5yr_citations_by_cite_yr5 group by cited_patent_id;


drop table patent_20180528.temp_5yr_citations_by_cite_yr4;
create table patent_20180528.temp_5yr_citations_by_cite_yr4 as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, p.num_times_cited_by_us_patents from (
select * from PatentsView_20180528.uspatentcitation  where cited_patent_id in 
(select distinct(patent_id) from patent_20180528.government_interest)) as a
left join PatentsView_20180528.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20180528.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*4 and datediff(c.citing_patent_date, c.cited_patent_date) > 365*3;

drop table patent_20180528.temp_5yr_citations_yr4;
create table patent_20180528.temp_5yr_citations_yr4 as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_4, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20180528.temp_5yr_citations_by_cite_yr4 group by cited_patent_id;

drop table patent_20180528.temp_5yr_citations_by_cite_yr3;
create table patent_20180528.temp_5yr_citations_by_cite_yr3 as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, p.num_times_cited_by_us_patents from (
select * from PatentsView_20180528.uspatentcitation  where cited_patent_id in 
(select distinct(patent_id) from patent_20180528.government_interest)) as a
left join PatentsView_20180528.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20180528.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*3 and datediff(c.citing_patent_date, c.cited_patent_date) > 365*2;


drop table patent_20180528.temp_5yr_citations_yr3;
create table patent_20180528.temp_5yr_citations_yr3 as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_3, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20180528.temp_5yr_citations_by_cite_yr3 group by cited_patent_id;

drop table patent_20180528.temp_5yr_citations_by_cite_yr2;
create table patent_20180528.temp_5yr_citations_by_cite_yr2 as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, p.num_times_cited_by_us_patents from (
select * from PatentsView_20180528.uspatentcitation  where cited_patent_id in 
(select distinct(patent_id) from patent_20180528.government_interest)) as a
left join PatentsView_20180528.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20180528.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*2 and datediff(c.citing_patent_date, c.cited_patent_date) > 365*1;


drop table patent_20180528.temp_5yr_citations_yr2;
create table patent_20180528.temp_5yr_citations_yr2 as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_2, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20180528.temp_5yr_citations_by_cite_yr2 group by cited_patent_id;


drop table patent_20180528.temp_5yr_citations_by_cite_yr1;
create table patent_20180528.temp_5yr_citations_by_cite_yr1 as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, p.num_times_cited_by_us_patents from (
select * from PatentsView_20180528.uspatentcitation  where cited_patent_id in 
(select distinct(patent_id) from patent_20180528.government_interest)) as a
left join PatentsView_20180528.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20180528.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*1;

drop table patent_20180528.temp_5yr_citations_yr1;
create table patent_20180528.temp_5yr_citations_yr1 as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_1, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20180528.temp_5yr_citations_by_cite_yr1 group by cited_patent_id;

select * from patent_20180528.temp_5yr_citations_yr1;

select * from patent_20180528.temp_5yr_citations_yr2;

select * from patent_20180528.temp_5yr_citations_yr3;

select * from patent_20180528.temp_5yr_citations_yr4;

select * from patent_20180528.temp_5yr_citations_yr5;


select count(distinct(patent_id)) from patent_20180528.government_interest;

select patent_id from patent_20180528.government_interest where patent_id 
not in (select distinct(patent_id) from patent_20180528.patent_inventor); 