/*Create 5 year citation counts and weighted citation counts */

-- table with each government interest patent and any citations within 5 years
-- patent_20170808.temp_updated_gi is the table with all the government interest and government assignee patents
create table patent_20170808.temp_5yr_citations_by_cite as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, p.num_times_cited_by_us_patents from (
select * from PatentsView_20170808.uspatentcitation  where cited_patent_id in 
(select patent_id from patent_20170808.temp_updated_gi)) as a
left join PatentsView_20170808.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20170808.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*5;

-- derivative table with citation counts and weighted citation count
create table patent_20170808.temp_5yr_citations as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_in_5yrs, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20170808.temp_5yr_citations_by_cite group by cited_patent_id;
