/*For each patent, create the 5 year citation counts and weighted citation counts
Uses only the government relationships in the government interest table (not government assignees)
Does not require any other new tables to be pre-generated
 */
 
-- --------------------------------------------------------------------------------
-- All Patents
-- --------------------------------------------------------------------------------
 
 -- table with all patents and any citations within 5 years
-- table has the id and date of both cited and citing patent ids
 create table patent_20170808.temp_5yr_citations_by_cite_all as 
select * from (
select b.cited_patent_id, p2.date as cited_patent_date, b.citing_patent_id,b.citing_patent_date, b.num_times_cited_by_us_patents from (
select a.cited_patent_id, a.citing_patent_id, p.date as citing_patent_date, 
p.num_times_cited_by_us_patents from (
select * from PatentsView_20170808.uspatentcitation) as a
left join PatentsView_20170808.patent p on a.citing_patent_id = p.patent_id) as b
left join PatentsView_20170808.patent p2 on b.cited_patent_id = p2.patent_id) as c
where datediff(c.citing_patent_date, c.cited_patent_date) <=365*5;

create index patent_ix on patent_20170808.temp_5yr_citations_by_cite_all(cited_patent_id);

-- derivative table with 5 year citation counts and weighted citation count
create table patent_20170808.temp_5yr_citations_all as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_in_5yrs, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20170808.temp_5yr_citations_by_cite_all group by cited_patent_id;

create index patent_ix on patent_20170808.temp_5yr_citations_all(patent_id);

-- --------------------------------------------------------------------------------
-- Government Interest Patents
-- --------------------------------------------------------------------------------

-- create citation tables for only government interest patents
-- table has the id and date of both cited and citing patent ids
create table patent_20170808.temp_5yr_citations_by_cite as 
select * from 
patent_20170808.temp_5yr_citations_by_cite_all where cited_patent_id in 
(select distinct(patent_id) from patent_20170808.patent_govintorg);


-- government interest derivative table with 5 year citation counts and weighted citation count
create table patent_20170808.temp_5yr_citations as
select cited_patent_id as patent_id, count(citing_patent_id) as num_citations_in_5yrs, sum(num_times_cited_by_us_patents) as weighted_cites_5yrs from 
patent_20170808.temp_5yr_citations_by_cite group by cited_patent_id;

create index patent_ix on patent_20170808.temp_5yr_citations(patent_id);
