/* Comparison of GI table including government assignees */

select count(*) from patent_20170808.temp_updated_gi; 185948
select count(*) from patent_20170808.patent_govintorg; 146350
-- there are about 38,000 new government interests identified from the government assignees

select count(distinct patent_id) from patent_20170808.temp_updated_gi; 142780
select count(distinct patent_id) from patent_20170808.patent_govintorg; 124379
-- there are about 18,000 new patents identified as govenrment interet patents from the government assignees

-- table of just GI patents, using all  the new GI and the 5 year citation counts
create table patent_20170808.temp_patent_level_gi as select e.patent_id, e.num_inventors, e.num_assignees, wf.sector_title as wipo_sector,
 wf.field_title as wipo_field, p.year, p.num_us_patents_cited, p.num_us_applications_cited, 
 p.num_foreign_documents_cited, p.kind, p.type, n.category_title as nber_category, n.subcategory_title as nber_subcategory, 
 cit.weighted_cites_5yrs, cit.num_citations_in_5yrs from
(select c.patent_id, c.num_inventors, d.num_assignees from 
(select a.patent_id, b.num_inventors
from (select distinct(patent_id) as patent_id from patent_20170808.temp_updated_gi) as a 
left join (select patent_id, count(inventor_id) as num_inventors from PatentsView_20170808.patent_inventor group by patent_id) as b
on a.patent_id = b.patent_id) as c
left join (select patent_id, count(assignee_id) as num_assignees from PatentsView_20170808.patent_assignee group by patent_id) as d
on c.patent_id = d.patent_id) as e
left join PatentsView_20170808.wipo w on w.patent_id = e.patent_id and w.sequence = 0
left join PatentsView_20170808.wipo_field wf on w.field_id=wf.id
left join PatentsView_20170808.patent p on p.patent_id = e.patent_id
left join  PatentsView_20170808.nber n on n.patent_id=e.patent_id
left join patent_20170808.temp_5yr_citations cit on cit.patent_id = e.patent_id;



set SQL_SAFE_UPDATES=0;
-- the following fields were generated with group bys so are null where there were no inventors, citations within 5 years etc; so replacing null with 0
update patent_20170808.temp_patent_level_gi set num_inventors = 0 where num_inventors is null;
update patent_20170808.temp_patent_level_gi set num_assignees = 0 where num_assignees is null;
update patent_20170808.temp_patent_level_gi set weighted_cites_5yrs = 0 where weighted_cites_5yrs is null;
update patent_20170808.temp_patent_level_gi set num_citations_in_5yrs= 0 where num_citations_in_5yrs is null;
create index patent_ix on patent_20170808.temp_patent_level_gi (patent_id);


-- create table with assignee type data
create table patent_20170808.temp_gi_assignee_type as 
select g.patent_id, a.`type` as assignee_type, a.organization from 
patent_20170808.temp_patent_level_gi g 
left join patent_20170808.patent_assignee pa on g.patent_id = pa.patent_id
left join patent_20171003.assignee a on pa.assignee_id = a.id; #use more recent assignee because the types are correct



select assignee_type, count(patent_id) as count from patent_20170808.temp_gi_assignee_type group by assignee_type order by count;




-- patent-level data for just non-GI patents
create table patent_20170808.temp_patent_level_nongi as 
select * from patent_20170808.temp_patent_level_all where patent_id not in
(select patent_id from patent_20170808.temp_patent_level_gi);


-- government-interest level table of just GI Patents
-- uses patent_20170808 for the DHS fix, which I obviously didn't want to do on the reporting database
create table patent_20170808.temp_gi_level_gi as select p.patent_id, g.name, g.level_one, g.level_two, g.level_three, p.in_gi from 
patent_20170808.temp_updated_gi p left join patent_20170808.government_organization g on p.organization_id = g.organization_id;


select * from patent_20170808.temp_gi_level_gi;
