select * from patent_20170808.assignees_by_type;
create index id_ix on patent_20170808.assignees_by_type (id);

create table patent_20170808.patent_assignee_type_lookup as 
select pa.patent_id, a.thes_types from patent_20170808.patent_assignee pa left join patent_20170808.assignees_by_type a on pa.assignee_id = a.id;

select * from patent_20170808.patent_assignee_type_lookup;
create index patent_ix on patent_20170808.patent_assignee_type_lookup (patent_id);

-- first hop patent cites

create table patent_20170808.temp_first_hop_by_type as
select cited_patent_id, citing_patent_id,
case when citing_patent_id in (select patent_id from patent_20170808.patent_assignee_type_lookup where thes_types = "Corporate") THEN 1
else 0 end as private_cite
from patent_20170808.temp_5yr_citations_by_cite;


create table patent_20170808.temp_first_level_cite_by_type as select cited_patent_id, sum(private_cite) as count_direct_firm_cite
from patent_20170808.temp_first_hop group by cited_patent_id;
create index patent_ix on patent_20170808.temp_first_level_cite_by_type (cited_patent_id);

select * from patent_20170808.temp_first_level_cite_by_type;

-- only for patents that don't have a first-degree connection
-- these are all the citations to patents that don't have a first degree private firm citation

create table patent_20170808.temp_second_hop_by_type as
select a.cited_patent_id, a.citing_patent_id, 
case when u.citation_id in (select patent_id from patent_20170808.patent_assignee_type_lookup where thes_types = "Corporate") THEN 1
else 0 end as private_cite_to_citation
from ( select cited_patent_id, citing_patent_id from patent_20170808.temp_5yr_citations_by_cite
where cited_patent_id in (select cited_patent_id from patent_20170808.temp_first_level_cite_by_type where count_direct_firm_cite =0)) as a
left join patent_20170808.uspatentcitation u on a.citing_patent_id = u.patent_id; 


select * from patent_20170808.temp_second_hop_by_type;

create table patent_20170808.temp_second_level_cite_by_type as 
select cited_patent_id, sum(private_cite_to_citation)>0  as count_second_level_firm_cite
 from patent_20170808.temp_second_hop_by_type group by cited_patent_id;
 
select * from patent_20170808.temp_second_level_cite_by_type;
 
create index patent_ix on patent_20170808.temp_second_level_cite_by_type (cited_patent_id);

-- third level citaiton counts for patents without direct cites

create table patent_20170808.temp_third_hop_by_type as
select a.cited_patent_id, a.citing_patent_id, 
case when u.citation_id in (select patent_id from patent_20170808.patent_assignee_type_lookup where thes_types = "Corporate") THEN 1
else 0 end as private_cite_to_citation
from ( select cited_patent_id, citing_patent_id from patent_20170808.temp_second_hop_by_type
where cited_patent_id in (select cited_patent_id from patent_20170808.temp_second_level_cite_by_type where count_second_level_firm_cite =0)) as a
left join patent_20170808.uspatentcitation u on a.citing_patent_id = u.patent_id;


create table patent_20170808.temp_third_level_cite_by_type as 
select cited_patent_id,
sum(private_cite_to_citation) as count_third_level_firm_cite
 from patent_20170808.temp_third_hop group by cited_patent_id;
create index patent_ix on patent_20170808.temp_third_level_cite_by_type (cited_patent_id);

select * from patent_20170808.temp_first_level_cite_by_type

drop table patent_20170808.temp_private_cite_network_by_type;

create table patent_20170808.temp_private_cite_network_by_type as 
select t.patent_id, f.count_direct_firm_cite, s.count_second_level_firm_cite, th.count_third_level_firm_cite,
case when f.count_direct_firm_cite > 0 THEN 1
when s.count_second_level_firm_cite >0 then 2
when th.count_third_level_firm_cite >0 then 3
else "More than 3" end as degrees,
case when t.patent_id in (select cited_patent_id from patent_20170808.temp_5yr_citations_by_cite) then 1
else 0 end as has_citations
from  patent_20170808.temp_patent_level_gi t
left join patent_20170808.temp_first_level_cite_by_type f on t.patent_id = f.cited_patent_id
left join patent_20170808.temp_second_level_cite_by_type s on t.patent_id = s.cited_patent_id
left join patent_20170808.temp_third_level_cite_by_type th on t.patent_id = th.cited_patent_id;

select degrees, count(*) from patent_20170808.temp_private_cite_network_by_type group by degrees; 


select * from `PatentsView_20170808`.location order by num_patents desc;

select * from patent_20170808.temp_private_cite_network_by_type;




