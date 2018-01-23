-- temp tables for 
create table patent_20170808.temp_first_hop as
select cited_patent_id, citing_patent_id,
case when citing_patent_id not in (select patent_id from patent_20170808.temp_updated_gi) THEN 1
else 0 end as private_cite
from patent_20170808.temp_5yr_citations_by_cite;

select * from patent_20170808.temp_first_hop;

create table patent_20170808.temp_first_level_cite as select cited_patent_id, 
case when sum(private_cite)>0 then 1
else 0 end as direct_private_cite
from patent_20170808.temp_first_hop group by cited_patent_id;
create index patent_ix on patent_20170808.temp_first_level_cite (cited_patent_id);


-- only for patents that don't have a first-degree connection
-- these are all the citations to patents that don't have a first degree private firm citation

create table patent_20170808.temp_second_hop as
select a.cited_patent_id, a.citing_patent_id, 
case when u.citation_id  not in (select patent_id from patent_20170808.temp_updated_gi) THEN 1
else 0 end as private_cite_to_citation
from ( select cited_patent_id, citing_patent_id from patent_20170808.temp_5yr_citations_by_cite
where cited_patent_id in (select cited_patent_id from patent_20170808.temp_first_level_cite where direct_private_cite =0)) as a
left join patent_20170808.uspatentcitation u on a.citing_patent_id = u.patent_id; 

create table patent_20170808.temp_second_level_cite as 
select cited_patent_id,
case when sum(private_cite_to_citation)>0 then 1
else 0 end as second_level_private_cite
 from patent_20170808.temp_second_hop group by cited_patent_id;
create index patent_ix on patent_20170808.temp_second_level_cite (cited_patent_id);


-- get third degree citations
drop table patent_20170808.temp_third_hop;

create table patent_20170808.temp_third_hop as
select a.cited_patent_id, a.citing_patent_id, 
case when u.citation_id not in (select patent_id from patent_20170808.temp_updated_gi) THEN 1
else 0 end as private_cite_to_citation
from ( select cited_patent_id, citing_patent_id from patent_20170808.temp_second_hop
where cited_patent_id in (select cited_patent_id from patent_20170808.temp_second_level_cite where second_level_private_cite =0)) as a
left join patent_20170808.uspatentcitation u on a.citing_patent_id = u.patent_id;


create table patent_20170808.temp_third_hop_debug as
select a.cited_patent_id, a.citing_patent_id, u.citation_id
from ( select cited_patent_id, citing_patent_id from patent_20170808.temp_second_hop
where cited_patent_id in (select cited_patent_id from patent_20170808.temp_second_level_cite where second_level_private_cite =0)) as a
left join patent_20170808.uspatentcitation u on a.citing_patent_id = u.patent_id;


create table patent_20170808.temp_third_level_cite as 
select cited_patent_id,
case when sum(private_cite_to_citation)>0 then 1
else 0 end as third_level_private_cite
 from patent_20170808.temp_third_hop group by cited_patent_id;
create index patent_ix on patent_20170808.temp_third_level_cite (cited_patent_id);


-- citation network count
drop table patent_20170808.temp_private_cite_network; 

create table patent_20170808.temp_private_cite_network as 
select t.patent_id, f.direct_private_cite, s.second_level_private_cite,
case when f.direct_private_cite = 1 THEN 1
when s.second_level_private_cite = 1 then 2
else 3 end as degrees,
case when f.direct_private_cite is null then 0
else 1 end as has_citations
from  patent_20170808.temp_patent_level_gi t
left join patent_20170808.temp_first_level_cite f on t.patent_id = f.cited_patent_id
left join patent_20170808.temp_second_level_cite s on t.patent_id = s.cited_patent_id;

select * from patent_20170808.temp_private_cite_network; 
