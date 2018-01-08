-- temp tables for 
create table patent_20170808.temp_first_hop as
select cited_patent_id, citing_patent_id,
case when citing_patent_id not in (select patent_id from patent_20170808.temp_updated_gi) THEN 1
else 0 end as private_cite
from patent_20170808.temp_5yr_citations_by_cite;

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

select * from patent_20170808.temp_second_hop;

select * from patent_20171003.location where id = 'f85j13mrscsz';
select * from patent_20171003.location where id = 'a';

select * from patent_20170808.temp_second_level_cite;








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

7910058	8802444	7897402	in_gi
7910058	8802444	7700040	in_gi
7910058	8802444	7449579	in_gi
8076467	8450468	5270163	in_gi
8076467	8450468	8076467	in_gi
8076467	8450468	7329742	in_gi

select * from patent_20170808.temp_third_hop_debug;

select * from patent_20170808.temp_updated_gi where patent_id = '7329742';
select * from patent_20170808.uspatentcitation where patent_id = '8076467';
5270163
7329742

select cited_patent_id, citing_patent_id, citation_id, case when citation_id in (select patent_id from patent_20170808.temp_updated_gi) then "in_gi" else "not_in_gi" end as cite_in_gi
 from  patent_20170808.temp_third_hop_debug;

select count(*), private_cite_to_citation from patent_20170808.temp_third_hop group by private_cite_to_citation;

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



select degrees, count(*) from patent_20170808.temp_private_cite_network group by degrees;

from patent_20170808.temp_private_cite_network;

select second_level_private_cite, count(cited_patent_id) from patent_20170808.temp_second_level_cite group by second_level_private_cite;

select count(distinct cited_patent_id), count(distinct citing_patent_id) from patent_20170808.temp_second_hop;


select * from patent_20170808.uspatentcitation;

select * from PatentsView_20170808.uspatentcitation;

select * from patent_20170808.temp_5yr_network_dev;