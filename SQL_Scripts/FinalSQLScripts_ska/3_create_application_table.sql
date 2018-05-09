-- create table with assignee type data
-- this uses the new (Mar 9th) thesaurus
create table patent_20171226.temp_gi_assignee_type as 
select g.patent_id, a.`type` as assignee_type, a.organization from 
patent_20171226.temp_patent_level_gi g 
left join patent_20171226.patent_assignee pa on g.patent_id = pa.patent_id
left join patent_20171003.assignee a on pa.assignee_id = a.id; #use more recent assignee because the types are correct

select assignee_type, count(patent_id) as count from patent_20171226.temp_gi_assignee_type group by assignee_type order by count;

select * from patent_20171226.temp_gi_assignee_type;
