/* Script to integrate government assignees and then use them to update government interest */
-- this has too mnay temp table, because my more complex queries took FOREVER to run


create table patent_20170808.temp_updatable_gi as select * from patent_20170808.patent_govintorg; # don't want to change the baseline table yet

-- patent_20170808.temp_gov_assignee comes from the hand-matched spreadsheet
create table patent_20170808.temp_updatable_assignee as select assignee_id, a.organization, g.government_org 
from PatentsView_20170808.assignee a left join patent_20170808.temp_gov_assignee g on a.organization = g.organization;
set SQL_SAFE_UPDATES=0;
update patent_20170808.temp_updatable_assignee set organization = government_org where government_org is not null; 

create table patent_20170808.temp_additional_govt as (select p.patent_id, a.assignee_id, a.organization from PatentsView_20170808.patent_assignee p right join
(select assignee_id, organization from patent_20170808.temp_updatable_assignee where organization
 in (select name from patent_20170808.government_organization)) as a on p.assignee_id = a.assignee_id);

-- needed to make the final table create statements not run incredibly slowly
create index patent_ix on patent_20170808.temp_updatable_gi (patent_id); 
create index patent_ix on patent_20170808.temp_additional_govt (patent_id);

replace into patent_20170808.temp_updatable_gi (patent_id, organization_id) select patent_id, organization_id
 from patent_20170808.temp_additional_govt t
 left join patent_20170808.government_organization g on t.organization = g.name;

create table patent_20170808.temp_updated_gi 
as select t.patent_id, t.organization_id, p.organization_id as in_gi
from patent_20170808.temp_updatable_gi t left join patent_20170808.patent_govintorg p 
on t.patent_id = p.patent_id and t.organization_id = p.organization_id;

update patent_20170808.temp_updated_gi set in_gi = 1 where in_gi is not null;
update patent_20170808.temp_updated_gi set in_gi = 0 where in_gi is null;
create index patent_ix on patent_20170808.temp_updated_gi (patent_id);

select * from patent_20170808.temp_updated_gi;
select * from patent_20170808.patent_govintorg; 