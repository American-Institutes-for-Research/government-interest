/* Export helper tables */

-- becomes in.assignees.all
select patent_id, a.* from patent_20171226.patent_assignee pa, patent_20171226.assignee a
where pa.assignee_id = a.id;

select * from patent p where p.id = '4671614';
select * from patent_govintorg pgio where pgio.patent_id = '4671614';

select * from patent_20170808.patent_assignee pa where pa.patent_id = '4671614';

select * from temp_gi_assignee_type;

/* Ok I found the tables
gi_inventor_gender has the patents joined with the Italian inventors gender look up
gi_has_fem_inv has an indicator for each patent for whether that patent has a female inventor */