/*Create the main Patent Level and Government Interest Level tables
These tables have a lot of details around the patents including information from the database
and  also from the citition count tables generated in the previous steps
 */
 
-- --------------------------------------------------------------------------------
-- All Patents
-- --------------------------------------------------------------------------------
CREATE TABLE patent_20180528.wipo_new LIKE PatentsView_20180528.wipo; 
INSERT patent_20180528.wipo_new SELECT * FROM PatentsView_20180528.wipo;

CREATE TABLE patent_20180528.wipo_field_new LIKE PatentsView_20180528.wipo_field; 
INSERT patent_20180528.wipo_field_new SELECT * FROM PatentsView_20180528.wipo_field;

CREATE TABLE patent_20180528.temp_5yr_citations_new LIKE patent_20180528.temp_5yr_citations; 
INSERT patent_20180528.temp_5yr_citations_new SELECT * FROM patent_20180528.temp_5yr_citations;

ALTER TABLE patent_20180528.wipo_new convert to CHARACTER SET utf8 COLLATE utf8_unicode_ci;
ALTER TABLE patent_20180528.wipo_field_new convert to CHARACTER SET utf8 COLLATE utf8_unicode_ci;
ALTER TABLE patent_20180528.temp_5yr_citations_new convert to CHARACTER SET utf8 COLLATE utf8_unicode_ci;
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
create table patent_20180528.temp_patent_level_all as 
select e.patent_id, e.num_inventors, e.num_assignees, wf.sector_title as wipo_sector,
	   wf.field_title as wipo_field, p.year, p.num_us_patents_cited, p.num_us_applications_cited, 
	   p.num_foreign_documents_cited, p.kind, p.type, n.category_title as nber_category, n.subcategory_title as nber_subcategory, 
	   cit.weighted_cites_5yrs, cit.num_citations_in_5yrs 
	from
		(select c.patent_id, c.num_inventors, d.num_assignees 
			from 
				(select patent_id, count(inventor_id) as num_inventors 
					from PatentsView_20180528.patent_inventor 
                    group by patent_id) as c
			left join (select patent_id, count(assignee_id) as num_assignees 
						from PatentsView_20180528.patent_assignee group by patent_id) as d
				on c.patent_id = d.patent_id) as e
			left join patent_20180528.wipo_new w 
				on w.patent_id = e.patent_id and w.sequence = 0
			left join patent_20180528.wipo_field_new wf 
				on w.field_id=wf.id
			left join PatentsView_20180528.patent p 
				on p.patent_id = e.patent_id
			left join  PatentsView_20180528.nber n 
				on n.patent_id=e.patent_id
			left join patent_20180528.temp_5yr_citations_new cit 
            on cit.patent_id = e.patent_id;

set SQL_SAFE_UPDATES=0;
update patent_20180528.temp_patent_level_all set num_inventors = 0 where num_inventors is null;
update patent_20180528.temp_patent_level_all set num_assignees = 0 where num_assignees is null;
update patent_20180528.temp_patent_level_all set weighted_cites_5yrs = 0 where weighted_cites_5yrs is null;
update patent_20180528.temp_patent_level_all set num_citations_in_5yrs= 0 where num_citations_in_5yrs is null;
create index patent_ix on patent_20180528.temp_patent_level_all(patent_id);


-- --------------------------------------------------------------------------------
-- Government Interest Patents
-- --------------------------------------------------------------------------------

-- table of just GI patents
-- each row is a patent and each patent appears only once
create table patent_20180528.temp_patent_level_gi as 
	select * 
		from patent_20180528.temp_patent_level_all 
        where patent_id in 
			(select distinct(patent_id) as patent_id 
				from patent_20180528.patent_govintorg);

create index patent_ix on patent_20180528.temp_patent_level_gi (patent_id);

-- government-interest level table of just GI Patents


create table patent_20180528.temp_gi_level_gi as 
select p.patent_id, g.name, g.level_one, g.level_two, g.level_three, p.organization_id
	from patent_20180528.patent_govintorg p 
		left join patent_20180528.government_organization g 
			on p.organization_id = g.organization_id;


-- --------------------------------------------------------------------------------
-- Non Government Interest Patents
-- --------------------------------------------------------------------------------

-- patent-level data for just non-GI patents
create table patent_20180528.temp_patent_level_nongi as 
select * 
	from patent_20180528.temp_patent_level_all 
    where patent_id not in
		(select patent_id 
			from patent_20180528.patent_govintorg);




