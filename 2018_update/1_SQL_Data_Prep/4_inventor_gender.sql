patent_20180528.patent_inventor
patent_20180528.temp_patent_level_gi

CREATE TABLE patent_20180528.temp_inventor_gender_new LIKE patent_20170808.temp_inventor_gender; 
INSERT patent_20180528.temp_inventor_gender_new SELECT * FROM patent_20170808.temp_inventor_gender;
ALTER TABLE patent_20180528.temp_inventor_gender_new convert to CHARACTER SET utf8 COLLATE utf8_unicode_ci;
/* Inventor gender data */
/*the tbales you wante are temp_gi_inventor_gender (which has gender and wipo sector etc)
and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors

/* the existing temp_inventor_gender is the results the italians produced, uploaded ot mysql */
create table patent_20180528.temp_govt_associated_inventors_clean as
	select patent_id, a.inventor_id, g.dumale 
		from
			(select * 
				from patent_20180528.patent_inventor 
				where patent_id in 
					(select patent_id from patent_20180528.temp_patent_level_gi)) as a 
										left join patent_20180528.temp_inventor_gender_new g 
											on a.inventor_id = g.id 
                                            where g.id is not null; #keep using the temp_inventor_gender from patent_20180528

create index patent_ix on patent_20180528.temp_govt_associated_inventors_clean (patent_id);

create table patent_20180528.temp_gi_inventor_gender as 
	select g.patent_id, g.num_inventors, g.year, g.wipo_sector, g.wipo_field, tg.inventor_id, tg.dumale
		from patent_20180528.temp_patent_level_gi g 
			left join patent_20180528.temp_govt_associated_inventors_clean tg  
				on tg.patent_id = g.patent_id
				where tg.dumale is not null; #the not null part excludes the patents from 1226 that are not in the inventor gender analysis

create index patent_ix on patent_20180528.temp_gi_inventor_gender(patent_id);

create table patent_20180528.temp_gi_has_female_inv as 
	select patent_id, case when min(dumale) = 0 then 1 else 0 endpatent_govintorg as has_fem_inv #exploits the fact that the dumale variable is 0 for women, so min will be 0 if there are any women
		from patent_20180528.temp_gi_inventor_gender 
        group by patent_id; 



