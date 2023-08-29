#lang north

-- @revision: 147355c994f5361ae6d7df3ebbe741e3
-- @parent: 600e89eb60b8d3614d06f4eabfefce43
-- @description: Changes the pk on study_instance_links.
-- @up {
ALTER TABLE study_instance_links
  DROP CONSTRAINT study_instance_links_pk,
  ADD CONSTRAINT study_instance_links_pk PRIMARY KEY (study_instance_id_a, pseudonym_b);
-- }

-- @down {
ALTER TABLE
  DROP CONSTRAINT study_instance_links_pk,
  ADD CONSTRAINT study_instance_links_pk PRIMARY KEY (study_instance_id_a, study_instance_id_b, pseudonym_b);
-- }
