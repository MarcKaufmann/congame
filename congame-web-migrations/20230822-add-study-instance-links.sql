#lang north

-- @revision: 600e89eb60b8d3614d06f4eabfefce43
-- @parent: 6710027b0fd519a763e8ef31704290a2
-- @description: Adds the study_instance_links table.
-- @up {
CREATE TABLE study_instance_links(
  study_instance_id_a INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  study_instance_id_b INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  pseudonym_b TEXT NOT NULL,
  relationship TEXT NOT NULL,

-- a -> b
--  instance_a: a
--  instance_b: b
--  pseudo_b: bob (meaning b)
--  relationship: source

-- b -> a
--  instance_a: b
--  instance_b: a
--  pseudo_b: alice (meaning a)
--  relationship: reporter | source

  CONSTRAINT study_instance_links_pk
    PRIMARY KEY (study_instance_id_a, study_instance_id_b, pseudonym_b),
  CONSTRAINT study_instance_links_relationship
    CHECK (relationship IN ('source', 'reporter'))
);
-- }

-- @down {
DROP TABLE study_instance_links;
-- }
