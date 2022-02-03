#lang north

-- @revision: b8fa9a13f93aeea31ec8740a4449d661
-- @parent: 99a599aad5e01714ab1c9b75f2649cba
-- @description: Adds the tags tables.
-- @up {
CREATE TABLE tags(
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);
-- }

-- @up {
CREATE TABLE study_instance_tags(
  study_instance_id INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,

  CONSTRAINT study_instance_tags_pk PRIMARY KEY (study_instance_id, tag_id)
);
-- }

-- @down {
DROP TABLE study_instance_tags;
-- }

-- @down {
DROP TABLE tags;
-- }
