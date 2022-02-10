#lang north

-- @revision: 96cbd7cabff4649f30032b2b171a7a7f
-- @parent: b8fa9a13f93aeea31ec8740a4449d661
-- @description: Adds a unique constraint on study_instances(slug).
-- @up {
CREATE UNIQUE INDEX study_instances_slug_uniq_idx
  ON study_instances(slug);
-- }

-- @down {
DROP INDEX study_instances_slug_uniq_idx;
-- }
