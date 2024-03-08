#lang north

-- @revision: e2578f4ce14178e995096cca7f636662
-- @parent: 147355c994f5361ae6d7df3ebbe741e3
-- @description: Adds the dsl_archive_path column to studies.
-- @up {
ALTER TABLE studies
  ADD COLUMN dsl_archive_path TEXT DEFAULT NULL;
-- }

-- @down {
ALTER TABLE studies
  DROP COLUMN dsl_archive_path;
-- }
