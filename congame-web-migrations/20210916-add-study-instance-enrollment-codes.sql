#lang north

-- @revision: 60722e0ecc30da6a30a9eab4a2552d02
-- @parent: 51956d4f927de6a3d9aae9d331f7f9d0
-- @description: Adds enrollment_code to study_instances.
-- @up {
ALTER TABLE study_instances
  ADD COLUMN enrollment_code TEXT NOT NULL DEFAULT '';
-- }

-- @down {
ALTER TABLE study_instances
  DROP COLUMN enrollment_code;
-- }
