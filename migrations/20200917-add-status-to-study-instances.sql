#lang north

-- @revision: 4b823e9c3a3c15057acd77d39a6fe1af
-- @parent: 62d7c8e971cd2345697e6f9f692a79af
-- @description: Adds the status column to study_instances.
-- @up {
ALTER TABLE study_instances
  ADD COLUMN status TEXT NOT NULL DEFAULT 'active';
-- }

-- @up {
ALTER TABLE study_instances
  ADD CONSTRAINT study_instances_status_chk CHECK (status IN ('active', 'inactive', 'archived'));
-- }

-- @down {
ALTER TABLE study_instances
  DROP CONSTRAINT study_instances_status_chk;
-- }

-- @down {
ALTER TABLE study_instances
  DROP COLUMN status;
-- }
