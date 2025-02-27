#lang north

-- @revision: bfd5af5e3d6361fe16339a847b62a6e6
-- @parent: e6208922e13df8f079342a6b899e0667
-- @description: Adds the correlation_id column to users.
-- @up {
ALTER TABLE users
  ADD COLUMN correlation_id TEXT;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN correlation_id;
-- }
