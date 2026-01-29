#lang north

-- @revision: 65d731fa82074e6046e6d1c1703d03ec
-- @parent: bfd5af5e3d6361fe16339a847b62a6e6
-- @description: Adds the push_endpoint and push_keys columns.
-- @up {
ALTER TABLE users
  ADD COLUMN push_endpoint TEXT,
  ADD COLUMN push_keys JSONB;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN push_endpoint,
  DROP COLUMN push_keys;
-- }
