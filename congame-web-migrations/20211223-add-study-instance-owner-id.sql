#lang north

-- @revision: 99a599aad5e01714ab1c9b75f2649cba
-- @parent: 31803f8d8703bf9e1725a711a8104c4b
-- @description: Adds owner_id to study_instances.
-- @up {
ALTER TABLE study_instances
  ADD COLUMN owner_id INTEGER REFERENCES users(id) ON DELETE CASCADE;
-- }

-- @up {
UPDATE
  study_instances
SET
  owner_id = (SELECT id FROM users WHERE roles @> '{admin}' ORDER BY id ASC LIMIT 1)
-- }

-- @up {
ALTER TABLE study_instances ALTER COLUMN owner_id SET NOT NULL;
-- }

-- @down {
ALTER TABLE study_instances
  DROP COLUMN owner_id;
-- }
