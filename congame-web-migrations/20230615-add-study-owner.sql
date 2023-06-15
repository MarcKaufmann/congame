#lang north

-- @revision: 0594b33d1de1bb94acd632abfcfe773a
-- @parent: 5fcee61162180b96881c46f7c00cda8d
-- @description: Alters studies to add owner_id.
-- @up {
ALTER TABLE studies
  ADD COLUMN owner_id INTEGER REFERENCES users(id);
-- }
-- @up {
UPDATE studies SET owner_id = (SELECT id FROM users WHERE roles @> '{admin}' LIMIT 1);
-- }
-- @up {
ALTER TABLE studies
  ALTER COLUMN owner_id SET NOT NULL;
-- }

-- @down {
ALTER TABLE studies
  DROP COLUMN owner_id;
-- }
