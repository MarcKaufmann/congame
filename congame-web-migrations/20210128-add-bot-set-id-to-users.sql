#lang north

-- @revision: f90e848455cf1f648fe4a463f16d398b
-- @parent: 42b1a5492dcd953906e5daccbe4e1ce2
-- @description: Alters some table.
-- @up {
ALTER TABLE users
  ADD COLUMN bot_set_id INTEGER REFERENCES bot_sets(id) ON DELETE CASCADE;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN bot_set_id;
-- }
