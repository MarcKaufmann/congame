#lang north

-- @revision: 9854b836548ac769b3465e56769641fb
-- @parent: 3d2e0e81d9f7f4966994c4fdacee0324
-- @description: Adds the parent_id column to users.
-- @up {
ALTER TABLE users
  ADD COLUMN parent_id INTEGER REFERENCES users(id) ON DELETE CASCADE;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN parent_id;
-- }
