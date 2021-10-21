#lang north

-- @revision: 3d2e0e81d9f7f4966994c4fdacee0324
-- @parent: 60722e0ecc30da6a30a9eab4a2552d02
-- @description: Replaces users(role) with users(roles).
-- @up {
ALTER TABLE users
  ADD COLUMN roles TEXT[] NOT NULL DEFAULT '{user}';
-- }
-- @up {
UPDATE users
  SET roles = ARRAY[role];
-- }
-- @up {
ALTER TABLE users
  DROP COLUMN role;
-- }

-- @down {
ALTER TABLE users
  ADD COLUMN role TEXT NOT NULL DEFAULT 'user';
-- }
-- @down {
UPDATE users
  SET role = ARRAY_TO_STRING(roles, ',');
-- }
-- @down {
ALTER TABLE users
  DROP COLUMN roles;
-- }
