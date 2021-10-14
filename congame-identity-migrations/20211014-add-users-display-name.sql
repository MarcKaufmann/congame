#lang north

-- @revision: 8e3b9d59d4ca5fff8c2ed70e9dad5fe1
-- @parent: efed79200bf19e497ce82c46ae7c7999
-- @description: Adds role & display_name to users.
-- @up {
ALTER TABLE users
  ADD COLUMN role TEXT NOT NULL DEFAULT 'user',
  ADD COLUMN display_name TEXT NOT NULL UNIQUE;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN role,
  DROP COLUMN display_name;
-- }
