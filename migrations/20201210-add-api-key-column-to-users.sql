#lang north

-- @revision: f0c629feda9b23e3a47584baec2624f7
-- @parent: 127f466d5eb5460306aa139905a56a3b
-- @description: Adds the api_key column to users.
-- @up {
ALTER TABLE users
  ADD COLUMN api_key TEXT;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN api_key;
-- }
