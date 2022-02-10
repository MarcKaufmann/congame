#lang north

-- @revision: 372de2a1ce9524f635fdd62547a44e95
-- @parent: 1a89dca7501faa2617a5c81f793cc18d
-- @description: Alters users to add an API key column.
-- @up {
ALTER TABLE users
  ADD COLUMN api_key TEXT UNIQUE;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN api_key;
-- }
