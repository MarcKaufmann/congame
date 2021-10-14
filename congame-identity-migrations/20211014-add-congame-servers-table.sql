#lang north

-- @revision: 63c66a91cd1a5ca35e7b517b789899db
-- @parent: 8e3b9d59d4ca5fff8c2ed70e9dad5fe1
-- @description: Adds the congame_servers table.
-- @up {
CREATE TABLE congame_servers(
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  url TEXT NOT NULL,
  key TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE congame_servers;
-- }
