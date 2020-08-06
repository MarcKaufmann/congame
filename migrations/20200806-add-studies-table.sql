#lang north

-- @revision: a9e8c34d310fcd7ff26e16b343928fdb
-- @parent: efed79200bf19e497ce82c46ae7c7999
-- @description: Adds the studies table.
-- @up {
CREATE TABLE studies(
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  slug TEXT UNIQUE NOT NULL,
  racket_module TEXT NOT NULL,
  racket_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE studies;
-- }
