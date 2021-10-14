#lang north

-- @revision: 0d833932c4c2324f299fbdf8e5a40df7
-- @parent: a9e8c34d310fcd7ff26e16b343928fdb
-- @description: Adds the study_instances table.
-- @up {
CREATE TABLE study_instances(
  id SERIAL PRIMARY KEY,
  study_id INTEGER NOT NULL REFERENCES studies(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  slug TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE study_instances;
-- }
