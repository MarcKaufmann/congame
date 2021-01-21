#lang north

-- @revision: bcdc2bbdb1dfa2df33ad2e7b11d559cc
-- @parent: f0c629feda9b23e3a47584baec2624f7
-- @description: Adds the bot_sets table.
-- @up {
CREATE TABLE bot_sets(
  id SERIAL PRIMARY KEY,
  study_id INTEGER NOT NULL REFERENCES studies(id) ON DELETE CASCADE,
  study_instance_id INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  bot_id TEXT NOT NULL,
  model_id TEXT NOT NULL,
  bot_count INTEGER NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE bot_sets;
-- }
