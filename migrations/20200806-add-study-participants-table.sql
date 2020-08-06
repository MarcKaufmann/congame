#lang north

-- @revision: 388a5452241ffbddc69d973cdd01d0e8
-- @parent: 0d833932c4c2324f299fbdf8e5a40df7
-- @description: Adds the study_participants table.
-- @up {
CREATE TABLE study_participants(
  id SERIAL PRIMARY KEY,
  user_id INTEGER REFERENCES users(id) ON DELETE SET NULL,
  instance_id INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  progress TEXT[] NOT NULL DEFAULT '{}',
  enrolled_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT study_participants_uniq UNIQUE (user_id, instance_id)
);
-- }

-- @down {
DROP TABLE study_participants;
-- }
