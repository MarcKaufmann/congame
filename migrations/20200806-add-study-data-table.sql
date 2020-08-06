#lang north

-- @revision: 711ebec2ec93d864c91b22cf9c6c8671
-- @parent: 388a5452241ffbddc69d973cdd01d0e8
-- @description: Adds the study_data table.
-- @up {
CREATE TABLE study_data(
  participant_id INTEGER NOT NULL REFERENCES study_participants(id) ON DELETE CASCADE,
  progress TEXT[] NOT NULL,
  key TEXT NOT NULL,
  value BYTEA NOT NULL,
  git_sha TEXT NOT NULL,
  last_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  first_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT study_data_pkey PRIMARY KEY (participant_id, progress, key)
);
-- }

-- @down {
DROP TABLE study_data;
-- }
