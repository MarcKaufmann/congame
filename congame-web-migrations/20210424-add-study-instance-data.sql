#lang north

-- @revision: daa8bb0e608841a4c4cfd518213fb81c
-- @parent: a5c65ee510e80802fe496f913efce93d
-- @description: Adds the study_instance_data table.
-- @up {
CREATE TABLE study_instance_data(
  instance_id INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  study_stack text[] NOT NULL,
  key text NOT NULL,
  value bytea NOT NULL,
  git_sha TEXT NOT NULL,
  last_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  first_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT study_instance_data_pkey PRIMARY KEY (instance_id, study_stack, key)
);
-- }

-- @down {
DROP TABLE study_instance_data;
-- }
