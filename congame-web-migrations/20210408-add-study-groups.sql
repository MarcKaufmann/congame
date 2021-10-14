#lang north

-- @revision: a5c65ee510e80802fe496f913efce93d
-- @parent: f90e848455cf1f648fe4a463f16d398b
-- @description: Adds study groups.
-- @up {
ALTER TABLE study_participants
  ADD COLUMN current_round_name TEXT NOT NULL DEFAULT '',
  ADD COLUMN current_group_name TEXT NOT NULL DEFAULT '';
-- }

-- @up {
ALTER TABLE study_data
  ADD COLUMN round_name TEXT NOT NULL DEFAULT '',
  ADD COLUMN group_name TEXT NOT NULL DEFAULT '';
-- }

-- @up {
ALTER TABLE study_data DROP CONSTRAINT study_data_pkey;
-- }

-- @up {
ALTER TABLE study_data ADD PRIMARY KEY (participant_id, round_name, group_name, study_stack, key);
-- }

-- @up {
CREATE TABLE study_group_data(
  round_name TEXT NOT NULL,
  group_name TEXT NOT NULL,
  study_stack TEXT[] NOT NULL,
  key TEXT NOT NULL,
  value BYTEA NOT NULL,
  git_sha TEXT NOT NULL,
  last_put_by INTEGER NOT NULL REFERENCES study_participants(id) ON DELETE SET NULL,
  last_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  first_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT study_group_data_pk PRIMARY KEY (round_name, group_name, study_stack, key)
);
-- }

-- @down {
DROP TABLE study_group_data;
-- }

-- @down {
ALTER TABLE study_data
  DROP COLUMN round_name,
  DROP COLUMN group_name;
-- }

-- @down {
ALTER TABLE study_data DROP CONSTRAINT study_data_pkey;
-- }

-- @down {
ALTER TABLE study_data ADD PRIMARY KEY (participant_id, study_stack, key);
-- }

-- @down {
ALTER TABLE study_participants
  DROP COLUMN current_group_name,
  DROP COLUMN current_round_name;
-- }
