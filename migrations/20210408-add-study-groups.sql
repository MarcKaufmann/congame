#lang north

-- @revision: a5c65ee510e80802fe496f913efce93d
-- @parent: f90e848455cf1f648fe4a463f16d398b
-- @description: Adds study groups and related tables.
-- @up {
ALTER TABLE studies
  ADD COLUMN type TEXT NOT NULL DEFAULT 'singleplayer',
  ADD CONSTRAINT type_chk CHECK (type IN ('singleplayer', 'multiplayer'));
-- }

-- @up {
CREATE TABLE study_rounds(
  id SERIAL NOT NULL PRIMARY KEY,
  study_id INTEGER NOT NULL REFERENCES studies(id) ON DELETE CASCADE,
  instance_id INTEGER NOT NULL REFERENCES study_instances(id) ON DELETE CASCADE,
  grouping_fn TEXT NOT NULL,
  group_size INTEGER NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @up {
CREATE TABLE study_groups(
  id SERIAL NOT NULL PRIMARY KEY,
  round_id INTEGER NOT NULL REFERENCES study_rounds(id) ON DELETE CASCADE
);
-- }

-- @up {
CREATE TABLE study_group_participants(
  group_id INTEGER NOT NULL REFERENCES study_groups(id) ON DELETE CASCADE,
  participant_id INTEGER NOT NULL REFERENCES study_participants(id) ON DELETE CASCADE,
  participant_role TEXT NOT NULL,

  CONSTRAINT study_group_members_pk PRIMARY KEY (group_id, participant_id)
);
-- }

-- @up {
CREATE TABLE study_group_data(
  group_id INTEGER NOT NULL REFERENCES study_groups(id) ON DELETE CASCADE,
  study_stack TEXT[] NOT NULL,
  key TEXT NOT NULL,
  value BYTEA NOT NULL,
  git_sha TEXT NOT NULL,
  last_put_by INTEGER NOT NULL REFERENCES study_participants(id) ON DELETE SET NULL,
  last_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  first_put_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE study_group_data;
-- }

-- @down {
DROP TABLE study_group_participants;
-- }

-- @down {
DROP TABLE study_groups;
-- }

-- @down {
DROP TABLE study_rounds;
-- }

-- @down {
ALTER TABLE studies
  DROP CONSTRAINT type_chk,
  DROP COLUMN type;
-- }
