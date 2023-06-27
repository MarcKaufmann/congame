#lang north

-- @revision: 6710027b0fd519a763e8ef31704290a2
-- @parent: 0594b33d1de1bb94acd632abfcfe773a
-- @description: Reworks rounds and groups.
-- @up {
ALTER TABLE study_data
  DROP CONSTRAINT study_data_pkey,
  DROP COLUMN round_name,
  DROP COLUMN group_name,
  ADD COLUMN round_stack TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN group_stack TEXT[] NOT NULL DEFAULT '{}';
-- }
-- @up {
DELETE FROM study_data;
-- }
-- @up {
ALTER TABLE study_data
  ADD PRIMARY KEY (participant_id, round_stack, group_stack, study_stack, key);
-- }

-- @down {
NOGOINGBACK;
-- }
