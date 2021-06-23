#lang north

-- @revision: 51956d4f927de6a3d9aae9d331f7f9d0
-- @parent: daa8bb0e608841a4c4cfd518213fb81c
-- @description: Drops the is_completed column off of study_participants.
-- @up {
ALTER TABLE study_participants
  DROP COLUMN is_completed;
-- }

-- @down {
ALTER TABLE study_participants
  ADD COLUMN is_completed BOOLEAN NOT NULL DEFAULT FALSE;
-- }
