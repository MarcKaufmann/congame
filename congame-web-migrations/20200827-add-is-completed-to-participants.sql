#lang north

-- @revision: b1c6a585e42d3fcf1963374e2aee7e68
-- @parent: 711ebec2ec93d864c91b22cf9c6c8671
-- @description: Adds the is_completed column to study_participants.
-- @up {
ALTER TABLE study_participants
  ADD COLUMN is_completed BOOLEAN NOT NULL DEFAULT FALSE;
-- }

-- @down {
ALTER TABLE study_participants
  DROP COLUMN is_completed;
-- }
