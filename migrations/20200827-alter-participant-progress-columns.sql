#lang north

-- @revision: 336cc949512519c62d3f4706d023cffd
-- @parent: 711ebec2ec93d864c91b22cf9c6c8671
-- @description: Alters some table.
-- @up {
ALTER TABLE study_participants RENAME COLUMN progress TO current_study_stack;
-- }

-- @up {
ALTER TABLE study_participants ADD COLUMN current_step_id TEXT;
-- }

-- @down {
ALTER TABLE study_participants RENAME COLUMN current_study_stack TO progress;
-- }

-- @down {
ALTER TABLE study_participants DROP COLUMN current_step_id;
-- }
