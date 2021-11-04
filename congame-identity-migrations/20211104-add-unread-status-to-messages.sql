#lang north

-- @revision: 1a89dca7501faa2617a5c81f793cc18d
-- @parent: 85f19cf355e5e91f53d5d76ba5f31d84
-- @description: Alters messages to add the is_unread flag.
-- @up {
ALTER TABLE messages
  ADD COLUMN is_unread BOOLEAN NOT NULL DEFAULT TRUE;
-- }

-- @down {
ALTER TABLE messages
  DROP COLUMN is_unread;
-- }
