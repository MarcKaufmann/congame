#lang north

-- @revision: 85f19cf355e5e91f53d5d76ba5f31d84
-- @parent: 63c66a91cd1a5ca35e7b517b789899db
-- @description: Adds the messages table.
-- @up {
CREATE TABLE messages(
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  sender TEXT NOT NULL,
  subject TEXT NOT NULL,
  data BYTEA NOT NULL,
  received_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE messages;
-- }
