#lang north

-- @revision: fa16a3ecaddad12055735a1f806f3ffe
-- @parent: 96cbd7cabff4649f30032b2b171a7a7f
-- @description: Adds the auth_tokens table.
-- @up {
CREATE TABLE auth_tokens(
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  token TEXT UNIQUE NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP + INTERVAL '1 HOUR'
);
-- }

-- @down {
DROP TABLE auth_tokens;
-- }
