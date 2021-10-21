#lang north

-- @revision: 31803f8d8703bf9e1725a711a8104c4b
-- @parent: 9854b836548ac769b3465e56769641fb
-- @description: Adds identity service columns to users.
-- @up {
ALTER TABLE users
  ADD COLUMN identity_service_url TEXT,
  ADD COLUMN identity_service_key TEXT;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN identity_service_url,
  DROP COLUMN identity_service_key;
-- }
