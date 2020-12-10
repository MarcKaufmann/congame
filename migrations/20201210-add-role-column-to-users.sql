#lang north

-- @revision: 127f466d5eb5460306aa139905a56a3b
-- @parent: d0153f4afc7038f0519f77863f0a9d2e
-- @description: Alters users to add a role column.
-- @up {
ALTER TABLE users
  ADD COLUMN role TEXT NOT NULL DEFAULT 'user';
-- }

-- @up {
UPDATE users SET role = 'admin' WHERE is_admin;
-- }

-- @up {
ALTER TABLE users
  DROP COLUMN is_admin;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN role;
-- }
