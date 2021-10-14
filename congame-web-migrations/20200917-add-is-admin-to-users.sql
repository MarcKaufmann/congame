#lang north

-- @revision: 62d7c8e971cd2345697e6f9f692a79af
-- @parent: b1c6a585e42d3fcf1963374e2aee7e68
-- @description: Adds the is_admin column to users.
-- @up {
ALTER TABLE users
  ADD COLUMN is_admin BOOLEAN NOT NULL DEFAULT FALSE;
-- }

-- @down {
ALTER TABLE users
  DROP COLUMN is_admin;
-- }
