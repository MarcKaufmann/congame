#lang north

-- @revision: d0153f4afc7038f0519f77863f0a9d2e
-- @parent: 4b823e9c3a3c15057acd77d39a6fe1af
-- @description: Alters the studies table to drop the racket_module column.
-- @up {
ALTER TABLE studies DROP COLUMN racket_module;
-- }

-- @down {
ALTER TABLE studies ADD COLUMN racket_module TEXT;
-- }
