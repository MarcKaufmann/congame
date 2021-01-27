#lang north

-- @revision: 42b1a5492dcd953906e5daccbe4e1ce2
-- @parent: bcdc2bbdb1dfa2df33ad2e7b11d559cc
-- @description: Alters some table.
-- @up {
create table payments(
  participant_id INTEGER REFERENCES study_participants(id),
  timestamp TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  payment_name TEXT NOT NULL,
  payment NUMERIC(6, 2),
  PRIMARY KEY (participant_id, payment_name)
);
-- }

-- @down {
drop table payments;
-- }
