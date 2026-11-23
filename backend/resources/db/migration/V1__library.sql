-- The library: works, their parts and movements, where they come from, and what the player keeps of them.
-- docs/storage.md says what each table is for.

-- every file the database refers to, by the SHA-256 of its content; its location is relative to the
-- data folder (blobs/…) or to the catalog's (catalog:…)
CREATE TABLE blob (
  hash        TEXT PRIMARY KEY,
  media_type  TEXT NOT NULL,
  size        INTEGER NOT NULL,
  location    TEXT NOT NULL,
  created_at  TEXT NOT NULL
);

CREATE TABLE work (
  id             INTEGER PRIMARY KEY,
  origin         TEXT NOT NULL CHECK (origin IN ('catalog', 'mine')),
  catalog_key    TEXT UNIQUE,
  title          TEXT NOT NULL,
  subtitle       TEXT,
  composer       TEXT,
  arranger       TEXT,
  collection     TEXT,
  edition        TEXT,
  licence        TEXT,
  created_at     TEXT NOT NULL,
  last_played_at TEXT
);

CREATE INDEX work_collection ON work (collection);

CREATE TABLE part (
  id          INTEGER PRIMARY KEY,
  work_id     INTEGER NOT NULL REFERENCES work ON DELETE CASCADE,
  position    INTEGER NOT NULL,
  name        TEXT NOT NULL,
  instrument  TEXT,
  UNIQUE (work_id, position)
);

CREATE TABLE movement (
  id          INTEGER PRIMARY KEY,
  work_id     INTEGER NOT NULL REFERENCES work ON DELETE CASCADE,
  position    INTEGER NOT NULL,
  title       TEXT NOT NULL,
  score       TEXT NOT NULL REFERENCES blob,
  measures    INTEGER,
  seconds     REAL,
  -- the path it had in the folder of scores, which earlier editions, recordings and settings name it by
  legacy_path TEXT,
  UNIQUE (work_id, position)
);

-- which part of the movement's MusicXML is which part of the work
CREATE TABLE movement_part (
  movement_id INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  part_id     INTEGER NOT NULL REFERENCES part ON DELETE CASCADE,
  xml_part    TEXT NOT NULL,
  PRIMARY KEY (movement_id, part_id)
);

CREATE TABLE source (
  id          INTEGER PRIMARY KEY,
  work_id     INTEGER NOT NULL REFERENCES work ON DELETE CASCADE,
  blob        TEXT NOT NULL REFERENCES blob,
  name        TEXT NOT NULL,
  pages       INTEGER,
  imported_at TEXT NOT NULL
);

CREATE TABLE import_check (
  id          INTEGER PRIMARY KEY,
  movement_id INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  part_id     INTEGER REFERENCES part ON DELETE CASCADE,
  measure     TEXT NOT NULL,
  kind        TEXT NOT NULL,
  detail      TEXT NOT NULL,
  resolved    INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE edition (
  id          INTEGER PRIMARY KEY,
  movement_id INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  position    INTEGER NOT NULL,
  name        TEXT NOT NULL,
  visible     INTEGER NOT NULL DEFAULT 1,
  edits       TEXT NOT NULL,
  UNIQUE (movement_id, name)
);

CREATE TABLE recording (
  id          INTEGER PRIMARY KEY,
  audio       TEXT NOT NULL REFERENCES blob,
  part_id     INTEGER REFERENCES part ON DELETE SET NULL,
  recorded_at TEXT NOT NULL,
  beats_per_minute REAL,
  listening   TEXT,
  reference   TEXT NOT NULL
);

CREATE TABLE recording_movement (
  recording_id INTEGER NOT NULL REFERENCES recording ON DELETE CASCADE,
  movement_id  INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  from_measure TEXT,
  to_measure   TEXT,
  PRIMARY KEY (recording_id, movement_id)
);

CREATE TABLE review (
  recording_id INTEGER PRIMARY KEY REFERENCES recording ON DELETE CASCADE,
  status      TEXT NOT NULL,
  notes       INTEGER,
  heard_right INTEGER,
  wrong       INTEGER,
  intonation  INTEGER,
  rhythm      INTEGER,
  detail      TEXT
);

CREATE TABLE practice (
  movement_id INTEGER PRIMARY KEY REFERENCES movement ON DELETE CASCADE,
  part_id     INTEGER REFERENCES part ON DELETE SET NULL,
  beats_per_minute INTEGER,
  sound_mix   TEXT,
  played_at   TEXT
);

CREATE TABLE setting (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
);

-- titles, composers, collections, part and movement names; accents and case ignored
CREATE VIRTUAL TABLE work_search USING fts5(
  title, subtitle, composer, arranger, collection, parts, movements,
  tokenize = 'unicode61 remove_diacritics 2'
);
