# Storage: design note

Status: proposal, to be reviewed before any code.

## What is kept, and where

| Kind | Examples | Where | Why |
|---|---|---|---|
| Content that never changes once made | scans, MusicXML sources, recordings | `data/blobs/`, each file named by the SHA-256 of its content | large; written once; the same content stored once; a corrected MusicXML is another blob, the old one kept while referenced |
| What is searched, queried and changes | works, movements, parts, editions, recordings and reviews, settings | `data/library.db` (SQLite) | queries across it, transactions, full-text search |
| What can be made again | renderings, the import's intermediate steps | `data/cache/` | deletable at any time |
| The scores the application provides | free editions | `catalog/`, read only: a folder per work with a `work.json` manifest | reference data, versioned; indexed into the database at start-up |

Settings now kept in the browser (tempo, sound mix, metronome, listening) move to the database: they then follow the player from one device to another.

## Technology

- SQLite through `org.xerial:sqlite-jdbc`, one connection pool of size one for writes, WAL mode. Plain JDBC, blocking, run in Ox forks.
- Flyway (`flyway-core`, which supports SQLite without another module) applies `backend/resources/db/migration/V<n>__<name>.sql` at start-up.
- Full-text search with FTS5, tokenizer `unicode61 remove_diacritics 2`: accents and case ignored.
- Backup: `VACUUM INTO` a copy of the database, plus `data/blobs/`.
- The SQL stays plain enough to move to PostgreSQL if several people ever use one server at once.

## Schema (first migration)

```sql
-- every blob the database refers to; location relative to data/ or catalog/
CREATE TABLE blob (
  hash        TEXT PRIMARY KEY,           -- sha256, hex
  media_type  TEXT NOT NULL,              -- application/vnd.recordare.musicxml+xml, application/pdf, audio/ogg…
  size        INTEGER NOT NULL,
  location    TEXT NOT NULL,              -- blobs/3f/a9c2… or catalog/vivaldi-op3-6/1-allegro.musicxml
  created_at  TEXT NOT NULL
);

CREATE TABLE work (
  id          INTEGER PRIMARY KEY,
  origin      TEXT NOT NULL CHECK (origin IN ('catalog', 'mine')),
  catalog_key TEXT UNIQUE,                -- the catalog folder, for a work from the catalog
  title       TEXT NOT NULL,
  subtitle    TEXT,
  composer    TEXT,
  arranger    TEXT,
  edition     TEXT,                       -- "Schott, 1912"
  licence     TEXT,                       -- "public domain", "CC BY-SA 3.0", "private copy"
  created_at  TEXT NOT NULL,
  last_played_at TEXT
);

-- the work's parts, as the player names them (renamable)
CREATE TABLE part (
  id          INTEGER PRIMARY KEY,
  work_id     INTEGER NOT NULL REFERENCES work ON DELETE CASCADE,
  position    INTEGER NOT NULL,
  name        TEXT NOT NULL,
  instrument  TEXT,                       -- violin, piano…: for the filters
  UNIQUE (work_id, position)
);

CREATE TABLE movement (
  id          INTEGER PRIMARY KEY,
  work_id     INTEGER NOT NULL REFERENCES work ON DELETE CASCADE,
  position    INTEGER NOT NULL,
  title       TEXT NOT NULL,              -- "Allegro", "No. 7"
  score       TEXT NOT NULL REFERENCES blob, -- the MusicXML of all its parts
  measures    INTEGER,
  seconds     REAL,                       -- at its written tempo
  UNIQUE (work_id, position)
);

-- which part of the movement's MusicXML is which part of the work
CREATE TABLE movement_part (
  movement_id INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  part_id     INTEGER NOT NULL REFERENCES part ON DELETE CASCADE,
  xml_part    TEXT NOT NULL,              -- the <score-part id> in the MusicXML
  PRIMARY KEY (movement_id, part_id)
);

-- what a work was made from: the scans and files imported
CREATE TABLE source (
  id          INTEGER PRIMARY KEY,
  work_id     INTEGER NOT NULL REFERENCES work ON DELETE CASCADE,
  blob        TEXT NOT NULL REFERENCES blob,
  name        TEXT NOT NULL,              -- the file's name as imported
  pages       INTEGER,
  imported_at TEXT NOT NULL
);

-- where the import's readings disagree, or are unsure: what the work page lists to check
CREATE TABLE import_check (
  id          INTEGER PRIMARY KEY,
  movement_id INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  part_id     INTEGER REFERENCES part ON DELETE CASCADE,
  measure     TEXT NOT NULL,
  kind        TEXT NOT NULL,              -- notes differ, measures do not pair, key…
  detail      TEXT NOT NULL,              -- JSON
  resolved    INTEGER NOT NULL DEFAULT 0
);

-- editions over a movement's score: the edits, in order, as JSON (only ever read whole)
CREATE TABLE edition (
  id          INTEGER PRIMARY KEY,
  movement_id INTEGER NOT NULL REFERENCES movement ON DELETE CASCADE,
  position    INTEGER NOT NULL,
  name        TEXT NOT NULL,
  visible     INTEGER NOT NULL DEFAULT 1,
  edits       TEXT NOT NULL,              -- JSON array of edits
  UNIQUE (movement_id, name)
);

-- a recording may run over several movements played in a row
CREATE TABLE recording (
  id          INTEGER PRIMARY KEY,
  audio       TEXT NOT NULL REFERENCES blob,
  part_id     INTEGER REFERENCES part ON DELETE SET NULL,
  recorded_at TEXT NOT NULL,
  beats_per_minute REAL,
  listening   TEXT,                       -- headphones, speakers, alone
  reference   TEXT NOT NULL               -- JSON: what was played, as sent to the review
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
  status      TEXT NOT NULL,              -- reviewing, done, failed
  notes       INTEGER, right INTEGER, wrong INTEGER, intonation INTEGER, rhythm INTEGER,
  detail      TEXT                        -- JSON: the whole review, as the analysis writes it
);

-- how the player last practised a movement
CREATE TABLE practice (
  movement_id INTEGER PRIMARY KEY REFERENCES movement ON DELETE CASCADE,
  part_id     INTEGER REFERENCES part ON DELETE SET NULL,
  beats_per_minute INTEGER,
  sound_mix   TEXT,                       -- JSON: engine and level per part
  played_at   TEXT
);

-- the player's settings that are not about one movement: metronome, tuner, listening
CREATE TABLE setting (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL                     -- JSON
);

-- search: titles, composers, part and movement names, accents and case ignored
CREATE VIRTUAL TABLE work_search USING fts5(
  title, subtitle, composer, arranger, parts, movements,
  tokenize = 'unicode61 remove_diacritics 2'
);
```

`work_search` is rewritten by the application whenever a work, its parts or its movements change (one row per work, `rowid` = `work.id`).

## Queries this answers

- The library: works by origin, the last played first; search by `work_search MATCH`; filters by `part.instrument` and number of parts.
- Continue: `practice` ordered by `played_at`, with its movement and part.
- A work: its movements, each with its recordings (`recording_movement`) and unresolved checks (`import_check`).
- Progress: the reviews of the recordings of one part over one movement, oldest first.
- Export: the movements chosen, the part chosen, the editions visible.

## Moving what exists

A one-time script:

- `scores/` becomes `catalog/`, which the server downloads into the data folder (`CatalogDownload`): works grouped from the file names (`vivaldi-op01n01m01…m04` is one sonata of four movements), a `work.json` written for each; private imports (`ghibli`, the Brahms dances) become works of origin `mine`.
- `editions/*.editions.json` become `edition` rows.
- `reviews/<id>/` become `recording` and `review` rows, their audio a blob.
- `rendered/` becomes `data/cache/rendered/`.

## Not decided here

- Several users: one database per user for now, if ever needed.
- How the import (Python) writes its results: into the database directly, or through the backend.
