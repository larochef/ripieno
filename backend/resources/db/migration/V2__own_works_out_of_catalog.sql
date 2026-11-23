-- The player's own works (scans of protected editions) are not part of the catalog: those it once brought in keep
-- their copy in the data folder and become works of the library like any import.
UPDATE work SET catalog_key = NULL WHERE origin = 'mine';
