.timer ON
BEGIN;

-- Open the original tab separate values geonames file
CREATE VIRTUAL TABLE geonamesvt USING VirtualText("cities1000.txt", "UTF-8", 0);

-- Create the real table
CREATE TABLE geonames (geonameid INT, name TEXT, asciiname TEXT, alternatenames TEXT, latitude DOUBLE, longitude DOUBLE, featureclass TEXT, featurecode TEXT, countrycode TEXT, cc2 TEXT, admin1code TEXT, admin2code TEXT, admin3code TEXT, admin4code TEXT, population LONG, elevation INT, dem INT, timezone TEXT, modificationdate TEXT);

-- import the contents of the temporary geonames table into the real table
INSERT INTO geonames (geonameid, name, asciiname, alternatenames, latitude, longitude, featureclass, featurecode, countrycode, cc2, admin1code, admin2code, admin3code, admin4code, population, elevation, dem, timezone, modificationdate)

SELECT COL001,COL002,COL003,COL004,COL005,COL006,COL007,COL008,COL009,COL010,COL011,COL012,COL013,COL014,COL015,COL016,COL017,COL018,COL019 FROM geonamesvt;

SELECT AddGeometryColumn("geonames", "Geometry", 4326, "POINT", "XY");
SELECT CreateSpatialIndex("geonames", "Geometry");
UPDATE geonames SET Geometry=MakePoint(longitude,latitude,4326);

DROP TABLE geonamesvt;
-- COMMIT that
ANALYZE geonames;

COMMIT;

VACUUM;
