-- script to create test schema for epgsql unit tests --
--
-- this script should be run as the same user the tests will be run as,
-- so that the test for connecting as the 'current user' succeeds
--
-- the following lines must be added to pg_hba.conf for all tests to
-- succeed:
--
-- host    epgsql_test_db1 epgsql_test             127.0.0.1/32    trust
-- host    epgsql_test_db1 epgsql_test_md5         127.0.0.1/32    md5
-- host    epgsql_test_db1 epgsql_test_cleartext   127.0.0.1/32    password
-- hostssl epgsql_test_db1 epgsql_test_cert        127.0.0.1/32    cert
--
-- any 'trust all' must be commented out for the invalid password test
-- to succeed.
--
-- ssl support must be configured, and the sslinfo contrib module
-- loaded for the ssl tests to succeed.


CREATE USER chaosbay;

CREATE DATABASE chaosbaydb WITH TEMPLATE = 'template0' OWNER = 'chaosbay' ENCODING 'UTF8';

GRANT ALL ON DATABASE chaosbaydb to chaosbay;

\c chaosbaydb;

CREATE TABLE torrents (infohash bytea primary key, name text, length bigint, timestamp bigint, data bytea);

CREATE TABLE comments (id serial unique primary key, name text, timestamp bigint, comment text);

CREATE FUNCTION count_comments(TEXT) RETURNS BIGINT AS $$ SELECT COUNT(*) FROM comments WHERE name = $1;$$ LANGUAGE SQL;

-- CREATE TABLE peer (hash_peer, ip, port, downloaded, uploaded, left, upspeed, downspeed, last);
