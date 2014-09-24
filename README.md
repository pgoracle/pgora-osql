pgora-osql
==========

Visit us at http://pgora.com

Overview
--------
pgOSQL is an extension that provides a PostgreSQL procedural language that
implements the PL/SQL language. 


Core Committers
----------------
  Denis Lussier <denisl@pgora.com>


Building from Source
--------------------

For installation there must be PostgreSQL dev environment installed
and pg_config in the PATH.   Then just run:

	$ make
	$ make install

To run regression tests:

	$ make installcheck

Notes:

* Location to pg_config can be set via PG_CONFIG variable:

	$ make PG_CONFIG=/path/to/pg_config
	$ make install PG_CONFIG=/path/to/pg_config
	$ make installcheck PG_CONFIG=/path/to/pg_config
