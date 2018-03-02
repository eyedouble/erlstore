
-module(erlstore_persistence_adaptor).

-define(output, {number(), map()|binary()|atom()}).

-callback start( string() )
	-> ok.

-callback getAll(atom(), map())
	-> ?output.

-callback get(atom(), binary(), map())
	-> ?output.

-callback write(atom(), binary(), map())
	-> ?output.

-callback delete(atom(), binary(), map())
	-> ?output.

-callback filter(atom(), list(), map())
	-> ?output.

-callback listTables( )
	-> ?output.

-callback isTable(atom())
	-> ?output.

-callback infoTable(atom())
	-> ?output.

-callback createTable(atom())
	-> ?output.

-callback deleteTable(atom())
	-> ?output.

-callback subscribe(atom())
	-> ?output.
