# whcl sqlite3 Module
#### ([&#x2b11;Main Module Docs](../))
<style>@import url(../../../doc/fossil-doc.css)</style>
<script src="../../../doc/highlightjs/highlight-cwal.min.js"></script>

- Source code: [](/dir/whcl/mod/sqlite3?ci=trunk)
- Test/demo code: [][test1.whcl]

**Contents:**

- [sqlite3 Module](#sqlite3module)
- [DB Class Members](#dbclassmemberfunctions)
    - [SQLite3.each](#sqlite3each)
    - [Transactions](#dbtransactions)
    - [User-Defined Functions (UDFs)](#userdefinedfunctionsudfs)
    - [(Non-)Deterministic UDFs](#udf-deterministic)
    - [UDF Notes, Caveats, and Limitations](#udf-notes-caveats)
- [SQLite3.Stmt Class](#sqlite3stmtclass)
    - [Stmt Member Functions](#stmtmemberfunctions)
    - [Non-Function Members](#nonfunctionmembers)
    - [Managing Stmt Lifetimes](#managingstmtlifetimes)
- [Symbol Resolution via Callbacks](#symbol-resolution)
- [Misc. Tips and Tricks](#misctipsandtricks)
    - [DB Initialization Wrappers](#db-init-wrappers)
    - [Dynamically Replacing Methods with Transaction-capable Versions](#transfunctioner)
- [SQLite3 Utility Modules for require.s2](#sqlite3modulesforrequires2)



<a id="sqlite3module"></a>
# sqlite3 Module

This module provides access to [sqlite3](https://sqlite.org/) databases.

A note about SQL compatibility: the underlying db is sqlite3, so any
SQL must be compatible with that. The module can be built using either
the system-level libsqlite3 or a site-local `sqlite3.[ch]`. As a rule,
the sqlite3 team recommends always building with one's own copy of the
library, rather than relying on a system-wide version which can be
changed at the whims of… *someone else*.

Tip: [`Buffer.appendf`](../../manual/type-buffer.md#method-appendf)
and
[`String.apply-format`](../../manual/type-string.md#method-apply-format)
can be used to escape strings for SQL. See the "q" and "Q" formatting
options in the `Buffer.appendf` docs.
*That said*, "bound" parameters (via the `SQLite3.Stmt.bind` member
and various work-alikes) are always an easier and safer option than
string escaping, in particular when accepting values from external
sources. This module has been used in many scripts without the q/Q
formatting options ever having been used.

Loading this module:

If this module is built into the whcl library then:

```whcl
decl -const SQLite3 [whcl load-api sqlite3]
```

> Recall that it is possible to determine whether it's built in via
something like:
```whcl
if {[[whcl load-api].index-of sqlite3] >= 0} {...}
```

If it's an external DLL:

```whcl
decl -const SQLite3 [whcl load-module '/path/to/this.so']
```

And then:

```whcl
decl db new SQLite3 ...
```

<a id="dbclassmemberfunctions"></a>
DB Class Member Functions
============================================================

Instances of this class are created with the module's constructor. In
sqlite jargon, an instance represents a single "db connection" or "db
handle."

The module provides the following methods, only a few of which are
"class methods", the majority being "instance methods":


Constructor
------------------------------------------------------------

Usage: `new SQLite3 filename [openflags]`


Opens a database file. Intended to be called on the prototype
instance, but may be called from any instance. Returns, on success, a
new DB instance. The 1st argument is the file name, and may be one of
sqlite3's special names like `":memory:"` or `""` (for a temporary
database in the filesystem). Note that each in-memory database opened
with the name `":memory:"` is a unique database, not a single shared
instance. The 2nd argument specifies the open-mode for the db, and can
optionally be a string with any of these case-sensitive characters:

- `r`: read-only.
- `w`: read/write. This is the default.
- `c`: create the db if it does not already exist. This is the
  default.
- `T`: enable SQL tracing for the db instance. This causes all SQL
  sent through sqlite to be streamed to stdout (the real stdout, not
  cwal's stdout abstraction). This is occasionally useful for testing
  and debugging, but should not otherwise be enabled.


begin
------------------------------------------------------------

Increments a transaction counter, starting a new transaction if the
counter is 1. This simulates nested transactions.  Calling this
obligates the script to call either `commit` or `rollback`. Once
`commit` and/or `rollback` have been called the same number of
times as `begin`, the transaction is either committed or (if any
`rollback` calls were made) rolled back. Returns this
object. Achtung: using transactions in conjunction with exceptions and
garbage collection requires care. This support is exceedingly useful
at the C level, but exceptions make it trickier to use properly from
script code. See the `transaction` member for one solution.


busy-timeout
------------------------------------------------------------

Usage: `busy-timeout [timeoutMs]`

Sets this database connection's "busy timeout" - the amount of time it
will wait on a lock before failing the current operation due to a
locking timeout. Set it to zero to disable the timeout handler. Throws
if passed a negative value.  Returns this object.


changes
------------------------------------------------------------

Usage: `changes`


Behaves exactly like
[`sqlite3_changes()`](https://www.sqlite.org/c3ref/changes.html),
returning the number of rows modified by the most recent INSERT,
UPDATE, or DELETE on this connection.


close
------------------------------------------------------------

Usage: `close`


Closes the database connection. This unbinds the underlying native
object from the script world and frees it. After calling this, calling
any other member functions will trigger an exception. Clients normally
don't need to call this - the garbage collector will do so as a side
effect of cleaning up the db handle.


commit
------------------------------------------------------------

Usage: `commit`


Decrements the transaction counter which gets incremented by `begin`,
committing or rolling back once the counter goes to zero, depending on
whether `rollback` was called for part of the nested
transaction. Returns this object. Throws if no transaction is pending.
It is important that clients *not call COMMIT directly from SQL code*,
as that will get the database and this connection's views of the world
out of sync.



create-udf
------------------------------------------------------------

Usage: `create-udf funcName [bool isAggregate=false] function`

FIXME: change the `isAggregate` flag to a `-aggregate`.

Binds a function to SQL. Described in [its own
subsection](#userdefinedfunctionsudfs). Returns this object.


each
------------------------------------------------------------

Usage: `each Object`


Runs a query and calls a callback for each row of the result set or
collects the results in an array. Described in detail in [its own
section](#sqlite3each). Returns this object.


exec
------------------------------------------------------------

Usage: `exec sql [ ...sql]`


Executes each argument as a single SQL statement, steps it one time
and ignores any non-error results. Returns this object.


exec-multi
------------------------------------------------------------

Usage: `exec-multi sql [ ...sql]`


Like `exec` but prepares and executes each argument as (potentially)
multiple SQL statements, separated by semicolons. This can, for
example, execute the contents of an arbitrary SQL file, which may
contain any number and type of SQL statements. e.g. a schema can be
imported this way.


get-filename
------------------------------------------------------------

Usage: `get-filename`


Returns the filename associated with this DB handle.


last-insert-id
------------------------------------------------------------

Usage: `integer last-insert-id`

Returns the most recent row ID inserted via an INSERT operation run on
this particular connection.


prepare
------------------------------------------------------------

Usage: `prepare sql`

Prepares the given SQL and returns a [SQLite3.Stmt](#sqlite3stmtclass)
instance. This is the only way to create new Stmt instances!


rollback
------------------------------------------------------------

Usage: `rollback [-force]`


Sets the current transaction into a rollback state. By default the
rollback is not immediately applied - it is run when the final pending
`commit/rollback` call closes the transaction stack (see `begin`). If
passed a truthy value then it immediately forces a rollback and clears
the transaction state, but doing so will cause any pending "outer"
commits to throw an exception if they get called because no
transaction is pending. The `-force` flag is intended for recovery
when transactions and exceptions overlap and get the db in a funky
transaction state. If the db gets in such a state, after handling all
pending exceptions, call (`catch {db.rollback -force}`) and ignore the
result (it might fail for legitimate reasons, e.g. might not have a
transaction active).


select-row
------------------------------------------------------------

Usage: `select-row [-tuple] sql [bind=undefined]`

Selects the first row from the given SQL's result set and returns it
as either an Object (by default) or a Tuple (if the `-tuple` is
used). The 2nd parameter is an object/array/tuple for binding data to
placeholders in the SQL (see `Stmt.bind`). A bind value of `undefined`
is a no-op (nothing is bound).


select-rows
------------------------------------------------------------

Usage: `select-row [-tuple] sql [bind=undefined]`

The multi-row variant of `select-row`, which takes the same argument
but returns an array of zero or more result rows as either objects or
tuples, depending on whether the `-tuple` flag is used. Note that the
outer list is always an array because because the number of result
rows is not known in advance (whereas the number of result columns
is).


select-value
------------------------------------------------------------

Usages:

- `select-value sql [bind=undefined [defaultResult=undefined]]`
- `select-value sql [defaultResult=undefined]`

Executes the given SQL and returns the first column from the first
result row. If no rows are found, the value of the final parameter is
returned (defaulting to undefined).  The second argument specifies a
single value or array/tuple/object of values to bind to the SQL before
execution. Passing `undefined` as the bind data behaves exactly as if
the argument had not been passed (because this simplifies script
code). If passed two arguments and the SQL has no parameters to bind,
the second argument is interpreted as the default result value.

> Tip: because `select-value` prepares and steps a statement a single
time, it can also be used for INSERT, UPDATE, and DELETE statements,
but those have no result, so will return the default value. Beware,
however, that that `RETURNING` features added to sqlite3 in 2021
permit even those types of statements to return results.


select-values
------------------------------------------------------------

Usage: `select-values sql [bind = undefined]`

Executes the given SQL and returns an array containing the value of
the first column of each row. The second parameter specifies any value
(or array/object of values) to bind to the SQL before
execution. Passing undefined as the bind data behaves exactly as if
the argument had not been passed (this simplifies certain uses by
allowing them to blindly pass on the bind value from a higher-level
interface). Returns an empty array if no values are found.


total-changes
------------------------------------------------------------

Usage: `total-changes`


Works like
[`sqlite3_total_changes()`](https://www.sqlite.org/c3ref/total_changes.html),
reporting the total number of rows inserted, modified or deleted by
all INSERT, UPDATE or DELETE statements on this connection.

transaction
------------------------------------------------------------

Usage: `transaction Function`


A safer variant of transactions support vis-a-vis exceptions. This
function effectively calls:

```
db begin; worker $db; db commit or rollback; return $db
```

It ends a `commit` if the function does not fail (throw or propagate
an error), and `rollback` if the function fails. Any exception or
fatal error is propagated back. Note that this uses the same
pseudo-nested transaction support as `begin/commit/rollback()` do,
meaning that a commit or rollback may not be immediate. See `commit`
for more details. In the context of the callback function, `this`
refers to the current Db object. Returns `this`. The return value of
the callback, if any, is ignored.

This method is covered in more detail [in a later
section](#dbtransactions).


transaction-state
------------------------------------------------------------

Usage: `transaction-state`


Returns 0 if no transaction is active (`begin` has not been called or
`transaction` is not running a callback), returns the transaction
level (number of active `begin` or `transaction` calls) if a
transaction is open and no rollback is pending, or returns a negative
value if a rollback is pending (via `rollback` or the equivalent
having been triggered at the C level). Negative values are the negated
form of the current transaction depth. e.g. -2 if two levels are still
open with a rollback pending.



DB Members in More Detail
------------------------------------------------------------

<a id="sqlite3each"></a>
### `SQLite3 each`

This databas member function executes a query and steps over the
results, calling a user-defined callback for each row. It can be used
for non-fetching queries, but `exec` and `exec-multi` are easier for
the simple cases (but `each` is handy when the SQL requires bound
values).

The main advantage to using this function over explicitly preparing
statements is that if the callback throws, C-level code takes care of
cleaning up the statement, as opposed to script-side code having to
catch the exception and `finalize` the statement.

Example:

```whcl
db each [object {
 sql "SQL CODE" /* required: Buffer or String (heredocs are useful for this) */
 bind X /* optional: parameter value or array/tuple/object of values to bind */
 mode o | t | a /* optional: rows as (o)bjects, (a)rrays, (t)uples (default). */
     /* ^^^^ these letters are case-INsensitive. */
 each string | script | function | array /* optional: eval'd/call()ed/appended to for each row*/
}]
```

Only the `sql` property is required, and `bind` must be set if the SQL
contains any binding placeholders (see the `Stmt.bind` method for
details). If the bind member is an array, tuple, or object, it is
bound as such (to map to an arbitrary number of binding placeholders),
otherwise it is assumed to be a single value for a single bound
parameter. Note that non-fetching queries can also be used but will
not cause the callback to be triggered (so it need not be specified).

In the scope of the callback, this will resolve to the current row data,
either in object or array form (depending on the `mode` property). The
callback may be any of:

- A function, which gets called for each row or
- An array, which gets the value of each row appended to it or
- A [script object](../../manual/type-script.md) which gets `eval`'d
  for each row or
- A string script snippet which will be `eval`'d for each row.

In order for string and script callbacks to be useful, `each`
[does not block symbol lookup](../../manual/builtin-proc.md#-xsym).

If the callback throws, that exception is propagated. If it returns a
literal `false` (as opposed to another falsy value) then iteration stops
without an error. This can be used, e.g. to implement a row limit
without including a LIMIT in the SQL clause.

In the context of the callback function/script, the following
scope-local variables are set:

- `rowRumber`: 1-based number of the current row (the step/iteration/callback count).
- `columnNames`: array of column name strings for the result set. They
  are in the order specified in the query string.
- `this` is the current row data, either an Object, Array, or Tuple, as
  described above for the "mode" property.

Trivia: `columnNames` gets defined only once for all calls to the
callback, whereas `this` and `rowNumber` necessarily get redefined (and
quite possibly recycled) for each row.

For examples, see [test1.whcl][].

<a id="dbtransactions"></a>
### Db Transactions

It is often useful to wrap db operations in transactions. Though sqlite
does not natively support nested transactions, this API adds a layer of
pseudo-nested transaction support because having it simplifies much of
the C-level code \[from which this binding originally derived\]. Script
code may use the `begin`, `commit`, and `rollback` functions to manage
transaction. However, using those requires careful attention to
exception handling in order to keep the transaction stack level correct
(and fatal errors may interfere with that). The simplest/safest approach
is to wrap all transaction-level work inside a function call:

```whcl
myDb transaction [proc {} {
 affirm this == $myDb; # just for demonstration purposes
 this exec "update foo set bar=1 where baz is null;"
 this transaction $someExternalFunction; # may recurse
 # if an exception is thrown or a fatal error triggered, the transaction
 # gets rolled back. If this function returns normally then the transaction
 # is committed.
}]
```

Note that the API allows these to be nested. If any nested part fails
(is rolled back), the whole transaction will be rolled back when the
call stack comes back around to the final transaction in the stack.
Because the stack stack is unwound in the face of failed assertions,
syntax errors, and the `exit` builtin command, such exit conditions
will also cause a rollback.

Here's an example of how one can detect a rolled back transaction:

```whcl
catch err {
    myDb transaction ...
}
if {$err} {
  # Transaction was rolled back (or maybe never started).
  # err is a normal Exception object.
}
```


#### Achtung: Avoid `ON CONFLICT ROLLBACK` Clauses

Reminder to self (and anyone who's listening): it turns out that using
an ON CONFLICT ROLLBACK clause in a CREATE TABLE's constraints can
mis-interact with our transaction level management, whereas ON CONFLICT
ABORT (the default constraint handler) behaves how we'd like. It's not
clear how/whether we can consolidate our pseudo-nested transactions with
the ROLLBACK handler.

Specifically, the ROLLBACK conflict handler resets sqlite3's view of the
transaction state, while this plugin is potentially still under the
impression that a transaction is active. After the conflict handler has
triggered, the next UPDATE/INSERT from client-side code will (at the
level of this plugin) think it's in a transaction. but sqlite has left
the transaction and is now running in auto-commit mode. That, in turn,
causes this plugin's sanity check for "is COMMIT called during a
transaction?" to trigger. That condition is normally very much a no-no
because the plugin cannot keep track of its transaction levels if the
SQL COMMIT command is used by client code, so COMMIT being called will
cause this plugin to complain (it used to trigger a C assert, but as of
20191108 it triggers an s2-level exception).

This case appears when using transactions, a constraint handler is
triggered, and the client code catches that exception and continues
processing the transaction. Hypothetically, that only happens in test
code, where an outer transaction is used to prevent changes to a
database, where an inner transaction fails as part of a negative test,
is caught by an exception handler, and the testing continues. In
practice, continuing like that wouldn't normally happen outside of test
code.

#### Achtung: Do Not Combine `transaction` with `begin`/`commit`/`rollback`

It might seem reasonable to roll back a transaction with something like:

```whcl
aDb transaction [proc {} {
  ...
  if {$isDryRunMode} {this rollback}
}]
```

That will, however, cause `transaction`'s idea of the transaction state
to get mismatched. In short, `commit` and `rollback` must *never*
be called unless there was a prior call to `begin`.

The "correct" way to roll back a `transaction` call without
triggering an error is demonstrated in the following section...

#### Implementing Dry-Run Mode

A fairly common requirement in db-using scripts is a "dry-run
mode," in which all db-related operations are performed for test
purposes but are rolled back rather than being allowed to persist.

That is trivial to implement by following this pattern:

```whcl
catch ex {
  myDb transaction [proc {} {
    ...
    if {$isDryRunMode} {throw this}
  }]
}
affirm $ex; # we're expecting an exception!
expr (ex.message==$myDb) || [throw $ex]; # an unexpected exception
# Else we aborted the transaction due to our app's dry-run mode.
unset ex
```

That is, throw some well-known value from the `transaction` callback
and check to see if that value is the exception's `message`
property. In this example we simply throw the db handle because it's
conveniently in reach.


<a id="userdefinedfunctionsudfs"></a>
### User-defined Functions (UDFs)

***EXPERIMENTAL:*** the API works, but the aggregate-handling
conventions are still up for change.

SQLite3 allows clients to bind C code to SQL functions, such that
calling those functions from SQL calls client-defined C code. In sqlite
parlance these are called User-Defined Functions, or UDFs. This s2
binding allows the same, allowing both script-defined functions and
script-bound native functions to be used as UDFs.

UDFs come in two forms: scalar functions and aggregate functions. Scalar
functions evaluate some inputs (or take no arguments) and return a value
(possibly `null`). Aggregate functions are intended to inspect data over
multiple rows and return a result only after all rows are traversed.

Aggregates conceptually have 2 functions: a so-called step function
(called for each row) and a final function (called once, at the end).
This interface packs them into one function, but it sets a flag on the
callback instance for the "final" aggregation call so that the callback
can determine which mode to run in. This flag convention means that
recursive UDFs likely won't work, but that does not seem like too
onerous of a limitation (and if it is, we'll find another mechanism).

Creating a normal function:

```whcl
decl f proc {} { decl rc 0; foreach v $argv { incr rc $v; }; return $rc}
myDb.create-udf 'myfunc' $f
```

Note that no validation is done on UDF argument counts or types: this
binding will pass on up to some internal hard-coded limit of arguments
and will convert SQL types to/from their closest cwal counterpart.

The scalar function created above can be used like:

```whcl
assert 3 == [myDb select-value "select myfunc(1,2)"]
```

Creating an aggregate function requires that we pass the `-aggregate`
flag as the first argument:

```whcl
myDb create-udf -aggregate myaggr [proc {} {
  if {$aggregateFinal} { ; # the "final" call
    # gather and reset our accumulated data:
    decl rc store.result
    set store.result 0
    return $rc
  }; # else it's a normal call: accumulate or do whatever we need to do...
  foreach v $argv { incr store.result $v }
} using {
  # place to accumulate results:
  store { result 0 } 
}]
```

> Reminder to self: there's still an unhandled case here: if the
aggregate throws before its final step, the 'final' call is not made,
so the next time it's called it will pick up where it left off,
accumulating data from its previous run. How best to solve that is
unclear. Splitting aggregate functions into two separate functions,
like the C API does, might (not yet sure) simplify that, but it would
uglify the script-side interface. Maybe we need a flag which tells it
that a given call is the _first_ call to the aggregate for a given
invocation context... except then it couldn't be sensibly used twice
in the same SQL statement, e.g. `SELECT sum(a), sum(b) ...`. Hmm.

The call-local `$aggregateFinal` variable is set to `true` on the
final call of an aggregate callback and `false` for all other calls
(also for scalar UDFs (because the plugin can't distinguish between
scalar and non-final aggregate calls without extra infrastructure)).

Any state used by the aggregate should be reset in the "final" call so
that the next time the aggregate is used it has a clean working state.
When creating an aggregate with non-trivial state, it may be necessary
t either initialize the callback's state via `import-symbols`/`using`,
or to check for its existence in the callback and initialize it as
needed.  An aggregate which may throw/propagate an exception should
clean up the state before throwing, as the framework will not make the
"final" aggregate call if it throws during a non-final call. (Reminder
to self: we may want to consider adding a "first" function to this
API, which the library would call before the aggregate is called the
first time (for each given DB-side invocation) so that any
initialization could be done there/then.

Aggregates are used like:

```whcl
myDb select-value "select myaggr(a) from t"
```

Trivia (`select myaggr(*) from t`) will result in each call to the
aggregate function being passed *no arguments*. That's just how sqlite
does it.

Function arguments and result values are converted from SQL to
script-space as needed, and such conversions should "just work", so
long as no exotic data typing is going on. Results values which cannot
be converted to SQL (e.g. Functions) will be converted to SQL NULL (as
is the `undefined` value).

`create-udf` has another call form which is simpler to use for more
advanced cases but requires slightly more code for the simpler cases
shown so far:

```whcl
myDb create-udf name [object {
  deterministic true /* default==false */
  aggregate false /* default==false */
  step [proc {} { … }]
}]
```

The deterministic flag is described below...

<a id='udf-deterministic'></a>
### Deterministic vs. Non-Deterministic UDFs

Most UDFs always return the same values for the same inputs, and are
thus said to be deterministic. sqlite [allows clients to tell
it](https://www.sqlite.org/c3ref/create_function.html) that functions
are deterministic so that it can better optimize out calls to them,
but by default it pessimistically assumes that functions are
nondeterministic (while the documentation recommends flagging
functions as deterministic whenever possible). Likewise, UDFs bound
with this API are not flagged as deterministic by default. To enable
it, use the `create-udf` form which takes a configuration object as its
2nd parameter, and set the config object's "deterministic" property to
true before passing it to `create-udf`. That property is only inspected
when the UDF is initialized. This approach is demonstrated
above. Optionally, the "deterministic" flag may be set on the callback
instance itself, but that flag is only checked when `create-udf` is
called with no configuration object.

Example of setting the deterministic flag on the callback:

```whcl
decl myAggregate proc agg {} {
    if {$aggregateFinal} { … final call … } else { … }
}
set myAggregate.deterministic true
myDb create-udf -aggregate myAggregate $myAggregate
```

But that can be done more simply using `create-udf`'s object form:

```whcl
myDb create-udf name [object {
  deterministic true
  step [proc {} { … }]
}]
```

<a id='udf-notes-caveats'></a>
### UDF Notes, Caveats, and Limitations

1.  The aggregate API conventions are up for reconsideration. i'm not
    all that happy with having some magically-named local variable, but
    no better option currently comes to mind (other than splitting the
    step/final methods into two functions).

2.  Error reporting: script/cwal-level errors triggered from UDFs are
    reported via an sqlite3-level error and also continue to propagate
    via the script engine. This means that errors (e.g. exceptions thrown)
    in UDFs will be reported to the user in an intuitive manner (the
    routine running the SQL in question will propagate it). It also
    means, however, that exit-causing conditions, syntax errors, and
    keywords like assert and exit, when triggered from UDF code,
    propagate as well, and will end the current script.

3.  Accumulating data for aggregates: where it is accumulated is your
    business. Storing it in the function instance sounds reasonable,
    mostly, but in such cases the data's owning scope will be the scope
    which owns the db handle (as the db stores UDFs in a container-like
    fashion (necessarily, for lifetime purposes)). Because SQL-type
    results won't participate in cycles, though, they are not subject to
    any "GC deathtraps", so their owning scope is largely irrelevant.

4.  Overwriting functions (creating the same-named UDF twice) does not
    work properly: for some reason, sqlite is keeping the old callback
    pointer, which has been killed off via GC at the point, leading to
    it stepping on a stale pointer when the UDF is called. Not sure why
    this is happening. Because of that, `createUDF()` will throw an
    exception if the same name (case-insensitive) is used more than
    once. UDFs live as long as the database they're added to (longer if
    a longer-lived reference is held elsewhere).

5.  Case-sensitivity: this function internally treats names
    case-insensitively (it has to cache them for lifetime purposes),
    and SQL function calls are case-insensitive as well.

6.  As is always the case in whcl, a function called without a `this`
    will be its own `this` in the context of any calls to it. i.e. in
    a UDF callback, `this` will be the callback itself. We might want
    to reconsider that and bind the db handle as `this`.


<a id="sqlite3stmtclass"></a>
SQLite3.Stmt Class
============================================================

The `Stmt` class represents SQL prepared statements. Statement
instances are created calling `SQLite3.prepare` on an opened
database, but convenience APIs like `SQLite3.each` exist so that
client code does not need to touch Statement handles directly for most
simply db uses.

***ACHTUNG:*** When using prepared statements, it is _important_ to
call their `finalize` method (or otherwise let the statement go out of
scope and be garbage collected) _before the underlying db handle is
closed_, or results are undefined. (Potential TODO: add a weak ref
from the statement handles to their dbs. In practice this has never
been a problem, but cleanup ordering could hypothetically lead to one
if client code does not finalize statements.) If destruction ordering
is left solely to the garbage collector, and the statements live in
the same scope as the database, their destruction order is undefined,
so be sure to `finalize` statements. It is highly recommended that one
use the `SQLite3.each` function, when possible, to avoid having to
manage statement handles.

> That said...
[sqlite3_close_v2()](https://www.sqlite.org/c3ref/close.html) exists
for just such bindings as this one, where it's possible that a db
handle gets GC'd before its statements do. Thus we internally use that
function to close the db. Even so: always make every effort to
finalize statement handles before the underlying db handle is closed.


<a id="stmtmemberfunctions"></a>
Stmt Member Functions
------------------------------------------------------------

This class has no "static" methods, only methods usable on instances
created via `SQLite3.prepare`.


### bind

`bind` associates bound parameters with values. It has four separate
usages, all of which return `this`:

Usage #1: `bind oneBasedColumn [Value = null]`

Binds a value to the given 1-based parameter index. Numbers and
strings are handled "as expected." Booleans are treated as numbers 0
and 1. `null` and `undefined` are both treated as SQL NULL. Buffers
are bound as blobs, not strings.


Usage #2: `bind array|tuple`

A convenience form of `bind` which binds all values from the array to
their associated bound parameter slots.  Note that while bind indexes
are 1-based, the array's/tuple's contents are 0-based.

Usage #3: `bind object`

Binds named parameters. The object parameter's property keys must
match the SQL bound parameter names identically, including the prefix
`:`, `$`, or `@` character. For example:

```whcl
stmt bind [object ":foo" 1 "@bar" 2.0 "$baz" 'three']
```

Literal integer keys are treated as 1-based parameter indexes, so
binding the object `object {1 $x 2 $y}` and `[array $x $y]` are
equivalent for SQL with two unnamed SQL parameter placeholders (a
literal question mark or `?N`, where N is a 1-based parameter index).


Usage #4: `bind undefined`

Is a special case to simplify some higher-level convenience routines
which might or might not (depending on local conditions) want to pass
a parameter to bind. Passing `undefined` has no effect - the argument is
simply ignored and this function becomes a no-op.


### clear-bindings

Usage: `clear-bindings`

Clears all `bind`'d values. Returns this object.



### each

Usage: `each function`


Steps through this statement's result set and calls the given callback
for each row. In the callback `this` is the Stmt object. If the
callback returns a literal `false` (as opposed to a different falsy
value), iteration stops immediately without an error. Returns this
object. The callback can use `this.get`, `this.row-to`, or similar
routines to fetch the current row's values. The callback *must not*
call routines which modify the statement cursor, e.g. `step`, `exec`,
or `finalize`.



### exec

Usage: `exec [bind params…]`


`bind`s the given SQL parameter(s) (if any), `step`s the statement one
time, `reset`s it, and returns `this`. Its `bind` parameters may be an
Object of key/value mappings, an Array/Tuple of positional parameters,
or a list of non-Object/Array/Tuple values to bind to the parameter
index corresponding to their position in the argument list. e.g.
`exec 1 2 3 ` and `exec [array 1 2 3]` are equivalent. Binding- and
SQL-execution errors are propagated out. This routine is intended to
simplify Statement management for some cases, e.g.:

```whcl
[[[db prepare "insert into t(a,b) values(?,?)"
  ].exec 1 2
 ].exec 3 4
].finalize`
```

Potential TODO: if passed multiple arrays or objects, `exec` it one
time for each such parameter. e.g. the above example could then use
`exec [array 1 2] [array 3 4]`.



### finalize

Usage: `finalize`

Frees all resources associated with the statement. It is critical that
clients call this when done with a statement, (A) to avoid wasting
resources and (B) so that the database does not misbehave. e.g. it
might throw an error if one tries to close it while statements are
still opened, and closing statements after the db is closed might step
on a stale pointer. This unbinds the underlying native object from the
script world and frees it. After calling this, calling any other
member functions will trigger an exception (as opposed to stepping on
a stale pointer). Scoping and garbage collection will normally ensure
that statements are finalized before their owning db, but clients
"really, really should" finalize statements themselves whenever
possible, to avoid potential downstream errors or side-effects. If
statements are stored solely as local variables, and not properties of
longer-lived objects, then scoping and refcounting will reliably take
care of finalizing them. See [this section](#managingstmtlifetimes) for
examples of how to manage their lifetimes.


### get-column

Usage: `get-column zeroBasedColumn`


Returns the value of the given 0-based result column index. It is
illegal to call this function unless `step` (or equivalent) has
returned true (or equivalent) to indicate that it has fetched a row of
data. The type of the returned data depends on the underlying db view
of the data - the closest-matching script-side data type is used. Note
that it is legal to use this in conjunction with one of the various
result type flags to `step`, but those features basically obviate this
one for many use cases.


### reset

Usage: `reset`

Resets the statement so that it can be executed again.

Potential TODO: if passed `true`, also call `clear-bindings`.


### row-to

Usage: `row-to [-tuple|-array|-object]`

Requires that this Statement have just been successfully `step`'d. It
fetches the current row of data as the data type specified by the flag
(defaulting to `-tuple`). See `step` for the specifics of the various
type flags. Throws if the Statement has not been stepped.


### step

Usage: `step [-bool|-stmt|-array|-tuple|-object]`


"Steps" the cursor one time and returns, by default, `true` if the
statement has a row of data, else `false`. The single flag argument
may be used to specify the result type:

- `-bool` (the default) returns true if the step results in a row being
  fetched, else false.
- `-stmt` results in the `Stmt` object itself being returned,
  regardless of whether a row was fetched or not. This is mostly of
  use when executing non-fetching statements in a chain of calls.
- `-array` and `-tuple` result in a list of the given type being
  returned if a row is fetched, or `undefined` if no row is fetched.
- `-object` returns the fetched row as an object, or `undefined` if no
  row is fetched. Remember that the order of the fields in objects is
  unspecified (and can potentially change at runtime), so the field
  order matching the column name order would be a pure coincidence
  (unless there is only one result column, of course ;).


Throws on error. For non-fetching statements, like INSERT or DELETE,
it returns the same as if no row were fetched.


<a id="nonfunctionmembers"></a>
Non-function Members
------------------------------------------------------------

The following non-function members are set on each new Stmt instance:

- `integer columnCount` is the number of result columns. It is 0 for
  non-fetching queries (INSERT, DELETE, etc.) and a positive value for
  fetching queries (SELECT and possibly some pragmas).
- `array columnNames` is a list of column name strings. Only set if
  `columnCount` is positive. Remember that the column names returned
  by sqlite are unspecified unless one uses an `AS` qualifier to
  explicitly name it. e.g. `SELECT a` has an *unspecified* column name,
  where as `SELECT a AS a`, and its shorter form `SELECT a a`, both have
  a well-defined name of `a`.
- `integer parameterCount` is the number of bindable parameter positions
  in the prepared statement. It is 0 if there are no bindable
  parameters.



<a id="managingstmtlifetimes"></a>
Managing Stmt Lifetimes
------------------------------------------------------------

As mentioned several times already, prepared statements must be
finalized (or destroyed by GC) before their owning db is closed, or
results are undefined (possibly a crash). So long as statements only
exist in newer scopes than the db was opened in, and never propagated
out of those scopes, this is never a problem. When in doubt, wrap a
statement up in a distinct scope (or in a catch block):

```whcl
decl -const db = ….;
eval -scope {
  decl -const stmt [db prepare ...]
  …
  # At the end of this scope, if stmt is not somehow propagated
  # out, it will be finalize()'d during GC. To force a dummy
  # value to be the result of the scope, do something like:
  eval undefined
}
# db is still opened here or an exception propagated and we never
# reached this point.
```

Calling `finalize` (or GC-induced destruction) disconnects a
script-side Statement object from its C-side bits, and calling any
further C-bound methods on the Statement afterwards will trigger an
exception (as opposed to stepping on stale C-level data). Thus an
explicit call to `finalize` is always safest in cases where references
to Statements might be held beyond the scope they are initialize
created in. If the Statement bubbles up the scope ownership chain (as
distinct from script-visibility purposes) to the same depth as (or an
older scope than) its owning db, there's a very real danger of
automatic finalization (at GC-time) being run too late. Using the
patterns demonstrated above (or equivalent) are strongly recommended
in order to keep that from happening. Or prefer to use
[SQLite3.each](#sqlite3each) for SELECT-style queries instead, as it
takes care of that statement management.



<a id="symbol-resolution"></a>
Symbol Resolution via Callbacks
============================================================

Normally in whcl, the search for resolving symbols stops at a function
call boundary. That makes using certain types of callbacks tedious, in
particular it severely limits the use of `eval`'able strings as
callback handlers. The following methods are configured such that
symbol resolution is permitted to pass on through them, to simplify
implementing client-side callbacks:

- `db.transaction`
- `db.each`
- `stmt.each`

When passing callback functions, as opposed to `eval`'able script
snippets, to these methods, the callbacks still need the `-xlookup`
flag if they want to reference symbols which are accessible from the
scope in which the above method is called.


<a id="misctipsandtricks"></a>
Misc. Tips and Tricks
============================================================

Practice with this code's predecessor has revealed some useful tips
and tricks for this module...


<a id="db-init-wrappers"></a>
DB Initialization Wrappers
------------------------------------------------------------

In non-trivial scripts it's often useful to have a function which,
when called, returns the app-wide shared database handle and opens the
db the first time it's called. e.g.:

```whcl
set -const myApp.db proc x {} {
  expr (x._ && [return x._])
  affirm [info is-object this.sqlite3]
  return set x._ new this.sqlite3 this.config.dbFile r
}
```

That can be encapsulated in a generic function which creates such
wrappers, with something along these lines:

```whcl
decl -const dbAccessor proc f {config} {
  affirm [info is-container $config]
  affirm [info is-string config.dbFile]
  return proc f {} {
    return (f._ ||
        [set -const f._ new S C.dbFile (C.dbOpenFlags || w)])
  } using{S $S C $config}
} using{S whcl.sqlite3}
```

(Got all that?)

Which would be used like:

```whcl
decl -const myApp object {
  config {
    dbFile '/path/to/my.db'
    dbOpenFlags wc
    ...
  }
}
set -const myApp.db [dbAccessor myApp.config]
```

It's a simple matter to extend that to accept some SQL code which gets
executed when the db is opened, e.g. to enable a certain pragma (e.g. to
turn on foreign keys). At its most basic, the constructor call might be
changed to something like:

```whcl
new S C.dbFile (C.dbOpenFlags || w) {
  this.exec-multi (C.dbOpenSql || 'select 1')
}
```

But a more "complete" solution might offer a choice of initializing the
db using either SQL or a callback function by setting a dbInit property
in the configuration object to either a string/buffer (SQL) or a
callback function:

```whcl
decl -const dbAccessor proc {config} {
    affirm [info is-container $config]
    affirm [info is-string config.dbFile]
    return proc f {}{
        expr f._ && [return f._]
        return set -const f._ new S C.dbFile (C.dbOpenFlags || w) {
            if {[info is-string C.dbInit]} {
                this.exec-multi C.dbInit
            } else if {[info has-function C.dbInit]} {
                C.dbInit this
                # or:
                # C.dbInit.call this
            }
        }
   } using{S $S C $config}
} using{S whcl.sqlite3}
```

<a id="transfunctioner"></a>
Dynamically Replacing Methods with Transaction-capable Versions
------------------------------------------------------------

Use case: we have a class which has multiple methods which we would like
to work within transactions, but we don't want to have to wrap all of
their code up in a `transaction` call. Here's a simple way to do it…

For this to work well we need a function which returns our database
handle. Assume, for purposes of this example, that we have such a
function and it looks something like this:

```whcl
decl -const db proc x {} {
    return (x._ || [set x._ [new S C.dbFile (C.dbOpenFlags || w)]])
} using {
    S whcl.sqlite3
    C [object dbFile "/path/to/my.db" dbOpenFlags w]
    # ^^^^ noting that that object need not be inlined - it could reference
    # an existing object or call a client-side function to get it.
}
```

That function opens its database, using the file specified in a
configuration object, the first time the function is called, and
returns that database object on each call.

With that in place, we can then create a function which transparently
converts arbitrary other functions into `db.transaction`
callbacks:

```whcl
########################################################################
# Returns a new function which wraps function f in a call to
# db.transaction(...). All arguments passed to the replacement
# function are passed on to the original function, and its "this" is
# applied to wrapped function. The replacement function returns
# the same thing as f.
proc transFunction {f} {
    affirm [info is-function $f]
    return proc {} {
        decl -const r object rc 0
        decl -const t this
        decl -const a $argv
        decl -const F proc {} {
            set r.rc [f.apply $t $a]
        } using {t $t a $a r $r f $f}
        [D].transaction $F
        return r.rc
    } using {f $f D $D}
} using{
    D $db
}
```

(Got all that?)

Next is an alternative implementation which uplifts the internal `F`
proc into the `using` state, which saves a non-trivial amount of
processing and memory on each call. This is functionally identical
_but_ is not as GC-friendly as the former implementation for reasons
out of scope here (in short: `F`'s imported symbols necessarily get
upscoped into `F`'s owning scope for lifetime management purposes,
which is not an issue unless the wrapped function returns cyclic data
structures which are intended to be short-lived).

```whcl
proc transFunction {f} {
    affirm [info is-function $f]
    return proc {} {
        decl -const r object rc 0
        decl -const t this
        decl -const a $argv
        [D].transaction [F.import-symbols r t a f]
        return r.rc
    } using {
        f $f D $D
        F [proc {} {set r.rc [f.apply $t $a]} using {}]
    }
} using{
    D $db
}
```

So, for example, if we have a class which has a `save` method and uses
our `db` function to get access to a shared database handle, we could
transactionalize that method with:

```whcl
set MyClass.save [transFunction MyClass.save]
```

Then future calls to that method would automatically wrap up the
`save` logic in a transaction (or take part in a pseudo-nested
transaction).


<!--

<a name="sqlite3modulesforrequires2"></a>
# SQLite3 Utility Modules for require.s2

This module sees heavy use in my websites, and several
[require.s2](../require/) modules have been created for use
with it: [](/dir/s2/require.d/sqlite3?ci=trunk)

-->

[test1.whcl]: /finfo/whcl/mod/sqlite3/test1.whcl
