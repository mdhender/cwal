# s2: Memory Capping
#### ([&#x2b11;Table of Contents](./)) ([&#x2b11;Misc. Features Index](misc-index.md))
# Memory Capping

Use case: Caleb has set up a copy of s2 reachable via a browser. We'd
like to ensure that nobody uses it to DoS his web server.

Part of addressing that involved adding a basic "clampdown mode" to
s2, which disables certain APIs which hosts might not like arbitrary
scripts to have access to (e.g. filesystem-related stuff). (Clampdown
mode was replaced by a [similar, but more granualar, mechanism](misc-disable.md) on
2020-02-08.) Another part is...

As of mid-December, 2014, cwal supports various forms of memory capping,
such that its allocator will fail (return `NULL`) for allocations once
some client-defined threshold has been crossed. It allows capping
dynamic memory based on any of the following (the `*` next to some is
explained below):

-   **Maximum single (re-)allocation size**: reject any single
    (re-)allocation request over a given size.
-   **Maximum concurrent active chunks** of allocated memory. If
    exceeded, the app can recover by freeing some cwal-managed memory.
-   `*` **Maximum concurrent memory**. Also recoverable by freeing
    cwal-managed memory. When reallocating, the new size is the one
    counted for this purpose.
-   **Maximum total allocation count**. Once this point is reached,
    there is no recovery - the allocator will keep failing until the
    interpreter is reset. Reallocations count as a single allocation,
    but a realloc to a larger size is subject to the other constraints.
-   `*` **Maximum total (re-)allocated memory**. This cap is also
    "fatal," in that the only way to recover is to reset the interpreter
    instance. When reallocating, only the larger of the old/new sizes is
    counted.

The options with no `*` preceding them are "free" for the interpreter,
in that activating them requires no resources it doesn't already have.
The ones with a `*` require that the engine keep track of the size of
all memory chunks it allocates. It does that by using the time-honored
trick of over-allocating *all* memory requests by `sizeof(void*)`
bytes, then writing the size of the chunk in the bytes preceding the
address returned to the caller. The relative weight of this overhead
depends largely on whether recycling is enabled or not. The overhead
is 4-8 bytes per allocation (for 32- resp. 64-bit), but recycling cuts
the number of allocations considerably, reducing the amortized cost of
this overhead. As a point of comparison: the mid-December 2014 s2
amalgamated unit test scripts have a capping-related overhead of
roughly 6-8kb on 64-bit, half that on 32-bit. When recycling is
disabled, it can add notably more. (But disabling of recycling is
something one normally only does when testing new C code, to ensure
that the recycler is not hiding any value lifetime misuse.) This
overhead *does* count against any limits set in the constraints
(because that simplifies the internals compared to not doing so).

[s2sh](s2sh.md) has CLI options to configure these capping options
(see the `--help` text).

Design notes: one might think we could use `sizeof(uint32_t)` instead
of `sizeof(void*)`, but that would break alignment assumptions: some
applications/systems expect that generically allocated memory be
aligned on a generic pointer address boundary. Thus we have to use
`sizeof(void*)`, even though 4 bytes of that gets wasted on 64-bit
platforms. Internally we explicitly use `uint32_t`, instead of
`cwal_size_t` (which can be 16-64 bits), for these sizes so that
32-bit builds do not need to over-allocate by 8 bytes when using
64-bit `cwal_size_t`. To be clear: there are no conceived use cases
involving allocation of memory chunks larger than 4GB for which s2 is
likely even remotely the best tool for the job. This limit only
applies when over-allocation for capping is enabled, and presumably
one would not set a cap if they need to allocate 4+GB chunks of
memory.

Memory capping is applied during the initialization of an s2
interpreter, and cannot (resp. must not) be changed afterwards. The
overhead costs mentioned above apply only when a capping option which
requires it is activated.

Despite the cost, there are a couple benefits to enabling the
over-allocating capping options (even if they're set to some hugely
unrealistic value, just to get them enabled):

-   It enables cwal to measure its concurrent and peak memory usage with
    (near-)byte precision, and the metrics from this tracking are
    included in the cwal core metrics dumps (try passing the `-m -v`
    options s2sh).
-   It allows the chunk recycler to make better usage of memory in
    some cases. When a large block has been recycled for a smaller
    request, the delta in bytes between the original (larger) and
    smaller sizes can be recovered only when the capping-related
    features are enabled.  When disabled, that delta (sometimes) gets
    "lost," in that it belongs to a recyclable block but is not
    counted towards its size for future recycling. This has the
    largest potential impact on string memory. Buffer and list memory
    have enough info to not to lose track of more than
    `sizeof(void*)-1` bytes in such an exchange, but that memory
    sometimes also recycles via strings, potentially lopping off bytes
    each time. That lopping off does not happen (more correctly, it
    gets "undone" at the right time) when cwal is tracking memory
    block sizes. To be clear, it doesn't "lose" bytes either way, but
    it can keep more precise track of each block's size, accounting
    for "slack" space, when over-allocation is enabled.  That, in
    turn, leads to more efficient memory reuse in the memory recycler.
