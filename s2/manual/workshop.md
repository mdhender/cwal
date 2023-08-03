# s2: The Workshop
#### ([&#x2b11;Table of Contents](./))
# The Workshop - In-Development and Experimental Features

This section acts as a staging ground for documenting features which
are in development but are not considered "official," in that they may
be removed or changed notably at any time.

<a id="workshop-interceptors"></a>
# Property Interceptors

> Disclosure: this feature will *not* leave the workshop because
implementing it *properly* requires more property-level
infrastructure than s2 has. Nonetheless, this section is retained,
partly as a reminder to myself why it would be... suboptimal... to
include the current implementation.

So i finally figured out how to get property interceptors working in s2,
and it seems to work reasonably well, but it adds duplicate property
search overhead to many (though i don't think it's most) property
searches. We could avoid the duplicate lookups only if we build this
feature into the cwal core (and only with some minor internal
refactoring), but i'm hesitant to do so because the room for unwanted
side-effects is just so large. This may or may not ever get added as a
core feature. Currently, the overall cost it adds to the common cases
don't seem to justify this syntactic-sugar feature, so it's not enabled
and there are no plans to enable it unless the double-lookup
inefficiency can be eliminated. This feature would likely be much easier
to implement properly if the engine used higher-level Property objects,
as opposed to simple key/value pairs with a few bits for flags, but such
a structure is far more heavy-weight than cwal is intended to be, and
there are no plans whatsoever to "grow" it in that direction.
