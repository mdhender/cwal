# s2 Scripting Engine
# s2: cwal's 2nd [^1] Scripting Language

s2 is documented in a gratuitous amount of detail in:

- Its [massive manual](manual/).
- Its C API docs are [in the header files](/dir/include/wh/cwal/s2?ci=trunk), primarily
  [s2.h](/finfo/include/wh/cwal/s2/s2.h) and [cwal.h](/finfo/include/wh/cwal/cwal.h).
- Its various [loadable modules](mod/).
- [s2sh](manual/s2sh.md) - the s2 shell.
    - Building a static s2sh [with Docker](../docker/README.md).

---

[^1]: s2 is actually cwal's *third* scripting language. The first one,
creatively called *s1*, was a throwaway prototype, never intended to
do anything more than act as an experiment. The second language,
th1ish, was TCL-inspired and had a pretty good run before a couple of
major, deep-seated design mistakes were discovered relatively late in
the process and it was abandoned in favor of... s2. The name s3 was
not used because that's the name of a computer chip
manufacturer. Should there ever be a "next" language for cwal, it will
likely be called s4, or maybe (as suggested by Caleb) *sewal:
Scripting Enging With a Language*, as a hat tip to [cwal, the
*Scripting Engine Without a Language*](/).
