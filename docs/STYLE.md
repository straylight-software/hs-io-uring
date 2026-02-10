━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                 // typographical // conventions
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

   "Wintermute could build a kind of personality into a shell. How subtle
    a form could manipulation take?"

                                                           — Neuromancer

# // rationale

It is common in the era of machine-assisted software engineering (which is
practiced here), sometimes called "vibecoding" (which is *not* practiced
here) to want to know if a given piece of code was human-authored or machine-
generated. Likewise if a piece of code had a Proper Human Review or was
validated some other way.

It is our observation that once the emotional charge and pejorative
phraserology is stripped off the real questions being asked is: "Was this work
done to a high standard, can I contribute to it with confidence that I'm
neither wasting my time nor sullying my reputation by association?"

We contend that machine-assisted software engineering is merely making
legible the degree to which we had over-pivoted to reputational proxies
for trust as the velocity of the software industry accelerated through
this century to date: basically we were checking the author attestation
and not much else by the end.

The right answer, as usual, is rigor of thought and diligence in execution
across all aspects of the craft. We do want an intuition for when a
badly-aligned or malfunctioning agent has been running through a file,
and so we adopt a set of conventions that are distinctive, subtle,
and demanding enough that any careless edit is likely to stand out.

                                                 - b7r6 // 2026

# // typographical // conventions

This document specifies the typographical standards for all code and documentation
within the straylight codebase. These conventions are not decorative — they encode
information, establish provenance, and serve as watermarks against tampering.

## // unicode // delimiters

We use Unicode box-drawing characters exclusively. ASCII approximations (`---`,
`===`, `***`) are *in poor taste*.

### Heavy Line (`━`)

File-level framing. 80 characters. Marks the boundary of a module:

```haskell
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                              // module title
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Double Line (`═`)

Major sections within a file:

```haskell
-- ════════════════════════════════════════════════════════════════════════════
--                                                           // major // section
-- ════════════════════════════════════════════════════════════════════════════
```

### Light Line (`─`)

Subsections. Contracts with nesting depth to respect indentation while maintaining
visual weight:

```haskell
-- ────────────────────────────────────────────────────────────────────────────
--                                                        // subsection // title
-- ────────────────────────────────────────────────────────────────────────────
```

### Em-Dash (`—`)

Attribution only. Never as a line-drawing element:

```text
                                                                — Neuromancer
```

## // double-slash // delimiter

Our primary delimiter is the `//` double slash.

```haskell
-- ────────────────────────────────────────────────────────────────────────────
--                                                          // core // packages
-- ────────────────────────────────────────────────────────────────────────────
```

The choice of `//` is deliberate:

- acknowledges Unix tradition
- acknowledges `HTTP` tradition
- acknowledges the `nix` path operator
- aesthetically balanced — few common delimiters share its bilateral symmetry
- scales cleanly: `///` remains legible while avoiding collision when needed
- low collision risk with actual code

## // code // block // headers

Source code files follow the same hierarchy with their most convenient
comment style.

## // comment // capitalization

### // workaday // lowercase

Working notes, observations, inline explanations:

```haskell
-- this is a workaround for upstream bug #1234
handle h -- for raw socket access
```

### // author // voice

Author Voice. Documentation that warrants a heading but lives inline:

```haskell
-- Derivation parsing requires the full store path prefix.
-- This is a fundamental constraint of the Nix model.
```

### // proper // grammar

Markdown and module descriptions use proper capitalization throughout.

## // TODO // convention

```haskell
-- TODO[b7r6]: minor debt, will address
```

With severity markers:

```haskell
-- TODO[b7r6]: !! urgent — this is embarrassing !!

-- TODO[b7r6]: !! be *very* mindful of these hardcodes !!
```

- bracket tag `[handle]` for ownership
- double-bang `!!` for severity and shame
- asterisk `*emphasis*` for specific words
- em-dash (`—`) not double-hyphen (`--`)

## // latin // abbreviations

Preferred over English equivalents when clear from context:

| Use | Meaning | Not |
|---|---|---|
| n.b. | nota bene | note: |
| i.e. | id est | that is |
| e.g. | exempli gratia | for example |
| cf. | confer | compare |
| et al. | et alii | and others |
| viz. | videlicet | namely |
| q.v. | quod vide | which see |

```haskell
defaultTimeout = 100 -- n.b. this is the default for warp
```

## // epigraph // watermarks

```text
--     "In the dream, just before he'd drenched the nest with fuel, he'd seen the 
--      T-A logo of Tessier-Ashpool neatly embossed into its side, as though the 
--      wasps themselves had worked it there."
--
--                                                                  — Neuromancer
```

Copyrighted quotes used under fair use. The precise alignment serves as a
watermark:

- 4-space indent for quote body
- continuation lines align to opening quote mark
- attribution right-justified with em-dash
- thematic resonance with the code's purpose

## // inline // headers

For annotating sections within lists or code blocks:

```haskell
-- ── section name ──────────────────────────────────────────────
```

The `──` bookends distinguish these from subsection dividers.

## Discouraged or Forbidden

- ASCII art when a superior Unicode alternative is available
- emojis (banned, pain of death)
- double-hyphen (`--`) where em-dash (`—`) is meant
- unattributed epigraphs
