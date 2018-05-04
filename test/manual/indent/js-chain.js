// Normal chaining.
let x = svg.mumble()
           .zzz;

// Chaining with an intervening line comment.
let x = svg.mumble()		// line comment
           .zzz;

// Chaining with multiple dots.
let x = svg.selectAll().something()
           .zzz;

// Nested chaining.
let x = svg.selectAll(d3.svg.something()
                        .zzz);

// Nothing to chain to.
let x = svg()
    .zzz;

// Nothing to chain to.
let x = svg().mumble.x() + 73
    .zzz;

// Local Variables:
// indent-tabs-mode: nil
// js-chain-indent: t
// js-indent-level: 2
// End:
