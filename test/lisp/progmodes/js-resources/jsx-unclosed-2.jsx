// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:

// The following tests go below any comments to avoid including
// misindented comments among the erroring lines.

// Don’t misinterpret inequality operators as JSX.
for (; i < length;) void 0
if (foo > bar) void 0

// Don’t misintrepet inequalities within JSX, either.
<div>
  {foo < bar}
</div>

// Don’t even misinterpret unary operators as JSX.
if (foo < await bar) void 0
while (await foo > bar) void 0

<div>
  {foo < await bar}
</div>

// Allow unary keyword names as null-valued JSX attributes.
// (As if this will EVER happen…)
<Foo yield>
  <Bar void>
    <Baz
      zorp
      typeof>
      <Please do_n0t delete this_stupidTest >
        How would we ever live without unary support
      </Please>
    </Baz>
  </Bar>
</Foo>

// “-” is not allowed in a JSXBoundaryElement’s name.
<ABC />
  <A-B-C /> // Weirdly-indented “continued expression.”

// “-” may be used in a JSXAttribute’s name.
<Foo a-b-c=""
     x-y-z="" />

// Weird spaces should be tolerated.
< div >
  < div >
    < div
      attr=""
      / >
    < div
      attr=""
      / >
  < / div>
< / div >

// Non-ASCII identifiers are acceptable.
<Über>
  <Québec διακριτικός sueño="">
    Guten Tag!
  </Québec>
</Über>
