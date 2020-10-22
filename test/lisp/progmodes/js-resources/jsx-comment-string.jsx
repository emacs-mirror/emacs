// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:

// The following tests go below any comments to avoid including
// misindented comments among the erroring lines.

// The JSX-like text in comments/strings should be treated like the enclosing
// syntax, not like JSX.

// <Foo>
void 0

"<Bar>"
void 0

<Chicken>
  {/* <Pork> */}
  <Beef attr="<Turkey>">
    Yum!
  </Beef>
</Chicken>
