// JSX text node values should be strings, but only JS string syntax
// is considered, so quote marks delimit strings like normal, with
// disastrous results (https://github.com/mooz/js2-mode/issues/409).
function Bug() {
  return <div>C'est Montréal</div>;
}
function Test(foo = /'/,
              bar = 123) {}

// This test is in a separate file because it can break other tests
// when indenting the whole buffer (not sure why).

// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:
