// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:

// The following test goes below any comments to avoid including
// misindented comments among the erroring lines.

// Properly parse/indent code with a self-closing tag inside the
// attribute of another self-closing tag.
<div>
  <div attr={() => <div attr="" />} />
</div>
