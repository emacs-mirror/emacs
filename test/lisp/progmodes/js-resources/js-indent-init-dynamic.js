var foo = function() {
  return 7;
};

var foo = function() {
      return 7;
    },
    bar = 8;

var foo = function() {
      return 7;
    },
    bar = function() {
      return 8;
    };

// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// js-indent-first-init: dynamic
// End:

// The following test intentionally produces a scan error and should
// be placed below all other tests to prevent awkward indentation.
// (It still thinks it's within the body of a function.)

var foo = function() {
  return 7;
  ,
  bar = 8;
