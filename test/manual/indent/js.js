var a = 1;
b = 2;

let c = 1,
    d = 2;

var e = 100500,
    + 1;

// Don't misinterpret "const"
/const/

function test ()
{
  return /[/]/.test ('/')     // (bug#19397)
}

var f = bar('/protocols/')
baz();

var h = 100500
1;

const i = 1,
      j = 2;

var k = 1,
    l = [
      1, 2,
      3, 4
    ],
    m = 5;

var n = function() {
  return 7;
},
    o = 8;

foo(bar, function() {
  return 2;
});

switch (b) {
case "a":
  2;
default:
  3;
}

var p = {
  case: 'zzzz',
  default: 'donkey',
  tee: 'ornery'
};

var evens = [e for each (e in range(0, 21))
               if (ed % 2 == 0)];

var funs = [
  function() {
    for (;;) {
    }
  },
  function(){},
];

!b
  !=b
  !==b

a++
b +=
  c

var re = /some value/
str.match(re)

baz(`http://foo.bar/${tee}`)
  .qux();

`multiline string
       contents
  are kept
        unchanged!`

class A {
  * x() {
    return 1
      * a(2);
  }

  *[Symbol.iterator]() {
    yield "Foo";
    yield "Bar";
  }
}

if (true)
  1
else
  2

Foobar
  .find()
  .catch((err) => {
    return 2;
  })
  .then((num) => {
    console.log(num);
  });

var z = [
  ...iterableObj,
  4,
  5
]

var arr = [
  -1, 2,
  -3, 4 +
    -5
];

// Regression test for bug#15582.
if (x > 72 &&
    y < 85) { // found
  do_something();
}

// Test that chaining doesn't happen when js-chain-indent is nil.
let x = svg.mumble()
    .zzz;

// https://github.com/mooz/js2-mode/issues/405
if (1) {
  isSet
    ? (isEmpty ? 2 : 3)
    : 4
}

// Regexp is not a continuation
bar(
  "string arg1",
  /abc/
)

// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:
