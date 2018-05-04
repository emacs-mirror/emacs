// -*- mode: js-jsx; -*-

var foo = <div></div>;

return (
  <div>
  </div>
  <div>
    <div></div>
    <div>
      <div></div>
    </div>
  </div>
);

React.render(
  <div>
    <div></div>
  </div>,
  {
    a: 1
  },
  <div>
    <div></div>
  </div>
);

return (
  // Sneaky!
  <div></div>
);

return (
  <div></div>
  // Sneaky!
);

React.render(
  <input
    />,
  {
    a: 1
  }
);

return (
  <div>
    {array.map(function () {
      return {
        a: 1
      };
    })}
  </div>
);

return (
  <div attribute={array.map(function () {
         return {
           a: 1
         };

         return {
           a: 1
         };

         return {
           a: 1
         };
       })}>
  </div>
);

// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:

// The following test has intentionally unclosed elements and should
// be placed below all other tests to prevent awkward indentation.

return (
  <div>
    {array.map(function () {
      return {
        a: 1
