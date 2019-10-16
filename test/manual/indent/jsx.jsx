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

return (
  <div attribute={{
         a: 1, // Indent relative to “attribute” column.
         b: 2
       } && {  // Dedent to “attribute” column.
         a: 1,
         b: 2
       }} />   // Also dedent.
);

return (
  <div attribute=
       {   // Indent properly on another line, too.
         {
           a: 1,
           b: 2,
         } && (
           // Indent other forms, too.
           a ? b :
             c ? d :
             e
         )
       } />
)

// JSXMemberExpression names are parsed/indented:
<Foo.Bar>
  <div>
    <Foo.Bar>
      Hello World!
    </Foo.Bar>
    <Foo.Bar>
      <div>
      </div>
    </Foo.Bar>
  </div>
</Foo.Bar>

// JSXOpeningFragment and JSXClosingFragment are parsed/indented:
<>
  <div>
    <>
      Hello World!
    </>
    <>
      <div>
      </div>
    </>
  </div>
</>

// Indent void expressions (no need for contextual parens / commas)
// (https://github.com/mooz/js2-mode/issues/140#issuecomment-166250016).
<div className="class-name">
  <h2>Title</h2>
  {array.map(() => {
    return <Element />;
  })}
  {message}
</div>
// Another example of above issue
// (https://github.com/mooz/js2-mode/issues/490).
<App>
  <div>
    {variable1}
    <Component/>
  </div>
</App>

// Comments and arrows can break indentation (Bug#24896 /
// https://github.com/mooz/js2-mode/issues/389).
const Component = props => (
  <FatArrow a={e => c}
            b={123}>
  </FatArrow>
);
const Component = props => (
  <NoFatArrow a={123}
              b={123}>
  </NoFatArrow>
);
const Component = props => ( // Parse this comment, please.
  <FatArrow a={e => c}
            b={123}>
  </FatArrow>
);
const Component = props => ( // Parse this comment, please.
  <NoFatArrow a={123}
              b={123}>
  </NoFatArrow>
);
// Another example of above issue (Bug#30225).
class {
  render() {
    return (
      <select style={{paddingRight: "10px"}}
              onChange={e => this.setState({value: e.target.value})}
              value={this.state.value}>
        <option>Hi</option>
      </select>
    );
  }
}

// JSX attributes of an arrow function’s expression body’s JSX
// expression should be indented with respect to the JSX opening
// element (Bug#26001 /
// https://github.com/mooz/js2-mode/issues/389#issuecomment-271869380).
class {
  render() {
    const messages = this.state.messages.map(
      message => <Message key={message.id}
                          text={message.text}
                          mine={message.mine} />
    );    return messages;
  }
  render() {
    const messages = this.state.messages.map(message =>
      <Message key={message.timestamp}
               text={message.text}
               mine={message.mine} />
    );    return messages;
  }
}

// Users expect tag closers to align with the tag’s start; this is the
// style used in the React docs, so it should be the default.
// - https://github.com/mooz/js2-mode/issues/389#issuecomment-390766873
// - https://github.com/mooz/js2-mode/issues/482
// - Bug#32158
const foo = (props) => (
  <div>
    <input
      cat={i => i}
    />
    <button
      className="square"
    >
      {this.state.value}
    </button>
  </div>
);

// Embedded JSX in parens breaks indentation
// (https://github.com/mooz/js2-mode/issues/411).
let a = (
  <div>
    {condition && <Component/>}
    {condition && <Component/>}
    <div/>
  </div>
)
let b = (
  <div>
    {condition && (<Component/>)}
    <div/>
  </div>
)
let c = (
  <div>
    {condition && (<Component/>)}
    {condition && "something"}
  </div>
)
let d = (
  <div>
    {(<Component/>)}
    {condition && "something"}
  </div>
)
// Another example of the above issue (Bug#27000).
function testA() {
  return (
    <div>
      <div> { ( <div/> ) } </div>
    </div>
  );
}
function testB() {
  return (
    <div>
      <div> { <div/> } </div>
    </div>
  );
}
// Another example of the above issue
// (https://github.com/mooz/js2-mode/issues/451).
class Classy extends React.Component {
  render () {
    return (
      <div>
        <ul className="tocListRoot">
          { this.state.list.map((item) => {
            return (<div />)
          })}
        </ul>
      </div>
    )
  }
}

// Self-closing tags should be indented properly
// (https://github.com/mooz/js2-mode/issues/459).
export default ({ stars }) => (
  <div className='overlay__container'>
    <div className='overlay__header overlay--text'>
      Congratulations!
    </div>
    <div className='overlay__reward'>
      <Icon {...createIconProps(stars > 0)} size='large' />
      <div className='overlay__reward__bottom'>
        <Icon {...createIconProps(stars > 1)} size='small' />
        <Icon {...createIconProps(stars > 2)} size='small' />
      </div>
    </div>
    <div className='overlay__description overlay--text'>
      You have created <large>1</large> reminder
    </div>
  </div>
)

// JS expressions should not break indentation
// (https://github.com/mooz/js2-mode/issues/462).
//
// In the referenced issue, the user actually wanted indentation which
// was simply different than Emacs’ SGML attribute indentation.
// Nevertheless, his issue highlighted our inability to properly
// indent code with JSX inside JSXExpressionContainers inside JSX.
return (
  <Router>
    <Bar>
      <Route exact path="/foo"
             render={() => (
               <div>nothing</div>
             )} />
      <Route exact path="/bar" />
    </Bar>
  </Router>
)

// Local Variables:
// indent-tabs-mode: nil
// js-indent-level: 2
// End:
