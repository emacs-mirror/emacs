use 5.038;
use feature 'class';
no warnings 'experimental';

class A {
}

class C
  : isa(A)
{
  method with_sig_and_attr
    : lvalue
    ($top,$down)
  {
    return $top-$down;
  }
}

class D {
  field $decorated :param :reader(get_decoration);
  field $no_attributes = not_an(attribute)
}
say "done!";
