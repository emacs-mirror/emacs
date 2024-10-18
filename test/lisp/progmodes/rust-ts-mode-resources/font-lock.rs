// -*- rust-ts-mode-indent-offset: 0 -*-
// Trait with function signature
trait Foo {
    fn foo();
//      ^ font-lock-function-name-face
}

// Macros
macro_rules! unsafe_foo {
    ($env:expr, $name:ident $(, $args:expr)*) => {
//    ^ font-lock-variable-name-face
//         ^ font-lock-type-face
//                ^ font-lock-variable-name-face
//                      ^ font-lock-type-face
//                          ^ font-lock-operator-face
//                                ^ font-lock-variable-name-face
//                                      ^ font-lock-type-face
//                                         ^ font-lock-operator-face
        {
            foo!($env, $name $(, $args)*);
//                ^ font-lock-variable-use-face
//                           ^ font-lock-operator-face
//                                     ^ font-lock-operator-face
        }
    };

fn main() {
    let x = 1usize;
//          ^ font-lock-number-face
//             ^ font-lock-number-face
    let x = 1_usize;
//          ^ font-lock-number-face
//             ^ font-lock-number-face
    let x = 1_f64;
//          ^ font-lock-number-face
//             ^ font-lock-number-face
    let x = 1.0f64;
//          ^ font-lock-number-face
//              ^ font-lock-number-face
    let x = 1.0_f64;
//            ^ font-lock-number-face
//               ^ font-lock-number-face
}
