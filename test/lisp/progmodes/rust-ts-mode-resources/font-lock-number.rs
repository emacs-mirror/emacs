fn main() {
    let x = 1usize;
//          ^ font-lock-number-face
//             ^ font-lock-type-face
    let x = 1_usize;
//          ^ font-lock-number-face
//             ^ font-lock-type-face
    let x = 1_f64;
//          ^ font-lock-number-face
//             ^ font-lock-type-face
    let x = 1.0f64;
//          ^ font-lock-number-face
//              ^ font-lock-type-face
    let x = 1.0_f64;
//          ^ font-lock-number-face
//              ^ font-lock-type-face
}
