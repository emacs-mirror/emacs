+// intentionally invalid syntax
+const THING: [u8; 48] = [];

// should recover here and highlight the text below
trait Foo() {
// ^ font-lock-keyword-face
}
