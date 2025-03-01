for idx, val := range arr {}
//   ^ font-lock-variable-name-face
//        ^ font-lock-variable-name-face
for idx := 0; idx < n; idx++ {}
//   ^ font-lock-variable-name-face

const (
	zero, one = 0, 1
//	 ^ font-lock-constant-face
//	       ^ font-lock-constant-face
)
