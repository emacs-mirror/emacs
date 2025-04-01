# SYNTAX TEST "Terraform.sublime-syntax"

/////////////////////////////////////////////////////////////////////
// INLINE COMMENTS
/////////////////////////////////////////////////////////////////////

/////
// Start of line inline comments with `#`.
/////

# inline comment
# ^ source.terraform comment.line.terraform

/////
// Start of line inline comments with `//`.
/////

// foo
# ^ source.terraform comment.line.terraform

/////
// Matches at random place in line + punctuation for `#`.
/////

        # bar
#     ^ -comment -punctuation
#       ^ punctuation.definition.comment.terraform
#       ^^^^^ comment.line.terraform

/////
// Matches at random place in line + punctuation for `//`.
/////

    // baz # blah
# ^ -comment -punctuation
#   ^^ punctuation.definition.comment.terraform
#   ^^^^^^^^^^^^^ comment.line.terraform

/////////////////////////////////////////////////////////////////////
// BLOCK COMMENTS
/////////////////////////////////////////////////////////////////////

/////
// Matches for a single line.
/////

    /* foo */
# ^ -comment -punctuation
#   ^^ punctuation.definition.comment.terraform
#   ^^^^^^^^ comment.block.terraform
#          ^^ punctuation.definition.comment.terraform

/////
// Matches over multiple lines.
/////

    /*
# ^ -comment -punctuation
#   ^^ punctuation.definition.comment.terraform
#   ^^^^ comment.block.terraform

  foo
# ^^^^ comment.block.terraform

    */
# ^^^^ comment.block.terraform
#   ^^ punctuation.definition.comment.terraform

/////
// Matches inline comments after block ends.
/////

     /* comment */    // inline
# ^ -comment -punctuation
#    ^^ punctuation.definition.comment.terraform
#    ^^^^^^^^^^^^ comment.block.terraform
#               ^^ punctuation.definition.comment.terraform
#                 ^^^ -comment -punctuation
#                     ^^ punctuation.definition.comment.terraform
#                     ^^^^^^^^ comment.line.terraform

/////////////////////////////////////////////////////////////////////
// LANGUAGE CONSTANTS
/////////////////////////////////////////////////////////////////////

/////
// Matches `true`.
/////

    true
# ^ -constant
#   ^^^^ constant.language.terraform
#         ^ -constant

/////
// Matches `false`.
/////

    false
# ^ -constant
#   ^^^^^ constant.language.terraform
#         ^ -constant


/////
// Matches `null`.
/////

    null
# ^ -constant
#   ^^^^ constant.language.terraform
#         ^ -constant

/////////////////////////////////////////////////////////////////////
// INTEGER CONSTANTS
/////////////////////////////////////////////////////////////////////

/////
// Matches integers.
/////

    444
# ^ -constant -numeric
#   ^^^ constant.numeric.integer.terraform

/////
// Matches zero.
/////

      0
# ^ -constant -numeric
#     ^ constant.numeric.integer.terraform

/////
// Matches one.
/////

      1
# ^ -constant -numeric
#     ^ constant.numeric.integer.terraform

/////
// Matches large integers.
/////

      26345645634561
# ^ -constant -numeric
#     ^^^^^^^^^^^^^^ constant.numeric.integer.terraform

/////
// Ignores integers inside identifiers.
/////

    aws_route53_zone.main.zone_id
#   ^^^^^^^^^^^^^^^^ -numeric -constant
#                   ^ punctuation.accessor.dot.terraform
#                    ^^^^ variable.other.member.terraform
#                        ^ punctuation.accessor.dot.terraform
#                         ^^^^^^^ variable.other.member.terraform

/////////////////////////////////////////////////////////////////////
// FLOATING-POINT CONSTANTS
/////////////////////////////////////////////////////////////////////

/////
// Matches floating-point numbers.
/////

      1.2
# ^ -constant -numeric
#     ^ constant.numeric.float.terraform
#      ^ punctuation.separator.decimal.terraform
#       ^ constant.numeric.float.terraform

/////
// Matches large floating-point numbers.
/////

      61.28888888888
#   ^ -constant -numeric
#     ^^ constant.numeric.float.terraform
#       ^ punctuation.separator.decimal.terraform
#        ^^^^^^^^^^^ constant.numeric.float.terraform

/////
// Matches integers with exponents.
/////

      1e12
#   ^ -constant -numeric
#     ^ constant.numeric.float.terraform
#      ^ punctuation.separator.exponent.terraform
#       ^^ constant.numeric.float.terraform

/////
// Matches floats with exponents.
/////

      1.4E12
#   ^ -constant -numeric
#     ^ constant.numeric.float.terraform
#      ^ punctuation.separator.decimal.terraform
#       ^ constant.numeric.float.terraform
#        ^ punctuation.separator.exponent.terraform
#         ^^ constant.numeric.float.terraform

/////
// Matches floats with postive signed exponents.
/////

      1.4e+12
#   ^ -constant -numeric
#     ^ constant.numeric.float.terraform
#      ^ punctuation.separator.decimal.terraform
#       ^ constant.numeric.float.terraform
#        ^^ punctuation.separator.exponent.terraform
#          ^^ constant.numeric.float.terraform

/////
// Matches floats with negative signed exponents.
/////

      1.4E-12
#   ^ -constant -numeric
#     ^ constant.numeric.float.terraform
#      ^ punctuation.separator.decimal.terraform
#       ^ constant.numeric.float.terraform
#        ^^ punctuation.separator.exponent.terraform
#          ^^ constant.numeric.float.terraform

/////////////////////////////////////////////////////////////////////
// STRINGS
/////////////////////////////////////////////////////////////////////

/////
// Matches punctuation and assigns meta_scope.
/////

      "a string"
#   ^ -punctuation -string
#     ^ punctuation.definition.string.begin.terraform
#     ^^^^^^^^^^ string.quoted.double.terraform
#              ^ punctuation.definition.string.end.terraform
#                 ^ -punctuation -string

/////
// Matches character escapes.
/////

      "a \n b \r c \t d \" e \udead f \udeadbeef"
#   ^ -punctuation -string
#     ^ punctuation.definition.string.begin.terraform
#     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double.terraform
#        ^^ constant.character.escape.terraform
#             ^^ constant.character.escape.terraform
#                  ^^ constant.character.escape.terraform
#                       ^^ constant.character.escape.terraform
#                            ^^^^^ constant.character.escape.terraform
#                                     ^^^^^^^^^^ constant.character.escape.terraform
#                                               ^ punctuation.definition.string.end.terraform

/////////////////////////////////////////////////////////////////////
// Identifiers
/////////////////////////////////////////////////////////////////////

      this_is_an_identifier
#     ^^^^^^^^^^^^^^^^^^^^^ variable.other.readwrite.terraform

      identifier.member_access
#     ^^^^^^^^^^ variable.other.readwrite.terraform
#               ^ punctuation.accessor.dot.terraform
#                ^^^^^^^^^^^^^ variable.other.member.terraform

      identifier-with-hyphens
#     ^^^^^^^^^^^^^^^^^^^^^^^ variable.other.readwrite.terraform

      __EOF__
#     ^^^^^^^ variable.other.readwrite.terraform

      identifier.
// comment
# <- comment

/////////////////////////////////////////////////////////////////////
// STRING INTERPOLATION
/////////////////////////////////////////////////////////////////////

/////
// Correct scopes during interpolation.
/////

      "some ${interpolation} string"
#   ^ -punctuation -string
#     ^ punctuation.definition.string.begin.terraform
#     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.string.terraform
#      ^^^^^ string.quoted.double.terraform
#           ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#             ^^^^^^^^^^^^^^ meta.interpolation.terraform
#              ^^^^^^^^^^^^ source.terraform
#                          ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                            ^^^^^^^ string.quoted.double.terraform
#                                  ^ punctuation.definition.string.end.terraform
#                                   ^ -punctuation -string

/////
// Matches left-trim and right-trim.
/////

      "%{~ fff ~}"
#     ^^^^^^^^^^^^ meta.string.terraform
#      ^^^^^^^^^^ meta.interpolation.terraform
#     ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#      ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#        ^ meta.interpolation.terraform keyword.operator.template.trim.left.terraform
#         ^^^^^ source.terraform
#              ^ keyword.operator.template.trim.right.terraform
#               ^ punctuation.section.interpolation.end.terraform
#                ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Matches operators
/////

    "${ something ? true : false }"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#    ^^ punctuation.section.interpolation.begin.terraform
#     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform
#      ^^^^^^^^^^^^^^^^^^^^^^^^^^ source.terraform
#                 ^ keyword.operator.terraform
#                   ^^^^ meta.interpolation.terraform constant.language.terraform
#                        ^ meta.interpolation.terraform keyword.operator.terraform
#                          ^^^^^ meta.interpolation.terraform constant.language.terraform
#                                ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                 ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Dot-access attributes in string interpolation
/////

    "hello ${aws_instance.ubuntu}"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#   ^^^^^^^ string.quoted.double.terraform
#          ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#            ^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform
#                        ^ meta.interpolation.terraform punctuation.accessor.dot.terraform
#                         ^^^^^^ meta.interpolation.terraform variable.other.member.terraform
#                               ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Handles function calls
/////

      "${formatdate("DD MMM YYYY hh:mm ZZZ", "2018-01-02T23:12:01Z")}"
#     ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#      ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#        ^^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform support.function.builtin.terraform
#                  ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                   ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform
#                                          ^ meta.interpolation.terraform meta.function-call.terraform punctuation.separator.terraform
#                                            ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                             ^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform
#                                                                  ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                                   ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                                                    ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Handles nested function calls.
/////

    id = "db-final-snapshot-${md5(timestamp())}"
#   ^^ variable.declaration.terraform variable.other.readwrite.terraform
#      ^ keyword.operator.assignment.terraform
#        ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#         ^^^^^^^^^^^^^^^^^^ string.quoted.double.terraform
#                           ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                             ^^^ meta.interpolation.terraform meta.function-call.terraform support.function.builtin.terraform
#                                ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                 ^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                                          ^ meta.interpolation.terraform meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                           ^ meta.interpolation.terraform meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                            ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                             ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                              ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Includes objects.
////

    "something ${{test = 3}}"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#    ^^^^^^^^^^ string.quoted.double.terraform
#              ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                ^ meta.interpolation.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#                 ^^^^ meta.interpolation.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                      ^ meta.interpolation.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                        ^ meta.interpolation.terraform meta.braces.terraform constant.numeric.integer.terraform
#                         ^ meta.interpolation.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                          ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                           ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Includes tuples.
////

    "something ${[1, 2, 3]}"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#    ^^^^^^^^^^ string.quoted.double.terraform
#              ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                ^ meta.interpolation.terraform punctuation.section.brackets.begin.terraform
#                 ^ meta.interpolation.terraform constant.numeric.integer.terraform
#                  ^ meta.interpolation.terraform punctuation.separator.terraform
#                    ^ meta.interpolation.terraform constant.numeric.integer.terraform
#                     ^ meta.interpolation.terraform punctuation.separator.terraform
#                       ^ meta.interpolation.terraform constant.numeric.integer.terraform
#                        ^ meta.interpolation.terraform punctuation.section.brackets.end.terraform
#                         ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                          ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Includes named values.
/////

    "${var.something}"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#    ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#      ^^^ meta.interpolation.terraform variable.language.terraform
#         ^ meta.interpolation.terraform punctuation.accessor.dot.terraform
#          ^^^^^^^^^ meta.interpolation.terraform variable.other.member.terraform
#                   ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                    ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////
// Handles regexes (various punctuation).
/////

    records = ["${replace("hostname.domain.com:1234", "/(.*):[0-9]{0,26}/", "$1")}"]
#   ^^^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#           ^ keyword.operator.assignment.terraform
#             ^ punctuation.section.brackets.begin.terraform
#              ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                 ^^^^^^^ meta.interpolation.terraform meta.function-call.terraform support.function.builtin.terraform
#                        ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                         ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                          ^^^^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform
#                                                  ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                   ^ meta.interpolation.terraform meta.function-call.terraform punctuation.separator.terraform
#                                                     ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                      ^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform
#                                                                        ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                                         ^ meta.interpolation.terraform meta.function-call.terraform punctuation.separator.terraform
#                                                                           ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                                            ^^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform
#                                                                              ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                                               ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                                                ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                                                                 ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                                                  ^ punctuation.section.brackets.end.terraform
#                                                                                   ^ -meta -string -variable -punctuation

/////
// Handles nested interpolation.
/////

    "${file("${path.module}/text_files/ecs_app")}"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#    ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#      ^^^^ meta.interpolation.terraform meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^ meta.interpolation.terraform meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#              ^^^^ meta.interpolation.terraform meta.function-call.terraform meta.interpolation.terraform variable.language.terraform
#                  ^ meta.interpolation.terraform meta.function-call.terraform meta.interpolation.terraform punctuation.accessor.dot.terraform
#                   ^^^^^^ meta.interpolation.terraform meta.function-call.terraform meta.interpolation.terraform variable.other.member.terraform
#                         ^ meta.interpolation.terraform meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                          ^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform
#                                             ^ meta.interpolation.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                              ^ meta.interpolation.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                               ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                                ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////////////////////////////////////////////////////////////////////
// Template If Directives
/////////////////////////////////////////////////////////////////////

/////
// Matches if/endif directives.
/////

      "${ if name == "Mary" }${name}${ endif ~}"
#    ^ -string -punctuation
#     ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#      ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform
#         ^^ meta.interpolation.terraform keyword.control.terraform
#                 ^^ meta.interpolation.terraform keyword.operator.terraform
#                    ^ source.terraform meta.interpolation.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^^^^^ source.terraform meta.interpolation.terraform string.quoted.double.terraform
#                         ^ source.terraform meta.interpolation.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                           ^ punctuation.section.interpolation.end.terraform
#                            ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                  ^ punctuation.section.interpolation.end.terraform
#                                   ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                      ^^^^^ meta.interpolation.terraform keyword.control.terraform
#                                            ^ meta.interpolation.terraform keyword.operator.template.trim.right.terraform
#                                             ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                              ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                               ^ -string -punctuation

/////
// Matches if/else/endif directives.
/////

      "%{ if name == "Mary" }${name}%{ else }${ "Mary" }%{ endif ~}"
#    ^ -string -punctuation
#     ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#      ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform
#         ^^ meta.interpolation.terraform keyword.control.terraform
#                 ^^ meta.interpolation.terraform keyword.operator.terraform
#                    ^ source.terraform meta.interpolation.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^^^^^ source.terraform meta.interpolation.terraform string.quoted.double.terraform
#                         ^ source.terraform meta.interpolation.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                           ^ punctuation.section.interpolation.end.terraform
#                            ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                  ^ punctuation.section.interpolation.end.terraform
#                                   ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                      ^^^^ meta.interpolation.terraform keyword.control.terraform
#                                           ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                            ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                               ^ meta.interpolation.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                ^^^^^ meta.interpolation.terraform string.quoted.double.terraform
#                                                      ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                                      ^ punctuation.section.interpolation.end.terraform
#                                                       ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                                          ^^^^^ meta.interpolation.terraform keyword.control.terraform
#                                                                ^ meta.interpolation.terraform keyword.operator.template.trim.right.terraform
#                                                                 ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                                                  ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                                   ^ -string -punctuation

/////
// Matches for/in/endfor directives.
/////

      "%{ for name in var.names ~}${name}%{ endfor ~}"
#   ^ -string -punctuation
#     ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#      ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#         ^^^ keyword.control.terraform
#      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.interpolation.terraform
#                  ^^ keyword.control.terraform
#                     ^^^ meta.interpolation.terraform variable.language.terraform
#                        ^ meta.interpolation.terraform punctuation.accessor.dot.terraform
#                         ^^^^^ meta.interpolation.terraform variable.other.member.terraform
#                               ^ keyword.operator.template.trim.right.terraform
#                                ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                 ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                       ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                        ^^ meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                                           ^^^^^^ meta.interpolation.terraform keyword.control.terraform
#                                                  ^ meta.interpolation.terraform keyword.operator.template.trim.right.terraform
#                                                   ^ meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                                    ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                     ^ -string -punctuation

/////////////////////////////////////////////////////////////////////
// Operators
/////////////////////////////////////////////////////////////////////

/////
// Comparison
/////

    a == b
#   ^ -keyword -operator
#     ^^ keyword.operator.terraform
#         ^ -keyword -operator

    a != b
#   ^ -keyword -operator
#     ^^ keyword.operator.terraform
#         ^ -keyword -operator

    a < b
#    ^ -keyword -operator
#     ^ keyword.operator.terraform
#      ^ -keyword -operator

    a <= b
#    ^ -keyword -operator
#     ^^ keyword.operator.terraform
#       ^ -keyword -operator

    a > b
#    ^ -keyword -operator
#     ^ keyword.operator.terraform
#      ^ -keyword -operator

    a >= b
#    ^ -keyword -operator
#     ^^ keyword.operator.terraform
#       ^ -keyword -operator

/////
// Arithmetic
/////

    a + b
#    ^ -keyword -operator
#     ^ keyword.operator.arithmetic.terraform
#      ^ -keyword -operator

    a - b
#    ^ -keyword -operator
#     ^ keyword.operator.arithmetic.terraform
#      ^ -keyword -operator

    a * b
#    ^ -keyword -operator
#     ^ keyword.operator.arithmetic.terraform
#      ^ -keyword -operator

    a / b
#    ^ -keyword -operator
#     ^ keyword.operator.arithmetic.terraform
#      ^ -keyword -operator

    a % b
#    ^ -keyword -operator
#     ^ keyword.operator.arithmetic.terraform
#      ^ -keyword -operator

    -a
#  ^ -keyword -operator
#   ^ keyword.operator.arithmetic.terraform
#    ^ -keyword -operator

/////
// Logic
/////

    a && b
#   ^^ -keyword -operator
#     ^^ keyword.operator.logical.terraform
#       ^^ -keyword -operator

    a || b
#   ^^ -keyword -operator
#     ^^ keyword.operator.logical.terraform
#       ^^ -keyword -operator

    !a
# ^^ -keyword -operator
#   ^ keyword.operator.logical.terraform
#    ^^ -keyword -operator

/////
// Conditional
/////

    length(some_list) > 0 ? some_list[0] : default
#   ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^^^^^^^^^ meta.function-call.terraform
#                   ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                     ^ keyword.operator.terraform
#                       ^ constant.numeric.integer.terraform
#                         ^ keyword.operator.terraform
#                                    ^ punctuation.section.brackets.begin.terraform
#                                     ^ constant.numeric.integer.terraform
#                                      ^ punctuation.section.brackets.end.terraform
#                                        ^ keyword.operator.terraform

/////
// Ellipsis
/////

    hhh([55, 2453, 2]...)
#   ^^^ meta.function-call.terraform variable.function.terraform
#      ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#       ^ punctuation.section.brackets.begin.terraform
#        ^^ constant.numeric.integer.terraform
#          ^ punctuation.separator.terraform
#            ^^^^ constant.numeric.integer.terraform
#                ^ punctuation.separator.terraform
#                  ^ constant.numeric.integer.terraform
#                   ^ punctuation.section.brackets.end.terraform
#                    ^^^ keyword.operator.terraform
#                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////////////////////////////////////////////////////////////////////
// Brackets: Index Operations and Arrays
/////////////////////////////////////////////////////////////////////

/////
// Index Operations
/////

    thing[1]
#       ^ -punctuation
#        ^ punctuation.section.brackets.begin.terraform
#         ^ constant.numeric.integer.terraform
#          ^ punctuation.section.brackets.end.terraform
#           ^ -punctuation

/////
// Arrays of literals
/////

    ["a", "b", "c"]
#   ^ punctuation.section.brackets.begin.terraform
#    ^ punctuation.definition.string.begin.terraform
#    ^^^ string.quoted.double.terraform
#      ^ punctuation.definition.string.end.terraform
#       ^ punctuation.separator.terraform
#         ^ punctuation.definition.string.begin.terraform
#         ^^^ string.quoted.double.terraform
#           ^ punctuation.definition.string.end.terraform
#            ^ punctuation.separator.terraform
#              ^ punctuation.definition.string.begin.terraform
#              ^^^ string.quoted.double.terraform
#                ^ punctuation.definition.string.end.terraform
#                 ^ punctuation.section.brackets.end.terraform

/////
// Allows inline comments
/////

    [1, /* inline */ 2]
#   ^ punctuation.section.brackets.begin.terraform
#    ^ constant.numeric.integer.terraform
#     ^ punctuation.separator.terraform
#       ^^ punctuation.definition.comment.terraform
#       ^^^^^^^^^^^^ comment.block.terraform
#                 ^^ punctuation.definition.comment.terraform
#                    ^ constant.numeric.integer.terraform
#                     ^ punctuation.section.brackets.end.terraform

/////
// Allows expression over multiple lines
/////

    [
#   ^ punctuation.section.brackets.begin.terraform
      1,
#     ^ constant.numeric.integer.terraform
#      ^ punctuation.separator.terraform
      2
#     ^ constant.numeric.integer.terraform
    ]
#   ^ punctuation.section.brackets.end.terraform

/////
// Allows operators
/////

    [ 1 + 2 ]
#   ^ punctuation.section.brackets.begin.terraform
#     ^ constant.numeric.integer.terraform
#       ^ keyword.operator.arithmetic.terraform
#         ^ constant.numeric.integer.terraform
#           ^ punctuation.section.brackets.end.terraform

/////
// Splat operator
/////

    tuple[*].foo.bar[0]
#        ^ punctuation.section.brackets.begin.terraform
#         ^ punctuation.section.brackets.end.terraform keyword.operator.splat.terraform
#          ^ punctuation.section.brackets.end.terraform
#           ^ punctuation.accessor.dot.terraform
#            ^^^ variable.other.member.terraform
#               ^ punctuation.accessor.dot.terraform
#                ^^^ variable.other.member.terraform
#                   ^ punctuation.section.brackets.begin.terraform
#                    ^ constant.numeric.integer.terraform
#                     ^ punctuation.section.brackets.end.terraform

/////
// Handle nested arrays
/////

    counts = [
#   ^^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#          ^ keyword.operator.assignment.terraform
#            ^ punctuation.section.brackets.begin.terraform
      [ 1, 2],
#     ^ punctuation.section.brackets.begin.terraform
#       ^ constant.numeric.integer.terraform
#        ^ punctuation.separator.terraform
#          ^ constant.numeric.integer.terraform
#           ^ punctuation.section.brackets.end.terraform
#            ^ punctuation.separator.terraform
      [ 6, 7]
#     ^ punctuation.section.brackets.begin.terraform
#       ^ constant.numeric.integer.terraform
#        ^ punctuation.separator.terraform
#          ^ constant.numeric.integer.terraform
#           ^ punctuation.section.brackets.end.terraform
    ]
#   ^ punctuation.section.brackets.end.terraform
#    ^ -punctuation

/////
// Attribute-access inside arrays
/////

    [ aws_instance.ubuntu, aws_instance.freebsd ]
#   ^ punctuation.section.brackets.begin.terraform
#                 ^ punctuation.accessor.dot.terraform
#                  ^^^^^^ variable.other.member.terraform
#                        ^ punctuation.separator.terraform
#                                      ^ punctuation.accessor.dot.terraform
#                                       ^^^^^^^ variable.other.member.terraform
#                                               ^ punctuation.section.brackets.end.terraform

/////
// Includes functions.
/////

    [ upper("ggh") ]
#   ^ punctuation.section.brackets.begin.terraform
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                  ^ punctuation.section.brackets.end.terraform

/////
// Include objects.
/////

    [{a = 1}, {g = 2}]
#   ^ punctuation.section.brackets.begin.terraform
#    ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#     ^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#       ^ meta.braces.terraform keyword.operator.assignment.terraform
#         ^ meta.braces.terraform constant.numeric.integer.terraform
#          ^ meta.braces.terraform punctuation.section.braces.end.terraform
#           ^ punctuation.separator.terraform
#             ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#              ^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                ^ meta.braces.terraform keyword.operator.assignment.terraform
#                  ^ meta.braces.terraform constant.numeric.integer.terraform
#                   ^ meta.braces.terraform punctuation.section.braces.end.terraform
#                    ^ punctuation.section.brackets.end.terraform

/////
// Includes named values
/////

    [local.thing1, local.thing2]
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^^^ variable.language.terraform
#         ^ punctuation.accessor.dot.terraform
#          ^^^^^^ variable.other.member.terraform
#                ^ punctuation.separator.terraform
#                  ^^^^^ variable.language.terraform
#                       ^ punctuation.accessor.dot.terraform
#                        ^^^^^^ variable.other.member.terraform
#                              ^ punctuation.section.brackets.end.terraform

/////////////////////////////////////////////////////////////////////
// Collections: Objects
/////////////////////////////////////////////////////////////////////

/////
// Key/value pairs separated by newlines.
/////

    {
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
      name = "John"
#     ^^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#          ^ meta.braces.terraform keyword.operator.assignment.terraform
#            ^ meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^ meta.braces.terraform string.quoted.double.terraform
      age = 52
#     ^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#         ^ meta.braces.terraform keyword.operator.assignment.terraform
#           ^^ meta.braces.terraform constant.numeric.integer.terraform
    }
#   ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////
// Key/value pairs separated by commas.
/////

    {name = "John", age = 52}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#         ^ meta.braces.terraform keyword.operator.assignment.terraform
#           ^ meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^^ meta.braces.terraform string.quoted.double.terraform
#                 ^ meta.braces.terraform punctuation.separator.terraform
#                   ^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                           ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////
// Allows operators in key values.
/////

    { name = 1 + 1 }
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#     ^^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#          ^ meta.braces.terraform keyword.operator.assignment.terraform
#            ^ meta.braces.terraform constant.numeric.integer.terraform
#              ^ meta.braces.terraform keyword.operator.arithmetic.terraform
#                ^ meta.braces.terraform constant.numeric.integer.terraform
#                  ^ meta.braces.terraform punctuation.section.braces.end.terraform
#                   ^ -meta

/////
// Allows tuples as key values.
/////

    { list = [ 1, 2, 3 ]}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#     ^^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#          ^ meta.braces.terraform keyword.operator.assignment.terraform
#            ^ meta.braces.terraform punctuation.section.brackets.begin.terraform
#              ^ meta.braces.terraform constant.numeric.integer.terraform
#               ^ meta.braces.terraform punctuation.separator.terraform
#                 ^ meta.braces.terraform constant.numeric.integer.terraform
#                  ^ meta.braces.terraform punctuation.separator.terraform
#                    ^ meta.braces.terraform constant.numeric.integer.terraform
#                      ^ meta.braces.terraform punctuation.section.brackets.end.terraform
#                       ^ meta.braces.terraform punctuation.section.braces.end.terraform
#                        ^ -meta

/////
// Allows function calls as values.
/////

    {
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
      a = upper("l"),
#     ^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#       ^ meta.braces.terraform keyword.operator.assignment.terraform
#         ^^^^^ meta.braces.terraform meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.braces.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.braces.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^ meta.braces.terraform meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.braces.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                   ^ meta.braces.terraform punctuation.separator.terraform
    }
#   ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////
// Allows nested collection literals.
/////

    {
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
      obj1 = {
#     ^^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#          ^ meta.braces.terraform keyword.operator.assignment.terraform
#            ^ meta.braces.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
        obj2 = {
#       ^^^^ meta.braces.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#            ^ meta.braces.terraform meta.braces.terraform keyword.operator.assignment.terraform
#              ^ meta.braces.terraform meta.braces.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
          value = 5
#         ^^^^^ meta.braces.terraform meta.braces.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#               ^ meta.braces.terraform meta.braces.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                 ^ meta.braces.terraform meta.braces.terraform meta.braces.terraform constant.numeric.integer.terraform
        }
#       ^ meta.braces.terraform meta.braces.terraform meta.braces.terraform punctuation.section.braces.end.terraform
      }
#     ^ meta.braces.terraform meta.braces.terraform punctuation.section.braces.end.terraform
    }
#   ^ meta.braces.terraform punctuation.section.braces.end.terraform
#    ^ -meta

/////
// Allows attribute-access as rvalue, including named values.
/////

    { lvalue = var.rvalue }
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#     ^^^^^^ meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#            ^ meta.braces.terraform keyword.operator.assignment.terraform
#              ^^^ meta.braces.terraform variable.language.terraform
#                 ^ meta.braces.terraform punctuation.accessor.dot.terraform
#                  ^^^^^^ meta.braces.terraform variable.other.member.terraform
#                         ^ meta.braces.terraform punctuation.section.braces.end.terraform
#                          ^ -meta

/////
// Allows strings as keys.
/////

    {"gggg" = "gggg"}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^ meta.braces.terraform meta.mapping.key.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#     ^^^^ meta.braces.terraform meta.mapping.key.terraform string.quoted.double.terraform
#         ^ meta.braces.terraform meta.mapping.key.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#           ^ meta.braces.terraform keyword.operator.assignment.terraform
#             ^ meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^^^ meta.braces.terraform string.quoted.double.terraform
#                  ^ meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                   ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////
// Allows expressions + operators as keys.
/////

    {(1 + 2) = "gggg"}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^ meta.braces.terraform meta.mapping.key.terraform punctuation.section.parens.begin.terraform
#     ^ meta.braces.terraform meta.mapping.key.terraform constant.numeric.integer.terraform
#       ^ meta.braces.terraform meta.mapping.key.terraform keyword.operator.arithmetic.terraform
#         ^ meta.braces.terraform meta.mapping.key.terraform constant.numeric.integer.terraform
#          ^ meta.braces.terraform meta.mapping.key.terraform punctuation.section.parens.end.terraform
#            ^ meta.braces.terraform keyword.operator.terraform
#              ^ meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^^^^ meta.braces.terraform string.quoted.double.terraform
#                    ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////
// Allows function calls as keys.
/////

    {(func()) = 1}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^ meta.braces.terraform meta.mapping.key.terraform punctuation.section.parens.begin.terraform
#     ^^^^ meta.braces.terraform meta.mapping.key.terraform meta.function-call.terraform variable.function.terraform
#         ^ meta.braces.terraform meta.mapping.key.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.braces.terraform meta.mapping.key.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#           ^ meta.braces.terraform meta.mapping.key.terraform punctuation.section.parens.end.terraform
#             ^ meta.braces.terraform keyword.operator.terraform
#               ^ meta.braces.terraform constant.numeric.integer.terraform
#                ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////
// Allows attribute-access as keys.
////

    {(var.path) = 1}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^ meta.braces.terraform meta.mapping.key.terraform punctuation.section.parens.begin.terraform
#     ^^^ meta.braces.terraform meta.mapping.key.terraform variable.language.terraform
#        ^ meta.braces.terraform meta.mapping.key.terraform punctuation.accessor.dot.terraform
#         ^^^^ meta.braces.terraform meta.mapping.key.terraform variable.other.member.terraform
#             ^ meta.braces.terraform meta.mapping.key.terraform punctuation.section.parens.end.terraform
#               ^ meta.braces.terraform keyword.operator.terraform
#                 ^ meta.braces.terraform constant.numeric.integer.terraform
#                  ^ meta.braces.terraform punctuation.section.braces.end.terraform

/////////////////////////////////////////////////////////////////////
// Attribute Access
/////////////////////////////////////////////////////////////////////

/////
// Matches dot-access
/////

    aws_instance.ubuntu[*].private_dns
#               ^ punctuation.accessor.dot.terraform
#                ^^^^^^ variable.other.member.terraform
#                      ^ punctuation.section.brackets.begin.terraform
#                       ^ punctuation.section.brackets.end.terraform keyword.operator.splat.terraform
#                        ^ punctuation.section.brackets.end.terraform
#                         ^ punctuation.accessor.dot.terraform
#                          ^^^^^^^^^^^ variable.other.member.terraform
#                                     ^ -variable -punctuation

/////
// Ignores dot-access in string literals
/////

    "aws_instance.ubuntu"
#   ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^ -variable
#   ^^^^^^^^^^^^^^^^^^^^^ string.quoted.double.terraform

/////
// Matches inside for-expressions
/////

   [for l in var.letters: upper(l)]
#  ^ punctuation.section.brackets.begin.terraform
#   ^^^ keyword.control.loop.for.terraform
#       ^ variable.other.readwrite.terraform
#         ^^ keyword.control.loop.in.terraform
#            ^^^ variable.language.terraform
#               ^ punctuation.accessor.dot.terraform
#                ^^^^^^^ variable.other.member.terraform
#                       ^ punctuation.section.block.loop.for.terraform
#                         ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                               ^ meta.function-call.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                 ^ punctuation.section.brackets.end.terraform

/////
// Attribute-only splat
/////

    tuple.*.foo.bar[0]
#        ^ punctuation.accessor.dot.terraform
#         ^ keyword.operator.splat.terraform
#          ^ punctuation.accessor.dot.terraform
#           ^^^ variable.other.member.terraform
#              ^ punctuation.accessor.dot.terraform
#               ^^^ variable.other.member.terraform
#                  ^ punctuation.section.brackets.begin.terraform
#                   ^ constant.numeric.integer.terraform
#                    ^ punctuation.section.brackets.end.terraform

/////
// Matches subscript indexes
/////

    aws_route53_zone.project.name_servers.1
#                   ^ punctuation.accessor.dot.terraform
#                    ^^^^^^^ variable.other.member.terraform
#                           ^ punctuation.accessor.dot.terraform
#                            ^^^^^^^^^^^^ variable.other.member.terraform
#                                        ^ punctuation.accessor.dot.terraform
#                                         ^ constant.numeric.integer.terraform
#                                          ^ -constant -punctuation -variable

/////////////////////////////////////////////////////////////////////
// Attribute Definition
/////////////////////////////////////////////////////////////////////

/////
// Basic definition
/////

    attribute = "string"
#   ^^^^^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#             ^ keyword.operator.assignment.terraform
#               ^^^^^^^^ meta.string.terraform string.quoted.double.terraform
#               ^ punctuation.definition.string.begin.terraform
#                      ^ punctuation.definition.string.end.terraform

    provider = google-beta.impersonated
#   ^^^^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#            ^ keyword.operator.assignment.terraform
#              ^^^^^^^^^^^ variable.other.readwrite.terraform
#                         ^ punctuation.accessor.dot.terraform
#                          ^^^^^^^^^^^^ variable.other.member.terraform


/////
// Meta-arguments
////

    for_each = toset([])
#   ^^^^^^^^ keyword.control.loop.for.terraform
#            ^ keyword.operator.assignment.terraform
#              ^^^^^^^^^ meta.function-call.terraform
#              ^^^^^ support.function.builtin.terraform
#                   ^ punctuation.section.parens.begin.terraform
#                    ^ punctuation.section.brackets.begin.terraform
#                     ^ punctuation.section.brackets.end.terraform
#                      ^ punctuation.section.parens.end.terraform

    count = length(var.availability_zones)
#   ^^^^^ variable.declaration.terraform keyword.control.conditional.terraform
#         ^ keyword.operator.assignment.terraform
#           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform
#           ^^^^^^ support.function.builtin.terraform
#                 ^ punctuation.section.parens.begin.terraform
#                  ^^^ variable.language.terraform
#                     ^ punctuation.accessor.dot.terraform
#                      ^^^^^^^^^^^^^^^^^^ variable.other.member.terraform
#                                        ^ punctuation.section.parens.end.terraform

/////
// Populate an attribute from a variable value
/////

    (foo) = "baz"
#   ^ punctuation.section.parens.begin.terraform
#    ^^^ variable.declaration.terraform variable.other.readwrite.terraform
#       ^ punctuation.section.parens.end.terraform
#         ^ keyword.operator.assignment.terraform
#           ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#           ^^^^^ string.quoted.double.terraform
#               ^ string.quoted.double.terraform punctuation.definition.string.end.terraform

/////////////////////////////////////////////////////////////////////
// Function Calls
/////////////////////////////////////////////////////////////////////

/////
// Basic call.
/////

    thing(l)
#   ^^^^^ meta.function-call.terraform variable.function.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^ meta.function-call.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Matches parameters, attribute-access, literals, operators, commas.
/////

    cidrthingy(aws_vpc.main.cidr_block, 4, count.index+1)
#   ^^^^^^^^^^ meta.function-call.terraform variable.function.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^^^^^^^ meta.function-call.terraform
#                     ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                      ^^^^ meta.function-call.terraform variable.other.member.terraform
#                          ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                           ^^^^^^^^^^ meta.function-call.terraform variable.other.member.terraform
#                                     ^ meta.function-call.terraform punctuation.separator.terraform
#                                       ^ meta.function-call.terraform constant.numeric.integer.terraform
#                                        ^ meta.function-call.terraform punctuation.separator.terraform
#                                          ^^^^^ meta.function-call.terraform
#                                               ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                                                ^^^^^ meta.function-call.terraform variable.other.member.terraform
#                                                     ^ meta.function-call.terraform keyword.operator.arithmetic.terraform
#                                                      ^ meta.function-call.terraform constant.numeric.integer.terraform
#                                                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                        ^ -meta -function-call -variable

/////
// Matches arrays and splat as parameters.
/////

      y6y([55, 2453, 2]..., [55555555])
#     ^^^ meta.function-call.terraform variable.function.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#          ^^ meta.function-call.terraform constant.numeric.integer.terraform
#            ^ meta.function-call.terraform punctuation.separator.terraform
#              ^^^^ meta.function-call.terraform constant.numeric.integer.terraform
#                  ^ meta.function-call.terraform punctuation.separator.terraform
#                    ^ meta.function-call.terraform constant.numeric.integer.terraform
#                     ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                      ^^^ meta.function-call.terraform keyword.operator.terraform
#                         ^ meta.function-call.terraform punctuation.separator.terraform
#                           ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                            ^^^^^^^^ meta.function-call.terraform constant.numeric.integer.terraform
#                                    ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Matches objects as parameters.
/////

    some({a = 1, b = "2"})
#   ^^^^ meta.function-call.terraform variable.function.terraform
#       ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#        ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#         ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#           ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#             ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#              ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                  ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                    ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                     ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                        ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Nested function calls.
/////

    func(thing(yep(1)))
#   ^^^^ meta.function-call.terraform variable.function.terraform
#       ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#        ^^^^^ meta.function-call.terraform meta.function-call.terraform variable.function.terraform
#             ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^^^ meta.function-call.terraform meta.function-call.terraform meta.function-call.terraform variable.function.terraform
#                 ^ meta.function-call.terraform meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform meta.function-call.terraform meta.function-call.terraform constant.numeric.integer.terraform
#                   ^ meta.function-call.terraform meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                    ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                      ^ -function

/////
// Parameters spanning multiple lines.
/////

    func(
#   ^^^^ meta.function-call.terraform variable.function.terraform
#       ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
      1,
#     ^ meta.function-call.terraform constant.numeric.integer.terraform
#      ^ meta.function-call.terraform punctuation.separator.terraform
      2
#     ^ meta.function-call.terraform constant.numeric.integer.terraform
    )
#   ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Allow object for-expressions.
////

    thing({for i, v in ["a"]: v => i...})
#   ^^^^^ meta.function-call.terraform variable.function.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#          ^^^ meta.function-call.terraform meta.braces.terraform keyword.control.loop.for.terraform
#              ^ meta.function-call.terraform variable.other.readwrite.terraform
#               ^ meta.function-call.terraform punctuation.separator.terraform
#                 ^ meta.function-call.terraform variable.other.readwrite.terraform
#                   ^^ meta.function-call.terraform keyword.control.loop.in.terraform
#                      ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                       ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^ meta.function-call.terraform string.quoted.double.terraform
#                          ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                           ^ meta.function-call.terraform punctuation.section.block.loop.for.terraform
#                             ^ meta.function-call.terraform variable.other.readwrite.terraform
#                               ^^ meta.function-call.terraform punctuation.separator.key-value.terraform
#                                  ^ meta.function-call.terraform variable.other.readwrite.terraform
#                                   ^^^ meta.function-call.terraform keyword.operator.terraform
#                                      ^ meta.function-call.terraform punctuation.section.braces.end.terraform
#                                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Allow tuple for-expressions.
/////

    func([for v in ["a", "b"]: v])
#   ^^^^ meta.function-call.terraform variable.function.terraform
#       ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#        ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#         ^^^ meta.function-call.terraform keyword.control.loop.for.terraform
#             ^ meta.function-call.terraform variable.other.readwrite.terraform
#               ^^ meta.function-call.terraform keyword.control.loop.in.terraform
#                  ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                   ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.separator.terraform
#                        ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                         ^^ meta.function-call.terraform string.quoted.double.terraform
#                           ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                            ^ meta.function-call.terraform punctuation.section.block.loop.for.terraform
#                              ^ meta.function-call.terraform variable.other.readwrite.terraform
#                               ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////////////////////////////////////////////////////////////////////
// Built-in Terraform Functions
// TODO: match % placeholders in format()-family first parameters
// TODO: match regexs in regex()-family first parameters
////////////////////////`/////////////////////////////////////////////

/////
// Numeric Functions
/////

      abs(23)
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^^ meta.function-call.terraform constant.numeric.integer.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      ceil(5.1)
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^^^ meta.function-call.terraform constant.numeric.float.terraform
#           ^ meta.function-call.terraform constant.numeric.float.terraform punctuation.separator.decimal.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      floor(5)
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform constant.numeric.integer.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      log(50, 10)
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^^ meta.function-call.terraform constant.numeric.integer.terraform
#           ^ meta.function-call.terraform punctuation.separator.terraform
#             ^^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      max(12, 54, 3)
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^^ meta.function-call.terraform constant.numeric.integer.terraform
#           ^ meta.function-call.terraform punctuation.separator.terraform
#             ^^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.separator.terraform
#                 ^ meta.function-call.terraform constant.numeric.integer.terraform
#                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      min(12, 54, 3)
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^^ meta.function-call.terraform constant.numeric.integer.terraform
#           ^ meta.function-call.terraform punctuation.separator.terraform
#             ^^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.separator.terraform
#                 ^ meta.function-call.terraform constant.numeric.integer.terraform
#                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      pow(3, 2)
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^ meta.function-call.terraform constant.numeric.integer.terraform
#          ^ meta.function-call.terraform punctuation.separator.terraform
#            ^ meta.function-call.terraform constant.numeric.integer.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      signum(-13)
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform keyword.operator.arithmetic.terraform
#             ^^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// String Functions
/////

      chomp("hello\n")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                 ^^ meta.function-call.terraform string.quoted.double.terraform constant.character.escape.terraform
#                   ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      format("Hello, %s!", "Ander")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                        ^ meta.function-call.terraform punctuation.separator.terraform
#                          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                           ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                 ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      formatlist("Hello, %s!", ["Valentina", "Ander"])
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform punctuation.separator.terraform
#                              ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                ^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                          ^ meta.function-call.terraform punctuation.separator.terraform
#                                            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                             ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                   ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      indent(2, "[\n  foo,\n  bar,\n]\n")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform constant.numeric.integer.terraform
#             ^ meta.function-call.terraform punctuation.separator.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      join(", ", ["foo", "bar", "baz"])
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#           ^^^ meta.function-call.terraform string.quoted.double.terraform
#              ^ meta.function-call.terraform punctuation.separator.terraform
#                ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                 ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.separator.terraform
#                        ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                         ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                             ^ meta.function-call.terraform punctuation.separator.terraform
#                               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                    ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      lower("HELLO")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      regex("[a-z]+", "53453453.345345aaabbbccc23454")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                   ^ meta.function-call.terraform punctuation.separator.terraform
#                     ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      regexall("[a-z]+", "1234abcd5678efgh9")
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.separator.terraform
#                        ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                         ^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      replace("1 + 2 + 3", "+", "-")
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                        ^ meta.function-call.terraform punctuation.separator.terraform
#                          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                           ^^ meta.function-call.terraform string.quoted.double.terraform
#                             ^ meta.function-call.terraform punctuation.separator.terraform
#                               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                ^^ meta.function-call.terraform string.quoted.double.terraform
#                                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      split(",", "foo")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^ meta.function-call.terraform string.quoted.double.terraform
#              ^ meta.function-call.terraform punctuation.separator.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      strrev("hello")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                   ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      substr("🤔🤷", 0, 1)
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^ meta.function-call.terraform string.quoted.double.terraform
#                ^ meta.function-call.terraform punctuation.separator.terraform
#                  ^ meta.function-call.terraform constant.numeric.integer.terraform
#                   ^ meta.function-call.terraform punctuation.separator.terraform
#                     ^ meta.function-call.terraform constant.numeric.integer.terraform
#                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      title("hello world")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                        ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      trimspace("  hello\n\n")
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      upper("hello")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Collection Functions
/////

      chunklist(["a", "b"], 2)
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                ^^^ meta.function-call.terraform string.quoted.double.terraform
#                   ^ meta.function-call.terraform punctuation.separator.terraform
#                     ^^^ meta.function-call.terraform string.quoted.double.terraform
#                        ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                         ^ meta.function-call.terraform punctuation.separator.terraform
#                           ^ meta.function-call.terraform constant.numeric.integer.terraform
#                            ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      coalesce("a", "b")
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^ meta.function-call.terraform string.quoted.double.terraform
#                 ^ meta.function-call.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      coalescelist([], ["c", "d"])
#     ^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                   ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                    ^ meta.function-call.terraform punctuation.separator.terraform
#                      ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                       ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^ meta.function-call.terraform string.quoted.double.terraform
#                          ^ meta.function-call.terraform punctuation.separator.terraform
#                            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                             ^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      compact(["a", "", "b"])
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^ meta.function-call.terraform string.quoted.double.terraform
#                 ^ meta.function-call.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                     ^ meta.function-call.terraform punctuation.separator.terraform
#                       ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^ meta.function-call.terraform string.quoted.double.terraform
#                          ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      concat(["a"], ["c"])
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^ meta.function-call.terraform string.quoted.double.terraform
#                ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                 ^ meta.function-call.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                    ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                     ^^ meta.function-call.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                        ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      contains(["a"], "a")
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^ meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                   ^ meta.function-call.terraform punctuation.separator.terraform
#                     ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^ meta.function-call.terraform string.quoted.double.terraform
#                        ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      distinct(["a", "b", "a"])
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^ meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.function-call.terraform punctuation.separator.terraform
#                    ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                     ^^ meta.function-call.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform punctuation.separator.terraform
#                         ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                          ^^ meta.function-call.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                             ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      element(["a", "b"], 1)
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^ meta.function-call.terraform string.quoted.double.terraform
#                 ^ meta.function-call.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                       ^ meta.function-call.terraform punctuation.separator.terraform
#                         ^ meta.function-call.terraform constant.numeric.integer.terraform
#                          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      flatten([[["a", "b"]], ["c"]])
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^^^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^ meta.function-call.terraform string.quoted.double.terraform
#                   ^ meta.function-call.terraform punctuation.separator.terraform
#                     ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^ meta.function-call.terraform string.quoted.double.terraform
#                        ^^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                          ^ meta.function-call.terraform punctuation.separator.terraform
#                            ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                              ^^ meta.function-call.terraform string.quoted.double.terraform
#                                ^^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      index(["a"], "b")
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^ meta.function-call.terraform string.quoted.double.terraform
#               ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                ^ meta.function-call.terraform punctuation.separator.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^ meta.function-call.terraform string.quoted.double.terraform
#                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      keys({a=1, c=2})
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#           ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#            ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#             ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#              ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                 ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                  ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      length([])
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#             ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      list("a", "b", "c")
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#           ^^ meta.function-call.terraform string.quoted.double.terraform
#             ^ meta.function-call.terraform punctuation.separator.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^ meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.function-call.terraform punctuation.separator.terraform
#                    ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                     ^^ meta.function-call.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      lookup({a="ay", b="bee"}, "a", "what?")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#             ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#              ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#               ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                     ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                      ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                       ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                             ^ meta.function-call.terraform punctuation.separator.terraform
#                               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                ^^ meta.function-call.terraform string.quoted.double.terraform
#                                  ^ meta.function-call.terraform punctuation.separator.terraform
#                                    ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                     ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      map("a", "b")
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#          ^^ meta.function-call.terraform string.quoted.double.terraform
#            ^ meta.function-call.terraform punctuation.separator.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^ meta.function-call.terraform string.quoted.double.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      matchkeys(["i-123", "i-abc"], ["us-west", "us-east"], ["us-east"])
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform punctuation.separator.terraform
#                         ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                          ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                 ^ meta.function-call.terraform punctuation.separator.terraform
#                                   ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                                    ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                     ^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                             ^ meta.function-call.terraform punctuation.separator.terraform
#                                               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                ^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                        ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                                         ^ meta.function-call.terraform punctuation.separator.terraform
#                                                           ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                                                            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                             ^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                                     ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                                                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      merge({a="b"}, {e="f"})
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#            ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#             ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#              ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                 ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                  ^ meta.function-call.terraform punctuation.separator.terraform
#                    ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#                     ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                      ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                       ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                          ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      range(1, 4)
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform constant.numeric.integer.terraform
#            ^ meta.function-call.terraform punctuation.separator.terraform
#              ^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      reverse([1, 2, 3])
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#              ^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.separator.terraform
#                 ^ meta.function-call.terraform constant.numeric.integer.terraform
#                  ^ meta.function-call.terraform punctuation.separator.terraform
#                    ^ meta.function-call.terraform constant.numeric.integer.terraform
#                     ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      setintersection(["a", "b"], ["b", "c"])
#     ^^^^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                    ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                     ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                      ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                       ^^ meta.function-call.terraform string.quoted.double.terraform
#                         ^ meta.function-call.terraform punctuation.separator.terraform
#                           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                            ^^ meta.function-call.terraform string.quoted.double.terraform
#                              ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                               ^ meta.function-call.terraform punctuation.separator.terraform
#                                 ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                   ^^ meta.function-call.terraform string.quoted.double.terraform
#                                     ^ meta.function-call.terraform punctuation.separator.terraform
#                                       ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                        ^^ meta.function-call.terraform string.quoted.double.terraform
#                                          ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      setproduct(["development"], ["app1", "app2"])
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                 ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                              ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                               ^ meta.function-call.terraform punctuation.separator.terraform
#                                 ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                   ^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                        ^ meta.function-call.terraform punctuation.separator.terraform
#                                          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                           ^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                                 ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      setunion(["a"], ["b"], ["d"])
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^ meta.function-call.terraform string.quoted.double.terraform
#                  ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                   ^ meta.function-call.terraform punctuation.separator.terraform
#                     ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                      ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                       ^^ meta.function-call.terraform string.quoted.double.terraform
#                         ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                          ^ meta.function-call.terraform punctuation.separator.terraform
#                            ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                              ^^ meta.function-call.terraform string.quoted.double.terraform
#                                ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                                 ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      slice(["a", "b"], 1, 1)
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^ meta.function-call.terraform string.quoted.double.terraform
#               ^ meta.function-call.terraform punctuation.separator.terraform
#                 ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^ meta.function-call.terraform string.quoted.double.terraform
#                    ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                     ^ meta.function-call.terraform punctuation.separator.terraform
#                       ^ meta.function-call.terraform constant.numeric.integer.terraform
#                        ^ meta.function-call.terraform punctuation.separator.terraform
#                          ^ meta.function-call.terraform constant.numeric.integer.terraform
#                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform
      sort(["e", "d"])
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#           ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#            ^^ meta.function-call.terraform string.quoted.double.terraform
#              ^ meta.function-call.terraform punctuation.separator.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^ meta.function-call.terraform string.quoted.double.terraform
#                   ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      transpose({a = ["1", "2"], b = ["2", "3"]})
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#                ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                  ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                    ^ meta.function-call.terraform meta.braces.terraform punctuation.section.brackets.begin.terraform
#                     ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                        ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                          ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                           ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                             ^ meta.function-call.terraform meta.braces.terraform punctuation.section.brackets.end.terraform
#                              ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                                ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                                  ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                                    ^ meta.function-call.terraform meta.braces.terraform punctuation.section.brackets.begin.terraform
#                                     ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                      ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                                        ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                                          ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                           ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                                             ^ meta.function-call.terraform meta.braces.terraform punctuation.section.brackets.end.terraform
#                                              ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      values({a=3, c=2, d=1})
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#             ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#              ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#               ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                  ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                    ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                     ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                       ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                        ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                         ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                          ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      zipmap(["a", "b"], [1, 2])
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^ meta.function-call.terraform string.quoted.double.terraform
#                ^ meta.function-call.terraform punctuation.separator.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^ meta.function-call.terraform string.quoted.double.terraform
#                     ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                      ^ meta.function-call.terraform punctuation.separator.terraform
#                        ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#                         ^ meta.function-call.terraform constant.numeric.integer.terraform
#                          ^ meta.function-call.terraform punctuation.separator.terraform
#                            ^ meta.function-call.terraform constant.numeric.integer.terraform
#                             ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                              ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Encoding Functions
/////

      base64decode("SGVsbG8gV29ybGQ=")
#     ^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      base64encode("Hello World")
#     ^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      base64gzip("Hello World")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                             ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      csvdecode("a,b,c\n1,2,3\n4,5,6")
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      jsondecode("{\"hello\": \"world\"}")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                        ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      jsonencode({hello="world"})
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#                 ^^^^^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                      ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                       ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^^^^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                              ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      urlencode("Hello World")
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      yamldecode("true")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      yamlencode({a = "b", c = "d"})
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#                 ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                     ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                        ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                          ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                            ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                              ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                               ^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Filesystem Functions
/////

      abspath(path.root)
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^^^^ meta.function-call.terraform variable.language.terraform
#                 ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                  ^^^^ meta.function-call.terraform variable.other.member.terraform
#                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      dirname("foo/bar/baz.txt")
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                              ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      pathexpand("~/.ssh/id_rsa")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      basename("foo/bar/baz.txt")
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      file("${path.module}/hello.txt")
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#           ^^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#             ^^^^ meta.function-call.terraform meta.interpolation.terraform variable.language.terraform
#                 ^ meta.function-call.terraform meta.interpolation.terraform punctuation.accessor.dot.terraform
#                  ^^^^^^ meta.function-call.terraform meta.interpolation.terraform variable.other.member.terraform
#                        ^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                         ^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                    ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      fileexists("${path.module}/hello.txt")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                   ^^^^ meta.function-call.terraform meta.interpolation.terraform variable.language.terraform
#                       ^ meta.function-call.terraform meta.interpolation.terraform punctuation.accessor.dot.terraform
#                        ^^^^^^ meta.function-call.terraform meta.interpolation.terraform variable.other.member.terraform
#                              ^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                               ^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      fileset(path.module, "files/*.txt")
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^^^^ meta.function-call.terraform variable.language.terraform
#                 ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                  ^^^^^^ meta.function-call.terraform variable.other.member.terraform
#                        ^ meta.function-call.terraform punctuation.separator.terraform
#                          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                           ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      filebase64("${path.module}/hello.txt")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                   ^^^^ meta.function-call.terraform meta.interpolation.terraform variable.language.terraform
#                       ^ meta.function-call.terraform meta.interpolation.terraform punctuation.accessor.dot.terraform
#                        ^^^^^^ meta.function-call.terraform meta.interpolation.terraform variable.other.member.terraform
#                              ^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                               ^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      templatefile("${path.module}/backends.tmpl", {
#     ^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                     ^^^^ meta.function-call.terraform meta.interpolation.terraform variable.language.terraform
#                         ^ meta.function-call.terraform meta.interpolation.terraform punctuation.accessor.dot.terraform
#                          ^^^^^^ meta.function-call.terraform meta.interpolation.terraform variable.other.member.terraform
#                                ^ meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                 ^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                ^ meta.function-call.terraform punctuation.separator.terraform
#                                                  ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
        port = 8080,
#       ^^^^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#            ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#              ^^^^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                  ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform
        ip_addrs = ["10.0.0.1", "10.0.0.2"]
#       ^^^^^^^^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                  ^ meta.function-call.terraform meta.braces.terraform punctuation.section.brackets.begin.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^^^^^^^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                             ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                               ^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                ^^^^^^^^^ meta.function-call.terraform meta.braces.terraform string.quoted.double.terraform
#                                         ^ meta.function-call.terraform meta.braces.terraform punctuation.section.brackets.end.terraform
      })
#     ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Date & Time Functions
/////

      formatdate("DD MMM YYYY hh:mm ZZZ", "2018-01-02T23:12:01Z")
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                       ^ meta.function-call.terraform punctuation.separator.terraform
#                                         ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                          ^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      timeadd("2017-11-22T00:00:00Z", "10m")
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                   ^ meta.function-call.terraform punctuation.separator.terraform
#                                     ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                      ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      timestamp()
#     ^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Hash & Crypto Functions
/////

      base64sha256("hello world")
#     ^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      base64sha512("hello world")
#     ^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      bcrypt("hello world")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                         ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      filebase64sha256(file("filename"))
#     ^^^^^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                     ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                      ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                          ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                           ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                            ^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                                     ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                       ^ -function

      filebase64sha512(file("filename"))
#     ^^^^^^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                     ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                      ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                          ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                           ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                            ^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                                     ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                       ^ -function

      filemd5(file("filename"))
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                             ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      filemd1(file("filename"))
#     ^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                 ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                  ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                            ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                             ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                              ^ -function

      filesha256(file("filename"))
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                    ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                     ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                 ^ -function

      filesha512(file("filename"))
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                    ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                     ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                 ^ -function

      md5("hello world")
#     ^^^ meta.function-call.terraform support.function.builtin.terraform
#        ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#         ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#          ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      rsadecrypt(filebase64("${path.module}/ciphertext"), file("privatekey.pem"))
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                          ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                           ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                            ^^ meta.function-call.terraform meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.begin.terraform
#                              ^^^^ meta.function-call.terraform meta.function-call.terraform meta.interpolation.terraform variable.language.terraform
#                                  ^ meta.function-call.terraform meta.function-call.terraform meta.interpolation.terraform punctuation.accessor.dot.terraform
#                                   ^^^^^^ meta.function-call.terraform meta.function-call.terraform meta.interpolation.terraform variable.other.member.terraform
#                                         ^ meta.function-call.terraform meta.function-call.terraform meta.interpolation.terraform punctuation.section.interpolation.end.terraform
#                                          ^^^^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                                                      ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                       ^ meta.function-call.terraform punctuation.separator.terraform
#                                                         ^^^^ meta.function-call.terraform meta.function-call.terraform support.function.builtin.terraform
#                                                             ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                                              ^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                               ^^^^^^^^^^^^^^^ meta.function-call.terraform meta.function-call.terraform string.quoted.double.terraform
#                                                                              ^ meta.function-call.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                                                ^ -function

      sha1("hello world")
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#           ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                       ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      sha256("hello world")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                         ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      sha512("hello world")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                         ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      uuid()
#     ^^^^ meta.function-call.terraform support.function.builtin.terraform
#         ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      uuidv5("dns", "www.terraform.io")
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^ meta.function-call.terraform string.quoted.double.terraform
#                 ^ meta.function-call.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                    ^^^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// IP Network Functions
/////

      cidrhost("10.12.127.0/20", 16)
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                              ^ meta.function-call.terraform punctuation.separator.terraform
#                                ^^ meta.function-call.terraform constant.numeric.integer.terraform
#                                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      cidrnetmask("172.16.0.0/12")
#     ^^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                 ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      cidrsubnet("172.16.0.0/12", 4, 2)
#     ^^^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                 ^^^^^^^^^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                               ^ meta.function-call.terraform punctuation.separator.terraform
#                                 ^ meta.function-call.terraform constant.numeric.integer.terraform
#                                  ^ meta.function-call.terraform punctuation.separator.terraform
#                                    ^ meta.function-call.terraform constant.numeric.integer.terraform
#                                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform

/////
// Type Conversions Functions
/////

      tobool(true)
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^^^^ meta.function-call.terraform constant.language.terraform
#                ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      tolist(["a", "b", "c"])
#     ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#           ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#            ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#             ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^ meta.function-call.terraform string.quoted.double.terraform
#                ^ meta.function-call.terraform punctuation.separator.terraform
#                  ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                   ^^ meta.function-call.terraform string.quoted.double.terraform
#                     ^ meta.function-call.terraform punctuation.separator.terraform
#                       ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                        ^^ meta.function-call.terraform string.quoted.double.terraform
#                          ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                           ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      tomap({a = 1, b = 2})
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.begin.terraform
#            ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#              ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                 ^ meta.function-call.terraform meta.braces.terraform punctuation.separator.terraform
#                   ^ meta.function-call.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#                     ^ meta.function-call.terraform meta.braces.terraform keyword.operator.assignment.terraform
#                       ^ meta.function-call.terraform meta.braces.terraform constant.numeric.integer.terraform
#                        ^ meta.function-call.terraform meta.braces.terraform punctuation.section.braces.end.terraform
#                         ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      tonumber(1)
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform constant.numeric.integer.terraform
#               ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      toset(["a", "b", "c"])
#     ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#          ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#           ^ meta.function-call.terraform punctuation.section.brackets.begin.terraform
#            ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^ meta.function-call.terraform string.quoted.double.terraform
#               ^ meta.function-call.terraform punctuation.separator.terraform
#                 ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^ meta.function-call.terraform string.quoted.double.terraform
#                    ^ meta.function-call.terraform punctuation.separator.terraform
#                      ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                       ^^ meta.function-call.terraform string.quoted.double.terraform
#                         ^ meta.function-call.terraform punctuation.section.brackets.end.terraform
#                          ^ meta.function-call.terraform punctuation.section.parens.end.terraform

      tostring("hello")
#     ^^^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#              ^ meta.function-call.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#               ^^^^^^ meta.function-call.terraform string.quoted.double.terraform
#                     ^ meta.function-call.terraform punctuation.section.parens.end.terraform


    provider::terraform::encode_tfvars({
#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.terraform
#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ support.function.builtin.terraform
#                                     ^ punctuation.section.parens.begin.terraform
#                                      ^ meta.braces.terraform punctuation.section.braces.begin.terraform
      example = "Hello!"
    })

/////////////////////////////////////////////////////////////////////
// TUPLE FOR-EXPRESSIONS
/////////////////////////////////////////////////////////////////////

/////
// Basic expression.
/////

    [for s in var.list : upper(s)]
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^ keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#          ^^ keyword.control.loop.in.terraform
#             ^^^ variable.language.terraform
#                ^ punctuation.accessor.dot.terraform
#                 ^^^^ variable.other.member.terraform
#                      ^ punctuation.section.block.loop.for.terraform
#                        ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                              ^ meta.function-call.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                ^ punctuation.section.brackets.end.terraform

/////
// Object or map source value.
////

    [for k, v in var.map : length(k) + length(v)]
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^ keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#         ^ punctuation.separator.terraform
#           ^ variable.other.readwrite.terraform
#             ^^ keyword.control.loop.in.terraform
#                ^^^ variable.language.terraform
#                   ^ punctuation.accessor.dot.terraform
#                    ^^^ variable.other.member.terraform
#                        ^ punctuation.section.block.loop.for.terraform
#                          ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                                ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                 ^ meta.function-call.terraform
#                                  ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                    ^ keyword.operator.arithmetic.terraform
#                                      ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                                            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                             ^ meta.function-call.terraform
#                                              ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                               ^ punctuation.section.brackets.end.terraform

/////
// Complex right-side expressions.
////

    [for o in var.list : o.interfaces[0].name]
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^ keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#          ^^ keyword.control.loop.in.terraform
#             ^^^ variable.language.terraform
#                ^ punctuation.accessor.dot.terraform
#                 ^^^^ variable.other.member.terraform
#                      ^ punctuation.section.block.loop.for.terraform
#                        ^ variable.other.readwrite.terraform
#                         ^ punctuation.accessor.dot.terraform
#                          ^^^^^^^^^^ variable.other.member.terraform
#                                    ^ punctuation.section.brackets.begin.terraform
#                                     ^ constant.numeric.integer.terraform
#                                      ^ punctuation.section.brackets.end.terraform
#                                       ^ punctuation.accessor.dot.terraform
#                                        ^^^^ variable.other.member.terraform
#                                            ^ punctuation.section.brackets.end.terraform

/////
// Legacy splat expression attribute access.
/////

    [for o in var.list : o.interfaces][0].name
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^ keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#          ^^ keyword.control.loop.in.terraform
#             ^^^ variable.language.terraform
#                ^ punctuation.accessor.dot.terraform
#                 ^^^^ variable.other.member.terraform
#                      ^ punctuation.section.block.loop.for.terraform
#                        ^ variable.other.readwrite.terraform
#                         ^ punctuation.accessor.dot.terraform
#                          ^^^^^^^^^^ variable.other.member.terraform
#                                    ^ punctuation.section.brackets.end.terraform
#                                     ^ punctuation.section.brackets.begin.terraform
#                                      ^ constant.numeric.integer.terraform
#                                       ^ punctuation.section.brackets.end.terraform
#                                        ^ punctuation.accessor.dot.terraform
#                                         ^^^^ variable.other.member.terraform

/////
// Multi-line for-expressions.
/////

    value = [
#   ^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#         ^ keyword.operator.assignment.terraform
#           ^ punctuation.section.brackets.begin.terraform
      for instance in aws_instance.ubuntu:
#     ^^^ keyword.control.loop.for.terraform
#         ^^^^^^^^ variable.other.readwrite.terraform
#                  ^^ keyword.control.loop.in.terraform
#                     ^^^^^^^^^^^^ variable.other.readwrite.terraform
#                                 ^ punctuation.accessor.dot.terraform
#                                  ^^^^^^ variable.other.member.terraform
#                                        ^ punctuation.section.block.loop.for.terraform
      instance.private_dns
#     ^^^^^^^^ variable.other.readwrite.terraform
#             ^ punctuation.accessor.dot.terraform
#              ^^^^^^^^^^^ variable.other.member.terraform
    ]
#   ^ punctuation.section.brackets.end.terraform

/////
// Match conditional on right-side expression.
/////

    value = [
#   ^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#         ^ keyword.operator.assignment.terraform
#           ^ punctuation.section.brackets.begin.terraform
      for instance in aws_instance.ubuntu:
#     ^^^ keyword.control.loop.for.terraform
#         ^^^^^^^^ variable.other.readwrite.terraform
#                  ^^ keyword.control.loop.in.terraform
#                     ^^^^^^^^^^^^ variable.other.readwrite.terraform
#                                 ^ punctuation.accessor.dot.terraform
#                                  ^^^^^^ variable.other.member.terraform
#                                        ^ punctuation.section.block.loop.for.terraform
      (instance.public_ip != "" ? list(instance.private_ip, instance.public_ip) : list(instance.private_ip))
#     ^ punctuation.section.parens.begin.terraform
#      ^^^^^^^^ variable.other.readwrite.terraform
#              ^ punctuation.accessor.dot.terraform
#               ^^^^^^^^^ variable.other.member.terraform
#                         ^^ keyword.operator.terraform
#                            ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                             ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                               ^ keyword.operator.terraform
#                                 ^^^^ meta.function-call.terraform support.function.builtin.terraform
#                                     ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                      ^^^^^^^^ meta.function-call.terraform
#                                              ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                                               ^^^^^^^^^^ meta.function-call.terraform variable.other.member.terraform
#                                                         ^ meta.function-call.terraform punctuation.separator.terraform
#                                                           ^^^^^^^^ meta.function-call.terraform
#                                                                   ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                                                                    ^^^^^^^^^ meta.function-call.terraform variable.other.member.terraform
#                                                                             ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                                               ^ keyword.operator.terraform
#                                                                                 ^^^^ meta.function-call.terraform support.function.builtin.terraform
#                                                                                     ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                                                                                      ^^^^^^^^ meta.function-call.terraform
#                                                                                              ^ meta.function-call.terraform punctuation.accessor.dot.terraform
#                                                                                               ^^^^^^^^^^ meta.function-call.terraform variable.other.member.terraform
#                                                                                                         ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                                                                                          ^ punctuation.section.parens.end.terraform
    ]
#   ^ punctuation.section.brackets.end.terraform

/////
// Match brackets on right-side expression.
/////

    value = [
#   ^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#         ^ keyword.operator.assignment.terraform
#           ^ punctuation.section.brackets.begin.terraform
      for instance in aws_instance.ubuntu:
#     ^^^ keyword.control.loop.for.terraform
#         ^^^^^^^^ variable.other.readwrite.terraform
#                  ^^ keyword.control.loop.in.terraform
#                     ^^^^^^^^^^^^ variable.other.readwrite.terraform
#                                 ^ punctuation.accessor.dot.terraform
#                                  ^^^^^^ variable.other.member.terraform
#                                        ^ punctuation.section.block.loop.for.terraform
      (instance.public_ip != "" ? [instance.private_ip, instance.public_ip] : [instance.private_ip])
#     ^ punctuation.section.parens.begin.terraform
#      ^^^^^^^^ variable.other.readwrite.terraform
#              ^ punctuation.accessor.dot.terraform
#               ^^^^^^^^^ variable.other.member.terraform
#                         ^^ keyword.operator.terraform
#                            ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                             ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                               ^ keyword.operator.terraform
#                                 ^ punctuation.section.brackets.begin.terraform
#                                          ^ punctuation.accessor.dot.terraform
#                                           ^^^^^^^^^^ variable.other.member.terraform
#                                                     ^ punctuation.separator.terraform
#                                                               ^ punctuation.accessor.dot.terraform
#                                                                ^^^^^^^^^ variable.other.member.terraform
#                                                                         ^ punctuation.section.brackets.end.terraform
#                                                                           ^ keyword.operator.terraform
#                                                                             ^ punctuation.section.brackets.begin.terraform
#                                                                                      ^ punctuation.accessor.dot.terraform
#                                                                                       ^^^^^^^^^^ variable.other.member.terraform
#                                                                                                 ^ punctuation.section.brackets.end.terraform
#                                                                                                  ^ punctuation.section.parens.end.terraform
    ]
#   ^ punctuation.section.brackets.end.terraform

/////
// Match if-conditionals on right-side.
/////

    [for s in var.list : upper(s) if s != ""]
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^ keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#          ^^ keyword.control.loop.in.terraform
#             ^^^ variable.language.terraform
#                ^ punctuation.accessor.dot.terraform
#                 ^^^^ variable.other.member.terraform
#                      ^ punctuation.section.block.loop.for.terraform
#                        ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                             ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                              ^ meta.function-call.terraform
#                               ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                 ^^ keyword.control.conditional.terraform
#                                    ^ variable.other.readwrite.terraform
#                                      ^^ keyword.operator.terraform
#                                         ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                          ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                           ^ punctuation.section.brackets.end.terraform

/////
// Matches bracket-literals as range expression.
/////

    [for i, v in ["a", "b", "c"]: v if i < 2]
#   ^ punctuation.section.brackets.begin.terraform
#    ^^^ keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#         ^ punctuation.separator.terraform
#           ^ variable.other.readwrite.terraform
#             ^^ keyword.control.loop.in.terraform
#                ^ punctuation.section.brackets.begin.terraform
#                 ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^ string.quoted.double.terraform
#                    ^ punctuation.separator.terraform
#                      ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                       ^^ string.quoted.double.terraform
#                         ^ punctuation.separator.terraform
#                           ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                            ^^ string.quoted.double.terraform
#                              ^ punctuation.section.brackets.end.terraform
#                               ^ punctuation.section.block.loop.for.terraform
#                                 ^ variable.other.readwrite.terraform
#                                   ^^ keyword.control.conditional.terraform
#                                      ^ variable.other.readwrite.terraform
#                                        ^ keyword.operator.terraform
#                                          ^ constant.numeric.integer.terraform
#                                           ^ punctuation.section.brackets.end.terraform

/////////////////////////////////////////////////////////////////////
// OBJECT FOR-EXPRESSIONS
/////////////////////////////////////////////////////////////////////

/////
// Matches basic syntax.
/////

    {for i, v in ["a", "b"]: v => i}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^^^ meta.braces.terraform keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#         ^ punctuation.separator.terraform
#           ^ variable.other.readwrite.terraform
#             ^^ keyword.control.loop.in.terraform
#                ^ punctuation.section.brackets.begin.terraform
#                 ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^ string.quoted.double.terraform
#                    ^ punctuation.separator.terraform
#                      ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                       ^^ string.quoted.double.terraform
#                         ^ punctuation.section.brackets.end.terraform
#                          ^ punctuation.section.block.loop.for.terraform
#                            ^ variable.other.readwrite.terraform
#                              ^^ punctuation.separator.key-value.terraform
#                                 ^ variable.other.readwrite.terraform
#                                  ^ punctuation.section.braces.end.terraform
#                                   ^ -meta

/////
// Matches ellipsis.
/////

    {for i, v in ["a"]: v => i...}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^^^ meta.braces.terraform keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#         ^ punctuation.separator.terraform
#           ^ variable.other.readwrite.terraform
#             ^^ keyword.control.loop.in.terraform
#                ^ punctuation.section.brackets.begin.terraform
#                 ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                  ^^ string.quoted.double.terraform
#                    ^ punctuation.section.brackets.end.terraform
#                     ^ punctuation.section.block.loop.for.terraform
#                       ^ variable.other.readwrite.terraform
#                         ^^ punctuation.separator.key-value.terraform
#                            ^ variable.other.readwrite.terraform
#                             ^^^ keyword.operator.terraform
#                                ^ punctuation.section.braces.end.terraform

/////
// Matches if-conditional.
/////

    {for s in var.list : substr(s, 0, 1) => s... if s != ""}
#   ^ meta.braces.terraform punctuation.section.braces.begin.terraform
#    ^^^ meta.braces.terraform keyword.control.loop.for.terraform
#        ^ variable.other.readwrite.terraform
#          ^^ keyword.control.loop.in.terraform
#             ^^^ variable.language.terraform
#                ^ punctuation.accessor.dot.terraform
#                 ^^^^ variable.other.member.terraform
#                      ^ punctuation.section.block.loop.for.terraform
#                        ^^^^^^ meta.function-call.terraform support.function.builtin.terraform
#                              ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#                               ^ meta.function-call.terraform
#                                ^ meta.function-call.terraform punctuation.separator.terraform
#                                  ^ meta.function-call.terraform constant.numeric.integer.terraform
#                                   ^ meta.function-call.terraform punctuation.separator.terraform
#                                     ^ meta.function-call.terraform constant.numeric.integer.terraform
#                                      ^ meta.function-call.terraform punctuation.section.parens.end.terraform
#                                        ^^ punctuation.separator.key-value.terraform
#                                           ^ variable.other.readwrite.terraform
#                                            ^^^ keyword.operator.terraform
#                                                ^^ keyword.control.conditional.terraform
#                                                   ^ variable.other.readwrite.terraform
#                                                     ^^ keyword.operator.terraform
#                                                        ^ string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                                         ^ string.quoted.double.terraform punctuation.definition.string.end.terraform
#                                                          ^ punctuation.section.braces.end.terraform

/////
// Matches over multiple-lines.
/////

    value = {
#   ^^^^^ variable.declaration.terraform variable.other.readwrite.terraform
#         ^ keyword.operator.assignment.terraform
#           ^ meta.braces.terraform punctuation.section.braces.begin.terraform
      for l in var.letters: l =>
#     ^^^ meta.braces.terraform keyword.control.loop.for.terraform
#         ^ variable.other.readwrite.terraform
#           ^^ keyword.control.loop.in.terraform
#              ^^^ variable.language.terraform
#                 ^ punctuation.accessor.dot.terraform
#                  ^^^^^^^ variable.other.member.terraform
#                         ^ punctuation.section.block.loop.for.terraform
#                           ^ variable.other.readwrite.terraform
#                             ^^ punctuation.separator.key-value.terraform
        upper(l)
#       ^^^^^ meta.function-call.terraform support.function.builtin.terraform
#            ^ meta.function-call.terraform punctuation.section.parens.begin.terraform
#             ^ meta.function-call.terraform
#              ^ meta.function-call.terraform punctuation.section.parens.end.terraform
    }
#   ^ punctuation.section.braces.end.terraform

/////////////////////////////////////////////////////////////////////
// BLOCKS
/////////////////////////////////////////////////////////////////////

/////
// Inline block with no labels.
////

    thing  {}
#   ^^^^^ meta.type.terraform entity.name.type.terraform
#          ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#           ^ meta.block.terraform punctuation.section.block.end.terraform

/////
// Inline block with string labels.
/////

    thing "label1"   "label2" {}
#   ^^^^^ meta.type.terraform entity.name.type.terraform
#         ^ meta.type.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#          ^^^^^^ meta.type.terraform string.quoted.double.terraform
#                ^ meta.type.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                    ^ meta.type.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                     ^^^^^^ meta.type.terraform string.quoted.double.terraform
#                           ^ meta.type.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
#                             ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#                              ^ meta.block.terraform punctuation.section.block.end.terraform
#                               ^ -meta

/////
// Inline block with identifier labels.
/////

    thing thing1 thing2 thing3 {}
#   ^^^^^ meta.type.terraform entity.name.type.terraform
#         ^^^^^^ meta.type.terraform entity.name.label.terraform
#                ^^^^^^ meta.type.terraform entity.name.label.terraform
#                       ^^^^^^ meta.type.terraform entity.name.label.terraform
#                              ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#                               ^ meta.block.terraform punctuation.section.block.end.terraform

//////
// Nested multi-line blocks with expressions.
/////

    resource "aws_security_group" "example" {
#   ^^^^^^^^ meta.type.terraform keyword.declaration.terraform
#            ^ meta.type.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^^^^^^^^^^^^^^ meta.type.terraform string.quoted.double.terraform
#                                 ^ meta.type.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                                  ^^^^^^^^ meta.type.terraform string.quoted.double.terraform
#                                           ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
      name = "example"
#     ^^^^ meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#          ^ meta.block.terraform keyword.operator.assignment.terraform
#            ^ meta.block.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#             ^^^^^^^^ meta.block.terraform string.quoted.double.terraform

      dynamic "ingress" {
#     ^^^^^^^ meta.block.terraform meta.type.terraform entity.name.type.terraform
#             ^ meta.block.terraform meta.type.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^^^^^^^ meta.block.terraform meta.type.terraform string.quoted.double.terraform
#                       ^ meta.block.terraform meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
        for_each = var.service_ports
#       ^^^^^^^^ variable.declaration.terraform keyword.control.loop.for.terraform
#                ^ keyword.operator.assignment.terraform
#                  ^^^ variable.language.terraform
#                     ^ punctuation.accessor.dot.terraform
#                      ^^^^^^^^^^^^^ variable.other.member.terraform
        content {
#       ^^^^^^^ meta.block.terraform meta.block.terraform meta.type.terraform entity.name.type.terraform
#               ^ meta.block.terraform meta.block.terraform meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
          from_port = ingress.value
#         ^^^^^^^^^ meta.block.terraform meta.block.terraform meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#                   ^ meta.block.terraform meta.block.terraform meta.block.terraform keyword.operator.assignment.terraform
#                            ^ meta.block.terraform meta.block.terraform meta.block.terraform punctuation.accessor.dot.terraform
#                             ^^^^^ meta.block.terraform meta.block.terraform meta.block.terraform variable.other.member.terraform
          to_port   = ingress.value
#         ^^^^^^^ meta.block.terraform meta.block.terraform meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#                   ^ meta.block.terraform meta.block.terraform meta.block.terraform keyword.operator.assignment.terraform
#                            ^ meta.block.terraform meta.block.terraform meta.block.terraform punctuation.accessor.dot.terraform
          protocol  = "tcp" + "IP"
#         ^^^^^^^^ meta.block.terraform meta.block.terraform meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#                   ^ meta.block.terraform meta.block.terraform meta.block.terraform keyword.operator.assignment.terraform
#                     ^ meta.block.terraform meta.block.terraform meta.block.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                      ^^^^ meta.block.terraform meta.block.terraform meta.block.terraform string.quoted.double.terraform
#                           ^ meta.block.terraform meta.block.terraform meta.block.terraform keyword.operator.arithmetic.terraform
#                             ^ meta.block.terraform meta.block.terraform meta.block.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#                              ^^^ meta.block.terraform meta.block.terraform meta.block.terraform string.quoted.double.terraform
        }
#       ^ meta.block.terraform meta.block.terraform meta.block.terraform punctuation.section.block.end.terraform
      }
#     ^ meta.block.terraform meta.block.terraform punctuation.section.block.end.terraform
    }
#   ^ meta.block.terraform punctuation.section.block.end.terraform
#    ^ -meta

/////
// Matches blocks with functions, objects, tuples.
/////

    thing label1 {
#   ^^^^^ meta.type.terraform entity.name.type.terraform
#         ^^^^^^ meta.type.terraform entity.name.label.terraform
#                ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
      func = function(param1)
#     ^^^^ meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#          ^ meta.block.terraform keyword.operator.assignment.terraform
#            ^^^^^^^^ meta.block.terraform meta.function-call.terraform variable.function.terraform
#                    ^ meta.block.terraform meta.function-call.terraform punctuation.section.parens.begin.terraform
#                     ^^^^^^ meta.block.terraform meta.function-call.terraform
#                           ^ meta.block.terraform meta.function-call.terraform punctuation.section.parens.end.terraform
      obj = {
#     ^^^ meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#         ^ meta.block.terraform keyword.operator.assignment.terraform
        key = "value"
#       ^^^ meta.block.terraform meta.braces.terraform meta.mapping.key.terraform string.unquoted.terraform
#           ^ meta.block.terraform meta.braces.terraform keyword.operator.assignment.terraform
#             ^ meta.block.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.begin.terraform
#              ^^^^^^ meta.block.terraform meta.braces.terraform string.quoted.double.terraform
#                   ^ meta.block.terraform meta.braces.terraform string.quoted.double.terraform punctuation.definition.string.end.terraform
      }
#     ^ meta.block.terraform meta.braces.terraform punctuation.section.braces.end.terraform
      tuple = [1, 2]
#     ^^^^^ meta.block.terraform variable.declaration.terraform variable.other.readwrite.terraform
#           ^ meta.block.terraform keyword.operator.assignment.terraform
#             ^ meta.block.terraform punctuation.section.brackets.begin.terraform
#              ^ meta.block.terraform constant.numeric.integer.terraform
#               ^ meta.block.terraform punctuation.separator.terraform
#                 ^ meta.block.terraform constant.numeric.integer.terraform
#                  ^ meta.block.terraform punctuation.section.brackets.end.terraform
    }
#   ^ meta.block.terraform punctuation.section.block.end.terraform
#    ^ -meta -block

/////////////////////////////////////////////////////////////////////
// TERRAFORM NAMED VALUES
/////////////////////////////////////////////////////////////////////

    var.something
#   ^^^ variable.language.terraform
#      ^ punctuation.accessor.dot.terraform
#       ^^^^^^^^^ variable.other.member.terraform

    local.something
#   ^^^^^ variable.language.terraform
#        ^ punctuation.accessor.dot.terraform
#         ^^^^^^^^^ variable.other.member.terraform

    module.name.output_name
#   ^^^^^^ variable.language.terraform
#         ^ punctuation.accessor.dot.terraform
#          ^^^^ variable.other.member.terraform
#              ^ punctuation.accessor.dot.terraform
#               ^^^^^^^^^^^ variable.other.member.terraform

    data.data_type.name
#   ^^^^ variable.language.terraform
#       ^ punctuation.accessor.dot.terraform
#        ^^^^^^^^^ variable.other.member.terraform
#                 ^ punctuation.accessor.dot.terraform
#                  ^^^^ variable.other.member.terraform

    path.module
#   ^^^^ variable.language.terraform
#       ^ punctuation.accessor.dot.terraform
#        ^^^^^^ variable.other.member.terraform

    terraform.workspace
#   ^^^^^^^^^ variable.language.terraform
#            ^ punctuation.accessor.dot.terraform
#             ^^^^^^^^^ variable.other.member.terraform

    count.index
#   ^^^^^ variable.language.terraform
#        ^ punctuation.accessor.dot.terraform
#         ^^^^^ variable.other.member.terraform

    each.key
#   ^^^^ variable.language.terraform
#       ^ punctuation.accessor.dot.terraform
#        ^^^ variable.other.member.terraform

    self.private_ip
#   ^^^^ variable.language.terraform
#       ^ punctuation.accessor.dot.terraform
#        ^^^^^^^^^^ variable.other.member.terraform

/////////////////////////////////////////////////////////////////////
// TERRAFORM TOP-LEVEL BLOCK TYPES
/////////////////////////////////////////////////////////////////////

    resource {}
#   ^^^^^^^^ meta.type.terraform keyword.declaration.terraform
#            ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#             ^ meta.block.terraform punctuation.section.block.end.terraform

    provider {}
#   ^^^^^^^^ meta.type.terraform keyword.declaration.terraform
#            ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#             ^ meta.block.terraform punctuation.section.block.end.terraform

    variable {}
#   ^^^^^^^^ meta.type.terraform keyword.declaration.terraform
#            ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#             ^ meta.block.terraform punctuation.section.block.end.terraform

    output {}
#   ^^^^^^ meta.type.terraform keyword.declaration.terraform
#          ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#           ^ meta.block.terraform punctuation.section.block.end.terraform

    locals {}
#   ^^^^^^ meta.type.terraform keyword.declaration.terraform
#          ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#           ^ meta.block.terraform punctuation.section.block.end.terraform

    module {}
#   ^^^^^^ meta.type.terraform keyword.declaration.terraform
#          ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#           ^ meta.block.terraform punctuation.section.block.end.terraform

    data {}
#   ^^^^ meta.type.terraform keyword.declaration.terraform
#        ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#         ^ meta.block.terraform punctuation.section.block.end.terraform

    terraform {}
#   ^^^^^^^^^ meta.type.terraform keyword.declaration.terraform
#             ^ meta.type.terraform meta.block.terraform punctuation.section.block.begin.terraform
#              ^ meta.block.terraform punctuation.section.block.end.terraform

/////////////////////////////////////////////////////////////////////
// TERRAFORM TYPE KEYWORDS
/////////////////////////////////////////////////////////////////////

    string
#   ^^^^^^ storage.type.terraform

    any
#   ^^^ storage.type.terraform

    number
#   ^^^^^^ storage.type.terraform

    bool
#   ^^^^ storage.type.terraform

/////////////////////////////////////////////////////////////////////
// HEREDOCS
/////////////////////////////////////////////////////////////////////

/////
// Basic example.
/////
    << EOF
#   ^^ keyword.operator.heredoc.terraform
#      ^^^ keyword.control.heredoc.terraform
    sdfdfsd
#   ^^^^^^^^ string.unquoted.heredoc.terraform
    EOF
#   ^^^^ keyword.control.heredoc.terraform

/////
// With leading-spaces-operator.
/////

    <<- END
#   ^^^ keyword.operator.heredoc.terraform
#       ^^^ keyword.control.heredoc.terraform
    heredoc
#   ^^^^^^^^ string.unquoted.heredoc.terraform
    EOF
#   ^^^^ string.unquoted.heredoc.terraform
    END
#   ^^^^ keyword.control.heredoc.terraform

/////
// Includes string interpolation.
/////

    <<- END
#   ^^^ keyword.operator.heredoc.terraform
#       ^^^ keyword.control.heredoc.terraform
    Hello, %{var.name}
#^^^^^^^^^^^^^^^^^^^^^ meta.string.terraform
#^^^^^^^^^^ string.unquoted.heredoc.terraform
#          ^^^^^^^^^^^ meta.interpolation.terraform
#          ^^ punctuation.section.interpolation.begin.terraform
#            ^^^^^^^^ source.terraform
#            ^^^ variable.language.terraform
#               ^ punctuation.accessor.dot.terraform
#                ^^^^ variable.other.member.terraform
#                    ^ punctuation.section.interpolation.end.terraform
    END
#   ^^^^ keyword.control.heredoc.terraform

    <<__EOF__
#   ^^ keyword.operator.heredoc.terraform
#     ^^^^^^^ keyword.control.heredoc.terraform
    aaa
    __EOF__
#   ^^^^^^^^ keyword.control.heredoc.terraform
/////////////////////////////////////////////////////////////////////
// IMPORTS
/////////////////////////////////////////////////////////////////////

/////
// Import with attribute access.
/////

    terraform import aws_instance.example i-abcd1234
#   ^^^^^^^^^ support.constant.terraform
#             ^^^^^^ keyword.control.import.terraform
#                    ^^^^^^^^^^^^ entity.name.label.terraform
#                                ^ punctuation.accessor.dot.terraform
#                                 ^^^^^^^ variable.other.member.terraform
#                                         ^^^^^^^^^^ entity.name.label.terraform

/////
// Import with numeric literals.
/////

    terraform import digitalocean_ssh_key.mykey 263654
#   ^^^^^^^^^ support.constant.terraform
#             ^^^^^^ keyword.control.import.terraform
#                    ^^^^^^^^^^^^^^^^^^^^ entity.name.label.terraform
#                                        ^ punctuation.accessor.dot.terraform
#                                         ^^^^^ variable.other.member.terraform
#                                               ^^^^^^ constant.numeric.integer.terraform

variable "instance_type" {
  default = "t2.micro"
}

resource "aws_instance" "example" {
  ami           = "ami-123456"
  instance_type = var.instance_type
}

data "aws_ami" "example" {
  most_recent = true
  owners      = ["self"]

  filter {
    name   = "name"
    values = ["my-ami-*"]
  }
}

resource "aws_instance" "example" {
  ami           = data.aws_ami.example.id
  instance_type = "t2.micro"
}
