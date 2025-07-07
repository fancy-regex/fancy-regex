This document attempts to describe a little about how certain features in Oniguruma work.
Once we implement them in fancy-regex, it can become fancy-regex documentation instead.
To experiment with Oniguruma, the `rust-onig` crate was used from commit c4378abcbf30d58cf5f230c0d2e6375f2be05a47 (the latest at the time) with some adaptions:
```diff
diff --git a/onig/examples/capturedump.rs b/onig/examples/capturedump.rs
index 1f854a4..5834d90 100644
--- a/onig/examples/capturedump.rs
+++ b/onig/examples/capturedump.rs
@@ -26,8 +26,13 @@ fn main() {
                 let res = regex.captures(&line);
                 match res {
                     Some(captures) => {
-                        for (i, mat) in captures.iter().enumerate() {
-                            println!("{} => '{}'", i, mat.unwrap());
+                        for (i, pos) in captures.iter_pos().enumerate() {
+                            match pos {
+                                Some((beg, end)) =>
+                                    println!("Group {} captured in position {}:{} with text \"{}\"", i, beg, end, &line[beg..end]),
+                                None =>
+                                    println!("Group {} is not captured", i)
+                            }
                         }
                     }
                     None => println!("{} => did not match", name),
```

--------------------

When a subroutine is called, it behaves similarly to having multiple capture groups with the same name. The later capture "replaces" the earlier one when accessing the capture groups programmatically after the match. There exists only one capture group number assigned.

```shell
echo 'input text' | cargo run --example capturedump -- '(?<a>.)\g<a>'
```
> Group 0 captured in position 0:2 with text "in"
> Group 1 captured in position 1:2 with text "n"

When a capture group name is re-used for multiple capture groups, each group gets a unique number.
TODO: Demonstrate what happens when accessing the capture group by name - does it get the latest match? - and investigate how is this affected by optional matches, etc. Does the highest numbered group which participated in the match win or the highest position match in the input string?
```shell
echo 'input text' | cargo run --example capturedump -- '(?<a>.)(?<a>\w)'
```
> Group 0 captured in position 0:2 with text "in"
> Group 1 captured in position 0:1 with text "i"
> Group 2 captured in position 1:2 with text "n"

Adding a backreference to a named group with multiple definitions, the backreference will attempt to match the most recent capture group which took part in the match, then if that fails, the next most recent etc.
TODO: find a better example to showcase this
```shell
echo 'input text' | cargo run --example capturedump -- '(?<a>.)(?<a>\w).*\k<a>'
```
> Group 0 captured in position 3:10 with text "ut text"
> Group 1 captured in position 3:4 with text "u"
> Group 2 captured in position 4:5 with text "t"

-----

From Oniguruma docs:

When we say "backreference a group," it actually means, "re-match the same
  text matched by the subexp in that group."

  \n  \k<n>     \k'n'     (n >= 1) backreference the nth group in the regexp
      \k<-n>    \k'-n'    (n >= 1) backreference the nth group counting
                          backwards from the referring position
      \k<+n>    \k'+n'    (n >= 1) backreference the nth group counting
                          forwards from the referring position
      \k<name>  \k'name'  backreference a group with the specified name

  When backreferencing with a name that is assigned to more than one groups,
  the last group with the name is checked first, if not matched then the
  previous one with the name, and so on, until there is a match.

Subexp calls ("Tanaka Akira special")   (* original function)

  When we say "call a group," it actually means, "re-execute the subexp in
  that group."

  \g<n>     \g'n'     (n >= 1) call the nth group
  \g<0>     \g'0'     call zero (call the total regexp)
  \g<-n>    \g'-n'    (n >= 1) call the nth group counting backwards from
                      the calling position
  \g<+n>    \g'+n'    (n >= 1) call the nth group counting forwards from
                      the calling position
  \g<name>  \g'name'  call the group with the specified name

  * Left-most recursive calls are not allowed.

    ex. (?<name>a|\g<name>b)    => error
        (?<name>a|b\g<name>c)   => OK

  * Calls with a name that is assigned to more than one groups are not
    allowed.

  * Call by number is forbidden if any named group is defined and
    ONIG_OPTION_CAPTURE_GROUP is not set.

  * The option status of the called group is always effective.

    ex. /(?-i:\g<name>)(?i:(?<name>a)){0}/.match("A")
