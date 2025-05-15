# Rust Onigmo

Rust (hopefully-safe) binding for Onigmo.

## Examples

Finding matches and capturing parts of the match:

```rust
use onigmo_regex::*;

let mut re = OnigmoRegex::new(r"(\d{4})-(\d{2})-(\d{2})".to_string()).unwrap();
let text = "The date was 2018-04-07";
let captures = re.captures(text).unwrap().unwrap();

assert_eq!(captures.get(1).unwrap().as_str(), "2018");
assert_eq!(captures.get(2).unwrap().as_str(), "04");
assert_eq!(captures.get(3).unwrap().as_str(), "07");
assert_eq!(captures.get(0).unwrap().as_str(), "2018-04-07");
```

```rust
use onigmo_regex::*;
let re = OnigmoRegex::new(r"(?m:^)(\d+)".to_string()).unwrap();
let text = "1 test 123\n2 foo";
let captures = re.captures_from_pos(text, 7).unwrap().unwrap();
let group = captures.get(1).unwrap();
assert_eq!(group.as_str(), "2");
assert_eq!(group.start(), 11);
assert_eq!(group.end(), 12);
```
