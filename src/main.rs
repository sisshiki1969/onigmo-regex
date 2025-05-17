use onigmo_regex::*;
fn main() {
    let re = Regex::new(r"(?<A::B>.)(?<a>.)").unwrap();
    let caps = re.captures("ab").unwrap();
    dbg!(caps);
    dbg!(re.names().unwrap());
}
