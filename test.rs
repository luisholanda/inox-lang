#[doc = r" Start of file header"]
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
fn main() {
    _main();
}
trait InoxIter {
    type Item;
    fn inox_iter(self) -> Vec<Self::Item>;
}
impl<T> InoxIter for Vec<T> {
    type Item = T;
    fn inox_iter(self) -> Vec<Self::Item> {
        self
    }
}
impl<K, V> InoxIter for HashMap<K, V>
where
    K: Hash + Eq + Clone,
{
    type Item = K;
    fn inox_iter(self) -> Vec<Self::Item> {
        self.keys().cloned().collect()
    }
}
fn _read() -> String {
    use std::io;
    use std::io::prelude::*;
    let mut input = String::new();
    io::stdout().flush();
    io::stdin().read_line(&mut input).unwrap_or(0);
    input
}
fn _print(text: String) {
    print!("{}", text)
}
fn _show(text: impl Debug) -> String {
    format!("{:?}", text)
}
fn _append<E, T>(mut e: E, x: T) -> E
where
    E: Extend<T>,
{
    e.extend(vec![x]);
    e
}
fn _concat(mut s: String, s2: String) -> String {
    s.push_str(s2.as_str());
    s
}
#[doc = r"End of file header"]
fn _reduce(_as: Vec<i32>) -> f32 {
    let mut _result: i32 = 1i32;
    for _a in &_as.clone().inox_iter() {
        _result = (_result) + (_a);
    }
    _result as f32
}
fn _join(_ss: Vec<String>, _sep: String) -> String {
    let mut _result: String = "".to_string();
    for _s in &_ss.clone().inox_iter() {
        _result = _concat(_result.clone(), _concat(_sep.clone(), _s.clone()));
    }
    _result as String
}
fn _main() -> () {
    let mut _vec_test: Vec<i32> = {
        let mut vec = Vec::with_capacity(2usize);
        vec.push(1i32);
        vec.push(2i32);
        vec
    };
    let _reduce: f32 = _reduce(_vec_test.clone());
    _print(_append(_show(_reduce.clone()), '\n'));
    {
        let __idx = 3i32 as usize;
        let __vec = &mut _vec_test;
        __vec.resize(__idx + 1, Default::default());
        __vec.insert(__idx, 4i32);
    };
    _print(_show(_vec_test.clone()));
    let _map_test: HashMap<String, String> = {
        let mut map = HashMap::with_capacity(1usize);
        map.insert("a".to_string(), _read());
        map
    };
    for _t in &_map_test.clone().inox_iter() {
        _print(_join(
            {
                let mut vec = Vec::with_capacity(4usize);
                vec.push(_t.clone());
                vec.push(":".to_string());
                vec.push(_map_test[_t].clone());
                vec.push("\n".to_string());
                vec
            },
            " ".to_string(),
        ));
    }
}
