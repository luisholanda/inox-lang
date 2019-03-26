use std::char;
use std::collections::VecDeque;

/// Takes in a string of a backslash escaped character
/// and convert it to the proper escaped character.
pub(crate) fn unescape(s: &str) -> Option<char> {
    let mut queue: VecDeque<_> = s.chars().collect();

    if queue.pop_front().unwrap() != '\\' {
        return None;
    }

    queue.pop_front().and_then(|ch| match ch {
        'b' => Some('\u{0008}'),
        'f' => Some('\u{000C}'),
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        '\'' => Some('\''),
        '\"' => Some('\"'),
        '\\' => Some('\\'),
        'u' => unescape_unicode(&mut queue),
        'x' => unescape_byte(&mut queue),
        c if c.is_digit(8) => unescape_octal(c, &mut queue),
        _ => None,
    })
}

fn unescape_unicode(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::new();

    for _ in 0..4 {
        s.push(queue.pop_front()?);
    }

    let u = u32::from_str_radix(&s, 16).ok()?;

    char::from_u32(u)
}

fn unescape_byte(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::new();

    for _ in 0..2 {
        s.push(queue.pop_front()?);
    }

    let u = u32::from_str_radix(&s, 16).ok()?;

    char::from_u32(u)
}

fn unescape_octal(c: char, queue: &mut VecDeque<char>) -> Option<char> {
    match unescape_octal_leading(c, queue) {
        None => unescape_octal_no_leading(c, queue),
        Some(ch) => {
            queue.pop_front();
            queue.pop_front();

            Some(ch)
        }
    }
}

fn unescape_octal_leading(c: char, queue: &mut VecDeque<char>) -> Option<char> {
    if c != '0' && c != '1' && c != '2' && c != '3' {
        return None;
    }

    let mut s = String::new();
    s.push(c);
    s.push(*queue.get(0)?);
    s.push(*queue.get(1)?);

    let u = u32::from_str_radix(&s, 8).ok()?;

    char::from_u32(u)
}

fn unescape_octal_no_leading(c: char, queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::new();
    s.push(c);
    s.push(queue.pop_front()?);

    let u = u32::from_str_radix(&s, 8).ok()?;

    char::from_u32(u)
}
