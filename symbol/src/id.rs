use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct Id {
    id: usize,
}

#[derive(Debug)]
pub struct IdGenerator {
    next_id: AtomicUsize,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self {
            next_id: AtomicUsize::new(1),
        }
    }

    pub fn generate(&self) -> Id {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);

        Id { id }
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}
