use std::collections::HashMap;

struct Elf {
    memory: HashMap<usize, usize>,
    count: usize,
    last: usize,
    start_vec: Vec<usize>,
}

impl Elf {
    fn new() -> Elf {
        Elf {
            memory: HashMap::new(),
            count: 0,
            last: 0,
            start_vec: Vec::new(),
        }
    }
    fn start(&mut self, v: usize) {
        self.start_vec.push(v);
    }

    fn next(&mut self) -> usize {
        let old_last = self.last;
        self.last = if self.count < self.start_vec.len() {
            self.start_vec[self.count]
        } else {
            match self.memory.get(&self.last) {
                Some(ref val) => self.count - *val,
                None => 0
            }
        };
        if self.count != 0 {
            self.memory.insert(old_last, self.count);
        }
        self.count += 1;
        self.last
    }
}

fn main() {
    let mut elf = Elf::new();
    for i in &[0, 3, 6] {
        elf.start(*i);
    }
    for _ in 0..(30000000-1) {
        let _ = elf.next();
    }
    println!("{}", elf.next());
}
