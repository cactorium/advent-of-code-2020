use std::io::{self, BufRead};
use std::collections::HashSet;

fn main() {
    let stdin = io::stdin();

    let mut nums: HashSet<i32> = HashSet::new();

    for line in stdin.lock().lines() {
        let num = line.unwrap().parse::<i32>().unwrap();
        let math = 2020 - num;

        if nums.contains(&math) {
            println!("{}", num * math);
        }
        nums.insert(num);
    }
}
