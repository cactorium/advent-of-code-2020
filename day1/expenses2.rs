use std::io::{self, BufRead};
use std::collections::HashSet;

fn main() {
    let stdin = io::stdin();

    let mut nums: HashSet<i32> = HashSet::new();

    for line in stdin.lock().lines() {
        let num = line.unwrap().parse::<i32>().unwrap();
        let rest = 2020 - num;

        for i in 1..(rest+1) {
            if nums.contains(&i) {
                if nums.contains(&(rest - i)) {
                    println!("{}", num * i * (rest - i));
                }
            }
        }
        nums.insert(num);
    }
}
