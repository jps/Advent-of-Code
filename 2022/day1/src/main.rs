/* Read file to find the elf that has the largest sum */
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);
    let mut greatest_group = 0;
    let mut greatest_value = 0;
    let mut current_sum = 0;
    let mut current_group = 1;

    for line in reader.lines() {
        let current_line = line.unwrap();

        if current_line == "" {
            println!("group:{current_group} with value of:{current_sum}");

            if current_sum > greatest_value {
                greatest_group = current_group;
                greatest_value = current_sum;

                println!("new greatest group is:{greatest_group} with value of:{greatest_value}")
            }
            current_group += 1;
            current_sum = 0;
        } else {
            let i: i32 = current_line.parse().expect("Error parsing string");
            current_sum += i;
        }
    }

    println!(
        "amount of groups:{current_group} - greatest group:{greatest_group} value:{greatest_value}"
    );
}
