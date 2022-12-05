/* Read file to find the elf that has the largest sum */
use std::collections::VecDeque;
use std::fs::File;
use std::io::{prelude::*, BufReader};

struct GroupValue {
    group: i32,
    value: i32,
}

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);
    let mut queue: VecDeque<GroupValue> = VecDeque::with_capacity(3);

    let mut current_sum = 0;
    let mut current_group = 1;

    for line in reader.lines() {
        let current_line = line.unwrap();

        if current_line == "" {
            println!("group:{current_group} with value of:{current_sum}");

            if queue.len() < 3 || queue.get(0).expect("expect queue item").value < current_sum {
                println!(
                    "adding group:{current_group} to largest queue with value of:{current_sum}"
                );

                let gb = GroupValue {
                    group: current_group,
                    value: current_sum,
                };

                if queue.len() == 3 {
                    queue.pop_front();
                }

                queue.push_front(gb);

                if queue.len() > 1 {
                    let mut i = 0;
                    while i < queue.len() - 1 {
                        let a = queue.get(i).expect("not found in queue").value;
                        let b = queue.get(i + 1).expect("not found in queue").value;

                        if a < b {
                            break;
                        }

                        queue.swap(i, i + 1);

                        i = i + 1;
                    }
                }
            }
            current_group += 1;
            current_sum = 0;
        } else {
            let i: i32 = current_line.parse().expect("Error parsing string");
            current_sum += i;
        }
    }

    let mut sum = 0;
    while queue.len() > 0 {
        sum += queue.pop_front().expect("missing element on queue").value
    }

    println!("amount of groups:{current_group} - sum of greatest 3 groups:{sum}");
}
