/* You are given the following information, but you may prefer to do some
 * research for yourself:
 *
 * - 1 Jan 1900 was a Monday.
 * - Thirty days has September,
 *   April, June and November.
 *   All the rest have thirty-one,
 *   Saving February alone,
 *   Which has twenty-eight, rain or shine.
 *   And on leap years, twenty-nine.
 * - A leap year occurs on any year evenly divisible by 4, but not on a century
 *   unless it is divisible by 400.
 *
 * How many Sundays fell on the first of the month during the twentieth century
 * (1 Jan 1901 to 31 Dec 2000)?
 */

struct Date {
    weekday: u8, // [0, 6]
    month: u8,   // [0, 11]
    year: u16,
}

impl Date {
    fn is_leap_year(&self) -> bool {
        (self.year % 400 == 0) || ((self.year % 4 == 0) && (self.year % 100 != 0))
    }

    fn days_in_month(&self) -> u8 {
        match self.month {
            1 => if self.is_leap_year() { 29 } else { 28 },
            3|5|8|10 => 30,
            0|2|4|6|7|9|11 => 31,
            _ => panic!("month index out of range"),
        }
    }

    fn next_month(&mut self) {
        self.weekday = (self.weekday + self.days_in_month()) % 7;
        self.month = (self.month +1 ) % 12;
        if self.month == 0 { self.year += 1; }
    }

    fn next_year(&mut self) {
        for _ in 0..12 { self.next_month(); }
    }
}

fn main() {
    let mut date = Date {
        weekday: 0, // Monday
        month: 0,   // January
        year: 1900,
    };
    date.next_year();
    let mut count = 0;
    while date.year < 2001 {
        if date.weekday == 6 { count += 1; }
        date.next_month();
    }
    println!("{}", count);
}
