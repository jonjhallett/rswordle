mod wordle;

fn main() {
    println!("{:?}", "terns=XYGXY".parse::<wordle::Round>().unwrap());
}
