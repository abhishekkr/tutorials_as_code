/*
`impl` => Associate functions with new types
*/

#[derive(Debug)]
struct Player {
    name: String,
    score: u64,
}

impl Player {
    // no self ref; a static method
    fn new(name: &str) -> Self {
        Self {
            name: String::from(name),
            score: 0,
        }
    }

    // * '&mut self' borrows unique mutable ref
    // * object is usable again
    fn update_score(&mut self, score: u64) {
        self.score += score;
    }

    // * '&self' borrows object from caller with shared immutable ref
    // * object is usable again
    fn show(&self) {
        println!("Player: {} (Scored {}p)", self.name, self.score);
    }

    // * 'self' takes ownership of object, moves it away from caller
    // * object is deallocated on return, unless ownership is explicitly transmitted
    // * complete ownership doesn't translate to mutability
    fn end(self) {
        println!(
            "Player {} finished game with {}p score.",
            self.name, self.score
        );
    }

    // * same as 'self' but with mutability
    fn end_score(mut self, score: u64) {
        self.score += score;
        println!(
            "Player {} finished game with {}p score.",
            self.name, self.score
        );
    }
}

fn main() {
    let mut johnd = Player::new("JohnDoe");
    johnd.update_score(70);
    johnd.show();
    johnd.update_score(30);
    johnd.show();
    johnd.end();

    let jackd = Player::new("JackDoe");
    jackd.end_score(50);
}
