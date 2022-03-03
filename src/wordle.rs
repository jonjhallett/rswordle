use std::error::Error;
use std::fmt::{self, Display};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
pub struct Round {
    guesses: Vec<Guess>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Guess(char, GuessResult);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GuessResult {
    Correct,
    Present,
    Missing,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RoundParseError {
    MissingWord,
    MissingGuesses,
    BadLen,
    BadGuessChar,
}

impl Display for RoundParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message = match self {
            RoundParseError::MissingWord => "word missing",
            RoundParseError::MissingGuesses => "guesses string missing",
            RoundParseError::BadLen => "wrong length",
            RoundParseError::BadGuessChar => "guess character should be G, Y or X",
        };
        write!(f, "{}", message)
    }
}

impl Error for RoundParseError {}

impl FromStr for Round {
    type Err = RoundParseError;

    fn from_str(round_string: &str) -> Result<Round, Self::Err> {
        let round_vec = round_string.split('=').collect::<Vec<&str>>();

        let (word, guesses_str) = match round_vec[..] {
            ["", _] => return Err(RoundParseError::MissingWord),
            [_, ""] => return Err(RoundParseError::MissingGuesses),
            [word, guesses_str] if word.len() == 5 && guesses_str.len() == 5 => (word, guesses_str),
            _ => return Err(RoundParseError::BadLen),
        };

        let guesses_result: Result<Vec<Guess>, Self::Err> = word
            .chars()
            .zip(guesses_str.chars())
            .map(|(word_char, guess_char)| match (word_char, guess_char) {
                (ch, 'G') => Ok(Guess(ch, GuessResult::Correct)),
                (ch, 'Y') => Ok(Guess(ch, GuessResult::Present)),
                (ch, 'X') => Ok(Guess(ch, GuessResult::Missing)),
                (_, _) => return Err(RoundParseError::BadGuessChar),
            })
            .collect();

        Ok(Round {
            guesses: guesses_result?,
        })
    }
}

#[test]
fn test_round_from_str() {
    assert_eq!(
        "terns=XYGXY".parse::<Round>(),
        Ok(Round {
            guesses: vec![
                Guess('t', GuessResult::Missing),
                Guess('e', GuessResult::Present),
                Guess('r', GuessResult::Correct),
                Guess('n', GuessResult::Missing),
                Guess('s', GuessResult::Present),
            ]
        })
    );
}

#[test]
fn test_round_badlen() {
    assert_eq!("tern=XXXXX".parse::<Round>(), Err(RoundParseError::BadLen));
    assert_eq!("terns=XXXX".parse::<Round>(), Err(RoundParseError::BadLen));
    assert_eq!("terns".parse::<Round>(), Err(RoundParseError::BadLen));
    let res = "terns=XXXXX=foo".parse::<Round>();
    assert_eq!(res, Err(RoundParseError::BadLen));
    assert_eq!(res.unwrap_err().to_string(), "wrong length");
}

#[test]
fn test_round_bad_guess_char() {
    let res = "terns=XXXXA".parse::<Round>();
    assert_eq!(res, Err(RoundParseError::BadGuessChar));
    assert_eq!(
        res.unwrap_err().to_string(),
        "guess character should be G, Y or X"
    );
}

#[test]
fn test_round_missing_word() {
    let res = "=XXXXX".parse::<Round>();
    assert_eq!(res, Err(RoundParseError::MissingWord));
    assert_eq!(res.unwrap_err().to_string(), "word missing");
}

#[test]
fn test_round_missing_guesses() {
    let res = "terns=".parse::<Round>();
    assert_eq!(res, Err(RoundParseError::MissingGuesses));
    assert_eq!(res.unwrap_err().to_string(), "guesses string missing");
}
