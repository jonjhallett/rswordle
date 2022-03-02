use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
pub struct Round {
    guesses: Vec<Guess>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Guess {
    Correct(char),
    Present(char),
    Missing(char),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RoundParseError {
    MissingWord,
    MissingGuesses,
    BadLen,
    BadGuessChar,
}

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
                (word_char, 'G') => Ok(Guess::Correct(word_char)),
                (word_char, 'Y') => Ok(Guess::Present(word_char)),
                (word_char, 'X') => Ok(Guess::Missing(word_char)),
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
                Guess::Missing('t'),
                Guess::Present('e'),
                Guess::Correct('r'),
                Guess::Missing('n'),
                Guess::Present('s')
            ]
        })
    );
}

#[test]
fn test_round_badlen() {
    assert_eq!("tern=XXXXX".parse::<Round>(), Err(RoundParseError::BadLen));
    assert_eq!("terns=XXXX".parse::<Round>(), Err(RoundParseError::BadLen));
    assert_eq!("terns".parse::<Round>(), Err(RoundParseError::BadLen));
    assert_eq!(
        "terns=XXXXX=foo".parse::<Round>(),
        Err(RoundParseError::BadLen)
    );
}

#[test]
fn test_round_bad_guess_char() {
    assert_eq!(
        "terns=XXXXA".parse::<Round>(),
        Err(RoundParseError::BadGuessChar)
    );
}

#[test]
fn test_round_missing_word() {
    assert_eq!("=XXXXX".parse::<Round>(), Err(RoundParseError::MissingWord));
}

#[test]
fn test_round_missing_guesses() {
    assert_eq!(
        "terns=".parse::<Round>(),
        Err(RoundParseError::MissingGuesses)
    );
}
