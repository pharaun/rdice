use nom::{
  IResult,
  bytes::complete::{tag, take_while1},
  combinator::map_res,
};

#[derive(Debug, PartialEq)]
pub struct Ast(Roll);

pub fn parse(input: &str) -> IResult<&str, Ast> {
    let (input, roll) = roll(input)?;

    Ok((input, Ast(roll)))
}

#[derive(Debug, PartialEq)]
pub struct Dice(u32);

#[derive(Debug, PartialEq)]
pub struct Roll(u32, Dice);

fn number(input: &str, radix: u32) -> IResult<&str, u32> {
    let is_digit = |c: char| c.is_digit(radix);
    let from_digit = |s: &str| u32::from_str_radix(s, radix);
    let map = map_res(take_while1(is_digit), from_digit);

    map(input)
}

fn dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("d")(input)?;
    let (input, digit) = number(input, 10)?;

    Ok((input, Dice(digit)))
}

fn roll(input: &str) -> IResult<&str, Roll> {
    let (input, roll) = number(input, 10)?;
    let (input, dice) = dice(input)?;

    Ok((input, Roll(roll, dice)))
}


#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_number() {
        assert_eq!(
            number("20", 10),
            Ok(("", 20))
        );
    }

    #[test]
    fn test_dice() {
        assert_eq!(
            dice("d10"),
            Ok(("", Dice(10)))
        );
    }

    #[test]
    fn test_roll() {
        assert_eq!(
            roll("5d10"),
            Ok(("", Roll(5, Dice(10))))
        );
    }
}
