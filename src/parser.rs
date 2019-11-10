use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
  },
  combinator::{
      map_res,
      opt,
  },
  branch::alt,
};

// Repeative pattern of accepting an parse then rewrapping the value of the parse
macro_rules! wrap_value {
    ($func:expr, $val:expr) => {
        {
            |i| {
                let (input, val) = $func(i)?;
                Ok((input, $val(val)))
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    Roll(Roll),
    Ops(Ops),
}

pub fn parse(input: &str) -> IResult<&str, Ast> {
    alt((
        wrap_value!(ops, Ast::Ops),
        wrap_value!(roll, Ast::Roll),
    ))(input)
}

#[derive(Debug, PartialEq)]
pub struct Dice(u32);

#[derive(Debug, PartialEq)]
pub struct Roll(u32, Dice);

#[derive(Debug, PartialEq)]
pub enum OpsVal {
    Roll(Roll),
    Number(u32),
}

#[derive(Debug, PartialEq)]
pub enum OpsType {
    Add,
    Sub,
}

#[derive(Debug, PartialEq)]
pub struct Ops(OpsType, OpsVal, OpsVal);


fn number(input: &str) -> IResult<&str, u32> {
    let radix = 10;

    let is_digit = |c: char| c.is_digit(radix);
    let from_digit = |s: &str| u32::from_str_radix(s, radix);
    let map = map_res(take_while1(is_digit), from_digit);

    map(input)
}

fn dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("d")(input)?;
    let (input, digit) = number(input)?;

    Ok((input, Dice(digit)))
}

fn roll(input: &str) -> IResult<&str, Roll> {
    let (input, roll) = opt(number)(input)?;
    let (input, dice) = dice(input)?;

    Ok((input,
        match roll {
            None       => Roll(1, dice),
            Some(roll) => Roll(roll, dice),
        }
    ))
}

fn ops_val(input: &str) -> IResult<&str, OpsVal> {
    alt((
        wrap_value!(roll, OpsVal::Roll),
        wrap_value!(number, OpsVal::Number),
    ))(input)
}

fn ops_type(input: &str) -> IResult<&str, OpsType> {
    alt((
        wrap_value!(tag("+"), |_| OpsType::Add),
        wrap_value!(tag("-"), |_| OpsType::Sub),
    ))(input)
}

// TODO: this does not handle precedence or nested ops
fn ops(input: &str) -> IResult<&str, Ops> {
    let (input, val1) = ops_val(input)?;
    let (input, op)   = ops_type(input)?;
    let (input, val2) = ops_val(input)?;

    Ok((input, Ops(op, val1, val2)))
}


#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_number() {
        assert_eq!(
            number("20"),
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

    #[test]
    fn test_roll_unspecified() {
        assert_eq!(
            roll("d10"),
            Ok(("", Roll(1, Dice(10))))
        );
    }

    #[test]
    fn test_ops_val_roll() {
        assert_eq!(
            ops_val("10d10"),
            Ok(("", OpsVal::Roll(Roll(10, Dice(10)))))
        );
    }

    #[test]
    fn test_ops_val_roll_unspecified() {
        assert_eq!(
            ops_val("d10"),
            Ok(("", OpsVal::Roll(Roll(1, Dice(10)))))
        );
    }

    #[test]
    fn test_ops_val_number() {
        assert_eq!(
            ops_val("10"),
            Ok(("", OpsVal::Number(10)))
        );
    }

    #[test]
    fn test_ops_type() {
        assert_eq!(
            ops_type("+"),
            Ok(("", OpsType::Add))
        );

        assert_eq!(
            ops_type("-"),
            Ok(("", OpsType::Sub))
        );
    }

    #[test]
    fn test_ops_add_fixed_fixed() {
        assert_eq!(
            ops("10+20"),
            Ok(("", Ops(OpsType::Add, OpsVal::Number(10), OpsVal::Number(20))))
        );
    }

    #[test]
    fn test_ops_sub_roll_roll_unspecified() {
        assert_eq!(
            ops("10d20-d5"),
            Ok(("", Ops(OpsType::Sub, OpsVal::Roll(Roll(10, Dice(20))), OpsVal::Roll(Roll(1, Dice(5))))))
        );
    }
}
