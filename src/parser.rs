use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
  },
  combinator::{
      map_res,
      opt,
      map,
  },
  branch::alt,
  sequence::preceded,
  multi::many0,
};

#[derive(Debug, PartialEq)]
pub struct Ast(OpsVal);

pub fn parse(input: &str) -> IResult<&str, Ast> {
    map(expr, Ast)(input)
}

#[derive(Debug, PartialEq)]
pub struct Dice(u32);

// TODO: This can probably be folded into the Expr
#[derive(Debug, PartialEq)]
pub struct Roll(u32, Dice);

#[derive(Debug, PartialEq)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum OpsVal {
    Roll(Roll),
    Number(u32),
    Expr(Oper, Box<OpsVal>, Box<OpsVal>),
}

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

// equiv to factor (in nom example)
fn ops_val(input: &str) -> IResult<&str, OpsVal> {
    alt((
        map(roll, OpsVal::Roll),
        map(number, OpsVal::Number),
        // TODO: parens here?
    ))(input)
}

fn fold_ops_val(initial: OpsVal, remainder: Vec<(Oper, OpsVal)>) -> OpsVal {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (oper, val) = pair;
        OpsVal::Expr(oper, Box::new(acc), Box::new(val))
    })
}

fn term(input: &str) -> IResult<&str, OpsVal> {
    let (input, initial)   = ops_val(input)?;
    let (input, remainder) = many0(alt((
        map(
            preceded(tag("*"), ops_val),
            |i| (Oper::Mul, i)
        ),
        map(
            preceded(tag("/"), ops_val),
            |i| (Oper::Div, i)
        ),
    )))(input)?;

    Ok((input, fold_ops_val(initial, remainder)))
}

fn expr(input: &str) -> IResult<&str, OpsVal> {
    let (input, initial)   = term(input)?;
    let (input, remainder) = many0(alt((
        map(
            preceded(tag("+"), term),
            |i| (Oper::Add, i)
        ),
        map(
            preceded(tag("-"), term),
            |i| (Oper::Sub, i)
        ),
    )))(input)?;

    Ok((input, fold_ops_val(initial, remainder)))
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
    fn test_term_mul_fixed_fixed() {
        assert_eq!(
            term("10*20"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Number(10)),
                        Box::new(OpsVal::Number(20))
            )))
        );
    }

    #[test]
    fn test_term_div_roll_roll_unspecified() {
        assert_eq!(
            term("d2/2d3"),
            Ok(("", OpsVal::Expr(
                        Oper::Div,
                        Box::new(OpsVal::Roll(Roll(1, Dice(2)))),
                        Box::new(OpsVal::Roll(Roll(2, Dice(3))))
            )))
        );
    }

    #[test]
    fn test_term_mul_fixed_mul_fixed_fixed() {
        assert_eq!(
            term("10*20*30"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Expr(
                            Oper::Mul,
                            Box::new(OpsVal::Number(10)),
                            Box::new(OpsVal::Number(20))
                        )),
                        Box::new(OpsVal::Number(30))
            )))
        );
    }

    #[test]
    fn test_expr_add_mul_fixed() {
        assert_eq!(
            expr("10+20*30"),
            Ok(("", OpsVal::Expr(
                        Oper::Add,
                        Box::new(OpsVal::Number(10)),
                        Box::new(OpsVal::Expr(
                            Oper::Mul,
                            Box::new(OpsVal::Number(20)),
                            Box::new(OpsVal::Number(30))
                        ))
            )))
        );
    }

    #[test]
    fn test_expr_all_groups() {
        assert_eq!(
            expr("10+d2*10d3/20-d3"),
            Ok(("", OpsVal::Expr(
                        Oper::Sub,
                        Box::new(OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Number(10)),
                            Box::new(OpsVal::Expr(
                                Oper::Div,
                                Box::new(OpsVal::Expr(
                                    Oper::Mul,
                                    Box::new(OpsVal::Roll(Roll(1, Dice(2)))),
                                    Box::new(OpsVal::Roll(Roll(10, Dice(3))))
                                )),
                                Box::new(OpsVal::Number(20))
                            ))
                        )),
                        Box::new(OpsVal::Roll(Roll(1, Dice(3))))
            )))
        );
    }
}
