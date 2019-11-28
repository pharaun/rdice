use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
  },
  character::complete::multispace0,
  combinator::{
      map_res,
      opt,
      map,
  },
  branch::alt,
  sequence::{
      preceded,
      delimited,
  },
  multi::many0,
};

#[derive(Debug, PartialEq)]
pub struct Ast(OpsVal);

pub fn parse(input: &str) -> IResult<&str, Ast> {
    map(expr, Ast)(input)
}

// We can inline an expr where-ever a number is accepted
#[derive(Debug, PartialEq)]
pub enum Num {
    Num(u32),
    // Can only see the value of the expression, not the computation in the output
    Inline(Box<OpsVal>),
}

// TODO: will make evaul a bit complicated, but each Dice can eval to 1 or more dice roll
#[derive(Debug, PartialEq)]
pub struct Dice(Num, DiceMeta);

#[derive(Debug, PartialEq)]
pub enum DiceMeta {
    Plain,
    Exploding(DiceOper),
}

#[derive(Debug, PartialEq)]
pub enum DiceOper {
    IEq, // Implicit Eq
    Eq(Num),
    Gt(Num),
    Lt(Num),
}


// TODO: This can probably be folded into the Expr
// TODO: add roll meta (for whole dice group effects)
#[derive(Debug, PartialEq)]
pub struct Roll(Num, Dice);

#[derive(Debug, PartialEq)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

// TODO: handle some form of doc string inline ie `2d10 + 2d6[crit] + 5` for eg.
#[derive(Debug, PartialEq)]
pub enum OpsVal {
    Roll(Roll),
    Number(Num),
    Expr(Oper, Box<OpsVal>, Box<OpsVal>),
}

fn number(input: &str) -> IResult<&str, u32> {
    let radix = 10;

    let is_digit = |c: char| c.is_digit(radix);
    let from_digit = |s: &str| u32::from_str_radix(s, radix);
    let map = map_res(take_while1(is_digit), from_digit);

    map(input)
}

fn num(input: &str) -> IResult<&str, Num> {
    alt((
        map(number, Num::Num),
        delimited(
            tag("[["),
            map(expr, |e| Num::Inline(Box::new(e))),
            tag("]]")
        ),
    ))(input)
}

fn dice_oper(input: &str) -> IResult<&str, DiceOper> {
    let (input, oper) = opt(alt((
        map(
            preceded(tag(">"), num),
            |i| DiceOper::Gt(i)
        ),
        map(
            preceded(tag("<"), num),
            |i| DiceOper::Lt(i)
        ),
        map(num, DiceOper::Eq))
    ))(input)?;

    Ok((
        input,
        match oper {
            None    => DiceOper::IEq,
            Some(x) => x,
        }
    ))
}

fn dice_meta(input: &str) -> IResult<&str, DiceMeta> {
    let (input, exploding) = opt(tag("!"))(input)?;

    match exploding {
        None    => Ok((input, DiceMeta::Plain)),
        Some(_) => {
            let (input, oper) = dice_oper(input)?;
            Ok((input, DiceMeta::Exploding(oper)))
        },
    }
}

fn dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("d")(input)?;
    let (input, digit) = num(input)?;
    let (input, meta) = dice_meta(input)?;

    Ok((input, Dice(digit, meta)))
}

fn roll(input: &str) -> IResult<&str, Roll> {
    let (input, roll) = opt(num)(input)?;
    let (input, dice) = dice(input)?;

    Ok((input,
        match roll {
            None       => Roll(Num::Num(1), dice),
            Some(roll) => Roll(roll, dice),
        }
    ))
}

// equiv to factor (in nom example)
fn ops_val(input: &str) -> IResult<&str, OpsVal> {
    delimited(
        multispace0,
        alt((
            // TODO: parens here?
            map(roll, OpsVal::Roll),
            map(num, OpsVal::Number),
        )),
        multispace0
    )(input)
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
        map(
            preceded(tag("%"), ops_val),
            |i| (Oper::Mod, i)
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
            Ok(("", Dice(Num::Num(10), DiceMeta::Plain)))
        );
    }

    #[test]
    fn test_roll() {
        assert_eq!(
            roll("5d10"),
            Ok(("", Roll(Num::Num(5), Dice(Num::Num(10), DiceMeta::Plain))))
        );
    }

    #[test]
    fn test_roll_unspecified() {
        assert_eq!(
            roll("d10"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain))))
        );
    }

    #[test]
    fn test_ops_val_roll() {
        assert_eq!(
            ops_val("10d10"),
            Ok(("", OpsVal::Roll(Roll(Num::Num(10), Dice(Num::Num(10), DiceMeta::Plain)))))
        );
    }

    #[test]
    fn test_ops_val_roll_unspecified() {
        assert_eq!(
            ops_val("d10"),
            Ok(("", OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain)))))
        );
    }

    #[test]
    fn test_ops_val_number() {
        assert_eq!(
            ops_val("10"),
            Ok(("", OpsVal::Number(Num::Num(10))))
        );
    }

    #[test]
    fn test_term_mul_fixed_fixed() {
        assert_eq!(
            term("10*20"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Number(Num::Num(10))),
                        Box::new(OpsVal::Number(Num::Num(20)))
            )))
        );
    }

    #[test]
    fn test_term_div_roll_roll_unspecified() {
        assert_eq!(
            term("d2/2d3"),
            Ok(("", OpsVal::Expr(
                        Oper::Div,
                        Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(2), DiceMeta::Plain)))),
                        Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice(Num::Num(3), DiceMeta::Plain))))
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
                            Box::new(OpsVal::Number(Num::Num(10))),
                            Box::new(OpsVal::Number(Num::Num(20)))
                        )),
                        Box::new(OpsVal::Number(Num::Num(30)))
            )))
        );
    }

    #[test]
    fn test_expr_add_mul_fixed() {
        assert_eq!(
            expr("10+20*30"),
            Ok(("", OpsVal::Expr(
                        Oper::Add,
                        Box::new(OpsVal::Number(Num::Num(10))),
                        Box::new(OpsVal::Expr(
                            Oper::Mul,
                            Box::new(OpsVal::Number(Num::Num(20))),
                            Box::new(OpsVal::Number(Num::Num(30)))
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
                            Box::new(OpsVal::Number(Num::Num(10))),
                            Box::new(OpsVal::Expr(
                                Oper::Div,
                                Box::new(OpsVal::Expr(
                                    Oper::Mul,
                                    Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(2), DiceMeta::Plain)))),
                                    Box::new(OpsVal::Roll(Roll(Num::Num(10), Dice(Num::Num(3), DiceMeta::Plain))))
                                )),
                                Box::new(OpsVal::Number(Num::Num(20)))
                            ))
                        )),
                        Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(3), DiceMeta::Plain))))
            )))
        );
    }

    #[test]
    fn test_spaceing() {
        assert_eq!(
            expr("  10 + d2 *  10d20  "),
            Ok(("", OpsVal::Expr(
                        Oper::Add,
                        Box::new(OpsVal::Number(Num::Num(10))),
                        Box::new(OpsVal::Expr(
                            Oper::Mul,
                            Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(2), DiceMeta::Plain)))),
                            Box::new(OpsVal::Roll(Roll(Num::Num(10), Dice(Num::Num(20), DiceMeta::Plain))))
                        ))
            )))
        );
    }

    #[test]
    fn test_inline() {
        assert_eq!(
            expr("[[d3]]d[[2d4+1]]"),
            Ok(("", OpsVal::Roll(
                        Roll(Num::Inline(Box::new(OpsVal::Roll(Roll(
                            Num::Num(1),
                            Dice(Num::Num(3), DiceMeta::Plain)
                        )))),
                        Dice(Num::Inline(Box::new(OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice(Num::Num(4), DiceMeta::Plain)))),
                            Box::new(OpsVal::Number(Num::Num(1)))
                        ))), DiceMeta::Plain)
            ))))
        );
    }

    #[test]
    fn test_num_number() {
        assert_eq!(
            num("10"),
            Ok(("", Num::Num(10)))
        );
    }

    #[test]
    fn test_num_inline() {
        assert_eq!(
            num("[[10]]"),
            Ok(("", Num::Inline(Box::new(OpsVal::Number(Num::Num(10))))))
        );
    }

    #[test]
    fn test_num_nested() {
        assert_eq!(
            num("[[10 + [[20]]]]"),
            Ok(("", Num::Inline(Box::new(OpsVal::Expr(
                                            Oper::Add,
                                            Box::new(OpsVal::Number(Num::Num(10))),
                                            Box::new(OpsVal::Number(
                                                    Num::Inline(Box::new(OpsVal::Number(Num::Num(20))))
                                            ))
            )))))
        );
    }

    #[test]
    fn test_dice_exploding_implicit_eq() {
        assert_eq!(
            dice("d10!"),
            Ok(("", Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::IEq))))
        );
    }

    #[test]
    fn test_dice_exploding_explicit_eq() {
        assert_eq!(
            dice("d10!10"),
            Ok(("", Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Eq(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_exploding_gt() {
        assert_eq!(
            dice("d10!>10"),
            Ok(("", Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Gt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_exploding_lt() {
        assert_eq!(
            dice("d10!<10"),
            Ok(("", Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Lt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_inline_eq() {
        assert_eq!(
            dice("d10![[10]]"),
            Ok(("", Dice(
                Num::Num(10),
                DiceMeta::Exploding(
                    DiceOper::Eq(Num::Inline(Box::new(OpsVal::Number(Num::Num(10)))))
                )
            )))
        );
    }
}
