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
    Compounding(DiceOper),
    Penetrating(DiceOper),
}

// TODO: unitify some of the Oper with RollMeta and add support for greater than or eq and lt or eq
#[derive(Debug, PartialEq)]
pub enum DiceOper {
    // Implicit Eq, to support `d[[2d10]]!` notion (Otherwise it would be stored as
    // d[[2d10]]![[2d10]] and evaulated twice which could diverge)
    IEq,
    Eq(Num),
    Gt(Num),
    Lt(Num),
}


// TODO: This can probably be folded into the Expr
// TODO: Consider breaking this into the full roll and 'limited' roll for inside inline expressions
// TODO: RollMeta -> Vec, because can have multiple effects on a roll pool (ie keep, drop, ...)
#[derive(Debug, PartialEq)]
pub struct Roll(Num, Dice, Vec<RollMeta>);

// TODO: not sure we want to allow anything other than plain outcome for a RollMeta inside an
// Inline Expression
// TODO: consider separating the Eq/Gt/Lt meta to its own (success meta)
#[derive(Debug, PartialEq)]
pub enum RollMeta {
    Eq(Num),
    Gt(Num),
    GEq(Num),
    Lt(Num),
    LEq(Num),
}

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
    let (input, meta) = opt(alt((
        map(
            preceded(tag("!!"), dice_oper),
            |i| DiceMeta::Compounding(i)
        ),
        map(
            preceded(tag("!p"), dice_oper),
            |i| DiceMeta::Penetrating(i)
        ),
        map(
            preceded(tag("!"), dice_oper),
            |i| DiceMeta::Exploding(i)
        ),
    )))(input)?;

    // TODO: find a good way to handle 'fallback' values
    Ok((
        input,
        match meta {
            None    => DiceMeta::Plain,
            Some(x) => x,
        }
    ))
}

fn dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("d")(input)?;
    let (input, digit) = num(input)?;
    let (input, meta) = dice_meta(input)?;

    Ok((input, Dice(digit, meta)))
}

// TODO: make the {} delimitation apply more broadly
fn delimited_dice(input: &str) -> IResult<&str, Dice> {
    alt((
        dice,
        delimited(
            tag("{"),
            dice,
            tag("}")
        ),
    ))(input)
}

fn roll_meta(input: &str) -> IResult<&str, RollMeta> {
    alt((
        map(
            preceded(tag(">="), num),
            |i| RollMeta::GEq(i)
        ),
        map(
            preceded(tag(">"), num),
            |i| RollMeta::Gt(i)
        ),
        map(
            preceded(tag("<="), num),
            |i| RollMeta::LEq(i)
        ),
        map(
            preceded(tag("<"), num),
            |i| RollMeta::Lt(i)
        ),
        map(
            preceded(tag("="), num),
            |i| RollMeta::Eq(i)
        )
    ))(input)
}

fn roll(input: &str) -> IResult<&str, Roll> {
    let (input, roll) = opt(num)(input)?;
    let (input, dice) = delimited_dice(input)?;
    let (input, meta) = many0(roll_meta)(input)?;

    Ok((input,
        match roll {
            None       => Roll(Num::Num(1), dice, meta),
            Some(roll) => Roll(roll, dice, meta),
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
            Ok(("", Roll(Num::Num(5), Dice(Num::Num(10), DiceMeta::Plain), vec![])))
        );
    }

    #[test]
    fn test_roll_unspecified() {
        assert_eq!(
            roll("d10"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![])))
        );
    }

    #[test]
    fn test_ops_val_roll() {
        assert_eq!(
            ops_val("10d10"),
            Ok(("", OpsVal::Roll(Roll(Num::Num(10), Dice(Num::Num(10), DiceMeta::Plain), vec![]))))
        );
    }

    #[test]
    fn test_ops_val_roll_unspecified() {
        assert_eq!(
            ops_val("d10"),
            Ok(("", OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![]))))
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
                        Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                        Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice(Num::Num(3), DiceMeta::Plain), vec![])))
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
                                    Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                                    Box::new(OpsVal::Roll(Roll(Num::Num(10), Dice(Num::Num(3), DiceMeta::Plain), vec![])))
                                )),
                                Box::new(OpsVal::Number(Num::Num(20)))
                            ))
                        )),
                        Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(3), DiceMeta::Plain), vec![])))
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
                            Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                            Box::new(OpsVal::Roll(Roll(Num::Num(10), Dice(Num::Num(20), DiceMeta::Plain), vec![])))
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
                            Dice(Num::Num(3), DiceMeta::Plain),
                            vec![]
                        )))),
                        Dice(Num::Inline(Box::new(OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice(Num::Num(4), DiceMeta::Plain), vec![]))),
                            Box::new(OpsVal::Number(Num::Num(1)))
                        ))), DiceMeta::Plain),
                        vec![]
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

    #[test]
    fn test_dice_compounding_lt() {
        assert_eq!(
            dice("d10!!<10"),
            Ok(("", Dice(Num::Num(10), DiceMeta::Compounding(DiceOper::Lt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_penetrating_lt() {
        assert_eq!(
            dice("d10!p<10"),
            Ok(("", Dice(Num::Num(10), DiceMeta::Penetrating(DiceOper::Lt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_eq() {
        assert_eq!(
            roll("d10=2"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![RollMeta::Eq(Num::Num(2))])))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_lt() {
        assert_eq!(
            roll("d10<2"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![RollMeta::Lt(Num::Num(2))])))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_gt() {
        assert_eq!(
            roll("d10>2"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![RollMeta::Gt(Num::Num(2))])))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_leq() {
        assert_eq!(
            roll("d10<=2"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![RollMeta::LEq(Num::Num(2))])))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_geq() {
        assert_eq!(
            roll("d10>=2"),
            Ok(("", Roll(Num::Num(1), Dice(Num::Num(10), DiceMeta::Plain), vec![RollMeta::GEq(Num::Num(2))])))
        );
    }

    #[test]
    fn test_roll_dice_nested_meta() {
        assert_eq!(
            roll("d10!3>2"),
            Ok(("", Roll(
                Num::Num(1),
                Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Eq(Num::Num(3)))),
                vec![RollMeta::Gt(Num::Num(2))]
            )))
        );
    }

    #[test]
    fn test_roll_dice_eq_meta() {
        assert_eq!(
            roll("d10!=2"),
            Ok(("", Roll(
                Num::Num(1),
                Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::IEq)),
                vec![RollMeta::Eq(Num::Num(2))]
            )))
        );
    }

    #[test]
    fn test_roll_braced_meta() {
        assert_eq!(
            roll("{d10!}>2"),
            Ok(("", Roll(
                Num::Num(1),
                Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::IEq)),
                vec![RollMeta::Gt(Num::Num(2))]
            )))
        );
    }
}
