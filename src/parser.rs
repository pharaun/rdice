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
  multi::{
      many0,
      separated_nonempty_list,
  },
};

// TODO: at some point reorganize the AST and remove features i don't care/want to implement.
#[derive(Debug, PartialEq)]
pub struct Ast(OpsVal);

pub fn parse(input: &str) -> IResult<&str, Ast> {
    map(target, Ast)(input)
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
pub enum Dice {
    Dice(Num, DiceMeta),

    // TODO: not all operation works on fate dice, might be worth seeing if i can't make it so that
    // we can't get those invalid operation in via types, if not, runtime checks are ok too
    Fate(DiceMeta),
}

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
#[derive(Debug, PartialEq)]
pub enum RollMeta {
    Drop(HiLo, Num),
    Keep(HiLo, Num),
    Reroll(EqOper, Num),

    // TODO: Critcal Success + Fumble - only for display purpose
    CriticalSuccess(EqOper, Num),
    CriticalFumble(EqOper, Num),
}

#[derive(Debug, PartialEq)]
pub enum HiLo {
    High,
    Low,
}

#[derive(Debug, PartialEq)]
pub enum EqOper {
    Eq,
    Gt,
    Lt,
}

// List of OpsVal::Roll or list of opsval expr whatever
#[derive(Debug, PartialEq)]
pub struct GroupRoll(Vec<OpsVal>, Vec<GroupMeta>);

// TODO: maybe instead of having a drop/keep meta on roll we make all roll go into a group and if
// there's more than 1 it can do special handling? For now -> roll or -> roll -> group.
#[derive(Debug, PartialEq)]
pub enum GroupMeta {
    Drop(HiLo, Num),
    Keep(HiLo, Num),
}


#[derive(Debug, PartialEq)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

// TODO: handle some form of doc string inline ie `2d10 + 2d6[crit] + 5` for eg.
#[derive(Debug, PartialEq)]
pub enum OpsVal {
    Roll(Roll),
    GroupRoll(GroupRoll),
    Number(Num),
    Expr(Oper, Box<OpsVal>, Box<OpsVal>),
    // TODO: Func call ie.
    // Func(&str (name), Box<OpsVal>),
    // for - floor/ceiling/abs/round (question tho of floating value calculation....)

    // TODO: i think this is an terminal value, so it should be in its own type but let's put it
    // here for now.
    // TODO: this may need further work, since ie it would need to eval each dice roll 1 by 1 ie
    // 10d5+2 > 3 -> any of the d5+2 that is greater than 3 is then a success for eg...
    // Probably can do 2 terminals (a sum one, or a Target val one) since the evaulation strategy
    // will vary
    Target(TargetOper, Num, Box<OpsVal>),
}

#[derive(Debug, PartialEq)]
pub enum TargetOper {
    Eq,
    Gt,
    GEq,
    Lt,
    LEq,
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
        map(
            preceded(tag("="), num),
            |i| DiceOper::Eq(i)
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

fn digit_dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("d")(input)?;
    let (input, digit) = num(input)?;
    let (input, meta) = dice_meta(input)?;

    Ok((input, Dice::Dice(digit, meta)))
}

fn fate_dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("dF")(input)?;
    let (input, meta) = dice_meta(input)?;

    Ok((input, Dice::Fate(meta)))
}

fn dice(input: &str) -> IResult<&str, Dice> {
    alt((
        fate_dice,
        digit_dice,
    ))(input)
}

fn roll_meta(input: &str) -> IResult<&str, RollMeta> {
    alt((
        map(
            preceded(tag("dl"), num),
            |i| RollMeta::Drop(HiLo::Low, i)
        ),
        // Alias for dl
        map(
            preceded(tag("d"), num),
            |i| RollMeta::Drop(HiLo::Low, i)
        ),
        map(
            preceded(tag("dh"), num),
            |i| RollMeta::Drop(HiLo::High, i)
        ),
        map(
            preceded(tag("kl"), num),
            |i| RollMeta::Keep(HiLo::Low, i)
        ),
        map(
            preceded(tag("kh"), num),
            |i| RollMeta::Keep(HiLo::High, i)
        ),
        // Alias for kh
        map(
            preceded(tag("k"), num),
            |i| RollMeta::Keep(HiLo::High, i)
        ),
        // Rerolls
        map(
            preceded(tag("r>"), num),
            |i| RollMeta::Reroll(EqOper::Gt, i)
        ),
        map(
            preceded(tag("r<"), num),
            |i| RollMeta::Reroll(EqOper::Lt, i)
        ),
        map(
            preceded(tag("r"), num),
            |i| RollMeta::Reroll(EqOper::Eq, i)
        ),
        map(
            preceded(tag("r="), num),
            |i| RollMeta::Reroll(EqOper::Eq, i)
        ),
        // Critical Success, Critical Fumble
        map(
            preceded(tag("cs>"), num),
            |i| RollMeta::CriticalSuccess(EqOper::Gt, i)
        ),
        map(
            preceded(tag("cs<"), num),
            |i| RollMeta::CriticalSuccess(EqOper::Lt, i)
        ),
        map(
            preceded(tag("cs"), num),
            |i| RollMeta::CriticalSuccess(EqOper::Eq, i)
        ),
        map(
            preceded(tag("cs="), num),
            |i| RollMeta::CriticalSuccess(EqOper::Eq, i)
        ),
        map(
            preceded(tag("cf>"), num),
            |i| RollMeta::CriticalFumble(EqOper::Gt, i)
        ),
        map(
            preceded(tag("cf<"), num),
            |i| RollMeta::CriticalFumble(EqOper::Lt, i)
        ),
        map(
            preceded(tag("cf"), num),
            |i| RollMeta::CriticalFumble(EqOper::Eq, i)
        ),
        map(
            preceded(tag("cf="), num),
            |i| RollMeta::CriticalFumble(EqOper::Eq, i)
        ),
    ))(input)
}

fn roll(input: &str) -> IResult<&str, Roll> {
    let (input, roll) = opt(num)(input)?;
    let (input, dice) = dice(input)?;
    let (input, meta) = many0(roll_meta)(input)?;

    Ok((input,
        match roll {
            None       => Roll(Num::Num(1), dice, meta),
            Some(roll) => Roll(roll, dice, meta),
        }
    ))
}

fn group_meta(input: &str) -> IResult<&str, GroupMeta> {
    alt((
        map(
            preceded(tag("dl"), num),
            |i| GroupMeta::Drop(HiLo::Low, i)
        ),
        // Alias for dl
        map(
            preceded(tag("d"), num),
            |i| GroupMeta::Drop(HiLo::Low, i)
        ),
        map(
            preceded(tag("dh"), num),
            |i| GroupMeta::Drop(HiLo::High, i)
        ),
        map(
            preceded(tag("kl"), num),
            |i| GroupMeta::Keep(HiLo::Low, i)
        ),
        map(
            preceded(tag("kh"), num),
            |i| GroupMeta::Keep(HiLo::High, i)
        ),
        // Alias for kh
        map(
            preceded(tag("k"), num),
            |i| GroupMeta::Keep(HiLo::High, i)
        ),
    ))(input)
}

fn group_roll(input: &str) -> IResult<&str, GroupRoll> {
    let (input, opsvals) = delimited(
        tag("{"),
        separated_nonempty_list(tag(","), expr),
        tag("}")
    )(input)?;
    let (input, meta) = many0(group_meta)(input)?;

    Ok((input, GroupRoll(opsvals, meta)))
}

// equiv to factor (in nom example)
fn ops_val(input: &str) -> IResult<&str, OpsVal> {
    delimited(
        multispace0,
        alt((
            // TODO: parens here?
            map(roll, OpsVal::Roll),
            map(group_roll, OpsVal::GroupRoll),
            map(num, OpsVal::Number),
        )),
        multispace0
    )(input)
}

fn parens(input: &str) -> IResult<&str, OpsVal> {
    alt((
        delimited(
            multispace0,
            delimited(
                tag("("),
                expr,
                tag(")"),
            ),
            multispace0,
        ),
        ops_val,
    ))(input)
}

fn fold_ops_val(initial: OpsVal, remainder: Vec<(Oper, OpsVal)>) -> OpsVal {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (oper, val) = pair;
        OpsVal::Expr(oper, Box::new(acc), Box::new(val))
    })
}

fn pow(input: &str) -> IResult<&str, OpsVal> {
    let (input, initial)   = parens(input)?;
    let (input, remainder) = many0(
        map(
            preceded(tag("^"), parens),
            |i| (Oper::Pow, i)
        )
    )(input)?;

    Ok((input, fold_ops_val(initial, remainder)))
}

fn term(input: &str) -> IResult<&str, OpsVal> {
    let (input, initial)   = pow(input)?;
    let (input, remainder) = many0(alt((
        map(
            preceded(tag("*"), pow),
            |i| (Oper::Mul, i)
        ),
        map(
            preceded(tag("/"), pow),
            |i| (Oper::Div, i)
        ),
        map(
            preceded(tag("%"), pow),
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

// TODO: make the {} delimitation apply more broadly
fn target(input: &str) -> IResult<&str, OpsVal> {
    let (input, opsval) = alt((
        // TODO: the delimited needs to apply before expr otherwise it becomes GroupRoll first
        delimited(
            tag("{"),
            expr,
            tag("}"),
        ),
        expr,
    ))(input)?;

    let (input, ttarget) = opt(alt((
        map(
            preceded(tag(">="), num),
            |i| (TargetOper::GEq, i)
        ),
        map(
            preceded(tag(">"), num),
            |i| (TargetOper::Gt, i)
        ),
        map(
            preceded(tag("<="), num),
            |i| (TargetOper::LEq, i)
        ),
        map(
            preceded(tag("<"), num),
            |i| (TargetOper::Lt, i)
        ),
        map(
            preceded(tag("="), num),
            |i| (TargetOper::Eq, i)
        )
    )))(input)?;

    Ok((input, match ttarget {
        None         => opsval,
        Some((t, n)) => OpsVal::Target(t, n, Box::new(opsval)),
    }))
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
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Plain)))
        );
    }

    #[test]
    fn test_fate() {
        assert_eq!(
            dice("dF"),
            Ok(("", Dice::Fate(DiceMeta::Plain)))
        );
    }

    #[test]
    fn test_roll() {
        assert_eq!(
            roll("5d10"),
            Ok(("", Roll(Num::Num(5), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![])))
        );
    }

    #[test]
    fn test_roll_unspecified() {
        assert_eq!(
            roll("d10"),
            Ok(("", Roll(Num::Num(1), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![])))
        );
    }

    #[test]
    fn test_roll_critical() {
        assert_eq!(
            roll("d10cs10cf>2"),
            Ok(("", Roll(
                Num::Num(1),
                Dice::Dice(Num::Num(10), DiceMeta::Plain),
                vec![
                    RollMeta::CriticalSuccess(EqOper::Eq, Num::Num(10)),
                    RollMeta::CriticalFumble(EqOper::Gt, Num::Num(2)),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_reroll() {
        assert_eq!(
            roll("d10r10r>2"),
            Ok(("", Roll(
                Num::Num(1),
                Dice::Dice(Num::Num(10), DiceMeta::Plain),
                vec![
                    RollMeta::Reroll(EqOper::Eq, Num::Num(10)),
                    RollMeta::Reroll(EqOper::Gt, Num::Num(2)),
                ]
            )))
        );
    }

    #[test]
    fn test_ops_val_roll() {
        assert_eq!(
            ops_val("10d10"),
            Ok(("", OpsVal::Roll(Roll(Num::Num(10), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![]))))
        );
    }

    #[test]
    fn test_ops_val_roll_unspecified() {
        assert_eq!(
            ops_val("d10"),
            Ok(("", OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![]))))
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
                        Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                        Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(3), DiceMeta::Plain), vec![])))
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
    fn test_expr_pow() {
        // TODO: ordering is wrong, roll20 is 4^(3^2) while this is (4^3)^2
        // But various languages differs here, some are former some are later
        assert_eq!(
            expr("4^3^2"),
            Ok(("", OpsVal::Expr(
                        Oper::Pow,
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(Num::Num(4))),
                            Box::new(OpsVal::Number(Num::Num(3)))
                        )),
                        Box::new(OpsVal::Number(Num::Num(2))),
            )))
        );
    }

    #[test]
    fn test_expr_pow_paren() {
        assert_eq!(
            expr("4^(3^2)"),
            Ok(("", OpsVal::Expr(
                        Oper::Pow,
                        Box::new(OpsVal::Number(Num::Num(4))),
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(Num::Num(3))),
                            Box::new(OpsVal::Number(Num::Num(2))),
                        ))
            )))
        );
    }

    #[test]
    fn test_expr_pow_mul() {
        assert_eq!(
            expr("4^3*2"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(Num::Num(4))),
                            Box::new(OpsVal::Number(Num::Num(3)))
                        )),
                        Box::new(OpsVal::Number(Num::Num(2))),
            )))
        );
    }

    #[test]
    fn test_expr_pow_mul_other() {
        assert_eq!(
            expr("4*3^2"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Number(Num::Num(4))),
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(Num::Num(3))),
                            Box::new(OpsVal::Number(Num::Num(2)))
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
                                    Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                                    Box::new(OpsVal::Roll(Roll(Num::Num(10), Dice::Dice(Num::Num(3), DiceMeta::Plain), vec![])))
                                )),
                                Box::new(OpsVal::Number(Num::Num(20)))
                            ))
                        )),
                        Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(3), DiceMeta::Plain), vec![])))
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
                            Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                            Box::new(OpsVal::Roll(Roll(Num::Num(10), Dice::Dice(Num::Num(20), DiceMeta::Plain), vec![])))
                        ))
            )))
        );
    }

    #[test]
    fn test_expr_paren() {
        assert_eq!(
            expr(" ( 1 + 2 ) * 3"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Number(Num::Num(1))),
                            Box::new(OpsVal::Number(Num::Num(2))),
                        )),
                        Box::new(OpsVal::Number(Num::Num(3))),
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
                            Dice::Dice(Num::Num(3), DiceMeta::Plain),
                            vec![]
                        )))),
                        Dice::Dice(Num::Inline(Box::new(OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(4), DiceMeta::Plain), vec![]))),
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
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::IEq))))
        );
    }

    #[test]
    fn test_dice_exploding_explicit_eq() {
        assert_eq!(
            dice("d10!10"),
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Eq(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_exploding_double_explicit_eq() {
        assert_eq!(
            dice("d10!=10"),
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Eq(Num::Num(10))))))
        );
    }


    #[test]
    fn test_dice_exploding_gt() {
        assert_eq!(
            dice("d10!>10"),
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Gt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_exploding_lt() {
        assert_eq!(
            dice("d10!<10"),
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Lt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_inline_eq() {
        assert_eq!(
            dice("d10![[10]]"),
            Ok(("", Dice::Dice(
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
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Compounding(DiceOper::Lt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_dice_penetrating_lt() {
        assert_eq!(
            dice("d10!p<10"),
            Ok(("", Dice::Dice(Num::Num(10), DiceMeta::Penetrating(DiceOper::Lt(Num::Num(10))))))
        );
    }

    #[test]
    fn test_roll_unspecified_drop() {
        assert_eq!(
            roll("d10dl1dh2d3"),
            Ok(("", Roll(
                Num::Num(1),
                Dice::Dice(Num::Num(10),
                DiceMeta::Plain),
                vec![
                    RollMeta::Drop(HiLo::Low, Num::Num(1)),
                    RollMeta::Drop(HiLo::High, Num::Num(2)),
                    RollMeta::Drop(HiLo::Low, Num::Num(3)),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_keep() {
        assert_eq!(
            roll("d10kl1kh2k3"),
            Ok(("", Roll(
                Num::Num(1),
                Dice::Dice(Num::Num(10),
                DiceMeta::Plain),
                vec![
                    RollMeta::Keep(HiLo::Low, Num::Num(1)),
                    RollMeta::Keep(HiLo::High, Num::Num(2)),
                    RollMeta::Keep(HiLo::High, Num::Num(3)),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_eq() {
        assert_eq!(
            target("d10=2"),
            Ok(("", OpsVal::Target(
                TargetOper::Eq,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Plain),
                    vec![]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_lt() {
        assert_eq!(
            target("d10<2"),
            Ok(("", OpsVal::Target(
                TargetOper::Lt,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Plain),
                    vec![]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_gt() {
        assert_eq!(
            target("d10>2"),
            Ok(("", OpsVal::Target(
                TargetOper::Gt,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Plain),
                    vec![]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_leq() {
        assert_eq!(
            target("d10<=2"),
            Ok(("", OpsVal::Target(
                TargetOper::LEq,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Plain),
                    vec![]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_geq() {
        assert_eq!(
            target("d10>=2"),
            Ok(("", OpsVal::Target(
                TargetOper::GEq,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Plain),
                    vec![]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_dice_nested_meta() {
        assert_eq!(
            target("d10!3d1>2"),
            Ok(("", OpsVal::Target(
                TargetOper::Gt,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::Eq(Num::Num(3)))),
                    vec![RollMeta::Drop(HiLo::Low, Num::Num(1))]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_braced_meta() {
        assert_eq!(
            target("{d10!}>2"),
            Ok(("", OpsVal::Target(
                TargetOper::Gt,
                Num::Num(2),
                Box::new(OpsVal::Roll(Roll(
                    Num::Num(1),
                    Dice::Dice(Num::Num(10), DiceMeta::Exploding(DiceOper::IEq)),
                    vec![]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_target_modifier() {
        assert_eq!(
            target("d10+1>3"),
            Ok(("", OpsVal::Target(
                TargetOper::Gt,
                Num::Num(3),
                Box::new(OpsVal::Expr(
                    Oper::Add,
                    Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![]))),
                    Box::new(OpsVal::Number(Num::Num(1)))
                ))
            )))
        );
    }

    #[test]
    fn test_roll_target_modifier_braced() {
        assert_eq!(
            target("{d10+1}>3"),
            Ok(("", OpsVal::Target(
                TargetOper::Gt,
                Num::Num(3),
                Box::new(OpsVal::Expr(
                    Oper::Add,
                    Box::new(OpsVal::Roll(Roll(Num::Num(1), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![]))),
                    Box::new(OpsVal::Number(Num::Num(1)))
                ))
            )))
        );
    }

    #[test]
    fn test_group_roll() {
        assert_eq!(
            group_roll("{2d10+2d2}kh2"),
            Ok(("", GroupRoll(
                vec![
                    OpsVal::Expr(
                        Oper::Add,
                        Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![]))),
                        Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                    )
                ],
                vec![GroupMeta::Keep(HiLo::High, Num::Num(2))],
            )))
        );
    }

    #[test]
    fn test_list_group_roll() {
        assert_eq!(
            group_roll("{2d10,2d2}kh2"),
            Ok(("", GroupRoll(
                vec![
                    OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![])),
                    OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(2), DiceMeta::Plain), vec![])),
                ],
                vec![GroupMeta::Keep(HiLo::High, Num::Num(2))],
            )))
        );
    }

    #[test]
    fn test_list_group_target_roll() {
        assert_eq!(
            target("{2d10,2d10+2d2}kh2>3"),
            Ok(("", OpsVal::Target(
                TargetOper::Gt,
                Num::Num(3),
                Box::new(OpsVal::GroupRoll(GroupRoll(
                    vec![
                        OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![])),
                        OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(10), DiceMeta::Plain), vec![]))),
                            Box::new(OpsVal::Roll(Roll(Num::Num(2), Dice::Dice(Num::Num(2), DiceMeta::Plain), vec![]))),
                        )
                    ],
                    vec![GroupMeta::Keep(HiLo::High, Num::Num(2))],
                )))
            )))
        );
    }
}
