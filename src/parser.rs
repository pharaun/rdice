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

type Num = u32;

#[derive(Debug, PartialEq)]
pub enum ComparePoint {
    // Default Behavor (when there's no Compare Point) (Rename to Default?)
    IEq,
    Eq(Num),
    Gt(Num),
    Lt(Num),
}

// TODO: computed dice (N+Y)dX/Nd(X+Y) (basic + fate)
#[derive(Debug, PartialEq)]
pub enum Dice {
    Dice(Num),

    // TODO: not all operation works on fate dice, might be worth seeing if i can't make it so that
    // we can't get those invalid operation in via types, if not, runtime checks are ok too
    Fate,
}

// TODO: This can probably be folded into the Expr
// TODO: RollMeta -> Vec, because can have multiple effects on a roll pool (ie keep, drop, ...)
// TODO: computed dice (N+Y)dX/Nd(X+Y) (basic + fate)
#[derive(Debug, PartialEq)]
pub struct Roll(Num, Dice, Vec<RollMeta>);

// TODO: have two type of meta, one for fate and one for regular dice rolls (if needed)
#[derive(Debug, PartialEq)]
pub enum RollMeta {
    // Display only:
    // * Matching?
    // * Sorting
    //
    // Supports:
    // * Target (success, fail)
    // * Exploding
    // * Compounding
    // * Penetrating
    // * Keep/Drop (Hi, Lo)
    // * Reroll (Std, Only Once)
    Drop(HiLo, Num),
    Keep(HiLo, Num),

    Reroll(ComparePoint),
    Exploding(ComparePoint),
    Compounding(ComparePoint),
    Penetrating(ComparePoint),

    // TODO: Critcal Success + Fumble - only for display purpose
    // TODO: have Critical(Type, ComparePoint) ? Where Type = Success/Fumble (alt Success/Fail)
    CriticalSuccess(ComparePoint),
    CriticalFumble(ComparePoint),

    // TODO: Display purpose (sort order s/sa and sd for sort ascending and sort descending)
    // TODO: should only have one sort order, multiples doesn't make sense
    Sort(SortOrder),
}

#[derive(Debug, PartialEq)]
pub enum HiLo {
    High,
    Low,
}

#[derive(Debug, PartialEq)]
pub enum SortOrder {
    Ascending,
    Descending,
}

// List of OpsVal::Roll or list of opsval expr whatever
// TODO: have this be a collection of Rolls (possibly a restricted RollMeta for inside a group)
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

    // TODO: i think this is an terminal value, so it should be in its own type but let's put it
    // here for now.
    // TODO: this may need further work, since ie it would need to eval each dice roll 1 by 1 ie
    // 10d5+2 > 3 -> any of the d5+2 that is greater than 3 is then a success for eg...
    // Probably can do 2 terminals (a sum one, or a Target val one) since the evaulation strategy
    // will vary
    // TODO: can have multiple (ie success == 1, 2, 3 and fail is 4, 5, 6 for eg of d6)
    Target(TargetOper, Box<OpsVal>),
}

#[derive(Debug, PartialEq)]
pub enum TargetOper {
    Success(ComparePoint),
    Fail(ComparePoint),
}


fn number(input: &str) -> IResult<&str, Num> {
    let radix = 10;

    let is_digit = |c: char| c.is_digit(radix);
    let from_digit = |s: &str| u32::from_str_radix(s, radix);
    let map = map_res(take_while1(is_digit), from_digit);

    map(input)
}

// TODO: have a final fallback option? in the alt for IEq?
fn compare_point(input: &str) -> IResult<&str, ComparePoint> {
    let (input, oper) = opt(alt((
        map(
            preceded(tag(">"), number),
            |i| ComparePoint::Gt(i)
        ),
        map(
            preceded(tag("<"), number),
            |i| ComparePoint::Lt(i)
        ),
        map(
            preceded(tag("="), number),
            |i| ComparePoint::Eq(i)
        ),
        map(
            number,
            |i| ComparePoint::Eq(i)
        )),
    ))(input)?;

    Ok((
        input,
        match oper {
            None    => ComparePoint::IEq,
            Some(x) => x,
        }
    ))
}

fn digit_dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("d")(input)?;
    let (input, digit) = number(input)?;

    Ok((input, Dice::Dice(digit)))
}

fn fate_dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = tag("dF")(input)?;

    Ok((input, Dice::Fate))
}

fn dice(input: &str) -> IResult<&str, Dice> {
    alt((
        fate_dice,
        digit_dice,
    ))(input)
}

fn drop_meta(input: &str) -> IResult<&str, RollMeta> {
    let (input, _) = tag("d")(input)?;

    alt((
        map(preceded(tag("l"), number), |i| RollMeta::Drop(HiLo::Low, i)),
        map(preceded(tag("h"), number), |i| RollMeta::Drop(HiLo::High, i)),
        // Alias for dl
        map(number, |i| RollMeta::Drop(HiLo::Low, i)),
    ))(input)
}

fn keep_meta(input: &str) -> IResult<&str, RollMeta> {
    let (input, _) = tag("k")(input)?;

    alt((
        map(preceded(tag("l"), number), |i| RollMeta::Keep(HiLo::Low, i)),
        map(preceded(tag("h"), number), |i| RollMeta::Keep(HiLo::High, i)),
        // Alias for kh
        map(number, |i| RollMeta::Keep(HiLo::High, i)),
    ))(input)
}

fn roll_meta(input: &str) -> IResult<&str, RollMeta> {
    alt((
        drop_meta,
        keep_meta,

        // Rerolls
        // TODO: add support for reroll once ie (ro>, ro3, ro=5)
        map(preceded(tag("r"), compare_point), |i| RollMeta::Reroll(i)),

        // Compounding
        map(preceded(tag("!!"), compare_point), |i| RollMeta::Compounding(i)),

        // Penetrating
        map(preceded(tag("!p"), compare_point), |i| RollMeta::Penetrating(i)),

        // Exploding
        map(preceded(tag("!"), compare_point), |i| RollMeta::Exploding(i)),

        // Critical Success, Critical Fumble
        map(preceded(tag("cs"), compare_point), |i| RollMeta::CriticalSuccess(i)),
        map(preceded(tag("cf"), compare_point), |i| RollMeta::CriticalFumble(i)),

        // Sort ordering
        map(tag("sa"), |_| RollMeta::Sort(SortOrder::Ascending)),
        map(tag("sd"), |_| RollMeta::Sort(SortOrder::Descending)),
        map(tag("s"), |_| RollMeta::Sort(SortOrder::Ascending)),
    ))(input)
}

fn roll(input: &str) -> IResult<&str, Roll> {
    let (input, roll) = opt(number)(input)?;
    let (input, dice) = dice(input)?;
    let (input, meta) = many0(roll_meta)(input)?;

    Ok((input,
        match roll {
            None       => Roll(1, dice, meta),
            Some(roll) => Roll(roll, dice, meta),
        }
    ))
}

fn group_meta(input: &str) -> IResult<&str, GroupMeta> {
    alt((
        map(
            preceded(tag("dl"), number),
            |i| GroupMeta::Drop(HiLo::Low, i)
        ),
        // Alias for dl
        map(
            preceded(tag("d"), number),
            |i| GroupMeta::Drop(HiLo::Low, i)
        ),
        map(
            preceded(tag("dh"), number),
            |i| GroupMeta::Drop(HiLo::High, i)
        ),
        map(
            preceded(tag("kl"), number),
            |i| GroupMeta::Keep(HiLo::Low, i)
        ),
        map(
            preceded(tag("kh"), number),
            |i| GroupMeta::Keep(HiLo::High, i)
        ),
        // Alias for kh
        map(
            preceded(tag("k"), number),
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
            map(number, OpsVal::Number),
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
// TODO: Implement the match feature (2d6mt?) (its a target outcome)
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
            preceded(tag(">"), number),
            |i| TargetOper::Success(ComparePoint::Gt(i))
        ),
        map(
            preceded(tag("<"), number),
            |i| TargetOper::Success(ComparePoint::Lt(i))
        ),
        map(
            preceded(tag("="), number),
            |i| TargetOper::Success(ComparePoint::Eq(i))
        ),
        // TODO: Fail checks, these should only parse if there is a preceding success check
        // 10d10>3f1
        map(
            preceded(tag("f>"), number),
            |i| TargetOper::Fail(ComparePoint::Gt(i))
        ),
        map(
            preceded(tag("f<"), number),
            |i| TargetOper::Fail(ComparePoint::Lt(i))
        ),
        map(
            preceded(tag("f="), number),
            |i| TargetOper::Fail(ComparePoint::Eq(i))
        ),
        map(
            preceded(tag("f"), number),
            |i| TargetOper::Fail(ComparePoint::Eq(i))
        )
    )))(input)?;

    Ok((input, match ttarget {
        None         => opsval,
        Some(t) => OpsVal::Target(t, Box::new(opsval)),
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
            Ok(("", Dice::Dice(10)))
        );
    }

    #[test]
    fn test_fate() {
        assert_eq!(
            dice("dF"),
            Ok(("", Dice::Fate))
        );
    }

    #[test]
    fn test_roll() {
        assert_eq!(
            roll("5d10"),
            Ok(("", Roll(5, Dice::Dice(10), vec![])))
        );
    }

    #[test]
    fn test_roll_unspecified() {
        assert_eq!(
            roll("d10"),
            Ok(("", Roll(1, Dice::Dice(10), vec![])))
        );
    }

    #[test]
    fn test_roll_critical() {
        assert_eq!(
            roll("d10cs10cf>2"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::CriticalSuccess(ComparePoint::Eq(10)),
                    RollMeta::CriticalFumble(ComparePoint::Gt(2)),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_sort() {
        // TODO: should only have one sort order, multiples doesn't make sense
        assert_eq!(
            roll("d10sdsa"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Sort(SortOrder::Descending),
                    RollMeta::Sort(SortOrder::Ascending),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_reroll() {
        assert_eq!(
            roll("d10r10r>2"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Reroll(ComparePoint::Eq(10)),
                    RollMeta::Reroll(ComparePoint::Gt(2)),
                ]
            )))
        );
    }

    #[test]
    fn test_ops_val_roll() {
        assert_eq!(
            ops_val("10d10"),
            Ok(("", OpsVal::Roll(Roll(10, Dice::Dice(10), vec![]))))
        );
    }

    #[test]
    fn test_ops_val_roll_unspecified() {
        assert_eq!(
            ops_val("d10"),
            Ok(("", OpsVal::Roll(Roll(1, Dice::Dice(10), vec![]))))
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
                        Box::new(OpsVal::Roll(Roll(1, Dice::Dice(2), vec![]))),
                        Box::new(OpsVal::Roll(Roll(2, Dice::Dice(3), vec![])))
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
    fn test_expr_pow() {
        // TODO: ordering is wrong, roll20 is 4^(3^2) while this is (4^3)^2
        // But various languages differs here, some are former some are later
        assert_eq!(
            expr("4^3^2"),
            Ok(("", OpsVal::Expr(
                        Oper::Pow,
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(4)),
                            Box::new(OpsVal::Number(3))
                        )),
                        Box::new(OpsVal::Number(2)),
            )))
        );
    }

    #[test]
    fn test_expr_pow_paren() {
        assert_eq!(
            expr("4^(3^2)"),
            Ok(("", OpsVal::Expr(
                        Oper::Pow,
                        Box::new(OpsVal::Number(4)),
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(3)),
                            Box::new(OpsVal::Number(2)),
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
                            Box::new(OpsVal::Number(4)),
                            Box::new(OpsVal::Number(3))
                        )),
                        Box::new(OpsVal::Number(2)),
            )))
        );
    }

    #[test]
    fn test_expr_pow_mul_other() {
        assert_eq!(
            expr("4*3^2"),
            Ok(("", OpsVal::Expr(
                        Oper::Mul,
                        Box::new(OpsVal::Number(4)),
                        Box::new(OpsVal::Expr(
                            Oper::Pow,
                            Box::new(OpsVal::Number(3)),
                            Box::new(OpsVal::Number(2))
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
                                    Box::new(OpsVal::Roll(Roll(1, Dice::Dice(2), vec![]))),
                                    Box::new(OpsVal::Roll(Roll(10, Dice::Dice(3), vec![])))
                                )),
                                Box::new(OpsVal::Number(20))
                            ))
                        )),
                        Box::new(OpsVal::Roll(Roll(1, Dice::Dice(3), vec![])))
            )))
        );
    }

    #[test]
    fn test_spaceing() {
        assert_eq!(
            expr("  10 + d2 *  10d20  "),
            Ok(("", OpsVal::Expr(
                        Oper::Add,
                        Box::new(OpsVal::Number(10)),
                        Box::new(OpsVal::Expr(
                            Oper::Mul,
                            Box::new(OpsVal::Roll(Roll(1, Dice::Dice(2), vec![]))),
                            Box::new(OpsVal::Roll(Roll(10, Dice::Dice(20), vec![])))
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
                            Box::new(OpsVal::Number(1)),
                            Box::new(OpsVal::Number(2)),
                        )),
                        Box::new(OpsVal::Number(3)),
            )))
        );
    }

// TODO: implement support for computed dice (this this is functionally equiv to [[1+2]]d[[3+4]]
//    #[test]
//    fn compute_dice() {
//        assert_eq!(
//            expr("(1+2)d(3+4)"),
//            Ok(("", OpsVal::Number(1)))
//        );
//    }

    #[test]
    fn test_dice_exploding_implicit_eq() {
        assert_eq!(
            roll("d10!"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Exploding(ComparePoint::IEq)
                ]
            )))
        );
    }

    #[test]
    fn test_dice_exploding_explicit_eq() {
        assert_eq!(
            roll("d10!10"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Exploding(ComparePoint::Eq(10))
                ]
            )))
        );
    }

    #[test]
    fn test_dice_exploding_double_explicit_eq() {
        assert_eq!(
            roll("d10!=10"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Exploding(ComparePoint::Eq(10))
                ]
            )))
        );
    }


    #[test]
    fn test_dice_exploding_gt() {
        assert_eq!(
            roll("d10!>10"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Exploding(ComparePoint::Gt(10))
                ]
            )))
        );
    }

    #[test]
    fn test_dice_exploding_lt() {
        assert_eq!(
            roll("d10!<10"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Exploding(ComparePoint::Lt(10))
                ]
            )))
        );
    }

    #[test]
    fn test_dice_compounding_lt() {
        assert_eq!(
            roll("d10!!<10"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Compounding(ComparePoint::Lt(10))
                ]
            )))
        );
    }

    #[test]
    fn test_dice_penetrating_lt() {
        assert_eq!(
            roll("d10!p<10"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Penetrating(ComparePoint::Lt(10))
                ]
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_drop() {
        assert_eq!(
            roll("d10dl1dh2d3"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Drop(HiLo::Low, 1),
                    RollMeta::Drop(HiLo::High, 2),
                    RollMeta::Drop(HiLo::Low, 3),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_keep() {
        assert_eq!(
            roll("d10kl1kh2k3"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Keep(HiLo::Low, 1),
                    RollMeta::Keep(HiLo::High, 2),
                    RollMeta::Keep(HiLo::High, 3),
                ]
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_eq() {
        assert_eq!(
            target("d10=2"),
            Ok(("", OpsVal::Target(
                TargetOper::Success(ComparePoint::Eq(2)),
                Box::new(OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
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
                TargetOper::Success(ComparePoint::Lt(2)),
                Box::new(OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
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
                TargetOper::Success(ComparePoint::Gt(2)),
                Box::new(OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
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
                TargetOper::Success(ComparePoint::Gt(2)),
                Box::new(OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![
                        RollMeta::Exploding(ComparePoint::Eq(3)),
                        RollMeta::Drop(HiLo::Low, 1),
                    ]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_braced_meta() {
        assert_eq!(
            target("{d10!}>2"),
            Ok(("", OpsVal::Target(
                TargetOper::Success(ComparePoint::Gt(2)),
                Box::new(OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![
                        RollMeta::Exploding(ComparePoint::IEq),
                    ]
                )))
            )))
        );
    }

    #[test]
    fn test_roll_target_modifier() {
        assert_eq!(
            target("d10+1>3"),
            Ok(("", OpsVal::Target(
                TargetOper::Success(ComparePoint::Gt(3)),
                Box::new(OpsVal::Expr(
                    Oper::Add,
                    Box::new(OpsVal::Roll(Roll(1, Dice::Dice(10), vec![]))),
                    Box::new(OpsVal::Number(1))
                ))
            )))
        );
    }

    #[test]
    fn test_roll_target_modifier_braced() {
        assert_eq!(
            target("{d10+1}>3"),
            Ok(("", OpsVal::Target(
                TargetOper::Success(ComparePoint::Gt(3)),
                Box::new(OpsVal::Expr(
                    Oper::Add,
                    Box::new(OpsVal::Roll(Roll(1, Dice::Dice(10), vec![]))),
                    Box::new(OpsVal::Number(1))
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
                        Box::new(OpsVal::Roll(Roll(2, Dice::Dice(10), vec![]))),
                        Box::new(OpsVal::Roll(Roll(2, Dice::Dice(2), vec![]))),
                    )
                ],
                vec![GroupMeta::Keep(HiLo::High, 2)],
            )))
        );
    }

    #[test]
    fn test_list_group_roll() {
        assert_eq!(
            group_roll("{2d10,2d2}kh2"),
            Ok(("", GroupRoll(
                vec![
                    OpsVal::Roll(Roll(2, Dice::Dice(10), vec![])),
                    OpsVal::Roll(Roll(2, Dice::Dice(2), vec![])),
                ],
                vec![GroupMeta::Keep(HiLo::High, 2)],
            )))
        );
    }

    #[test]
    fn test_list_group_target_roll() {
        assert_eq!(
            target("{2d10,2d10+2d2}kh2>3"),
            Ok(("", OpsVal::Target(
                TargetOper::Success(ComparePoint::Gt(3)),
                Box::new(OpsVal::GroupRoll(GroupRoll(
                    vec![
                        OpsVal::Roll(Roll(2, Dice::Dice(10), vec![])),
                        OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Roll(Roll(2, Dice::Dice(10), vec![]))),
                            Box::new(OpsVal::Roll(Roll(2, Dice::Dice(2), vec![]))),
                        )
                    ],
                    vec![GroupMeta::Keep(HiLo::High, 2)],
                )))
            )))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_fail_eq() {
        // TODO: this should fail, and should only work for d10=2f=4 for eg
        assert_eq!(
            target("d10f=2"),
            Ok(("", OpsVal::Target(
                TargetOper::Fail(ComparePoint::Eq(2)),
                Box::new(OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![]
                )))
            )))
        );
    }
}
