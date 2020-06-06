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

pub fn parse(input: &str) -> IResult<&str, OpsVal> {
    expr(input)
}

type Num = u32;

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
    // * Matching
    Matching(Matching, ComparePoint),

    // * Sorting
    // TODO: Display purpose (sort order s/sa and sd for sort ascending and sort descending)
    // TODO: should only have one sort order, multiples doesn't make sense
    Sort(SortOrder),

    // * Target (success, fail)
    Target(Critical, ComparePoint),

    // * Exploding (Std, Compounding, Penetrating)
    Exploding(Exploding, ComparePoint),

    // * Keep/Drop (Hi, Lo)
    // NOTE: can't replace with group, there's subtle details in difference in behavor here
    Drop(HiLo, Num),
    Keep(HiLo, Num),

    // * Reroll (Std, Only Once)
    Reroll(Reroll, ComparePoint),
}

#[derive(Debug, PartialEq)]
pub enum ComparePoint {
    // Default Behavor (when there's no Compare Point) (Rename to Default?)
    IEq,
    Eq(Num),
    Gt(Num),
    Lt(Num),
}

#[derive(Debug, PartialEq)]
pub enum Exploding {
    Exploding,
    Compounding,
    Penetrating,
}

#[derive(Debug, PartialEq)]
pub enum Reroll {
    Everytime,
    Once,
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

#[derive(Debug, PartialEq)]
pub enum Critical {
    Success,
    Fail,
}

#[derive(Debug, PartialEq)]
pub enum Matching {
    Matching,
    Target,
}

// List of OpsVal::Roll or list of opsval expr whatever
// TODO: have this be a collection of Rolls (possibly a restricted RollMeta for inside a group)
#[derive(Debug, PartialEq)]
pub struct GroupRoll(Vec<OpsVal>, Vec<GroupMeta>);

// TODO: maybe instead of having a drop/keep meta on roll we make all roll go into a group and if
// there's more than 1 it can do special handling? For now -> roll or -> roll -> group.
#[derive(Debug, PartialEq)]
pub enum GroupMeta {
    // * Keep/Drop (Hi, Lo)
    // NOTE: can't replace with Roll, there's subtle details in difference in behavor here
    Drop(HiLo, Num),
    Keep(HiLo, Num),

    // * Target (success, fail)
    Target(Critical, ComparePoint),
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
}


fn number(input: &str) -> IResult<&str, Num> {
    let radix = 10;

    let is_digit = |c: char| c.is_digit(radix);
    let from_digit = |s: &str| u32::from_str_radix(s, radix);
    let map = map_res(take_while1(is_digit), from_digit);

    map(input)
}

fn implied_compare_point(input: &str) -> IResult<&str, ComparePoint> {
    let (input, oper) = opt(compare_point)(input)?;

    Ok((
        input,
        match oper {
            None    => ComparePoint::IEq,
            Some(x) => x,
        }
    ))
}

fn compare_point(input: &str) -> IResult<&str, ComparePoint> {
    alt((
        map(preceded(tag(">"), number), |i| ComparePoint::Gt(i)),
        map(preceded(tag("<"), number), |i| ComparePoint::Lt(i)),
        map(preceded(tag("="), number), |i| ComparePoint::Eq(i)),
        map(number, |i| ComparePoint::Eq(i)),
    ))(input)
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

fn drop_meta(input: &str) -> IResult<&str, (HiLo, Num)> {
    let (input, _) = tag("d")(input)?;

    alt((
        map(preceded(tag("l"), number), |i| (HiLo::Low, i)),
        map(preceded(tag("h"), number), |i| (HiLo::High, i)),
        // Alias for dl
        map(number, |i| (HiLo::Low, i)),
    ))(input)
}

fn keep_meta(input: &str) -> IResult<&str, (HiLo, Num)> {
    let (input, _) = tag("k")(input)?;

    alt((
        map(preceded(tag("l"), number), |i| (HiLo::Low, i)),
        map(preceded(tag("h"), number), |i| (HiLo::High, i)),
        // Alias for kh
        map(number, |i| (HiLo::High, i)),
    ))(input)
}

fn reroll_meta(input: &str) -> IResult<&str, (Reroll, ComparePoint)> {
    alt((
        map(preceded(tag("ro"), implied_compare_point), |i| (Reroll::Once, i)),
        map(preceded(tag("r"), implied_compare_point), |i| (Reroll::Everytime, i)),
    ))(input)
}

fn exploding_meta(input: &str) -> IResult<&str, (Exploding, ComparePoint)> {
    alt((
        map(preceded(tag("!!"), implied_compare_point), |i| (Exploding::Compounding, i)),
        map(preceded(tag("!p"), implied_compare_point), |i| (Exploding::Penetrating, i)),
        map(preceded(tag("!"), implied_compare_point), |i| (Exploding::Exploding, i)),
    ))(input)
}

fn matching_meta(input: &str) -> IResult<&str, (Matching, ComparePoint)> {
    alt((
        map(preceded(tag("mt"), implied_compare_point), |i| (Matching::Target, i)),
        map(preceded(tag("m"), implied_compare_point), |i| (Matching::Matching, i)),
    ))(input)
}

fn sort_meta(input: &str) -> IResult<&str, SortOrder> {
    alt((
        map(tag("sa"), |_| SortOrder::Ascending),
        map(tag("sd"), |_| SortOrder::Descending),
        map(tag("s"), |_| SortOrder::Ascending),
    ))(input)
}

fn critical_meta(input: &str) -> IResult<&str, (Critical, ComparePoint)> {
    alt((
        map(preceded(tag("f"), compare_point), |i| (Critical::Fail, i)),
        map(compare_point, |i| (Critical::Success, i)),
    ))(input)
}

fn roll_meta(input: &str) -> IResult<&str, RollMeta> {
    alt((
        map(drop_meta, |(h, i)| RollMeta::Drop(h, i)),
        map(keep_meta, |(h, i)| RollMeta::Keep(h, i)),
        map(reroll_meta, |(r, i)| RollMeta::Reroll(r, i)),
        map(exploding_meta, |(e, i)| RollMeta::Exploding(e, i)),
        map(matching_meta, |(m, i)| RollMeta::Matching(m, i)),
        map(sort_meta, |i| RollMeta::Sort(i)),
        map(critical_meta, |(c, i)| RollMeta::Target(c, i)),
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
        map(drop_meta, |(h, i)| GroupMeta::Drop(h, i)),
        map(keep_meta, |(h, i)| GroupMeta::Keep(h, i)),
        map(critical_meta, |(c, i)| GroupMeta::Target(c, i)),
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


#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_critical_fail() {
        assert_eq!(
            critical_meta("f=3"),
            Ok(("", (Critical::Fail, ComparePoint::Eq(3))))
        );
    }

    #[test]
    fn test_critical_success() {
        assert_eq!(
            critical_meta("=3"),
            Ok(("", (Critical::Success, ComparePoint::Eq(3))))
        );
    }

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
            roll("d10ro10r>2"),
            Ok(("", Roll(
                1,
                Dice::Dice(10),
                vec![
                    RollMeta::Reroll(Reroll::Once, ComparePoint::Eq(10)),
                    RollMeta::Reroll(Reroll::Everytime, ComparePoint::Gt(2)),
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
                    RollMeta::Exploding(Exploding::Exploding, ComparePoint::IEq)
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
                    RollMeta::Exploding(Exploding::Exploding, ComparePoint::Eq(10))
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
                    RollMeta::Exploding(Exploding::Exploding, ComparePoint::Eq(10))
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
                    RollMeta::Exploding(Exploding::Exploding, ComparePoint::Gt(10))
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
                    RollMeta::Exploding(Exploding::Exploding, ComparePoint::Lt(10))
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
                    RollMeta::Exploding(Exploding::Compounding, ComparePoint::Lt(10)),
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
                    RollMeta::Exploding(Exploding::Penetrating, ComparePoint::Lt(10)),
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
            expr("d10=2"),
            Ok(("", OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![RollMeta::Target(Critical::Success, ComparePoint::Eq(2))]
            ))))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_lt() {
        assert_eq!(
            expr("d10<2"),
            Ok(("", OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![RollMeta::Target(Critical::Success, ComparePoint::Lt(2))]
            ))))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_gt() {
        assert_eq!(
            expr("d10>2"),
            Ok(("", OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![RollMeta::Target(Critical::Success, ComparePoint::Gt(2))]
            ))))
        );
    }

    #[test]
    fn test_roll_dice_nested_meta() {
        assert_eq!(
            expr("d10!3d1>2"),
            Ok(("", OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![
                        RollMeta::Exploding(Exploding::Exploding, ComparePoint::Eq(3)),
                        RollMeta::Drop(HiLo::Low, 1),
                        RollMeta::Target(Critical::Success, ComparePoint::Gt(2))
                    ]
            ))))
        );
    }

    #[test]
    fn test_roll_braced_meta() {
        assert_eq!(
            expr("{d10!}>2"),
            Ok(("", OpsVal::GroupRoll(GroupRoll(
                vec![
                    OpsVal::Roll(Roll(
                        1,
                        Dice::Dice(10),
                        vec![
                            RollMeta::Exploding(Exploding::Exploding, ComparePoint::IEq),
                        ]
                    ))
                ],
                vec![GroupMeta::Target(Critical::Success, ComparePoint::Gt(2))],
            ))))
        );
    }

    #[test]
    fn test_roll_target_modifier_braced() {
        assert_eq!(
            expr("{d10+1}>3"),
            Ok(("", OpsVal::GroupRoll(GroupRoll(
                vec![
                    OpsVal::Expr(
                        Oper::Add,
                        Box::new(OpsVal::Roll(Roll(1, Dice::Dice(10), vec![]))),
                        Box::new(OpsVal::Number(1))
                    )
                ],
                vec![GroupMeta::Target(Critical::Success, ComparePoint::Gt(3))],
            ))))
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
            expr("{2d10,2d10+2d2}kh2>3"),
            Ok(("", OpsVal::GroupRoll(GroupRoll(
                    vec![
                        OpsVal::Roll(Roll(2, Dice::Dice(10), vec![])),
                        OpsVal::Expr(
                            Oper::Add,
                            Box::new(OpsVal::Roll(Roll(2, Dice::Dice(10), vec![]))),
                            Box::new(OpsVal::Roll(Roll(2, Dice::Dice(2), vec![]))),
                        )
                    ],
                    vec![
                        GroupMeta::Keep(HiLo::High, 2),
                        GroupMeta::Target(Critical::Success, ComparePoint::Gt(3)),
                    ],
            ))))
        );
    }

    #[test]
    fn test_roll_unspecified_roll_meta_fail_eq() {
        // TODO: this should fail, and should only work for d10=2f=4 for eg
        assert_eq!(
            expr("d10f=2"),
            Ok(("", OpsVal::Roll(Roll(
                    1,
                    Dice::Dice(10),
                    vec![RollMeta::Target(Critical::Fail, ComparePoint::Eq(2))],
            ))))
        );
    }

//    #[test]
//    fn test_roll_unspecified_roll_meta_success_fail_eq() {
//        assert_eq!(
//            expr("d10=2f=3"),
//            Ok(("", OpsVal::Target(
//                vec![
//                    Critical::Success(ComparePoint::Eq(2)),
//                    Critical::Fail(ComparePoint::Eq(3)),
//                ],
//                Box::new(OpsVal::Roll(Roll(
//                    1,
//                    Dice::Dice(10),
//                    vec![]
//                )))
//            )))
//        );
//    }
}
