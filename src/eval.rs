use crate::parser::{
    Num,
    Dice,
    Roll,
    RollMeta,
    ComparePoint,
    Exploding,
    Reroll,
    HiLo,
    SortOrder,
    Critical,
    Matching,
    GroupRoll,
    GroupMeta,
    Oper,
    OpsVal,
};
use crate::parser::parse;

use rand::Rng;


// TODO: tmp for now, ultimately want to fully eval the tree,
// use this to hold temporary or yet to be eval values
#[derive(Debug, PartialEq)]
pub enum Eval {
    Number(Num),
    Rolls(Vec<Num>),
}

pub fn eval(input: OpsVal) -> Result<Eval, &'static str> {
    let mut rng = rand::thread_rng();

    println!("Debug: {:?}", input);

    match input {
        OpsVal::Roll(Roll(n, Dice::Dice(s), ms)) => {
            let mut dice = || {rng.gen_range(1..=s)};
            let mut rolls = roll(n, &mut dice);

            // Apply the meta
            let rmeta = struct_meta(ms);
            rolls = reroll(rolls, &rmeta.reroll, &mut dice);

            Ok(Eval::Rolls(rolls))
        },
        OpsVal::Roll(Roll(_, Dice::Fate, _)) => Err("no"),
        OpsVal::GroupRoll(_)  => Err("no"),
        OpsVal::Number(n)     => Ok(Eval::Number(n)),
        OpsVal::Expr(_,_,_)   => Err("no"),
    }
}

fn roll<F: FnMut() -> Num>(n: Num, d: &mut F) -> Vec<Num> {
    let mut ret: Vec<Num> = vec![];
    for _ in 0..n {
        ret.push(d())
    }
    ret
}

fn reroll<F: FnMut() -> Num>(rolls: Vec<Num>, reroll: &SReroll, d: &mut F) -> Vec<Num> {
    let mut ret = vec![];

    for roll in rolls {
        // Potential roll
        let mut p_roll = roll;

        // Reroll once
        if reroll.once.is_some() {
            if compare(p_roll, &reroll.once.as_ref().unwrap()) {
                p_roll = d();
            }
        }

        // Bail if reroll runs more than 100 times
        let mut bail = 0;

        while bail < 100 && !reroll.everytime.is_empty() {
            bail += 1;
            for et in &reroll.everytime {
                if compare(p_roll, et) {
                    p_roll = d();
                }
            }
        }

        // We either have a valid roll or it gave up
        if bail == 100 {
            println!("Bailed");
        }
        ret.push(p_roll);
    }

    println!("vec: {:?}", ret);
    ret
}

fn compare(num: Num, comp: &ComparePoint) -> bool {
    match comp {
        ComparePoint::IEq   => false, // Not used in reroll....
        ComparePoint::Eq(n) => num == *n,
        ComparePoint::Gt(n) => num >= *n,
        ComparePoint::Lt(n) => num <= *n,
    }
}



#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_number() {
        let data = eval(parse("10").unwrap().1);
        assert_eq!(data, Ok(Eval::Number(10)));
    }

    #[test]
    fn test_single_roll() {
        let data = eval(parse("d6").unwrap().1).unwrap();
        match data {
            Eval::Rolls(v) => assert!((1..=6).contains(&v[0])),
            _ => panic!("failure"),
        }
    }

    #[test]
    fn test_five_roll() {
        let data = eval(parse("5d6").unwrap().1).unwrap();
        match data {
            Eval::Rolls(val) => {
                assert_eq!(val.len(), 5);
                for v in val {
                    assert!((1..=6).contains(&v));
                }
            },
            _ => panic!("failure"),
        }
    }

    #[test]
    fn test_ten_reroll_allow_ten() {
        let data = eval(parse("10d10r<5r>8r7").unwrap().1).unwrap();
        match data {
            Eval::Rolls(val) => {
                assert_eq!(val.len(), 10);
                for v in val {
                    assert_eq!(v, 6);
                }
            },
            _ => panic!("failure"),
        }
    }

    #[test]
    fn test_hundred_reroll_once_ten() {
        let data = eval(parse("100d2ro1").unwrap().1).unwrap();
        match data {
            Eval::Rolls(val) => {
                // TODO: no way to validate it unless we pre-seed the rng here
                // or add in built in anonation to the roll values itself
                assert_eq!(val.len(), 100);
            },
            _ => panic!("failure"),
        }
    }
}






// Bookkeeping stuff here
struct SMatching(Matching, ComparePoint);
struct SSort(SortOrder);
struct STarget(Critical, ComparePoint);
struct SExploding(Exploding, ComparePoint);
struct SDrop(HiLo, Num);
struct SKeep(HiLo, Num);
struct SReroll {
    once: Option<ComparePoint>,
    everytime: Vec<ComparePoint>,
}

struct RMeta {
    matching: Vec<SMatching>,
    sort: Vec<SSort>,
    target: Vec<STarget>,
    exploding: Vec<SExploding>,
    drop: Vec<SDrop>,
    keep: Vec<SKeep>,
    reroll: SReroll,
}

fn struct_meta(roll_meta: Vec<RollMeta>) -> RMeta {
    let mut meta = RMeta {
        matching: vec![],
        sort: vec![],
        target: vec![],
        exploding: vec![],
        drop: vec![],
        keep: vec![],
        reroll: SReroll {
            once: None,
            everytime: vec![],
        },
    };

    for m in roll_meta {
        match m {
            RollMeta::Matching(a, b)  => meta.matching.push(SMatching(a, b)),
            RollMeta::Sort(a)         => meta.sort.push(SSort(a)),
            RollMeta::Target(a, b)    => meta.target.push(STarget(a, b)),
            RollMeta::Exploding(a, b) => meta.exploding.push(SExploding(a, b)),
            RollMeta::Drop(a, b)      => meta.drop.push(SDrop(a, b)),
            RollMeta::Keep(a, b)      => meta.keep.push(SKeep(a, b)),

            RollMeta::Reroll(Reroll::Everytime, b) => meta.reroll.everytime.push(b),
            RollMeta::Reroll(Reroll::Once, b) => {
                if meta.reroll.once.is_some() {
                    panic!("Only allow `ro` once");
                } else {
                    meta.reroll.once = Some(b)
                }
            },
        }
    }
    meta
}
