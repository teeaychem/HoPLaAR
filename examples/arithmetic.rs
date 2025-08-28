use hoplaar::logic::propositional::{Prop, PropFormula, Valuation};

fn bits_needed(i: usize) -> u32 {
    let mut bit_count: u32 = 1;
    let base: usize = 2;
    loop {
        if base.pow(bit_count) > i {
            break;
        }
        bit_count += 1
    }
    bit_count
}

fn bitlength(i: usize) -> u32 {
    if i == 0 { 0 } else { 1 + bitlength(i / 2) }
}

fn as_bits(i: usize, using: Option<u32>) -> Vec<bool> {
    let mut remaining = i;
    let bits_needed = if let Some(required) = using {
        required
    } else {
        bits_needed(i)
    };
    let base: usize = 2;
    let mut bit_vec = vec![false; bits_needed.try_into().unwrap()];
    for x in (0..bits_needed).rev() {
        if base.pow(x) <= remaining {
            bit_vec[(bits_needed - (x + 1)) as usize] = true;
            remaining -= base.pow(x);
        }
    }
    bit_vec
}

fn as_int(bits: &[bool]) -> usize {
    let base: u32 = 2;
    let mut total = 0;
    for (a, &b) in bits.iter().rev().enumerate() {
        if b {
            total += base.pow(a as u32)
        }
    }
    total as usize
}

fn bit(i: usize, n: usize) -> bool {
    let mut mut_i = i;
    let mut mut_n = n;
    loop {
        if mut_n == 0 {
            break;
        } else {
            mut_n -= 1;
            mut_i /= 2;
        }
    }
    mut_i % 2 == 1
}

fn one_index_atom(id: &str) -> impl Fn(usize) -> PropFormula {
    let id_str = id.to_string();
    move |i| PropFormula::Atom(Prop::from(&format!("{id_str}_{i}")))
}

fn two_index_atom(id: &str) -> impl Fn(usize, usize) -> PropFormula {
    let id_str = id.to_string();
    move |i, j| PropFormula::Atom(Prop::from(format!("{}_{}_{}", id_str, i, j).as_str()))
}

/* Adder */

fn halfsum(x: PropFormula, y: PropFormula) -> PropFormula {
    PropFormula::Iff(x, y.negate())
}

fn halfcarry(x: PropFormula, y: PropFormula) -> PropFormula {
    PropFormula::And(x, y)
}

fn half_adder(x: PropFormula, y: PropFormula, s: PropFormula, c: PropFormula) -> PropFormula {
    PropFormula::And(
        PropFormula::Iff(s, halfsum(x.clone(), y.clone())),
        PropFormula::Iff(c, halfcarry(x, y)),
    )
}

fn carry(x: PropFormula, y: PropFormula, z: PropFormula) -> PropFormula {
    PropFormula::Or(
        PropFormula::And(x.clone(), y.clone()),
        PropFormula::And(PropFormula::Or(x, y), z),
    )
}

fn sum(x: PropFormula, y: PropFormula, z: PropFormula) -> PropFormula {
    halfsum(halfsum(x, y), z)
}

fn full_adder(
    x: PropFormula,
    y: PropFormula,
    z: PropFormula,
    s: PropFormula,
    c: PropFormula,
) -> PropFormula {
    PropFormula::And(
        PropFormula::Iff(s, sum(x.clone(), y.clone(), z.clone())),
        PropFormula::Iff(c, carry(x, y, z)),
    )
}

fn ripple_carry(
    x: impl Fn(usize) -> PropFormula,
    y: impl Fn(usize) -> PropFormula,
    c: impl Fn(usize) -> PropFormula,
    out: impl Fn(usize) -> PropFormula,
    n: usize,
) -> PropFormula {
    PropFormula::conjoin((0..n).map(|i| full_adder(x(i), y(i), c(i), out(i), c(i + 1))))
}

fn ripple_carry_0(
    x: impl Fn(usize) -> PropFormula,
    y: impl Fn(usize) -> PropFormula,
    c: impl Fn(usize) -> PropFormula,
    out: impl Fn(usize) -> PropFormula,
    n: usize,
) -> PropFormula {
    ripple_carry(
        x,
        y,
        |i| {
            if i == 0 { PropFormula::False } else { c(i) }
        },
        out,
        n,
    )
}

fn ripple_carry_1(
    x: impl Fn(usize) -> PropFormula,
    y: impl Fn(usize) -> PropFormula,
    c: impl Fn(usize) -> PropFormula,
    out: impl Fn(usize) -> PropFormula,
    n: usize,
) -> PropFormula {
    ripple_carry(
        x,
        y,
        |i| {
            if i == 0 { PropFormula::True } else { c(i) }
        },
        out,
        n,
    )
}

fn mux(sel: PropFormula, in0: PropFormula, in1: PropFormula) -> PropFormula {
    PropFormula::Or(
        PropFormula::And(sel.clone().negate(), in0),
        PropFormula::And(sel, in1),
    )
}

fn offset(n: usize, x: impl Fn(usize) -> PropFormula) -> impl Fn(usize) -> PropFormula {
    move |i| x(n + i)
}

#[allow(clippy::too_many_arguments)]
fn make_fm(
    x: impl Fn(usize) -> PropFormula,
    y: impl Fn(usize) -> PropFormula,
    c0: impl Fn(usize) -> PropFormula,
    c1: impl Fn(usize) -> PropFormula,
    s0: impl Fn(usize) -> PropFormula,
    s1: impl Fn(usize) -> PropFormula,
    c: impl Fn(usize) -> PropFormula,
    s: impl Fn(usize) -> PropFormula,
    n: usize,
    k: usize,
) -> PropFormula {
    let k_n_min = std::cmp::min(n, k);

    let f =
        PropFormula::conjoin((0..k_n_min).map(|i| PropFormula::Iff(s(i), mux(c(0), s0(i), s1(i)))));

    PropFormula::And(
        PropFormula::And(
            ripple_carry_0(&x, &y, &c0, &s0, k_n_min),
            ripple_carry_1(&x, &y, &c1, &s1, k_n_min),
        ),
        PropFormula::And(
            PropFormula::Iff(c(k_n_min), mux(c(0), c0(k_n_min), c1(k_n_min))),
            f,
        ),
    )
}

#[allow(clippy::too_many_arguments)]
fn carry_select(
    x: impl Fn(usize) -> PropFormula,
    y: impl Fn(usize) -> PropFormula,
    c0: impl Fn(usize) -> PropFormula,
    c1: impl Fn(usize) -> PropFormula,
    s0: impl Fn(usize) -> PropFormula,
    s1: impl Fn(usize) -> PropFormula,
    c: impl Fn(usize) -> PropFormula,
    s: impl Fn(usize) -> PropFormula,
    mut n: usize,
    k: usize,
) -> PropFormula {
    let mut k_n_min = std::cmp::min(n, k);
    let mut fm = make_fm(&x, &y, &c0, &c1, &s0, &s1, &c, &s, n, k);
    let mut loop_count = 0;
    loop {
        if k_n_min < k {
            break;
        } else {
            loop_count += 1;
            n -= k;
            k_n_min = std::cmp::min(n, k);
            fm = PropFormula::And(
                fm,
                make_fm(
                    offset(k * loop_count, &x),
                    offset(k * loop_count, &y),
                    offset(k * loop_count, &c0),
                    offset(k * loop_count, &c1),
                    offset(k * loop_count, &s0),
                    offset(k * loop_count, &s1),
                    offset(k * loop_count, &c),
                    offset(k * loop_count, &s),
                    n,
                    k,
                ),
            );
        }
    }
    fm
}

fn adder_test(n: usize, k: usize) -> PropFormula {
    let x = one_index_atom("x");
    let y = one_index_atom("y");
    let c0 = one_index_atom("c0");
    let c1 = one_index_atom("c1");
    let c2 = one_index_atom("c2");
    let s0 = one_index_atom("s0");
    let s1 = one_index_atom("s1");
    let s2 = one_index_atom("s2");
    let c = one_index_atom("c");
    let s = one_index_atom("s");

    PropFormula::Imp(
        PropFormula::And(
            PropFormula::And(
                carry_select(&x, &y, c0, c1, s0, s1, &c, &s, n, k),
                c(0).negate(),
            ),
            ripple_carry_0(x, y, &c2, &s2, n),
        ),
        PropFormula::And(
            PropFormula::Iff(c(n), c2(n)),
            (0..n).fold(PropFormula::True, |acc, i| {
                PropFormula::And(acc, PropFormula::Iff(s(i), s2(i)))
            }),
        ),
    )
}

fn ripple_add(x: usize, y: usize) -> usize {
    let x_size = bits_needed(x);
    let y_size = bits_needed(y);
    let max_size = std::cmp::max(x_size, y_size + 1);
    let x_bitvec = as_bits(x, Some(max_size));
    let y_bitvec = as_bits(y, Some(max_size));

    let max_size = max_size as usize; // u32 -> usize

    let the_adder = ripple_carry_0(
        one_index_atom("x"),
        one_index_atom("y"),
        one_index_atom("c"),
        one_index_atom("out"),
        max_size,
    );

    let mut new_valuation = Valuation::from_prop_set(the_adder.atoms());

    for (i, &val) in x_bitvec.iter().rev().enumerate() {
        let prop = Prop::from(&format!("x_{i}"));
        new_valuation.set(&prop, val);
        new_valuation.fix(&prop);
    }

    for (i, &val) in y_bitvec.iter().rev().enumerate() {
        let prop = Prop::from(&format!("y_{i}"));
        new_valuation.set(&prop, val);
        new_valuation.fix(&prop);
    }

    let mut out_vec = vec![false; max_size];

    match the_adder.all_sat_valuations_from(new_valuation).as_slice() {
        [v] => {
            (0..max_size).for_each(|i| {
                out_vec[max_size - (i + 1)] = v.get(&Prop::from(&format!("out_{i}")))
            });

            as_int(&out_vec)
        }

        valuations => {
            println!("Found {} valuations during ripple_add.", valuations.len());
            for valuation in valuations {
                println!("{valuation}");
            }

            panic!()
        }
    }
}

fn ripple_add_fm(x: usize, y: usize) -> PropFormula {
    let x_size = bits_needed(x);
    let y_size = bits_needed(y);
    let max_size: usize = std::cmp::max(
        x_size.try_into().ok().unwrap(),
        (y_size + 1).try_into().ok().unwrap(),
    );
    let x_bitvec = as_bits(x, Some(max_size.try_into().ok().unwrap()));
    let y_bitvec = as_bits(y, Some(max_size.try_into().ok().unwrap()));

    let the_adder = ripple_carry_0(
        one_index_atom("x"),
        one_index_atom("y"),
        one_index_atom("c"),
        one_index_atom("out"),
        max_size,
    );

    let x_fm = x_bitvec
        .iter()
        .rev()
        .enumerate()
        .fold(PropFormula::True, |acc, (n_i, n_b)| {
            let the_atom = PropFormula::Atom(Prop::from(&format!("x_{}", n_i)));
            let the_negate = the_atom.clone().negate();
            PropFormula::And(acc, if *n_b { the_atom } else { the_negate })
        });

    let y_fm = y_bitvec
        .iter()
        .rev()
        .enumerate()
        .fold(PropFormula::True, |acc, (n_i, n_b)| {
            let the_atom = PropFormula::Atom(Prop::from(&format!("y_{}", n_i)));
            let the_negate = the_atom.clone().negate();
            PropFormula::And(acc, if *n_b { the_atom } else { the_negate })
        });

    PropFormula::And(the_adder, PropFormula::And(x_fm, y_fm))
}

fn carry_add(x: usize, y: usize, k: usize) -> Option<usize> {
    let x_size = bits_needed(x);
    let y_size = bits_needed(y);
    let max_size: usize =
        std::cmp::max((x_size + 1).try_into().ok()?, (y_size + 1).try_into().ok()?);
    let x_bitvec = as_bits(x, Some(max_size.try_into().ok()?));
    let y_bitvec = as_bits(y, Some(max_size.try_into().ok()?));

    let the_carrier = carry_select(
        one_index_atom("x"),
        one_index_atom("y"),
        one_index_atom("c0"),
        one_index_atom("c1"),
        one_index_atom("s0"),
        one_index_atom("s1"),
        one_index_atom("c"),
        one_index_atom("s"),
        max_size,
        k,
    );

    let mut new_valuation = Valuation::from_prop_set(the_carrier.atoms());
    x_bitvec.iter().rev().enumerate().for_each(|(i, &val)| {
        let prop = Prop::from(&format!("x_{i}"));
        new_valuation.set(&prop, val);
        new_valuation.fix(&prop);
    });

    y_bitvec.iter().rev().enumerate().for_each(|(i, &val)| {
        let prop = Prop::from(&format!("y_{i}"));
        new_valuation.set(&prop, val);
        new_valuation.fix(&prop);
    });

    let mut outvec = vec![false; max_size];
    let vals = the_carrier.all_sat_valuations_from(new_valuation);

    if vals.len() != 1 {
        return None;
    }

    let v = vals.first()?;
    (0..max_size)
        .for_each(|i| outvec[max_size - (i + 1)] = v.get(&Prop::from(&format!("out_{i}"))));

    Some(as_int(&outvec))
}

/* Multiplier */

#[rustfmt::skip]
fn ripple_shift(
    u: impl Fn(usize) -> PropFormula,
    v: impl Fn(usize) -> PropFormula,
    c: impl Fn(usize) -> PropFormula,
    z: PropFormula,
    w: impl Fn(usize) -> PropFormula,
    n: usize,
) -> PropFormula {
    ripple_carry_0(
        u,
        v,
        |i| { if i == n { w(n - 1) } else { c(i + 1) } },
        |i| { if i == 0 { z.clone() } else { w(i - 1) } },
        n,
    )
}

fn multiplier(
    x: impl Fn(usize, usize) -> PropFormula,
    u: impl Fn(usize, usize) -> PropFormula,
    v: impl Fn(usize, usize) -> PropFormula,
    out: impl Fn(usize) -> PropFormula,
    n: usize,
) -> PropFormula {
    // println!("n is: {n}");
    if n == 1 {
        PropFormula::And(PropFormula::Iff(out(0), x(0, 0)), out(1).negate())
    } else {
        let the_first_part = ripple_shift(
            |i| {
                if i == n - 1 {
                    PropFormula::False
                } else {
                    x(0, i + 1)
                }
            },
            |i| x(1, i),
            |i| v(2, i),
            out(1),
            |i| u(2, i),
            n,
        );
        let the_second_part = if n == 2 {
            PropFormula::And(
                PropFormula::Iff(out(2), u(2, 0)),
                PropFormula::Iff(out(3), u(2, 1)),
            )
        } else {
            (2..n).fold(PropFormula::True, |acc, k| {
                // println!("fold {k}");
                let this = ripple_shift(
                    |i| u(k, i),
                    |i| x(k, i),
                    |i| v(k + 1, i),
                    out(k),
                    |i| if k == n - 1 { out(n + i) } else { u(k + 1, i) },
                    n,
                );
                PropFormula::And(acc, this)
            })
        };
        PropFormula::And(
            PropFormula::Iff(out(0), x(0, 0)),
            PropFormula::And(the_first_part, the_second_part),
        )
        .simplify()
    }
}

fn ripple_multiply(x: usize, y: usize) -> usize {
    let m_bits_needed = bits_needed(x * y);
    let max_size = std::cmp::max(m_bits_needed, 2 * m_bits_needed - 2);

    let x_expr = &one_index_atom("x");
    let y_expr = &one_index_atom("y");

    let the_muler = multiplier(
        |i, j| PropFormula::And(x_expr(i), y_expr(j)),
        two_index_atom("u"),
        two_index_atom("v"),
        one_index_atom("out"),
        max_size as usize,
    );

    let mut mul_valuation = Valuation::from_prop_set(the_muler.atoms());

    as_bits(x, Some(max_size))
        .iter()
        .rev()
        .enumerate()
        .for_each(|(i, &val)| {
            let prop = Prop::from(&format!("x_{i}"));
            mul_valuation.set(&prop, val);
            mul_valuation.fix(&prop);
        });

    as_bits(y, Some(max_size))
        .iter()
        .rev()
        .enumerate()
        .for_each(|(i, &val)| {
            let prop = Prop::from(&format!("y_{i}"));
            mul_valuation.set(&prop, val);
            mul_valuation.fix(&prop);
        });

    let mut out_vec = vec![false; max_size as usize];

    match the_muler.all_sat_valuations_from(mul_valuation).as_slice() {
        [v] => {
            (0..max_size).rev().for_each(|i| {
                out_vec[(max_size - (i + 1)) as usize] = v.get(&Prop::from(&format!("out_{i}")));
            });
            as_int(&out_vec)
        }

        valuations => {
            println!("Found {} valuations during ripple_add.", valuations.len());
            for valuation in valuations {
                println!("{valuation}");
            }

            panic!()
        }
    }
}

fn conguruent_to(x: impl Fn(usize) -> PropFormula, m: usize, n: usize) -> PropFormula {
    PropFormula::conjoin(as_bits(m, Some(n as u32)).iter().rev().enumerate().map(
        |(i, b)| match b {
            true => x(i),
            false => x(i).negate(),
        },
    ))
}

/* Primer */

fn prime(p: usize) -> PropFormula {
    let x = one_index_atom("x");
    let y = one_index_atom("y");
    let out = one_index_atom("out");
    let u = two_index_atom("u");
    let v = two_index_atom("v");
    let n = bits_needed(p) as usize;

    PropFormula::And(
        multiplier(|i, j| PropFormula::And(x(i), y(j)), u, v, &out, n - 1),
        conguruent_to(out, p, std::cmp::max(n, 2 * n - 2)),
    )
    .negate()
}

fn main() {
    let x = ripple_carry(
        one_index_atom("x"),
        one_index_atom("y"),
        one_index_atom("c"),
        one_index_atom("out"),
        2,
    );
    println!("{x}");

    assert_eq!(3, ripple_add(1, 2));

    assert_eq!(38, ripple_add(15, 23));

    let x = 157;
    let y = 23;
    println!("{x} + {y} = {}", ripple_add(x, y));

    let x = 3;
    let y = 2;
    println!("{x} * {y} = {}", ripple_multiply(x, y));

    for i in 1..14 {
        println!("It is {} that {i} is prime", prime(i).tautology());
    }
}
