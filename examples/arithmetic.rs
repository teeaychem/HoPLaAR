#![allow(dead_code)]

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

fn as_bits(mut n: usize, bit_count: Option<u32>) -> Vec<bool> {
    let bits_needed = match bit_count {
        Some(request) => request,
        None => bits_needed(n),
    };

    let mut bit_vec = vec![false; bits_needed as usize];

    for idx in (0..bits_needed).rev() {
        if 2_usize.pow(idx) <= n {
            bit_vec[(bits_needed - (idx + 1)) as usize] = true;
            n -= 2_usize.pow(idx);
        }
    }

    bit_vec
}

fn as_int(bits: &[bool]) -> usize {
    let mut total = 0;
    for (idx, &bit) in bits.iter().rev().enumerate() {
        if bit {
            total += 2_usize.pow(idx as u32)
        }
    }
    total
}

fn one_index_atom(id: &str) -> impl Fn(usize) -> PropFormula {
    move |i| PropFormula::Atom(Prop::from(&format!("{id}_{i}")))
}

fn two_index_atom(id: &str) -> impl Fn(usize, usize) -> PropFormula {
    move |i, j| PropFormula::Atom(Prop::from(&format!("{id}_{i}_{j}")))
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

fn ripple_carry<X, Y, C, O>(x: X, y: Y, c: C, out: O, n: usize) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
    Y: Fn(usize) -> PropFormula,
    C: Fn(usize) -> PropFormula,
    O: Fn(usize) -> PropFormula,
{
    PropFormula::conjoin((0..n).map(|i| full_adder(x(i), y(i), c(i), out(i), c(i + 1))))
}

fn ripple_carry_0<X, Y, C, O>(x: X, y: Y, c: C, out: O, n: usize) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
    Y: Fn(usize) -> PropFormula,
    C: Fn(usize) -> PropFormula,
    O: Fn(usize) -> PropFormula,
{
    let c = |i| {
        if i == 0 { PropFormula::False } else { c(i) }
    };

    ripple_carry(x, y, c, out, n)
}

fn ripple_carry_1<X, Y, C, O>(x: X, y: Y, c: C, out: O, n: usize) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
    Y: Fn(usize) -> PropFormula,
    C: Fn(usize) -> PropFormula,
    O: Fn(usize) -> PropFormula,
{
    let c = |i| {
        if i == 0 { PropFormula::True } else { c(i) }
    };

    ripple_carry(x, y, c, out, n)
}

fn mux(sel: PropFormula, in0: PropFormula, in1: PropFormula) -> PropFormula {
    PropFormula::Or(
        PropFormula::And(sel.clone().negate(), in0),
        PropFormula::And(sel, in1),
    )
}

#[allow(clippy::too_many_arguments)]
fn make_fm<X, Y, C0, C1, S0, S1, C, S>(
    x: X,
    y: Y,
    c0: C0,
    c1: C1,
    s0: S0,
    s1: S1,
    c: C,
    s: S,
    n: usize,
    k: usize,
) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
    Y: Fn(usize) -> PropFormula,
    C0: Fn(usize) -> PropFormula,
    C1: Fn(usize) -> PropFormula,
    S0: Fn(usize) -> PropFormula,
    S1: Fn(usize) -> PropFormula,
    C: Fn(usize) -> PropFormula,
    S: Fn(usize) -> PropFormula,
{
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

fn offset<X>(n: usize, x: X) -> impl Fn(usize) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
{
    move |i| x(n + i)
}

#[allow(clippy::too_many_arguments)]
fn carry_select<X, Y, C0, C1, S0, S1, C, S>(
    x: X,
    y: Y,
    c0: C0,
    c1: C1,
    s0: S0,
    s1: S1,
    c: C,
    s: S,
    mut n: usize,
    k: usize,
) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
    Y: Fn(usize) -> PropFormula,
    C0: Fn(usize) -> PropFormula,
    C1: Fn(usize) -> PropFormula,
    S0: Fn(usize) -> PropFormula,
    S1: Fn(usize) -> PropFormula,
    C: Fn(usize) -> PropFormula,
    S: Fn(usize) -> PropFormula,
{
    let mut k_n_min = std::cmp::min(n, k);

    let mut fm = make_fm(&x, &y, &c0, &c1, &s0, &s1, &c, &s, n, k);

    for loop_count in 0.. {
        if k_n_min < k {
            break;
        } else {
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

    let a = PropFormula::And(
        PropFormula::And(
            carry_select(&x, &y, c0, c1, s0, s1, &c, &s, n, k),
            c(0).negate(),
        ),
        ripple_carry_0(x, y, &c2, &s2, n),
    );

    let c_rhs = PropFormula::conjoin((0..n).map(|i| PropFormula::Iff(s(i), s2(i))));
    let c = PropFormula::And(PropFormula::Iff(c(n), c2(n)), c_rhs);

    PropFormula::Imp(a, c)
}

fn ripple_add(x: usize, y: usize) -> usize {
    let max_size = std::cmp::max(bits_needed(x), bits_needed(y) + 1);

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
            for i in 0..max_size {
                out_vec[max_size - (i + 1)] = v.get(&Prop::from(&format!("out_{i}")))
            }

            as_int(&out_vec)
        }

        valuations => {
            println!("Found {} valuations during ripple_add.", valuations.len());
            // for valuation in valuations {
            //     println!("{valuation}");
            // }
            panic!()
        }
    }
}

fn ripple_add_fm(x: usize, y: usize) -> PropFormula {
    let max_size = std::cmp::max(bits_needed(x), bits_needed(y) + 1);

    let x_bitvec = as_bits(x, Some(max_size));
    let y_bitvec = as_bits(y, Some(max_size));

    let max_size = max_size as usize;

    let the_adder = ripple_carry_0(
        one_index_atom("x"),
        one_index_atom("y"),
        one_index_atom("c"),
        one_index_atom("out"),
        max_size,
    );

    let x_conjuncts = x_bitvec.iter().rev().enumerate().map(|(n_i, n_b)| {
        let prop = Prop::from(&format!("x_{n_i}"));
        let atom = PropFormula::Atom(prop);
        if *n_b { atom } else { atom.negate() }
    });

    let x_fm = PropFormula::conjoin(x_conjuncts);

    let y_conjuncts = y_bitvec.iter().rev().enumerate().map(|(n_i, n_b)| {
        let prop = Prop::from(&format!("y_{n_i}"));
        let atom = PropFormula::Atom(prop);
        if *n_b { atom } else { atom.negate() }
    });

    let y_fm = PropFormula::conjoin(y_conjuncts);

    PropFormula::And(the_adder, PropFormula::And(x_fm, y_fm))
}

fn carry_add(x: usize, y: usize, k: usize) -> Option<usize> {
    let max_size = std::cmp::max(bits_needed(x), bits_needed(y)) + 1;

    let x_bitvec = as_bits(x, Some(max_size));
    let y_bitvec = as_bits(y, Some(max_size));

    let max_size = max_size as usize;

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
    let vals = the_carrier.all_sat_valuations_from(new_valuation);

    if vals.len() != 1 {
        return None;
    }

    let v = vals.first()?;
    for i in 0..max_size {
        out_vec[max_size - (i + 1)] = v.get(&Prop::from(&format!("out_{i}")))
    }

    Some(as_int(&out_vec))
}

/* Multiplier */

fn ripple_shift<U, V, C, W>(u: U, v: V, c: C, z: PropFormula, w: W, n: usize) -> PropFormula
where
    U: Fn(usize) -> PropFormula,
    V: Fn(usize) -> PropFormula,
    C: Fn(usize) -> PropFormula,
    W: Fn(usize) -> PropFormula,
{
    let c = |i| {
        if i == n { w(n - 1) } else { c(i + 1) }
    };

    let out = |i| {
        if i == 0 { z.clone() } else { w(i - 1) }
    };

    ripple_carry_0(u, v, c, out, n)
}

fn multiplier<X, U, V, O>(x: X, u: U, v: V, out: O, n: usize) -> PropFormula
where
    X: Fn(usize, usize) -> PropFormula,
    U: Fn(usize, usize) -> PropFormula,
    V: Fn(usize, usize) -> PropFormula,
    O: Fn(usize) -> PropFormula,
{
    if n == 1 {
        return PropFormula::And(PropFormula::Iff(out(0), x(0, 0)), out(1).negate());
    }

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

    if n == 2 {
        return PropFormula::And(
            PropFormula::Iff(out(2), u(2, 0)),
            PropFormula::Iff(out(3), u(2, 1)),
        );
    }

    let the_second_part = PropFormula::conjoin((2..n).map(|k| {
        ripple_shift(
            |i| u(k, i),
            |i| x(k, i),
            |i| v(k + 1, i),
            out(k),
            |i| if k == n - 1 { out(n + i) } else { u(k + 1, i) },
            n,
        )
    }));

    PropFormula::And(
        PropFormula::Iff(out(0), x(0, 0)),
        PropFormula::And(the_first_part, the_second_part),
    )
    .simplify()
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

    for (i, &val) in as_bits(x, Some(max_size)).iter().rev().enumerate() {
        let prop = Prop::from(&format!("x_{i}"));
        mul_valuation.set(&prop, val);
        mul_valuation.fix(&prop);
    }

    for (i, &val) in as_bits(y, Some(max_size)).iter().rev().enumerate() {
        let prop = Prop::from(&format!("y_{i}"));
        mul_valuation.set(&prop, val);
        mul_valuation.fix(&prop);
    }

    let max_size = max_size as usize;

    let mut out_vec = vec![false; max_size];

    match the_muler.all_sat_valuations_from(mul_valuation).as_slice() {
        [v] => {
            for i in 0..max_size {
                let prop = Prop::from(&format!("out_{i}"));
                out_vec[max_size - (i + 1)] = v.get(&prop);
            }
            as_int(&out_vec)
        }

        valuations => {
            println!("Found {} valuations during ripple_add.", valuations.len());
            // for valuation in valuations {
            //     println!("{valuation}");
            // }
            panic!()
        }
    }
}

fn conguruent_to<X>(x: X, m: usize, n: usize) -> PropFormula
where
    X: Fn(usize) -> PropFormula,
{
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
