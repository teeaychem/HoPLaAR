use std::collections::HashMap;

use crate::logic::first_order::terms::Var;

pub type Valuation<Domain> = HashMap<Var, Domain>;
