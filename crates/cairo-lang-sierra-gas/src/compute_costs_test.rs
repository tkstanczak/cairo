use std::fs;
use std::path::PathBuf;

use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::lib_func;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{Invocation, Program, StatementIdx};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::SpecificCostContextTrait;
use crate::compute_costs::compute_costs;
use crate::objects::BranchCost;

cairo_lang_test_utils::test_file_test!(
    test_compute_costs,
    "src/test_data",
    {
        // fib_jumps :"fib_jumps",
    },
    test_compute_costs
);

fn dummy_get_cost(lib_func: &ConcreteLibfuncId) -> Vec<BranchCost> {
    todo!()
}

struct DummySpecificCostContext {}
impl SpecificCostContextTrait<i32> for DummySpecificCostContext {
    fn to_cost_map(cost: i32) -> OrderedHashMap<CostTokenType, i64> {
        todo!()
    }

    fn get_withdraw_gas_values(
        &self,
        _idx: &StatementIdx,
        branch_cost: &crate::objects::BranchCost,
        wallet_value: &i32,
        future_wallet_value: i32,
    ) -> OrderedHashMap<CostTokenType, i64> {
        todo!()
    }

    fn get_branch_align_values(
        &self,
        wallet_value: &i32,
        branch_requirement: &i32,
    ) -> OrderedHashMap<CostTokenType, i64> {
        todo!()
    }

    fn get_branch_requirements(
        &self,
        wallet_at_fn: &mut dyn FnMut(&StatementIdx) -> i32,
        idx: &StatementIdx,
        invocation: &Invocation,
        libfunc_cost: &[crate::objects::BranchCost],
    ) -> Vec<i32> {
        todo!()
    }
}

fn test_compute_costs(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let program = cairo_lang_sierra::ProgramParser::new().parse(&inputs["test_program"]).unwrap();

    let gas_info = compute_costs(&program, &dummy_get_cost, &DummySpecificCostContext {});

    OrderedHashMap::from([("gas_solution".into(), format!("{gas_info}"))])
}
