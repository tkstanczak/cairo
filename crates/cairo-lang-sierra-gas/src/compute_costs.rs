// TODO:
// 1. Other gas tokens
// 2. AP
// 3. Withdraw gas builtins.
// 4. refund gas

use std::collections::hash_map;
use std::ops::{Add, Sub};

use cairo_lang_sierra::extensions::gas::{BuiltinCostWithdrawGasLibfunc, CostTokenType};
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{BranchInfo, Invocation, Program, Statement, StatementIdx};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::iterators::zip_eq3;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::zip_eq;

use crate::gas_info::GasInfo;
use crate::objects::{BranchCost, ConstCost, PreCost};

#[cfg(test)]
#[path = "compute_costs_test.rs"]
mod test;

type VariableValues = OrderedHashMap<(StatementIdx, CostTokenType), i64>;

/// A trait for the cost type (either [PreCost] for pre-cost computation, or `i32` for the post-cost
/// computation).
pub trait CostTypeTrait:
    std::fmt::Debug + Default + Clone + Eq + Add<Output = Self> + Sub<Output = Self>
{
    fn max(values: impl Iterator<Item = Self>) -> Self;
}

impl CostTypeTrait for i32 {
    fn max(values: impl Iterator<Item = Self>) -> Self {
        values.max().unwrap_or_default()
    }
}

impl CostTypeTrait for PreCost {
    fn max(values: impl Iterator<Item = Self>) -> Self {
        let mut res = Self::default();
        for value in values {
            for (token_type, val) in value.0 {
                res.0.insert(token_type, std::cmp::max(*res.0.get(&token_type).unwrap_or(&0), val));
            }
        }
        res
    }
}

/// Computes the [GasInfo] for a given program.
///
/// The `specific_cost_context` argument controls whether the computation is pre-cost or post-cost.
pub fn compute_costs<
    CostType: CostTypeTrait,
    SpecificCostContext: SpecificCostContextTrait<CostType>,
>(
    program: &Program,
    get_cost_fn: &dyn Fn(&ConcreteLibfuncId) -> Vec<BranchCost>,
    specific_cost_context: &SpecificCostContext,
) -> GasInfo {
    let mut context = CostContext {
        program,
        costs: UnorderedHashMap::default(),
        get_cost_fn,
        cost_vars_values: UnorderedHashMap::default(),
    };

    for i in 0..program.statements.len() {
        context.prepare_wallet_at(&StatementIdx(i), specific_cost_context);
    }

    let mut variable_values = VariableValues::default();
    for i in 0..program.statements.len() {
        analyze_gas_statements(
            &context,
            specific_cost_context,
            &StatementIdx(i),
            &mut variable_values,
        );
    }

    let function_costs = program
        .funcs
        .iter()
        .map(|func| {
            let res = SpecificCostContext::to_cost_map(
                context.wallet_at(&func.entry_point).get_pure_value(),
            );
            (func.id.clone(), res)
        })
        .collect();

    GasInfo { variable_values, function_costs }
}

/// Returns the statements whose wallet value is needed by
/// [get_branch_requirements].
fn get_branch_requirements_dependencies(
    idx: &StatementIdx,
    invocation: &Invocation,
    libfunc_cost: &[BranchCost],
) -> OrderedHashSet<StatementIdx> {
    let mut res: OrderedHashSet<StatementIdx> = Default::default();
    for (branch_info, branch_cost) in zip_eq(&invocation.branches, libfunc_cost) {
        match branch_cost {
            BranchCost::FunctionCall { const_cost: _, function } => {
                res.insert(function.entry_point);
            }
            BranchCost::WithdrawGas { const_cost: _, success: true, with_builtin_costs: _ } => {
                // If withdraw_gas succeeds, we don't need to take future_wallet_value into account,
                // so we simply return.
                continue;
            }
            _ => {}
        }
        res.insert(idx.next(&branch_info.target));
    }

    res
}

/// Returns the required value for the wallet for each branch.
fn get_branch_requirements<
    CostType: CostTypeTrait,
    SpecificCostContext: SpecificCostContextTrait<CostType>,
>(
    specific_context: &SpecificCostContext,
    wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<CostType>,
    idx: &StatementIdx,
    invocation: &Invocation,
    libfunc_cost: &[BranchCost],
) -> Vec<WalletInfo<CostType>> {
    zip_eq(&invocation.branches, libfunc_cost)
        .map(|(branch_info, branch_cost)| {
            specific_context.get_branch_requirement(wallet_at_fn, idx, branch_info, branch_cost)
        })
        .collect()
}

/// For every `branch_align` and `withdraw_gas` statements, computes the required cost variables.
///
/// * For `branch_align` this is the amount of cost *reduced* from the wallet.
/// * For `withdraw_gas` this is the amount that should be withdrawn and added to the wallet.
fn analyze_gas_statements<
    CostType: CostTypeTrait,
    SpecificCostContext: SpecificCostContextTrait<CostType>,
>(
    context: &CostContext<'_, CostType>,
    specific_context: &SpecificCostContext,
    idx: &StatementIdx,
    variable_values: &mut VariableValues,
) {
    let Statement::Invocation(invocation) = &context.program.get_statement(idx).unwrap() else {
            return;
        };
    let libfunc_cost: Vec<BranchCost> = context.get_cost(&invocation.libfunc_id);
    let branch_requirements: Vec<WalletInfo<CostType>> = get_branch_requirements(
        specific_context,
        &|statement_idx| context.wallet_at(statement_idx),
        idx,
        invocation,
        &libfunc_cost,
    );

    let wallet_value = context.wallet_at(idx).get_value(&context.cost_vars_values);

    if invocation.branches.len() > 1 {
        for (branch_info, branch_cost, branch_requirement) in
            zip_eq3(&invocation.branches, &libfunc_cost, &branch_requirements)
        {
            let future_wallet_value = context
                .wallet_at(&idx.next(&branch_info.target))
                .get_value(&context.cost_vars_values);
            // TODO(lior): Consider checking that idx.next(&branch_info.target) is indeed branch
            //   align.
            if let BranchCost::WithdrawGas { success: true, .. } = branch_cost {
                for (token_type, amount) in specific_context.get_withdraw_gas_values(
                    idx,
                    branch_cost,
                    &wallet_value,
                    future_wallet_value,
                ) {
                    assert_eq!(
                        variable_values.insert((*idx, token_type), std::cmp::max(amount, 0)),
                        None
                    );

                    assert_eq!(
                        variable_values.insert(
                            (idx.next(&branch_info.target), token_type),
                            std::cmp::max(-amount, 0),
                        ),
                        None
                    );
                }
            } else {
                for (token_type, amount) in specific_context.get_branch_align_values(
                    &wallet_value,
                    &branch_requirement.get_value(&context.cost_vars_values),
                ) {
                    assert_eq!(
                        variable_values.insert((idx.next(&branch_info.target), token_type), amount),
                        None
                    );
                }
            }
        }
    } else if let Some(CostVar(value)) = context.cost_vars_values.get(idx) {
        // TODO: handle unassigned.
        for (token_type, amount) in SpecificCostContext::to_cost_map(value.clone()) {
            assert_eq!(variable_values.insert((*idx, token_type), amount), None);
        }
    }
}

pub trait SpecificCostContextTrait<CostType: CostTypeTrait> {
    /// Converts a `CostType` to a [OrderedHashMap] from [CostTokenType] to i64.
    fn to_cost_map(cost: CostType) -> OrderedHashMap<CostTokenType, i64>;

    /// Computes the value that should be withdrawn and added to the wallet for each token type.
    fn get_withdraw_gas_values(
        &self,
        idx: &StatementIdx,
        branch_cost: &BranchCost,
        wallet_value: &CostType,
        future_wallet_value: CostType,
    ) -> OrderedHashMap<CostTokenType, i64>;

    /// Computes the value that should be reduced from the wallet for each token type.
    fn get_branch_align_values(
        &self,
        wallet_value: &CostType,
        branch_requirement: &CostType,
    ) -> OrderedHashMap<CostTokenType, i64>;

    /// Returns the required value for the wallet for a single branch.
    // TODO: Consider changing get to create and verifying the variable didn't exist before.
    fn get_branch_requirement(
        &self,
        wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<CostType>,
        idx: &StatementIdx,
        branch_info: &BranchInfo,
        branch_cost: &BranchCost,
    ) -> WalletInfo<CostType>;
}

/// The information about the wallet value at a given statement.
#[derive(Clone, Debug, Default)]
pub struct WalletInfo<CostType: CostTypeTrait> {
    /// The minimum wallet value before executing the statement.
    value: CostType,
    /// An optional list of cost variables (see [CostContext::cost_vars]) representing what should
    /// be added to the wallet value.
    cost_vars: OrderedHashSet<StatementIdx>,
}

impl<CostType: CostTypeTrait> WalletInfo<CostType> {
    fn merge(
        branches: Vec<Self>,
        cost_vars_values: &mut UnorderedHashMap<StatementIdx, CostVar<CostType>>,
    ) -> Self {
        let n_branches = branches.len();
        let max_value = CostType::max(branches.iter().map(|wallet_info| wallet_info.value.clone()));

        // A map from unassigned variables (StatementIdx) to the number of branches that use it.
        let mut var_count: OrderedHashMap<StatementIdx, usize> = Default::default();

        // Go over the branches, handle assigned variables, and count unassigned variables.
        let branches: Vec<Self> = branches
            .into_iter()
            .map(|WalletInfo { value: mut wallet_info_value, cost_vars }| {
                let cost_vars = cost_vars
                    .into_iter()
                    .filter(|cost_var| {
                        if let Some(CostVar(value)) = cost_vars_values.get(cost_var) {
                            // This variable was already assigned.
                            wallet_info_value = wallet_info_value.clone() + value.clone();
                            false
                        } else {
                            *var_count.entry(*cost_var).or_insert(0) += 1;
                            true
                        }
                    })
                    .collect();
                WalletInfo { value: wallet_info_value, cost_vars }
            })
            .collect();
        println!("branches: {branches:?}");

        for wallet_info in &branches {
            let mut assigned_diff = false;
            for cost_var in &wallet_info.cost_vars {
                let count = var_count[*cost_var];
                if count == 1 && count < n_branches {
                    // This variable appears only in this branch. Use it for the difference between
                    // the branch and the max value.
                    assert!(
                        !assigned_diff,
                        "Found multiple unassigned variables, which is not supported yet."
                    );
                    println!("Assign {cost_var} to diff"); // TODO: remove.
                    cost_vars_values
                        .insert(*cost_var, CostVar(max_value.clone() - wallet_info.value.clone()));
                    assigned_diff = true;
                }
            }
        }

        // The set of variables that appear in all branches.
        let mut intersection: OrderedHashSet<StatementIdx> = Default::default();

        for (cost_var, count) in var_count {
            if count == branches.len() {
                // All branches have this variable.
                intersection.insert(cost_var);
            } else if let hash_map::Entry::Vacant(entry) = cost_vars_values.entry(cost_var) {
                println!("Assign {cost_var} to diff");
                // This variable must be assigned. Set it to zero.
                entry.insert(CostVar(Default::default()));
            }
        }

        WalletInfo { value: max_value, cost_vars: intersection }
    }

    /// Returns the value after assigning the cost variables. All used cost variables must be
    /// assigned before calling this function.
    fn get_value(
        &self,
        cost_vars_values: &UnorderedHashMap<StatementIdx, CostVar<CostType>>,
    ) -> CostType {
        itertools::fold(&self.cost_vars, self.value.clone(), |sum, cost_var| {
            let Some(CostVar(value)) = cost_vars_values.get(&cost_var) else {
                panic!("Encountered unassigned cost variable for statement {cost_var}");
            };
            sum + value.clone()
        })
    }

    /// Returns the value, assuming there are no used cost variables (panics otherwise).
    fn get_pure_value(self) -> CostType {
        assert!(self.cost_vars.is_empty(), "Encountered unexpected cost variables.");
        self.value
    }
}

/// Implements a cast from CostType to WalletInfo.
impl<CostType: CostTypeTrait> From<CostType> for WalletInfo<CostType> {
    fn from(value: CostType) -> Self {
        WalletInfo { value, cost_vars: Default::default() }
    }
}

/// Implements addition of WalletInfo.
impl<CostType: CostTypeTrait> std::ops::Add for WalletInfo<CostType> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        WalletInfo {
            value: self.value + other.value,
            cost_vars: self.cost_vars.disjoint_union(&other.cost_vars),
        }
    }
}

/// Represents a variable wallet value. Such variables are created by specific libfuncs
/// (such as `withdraw_gas()` and `redeposit_gas()`) that can take a variable amount of gas.
///
/// The variable values are maintained in [CostContext::cost_vars].
#[derive(Debug)]
struct CostVar<CostType>(CostType);

/// Represents the status of the computation of the wallet at a given statement.
enum CostComputationStatus<CostType: CostTypeTrait> {
    /// The computation is in progress.
    InProgress,
    /// The computation was completed.
    Done(WalletInfo<CostType>),
}

/// Helper struct for computing the wallet value at each statement.
struct CostContext<'a, CostType: CostTypeTrait> {
    /// The Sierra program.
    program: &'a Program,
    /// A callback function returning the cost of a libfunc for every output branch.
    get_cost_fn: &'a dyn Fn(&ConcreteLibfuncId) -> Vec<BranchCost>,
    /// The cost before executing a Sierra statement.
    costs: UnorderedHashMap<StatementIdx, CostComputationStatus<CostType>>,
    /// A map from a statement to the [CostVar] variable it is responsible of, if exists.
    /// See [CostVar].
    cost_vars_values: UnorderedHashMap<StatementIdx, CostVar<CostType>>,
}
impl<'a, CostType: CostTypeTrait> CostContext<'a, CostType> {
    /// Returns the cost of a libfunc for every output branch.
    fn get_cost(&self, libfunc_id: &ConcreteLibfuncId) -> Vec<BranchCost> {
        (self.get_cost_fn)(libfunc_id)
    }

    /// Returns the required value in the wallet before executing statement `idx`.
    ///
    /// Assumes that [Self::prepare_wallet_at] was called before.
    ///
    /// For `branch_align` the function returns the result as if the alignment is zero (since the
    /// alignment is not know at this point).
    fn wallet_at(&self, idx: &StatementIdx) -> WalletInfo<CostType> {
        match self.costs.get(idx) {
            Some(CostComputationStatus::Done(res)) => res.clone(),
            _ => {
                panic!("Wallet value for statement {idx} was not yet computed.")
            }
        }
    }

    /// Prepares the values for [Self::wallet_at].
    fn prepare_wallet_at<SpecificCostContext: SpecificCostContextTrait<CostType>>(
        &mut self,
        idx: &StatementIdx,
        specific_cost_context: &SpecificCostContext,
    ) {
        // A stack of statements to call `no_cache_compute_wallet_at()` on.
        let mut statements_to_visit = vec![*idx];

        while let Some(current_idx) = statements_to_visit.last() {
            // Check the current status of the computation.
            match self.costs.get(current_idx) {
                Some(CostComputationStatus::InProgress) => {
                    // The computation of the dependencies was completed.
                    let res = self.no_cache_compute_wallet_at(current_idx, specific_cost_context);
                    println!("Cost of {current_idx} is {res:?}."); // TODO: Remove this line.
                    // Update the cache with the result.
                    self.costs.insert(*current_idx, CostComputationStatus::Done(res.clone()));

                    // Remove `idx` from `statements_to_visit`.
                    statements_to_visit.pop();
                    continue;
                }
                Some(CostComputationStatus::Done(_)) => {
                    // Remove `idx` from `statements_to_visit`.
                    statements_to_visit.pop();
                    continue;
                }
                None => (),
            }

            // Mark the statement's computation as in-progress.
            self.costs.insert(*current_idx, CostComputationStatus::InProgress);

            // Keep the current statement in the stack, and add the missing dependencies on top of
            // it.
            match &self.program.get_statement(current_idx).unwrap() {
                // Return has no dependencies.
                Statement::Return(_) => {}
                Statement::Invocation(invocation) => {
                    let libfunc_cost: Vec<BranchCost> = self.get_cost(&invocation.libfunc_id);

                    let missing_dependencies = get_branch_requirements_dependencies(
                        current_idx,
                        invocation,
                        &libfunc_cost,
                    )
                    .into_iter()
                    .filter(|dep| match self.costs.get(dep) {
                        None => true,
                        Some(CostComputationStatus::Done(_)) => false,
                        Some(CostComputationStatus::InProgress) => {
                            panic!("Found an unexpected cycle during cost computation.");
                        }
                    });
                    statements_to_visit.extend(missing_dependencies);
                }
            };
        }
    }

    /// Helper function for `prepare_wallet_at()`.
    ///
    /// Assumes that the values was already computed for the dependencies.
    fn no_cache_compute_wallet_at<SpecificCostContext: SpecificCostContextTrait<CostType>>(
        &mut self,
        idx: &StatementIdx,
        specific_cost_context: &SpecificCostContext,
    ) -> WalletInfo<CostType> {
        match &self.program.get_statement(idx).unwrap() {
            Statement::Return(_) => Default::default(),
            Statement::Invocation(invocation) => {
                let libfunc_cost: Vec<BranchCost> = self.get_cost(&invocation.libfunc_id);

                for dependency in
                    get_branch_requirements_dependencies(idx, invocation, &libfunc_cost)
                {
                    self.prepare_wallet_at(&dependency, specific_cost_context);
                }

                // For each branch, compute the required value for the wallet.
                let branch_requirements: Vec<WalletInfo<CostType>> = get_branch_requirements(
                    specific_cost_context,
                    &|statement_idx| self.wallet_at(statement_idx),
                    idx,
                    invocation,
                    &libfunc_cost,
                );

                // The wallet value at the beginning of the statement is the maximal value
                // required by all the branches.
                WalletInfo::merge(branch_requirements, &mut self.cost_vars_values)
            }
        }
    }
}

pub struct PreCostContext {}

impl SpecificCostContextTrait<PreCost> for PreCostContext {
    fn to_cost_map(cost: PreCost) -> OrderedHashMap<CostTokenType, i64> {
        let res = cost.0;
        res.into_iter().map(|(token_type, val)| (token_type, val as i64)).collect()
    }

    fn get_withdraw_gas_values(
        &self,
        _idx: &StatementIdx,
        _branch_cost: &BranchCost,
        wallet_value: &PreCost,
        future_wallet_value: PreCost,
    ) -> OrderedHashMap<CostTokenType, i64> {
        let res = (future_wallet_value - wallet_value.clone()).0;
        CostTokenType::iter_precost()
            .map(|token_type| (*token_type, *res.get(token_type).unwrap_or(&0) as i64))
            .collect()
    }

    fn get_branch_align_values(
        &self,
        wallet_value: &PreCost,
        branch_requirement: &PreCost,
    ) -> OrderedHashMap<CostTokenType, i64> {
        let res = (wallet_value.clone() - branch_requirement.clone()).0;
        CostTokenType::iter_precost()
            .map(|token_type| (*token_type, *res.get(token_type).unwrap_or(&0) as i64))
            .collect()
    }

    fn get_branch_requirement(
        &self,
        wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<PreCost>,
        idx: &StatementIdx,
        branch_info: &BranchInfo,
        branch_cost: &BranchCost,
    ) -> WalletInfo<PreCost> {
        let branch_cost = match branch_cost {
            BranchCost::Regular { const_cost: _, pre_cost } => pre_cost.clone(),
            BranchCost::BranchAlign => Default::default(),
            BranchCost::FunctionCall { const_cost: _, function } => {
                wallet_at_fn(&function.entry_point).get_pure_value()
            }
            BranchCost::WithdrawGas { const_cost: _, success, with_builtin_costs: _ } => {
                if *success {
                    // If withdraw_gas succeeds, we don't need to take
                    // future_wallet_value into account, so we simply return.
                    return Default::default();
                } else {
                    Default::default()
                }
            }
            BranchCost::RedepositGas => Default::default(),
        };
        let future_wallet_value = wallet_at_fn(&idx.next(&branch_info.target));
        WalletInfo::from(branch_cost) + future_wallet_value
    }
}

pub struct PostcostContext<'a> {
    pub get_ap_change_fn: &'a dyn Fn(&StatementIdx) -> usize,
    pub precost_gas_info: &'a GasInfo,
}

impl<'a> SpecificCostContextTrait<i32> for PostcostContext<'a> {
    fn to_cost_map(cost: i32) -> OrderedHashMap<CostTokenType, i64> {
        if cost == 0 {
            Default::default()
        } else {
            [(CostTokenType::Const, cost as i64)].into_iter().collect()
        }
    }

    fn get_withdraw_gas_values(
        &self,
        idx: &StatementIdx,
        branch_cost: &BranchCost,
        wallet_value: &i32,
        future_wallet_value: i32,
    ) -> OrderedHashMap<CostTokenType, i64> {
        let BranchCost::WithdrawGas { const_cost, success: true, with_builtin_costs } = branch_cost else {
            panic!("Unexpected BranchCost: {:?}.", branch_cost);
        };

        let mut amount =
            ((const_cost.cost() + future_wallet_value) as i64) - (*wallet_value as i64);

        if *with_builtin_costs {
            let steps = BuiltinCostWithdrawGasLibfunc::cost_computation_steps(|token_type| {
                self.precost_gas_info.variable_values[(*idx, token_type)].into_or_panic()
            })
            .into_or_panic::<i32>();
            amount += ConstCost { steps, ..Default::default() }.cost().into_or_panic::<i64>();
        }

        [(CostTokenType::Const, amount)].into_iter().collect()
    }

    fn get_branch_align_values(
        &self,
        wallet_value: &i32,
        branch_requirement: &i32,
    ) -> OrderedHashMap<CostTokenType, i64> {
        let amount = (wallet_value - branch_requirement) as i64;
        [(CostTokenType::Const, amount as i64)].into_iter().collect()
    }

    fn get_branch_requirement(
        &self,
        wallet_at_fn: &dyn Fn(&StatementIdx) -> WalletInfo<i32>,
        idx: &StatementIdx,
        branch_info: &BranchInfo,
        branch_cost: &BranchCost,
    ) -> WalletInfo<i32> {
        let branch_cost_val = match &*branch_cost {
            BranchCost::Regular { const_cost, pre_cost: _ } => const_cost.cost(),
            BranchCost::BranchAlign => {
                let ap_change = (self.get_ap_change_fn)(idx);
                if ap_change == 0 {
                    0
                } else {
                    ConstCost { steps: 1, holes: ap_change as i32, range_checks: 0 }.cost()
                }
            }
            BranchCost::FunctionCall { const_cost, function } => {
                wallet_at_fn(&function.entry_point).get_pure_value() + const_cost.cost()
            }
            BranchCost::WithdrawGas { const_cost, success, with_builtin_costs } => {
                let mut cost = const_cost.cost();

                if *with_builtin_costs {
                    // TODO(lior): Avoid code duplication with get_withdraw_gas_values.
                    let steps =
                        BuiltinCostWithdrawGasLibfunc::cost_computation_steps(|token_type| {
                            self.precost_gas_info.variable_values[(*idx, token_type)]
                                .into_or_panic()
                        })
                        .into_or_panic::<i32>();
                    cost += ConstCost { steps, ..Default::default() }.cost();
                }

                // If withdraw_gas succeeds, we don't need to take
                // future_wallet_value into account, so we simply return.
                if *success {
                    return WalletInfo::from(cost);
                }
                cost
            }
            BranchCost::RedepositGas => 0,
        };
        let new_variables: OrderedHashSet<StatementIdx> = match &*branch_cost {
            BranchCost::RedepositGas => [*idx].into_iter().collect(),
            _ => Default::default(),
        };
        let future_wallet_value = wallet_at_fn(&idx.next(&branch_info.target));
        WalletInfo { value: branch_cost_val, cost_vars: new_variables } + future_wallet_value
    }
}
