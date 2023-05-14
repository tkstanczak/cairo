use std::fs;
use std::ops::DerefMut;
use std::path::PathBuf;
use std::sync::Mutex;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::{Function, Program};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_test_utils::test_lock;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use once_cell::sync::Lazy;

use crate::{calc_gas_postcost_info, calc_gas_precost_info, compute_postcost_info};

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
static SHARED_DB: Lazy<Mutex<RootDatabase>> =
    Lazy::new(|| Mutex::new(RootDatabase::builder().detect_corelib().build().unwrap()));

cairo_lang_test_utils::test_file_test!(
    test_solve_gas,
    "src/test_data",
    {
        redeposit :"redeposit",
        fib_jumps :"fib_jumps",
    },
    test_solve_gas
);

/// Returns a parsed example program from the example directory.
fn get_example_program(name: &str) -> Program {
    // Pop the "/sierra_gas" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["cairo-lang-sierra", "examples", &format!("{name}.sierra")].into_iter());
    cairo_lang_sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

fn test_solve_gas(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let (program, sierra_program_str) = if let Some(path) = inputs.get("test_file_name") {
        (get_example_program(path), None)
    } else {
        let mut locked_db = test_lock(&SHARED_DB);

        // Parse code and create semantic model.
        let test_module =
            setup_test_module(locked_db.deref_mut(), inputs["cairo"].as_str()).unwrap();
        let db = locked_db.snapshot();
        DiagnosticsReporter::stderr().ensure(&db).unwrap();

        // Compile to Sierra.
        let sierra_program = db.get_sierra_program(vec![test_module.crate_id]).unwrap();
        let sierra_program = replace_sierra_ids_in_program(&db, &sierra_program);
        let sierra_program_str = sierra_program.to_string();

        (sierra_program, Some(sierra_program_str))
    };

    // let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new(&program).unwrap();

    let enforced_costs = if let Some(enforced_costs_str) = inputs.get("enforced_costs") {
        enforced_costs_str
            .split('\n')
            .map(|line| {
                // line is the name of the function and the enforced cost, separated by a space.
                let mut words = line.split(' ');
                let name = words.next().unwrap();
                // Read the next word as a u64.
                let cost = words.next().unwrap();

                assert!(
                    words.next().is_none(),
                    "Expected a line of the form '<function name> <cost>'. Found: '{line}'."
                );

                // Get the function id from the name by searching program.funcs.
                let function_id = program
                    .funcs
                    .iter()
                    .find_map(|Function { id, .. }| {
                        if id.debug_name.as_ref().unwrap() == &name { Some(id) } else { None }
                    })
                    .unwrap_or_else(|| panic!("Function {name} was not found."));

                (
                    function_id.clone(),
                    cost.parse::<i32>()
                        .unwrap_or_else(|_| panic!("Expected a number as the enforced cost.")),
                )
            })
            .collect::<OrderedHashMap<_, _>>()
    } else {
        Default::default()
    };

    println!("enforced_costs: {:?}", enforced_costs);

    let gas_info0 = calc_gas_precost_info(&program, Default::default()).unwrap();
    let gas_info1 = compute_postcost_info(&program, &|_| 0, &gas_info0, &enforced_costs).unwrap();
    let gas_info = gas_info0.combine(gas_info1);

    let mut res = OrderedHashMap::from([("gas_solution".into(), format!("{gas_info}"))]);
    if let Some(sierra_program_str) = sierra_program_str {
        res.insert("sierra_program".into(), sierra_program_str);
    }
    res
}
