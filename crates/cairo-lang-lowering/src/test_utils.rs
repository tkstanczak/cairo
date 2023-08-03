use std::sync::{Arc, Mutex};

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::db::{
    init_dev_corelib, init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use cairo_lang_syntax::node::db::{HasGreenInterner, SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::green::SyntaxInterner;
use cairo_lang_utils::Upcast;
use once_cell::sync::Lazy;

use crate::db::{LoweringDatabase, LoweringGroup};

#[salsa::database(
    LoweringDatabase,
    SemanticDatabase,
    DefsDatabase,
    ParserDatabase,
    SyntaxDatabase,
    FilesDatabase
)]
pub struct LoweringDatabaseForTesting {
    storage: salsa::Storage<LoweringDatabaseForTesting>,
    green_interner: Arc<SyntaxInterner>,
}
impl salsa::Database for LoweringDatabaseForTesting {}
impl salsa::ParallelDatabase for LoweringDatabaseForTesting {
    fn snapshot(&self) -> salsa::Snapshot<LoweringDatabaseForTesting> {
        salsa::Snapshot::new(LoweringDatabaseForTesting {
            storage: self.storage.snapshot(),
            green_interner: self.green_interner.clone(),
        })
    }
}
impl LoweringDatabaseForTesting {
    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> LoweringDatabaseForTesting {
        LoweringDatabaseForTesting {
            storage: self.storage.snapshot(),
            green_interner: self.green_interner.clone(),
        }
    }
}
pub static SHARED_DB: Lazy<Mutex<LoweringDatabaseForTesting>> = Lazy::new(|| {
    let mut res = LoweringDatabaseForTesting {
        storage: Default::default(),
        green_interner: Default::default(),
    };
    init_files_group(&mut res);
    res.set_semantic_plugins(get_default_plugins());
    let corelib_path = detect_corelib().expect("Corelib not found in default location.");
    init_dev_corelib(&mut res, corelib_path);
    Mutex::new(res)
});
impl Default for LoweringDatabaseForTesting {
    fn default() -> Self {
        SHARED_DB.lock().unwrap().snapshot()
    }
}
impl AsFilesGroupMut for LoweringDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl HasGreenInterner for LoweringDatabaseForTesting {
    fn get_interner(&self) -> &SyntaxInterner {
        &self.green_interner
    }
}
impl Upcast<dyn DefsGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for LoweringDatabaseForTesting {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
