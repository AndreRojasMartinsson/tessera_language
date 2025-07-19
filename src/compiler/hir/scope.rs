use bitflags::bitflags;
use gxhash::GxBuildHasher;
use indexmap::IndexMap;
use indextree::{Arena, NodeId};

use crate::{atoms::KwAtom, hir::utils::HirId};

pub type Scopes = Arena<Scope>;
pub type ScopeId = NodeId;

bitflags! {
    #[derive(Default, Debug, Clone)]
    pub struct ScopeFlags: u8 {
        const TOP = 1 << 0;
        const FUNCTION = 1 << 1;
        const VAR = Self::TOP.bits() | Self::FUNCTION.bits();
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub flags: ScopeFlags,

    pub lexical: IndexMap<KwAtom, HirId, GxBuildHasher>,
    /// Var declared names
    pub var: IndexMap<KwAtom, HirId, GxBuildHasher>,
    /// Function declarations
    pub function: IndexMap<KwAtom, HirId, GxBuildHasher>,
}

impl Scope {
    pub fn new(flags: ScopeFlags) -> Self {
        Self {
            flags,
            lexical: IndexMap::default(),
            var: IndexMap::default(),
            function: IndexMap::default(),
        }
    }
}

pub struct ScopeBuilder {
    scopes: Scopes,
    root_scope_id: ScopeId,
    current_scope_id: ScopeId,
}

impl ScopeBuilder {
    pub fn new() -> Self {
        let mut arena = Arena::new();
        let root_scope_id = arena.new_node(Scope::new(ScopeFlags::TOP));

        Self {
            scopes: arena,
            root_scope_id,
            current_scope_id: root_scope_id,
        }
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes[self.current_scope_id].get()
    }

    pub fn enter_scope(&mut self, flags: ScopeFlags) {
        let parent_scope = self.current_scope();

        let scope = Scope::new(flags);
        let new_scope_id = self.scopes.new_node(scope);
        self.current_scope_id.append(new_scope_id, &mut self.scopes);
        self.current_scope_id = new_scope_id;
    }

    pub fn leave_scope(&mut self) {
        self.current_scope_id = self.scopes[self.current_scope_id].parent().unwrap();
    }
}

impl Default for ScopeBuilder {
    fn default() -> Self {
        Self::new()
    }
}
